(library (sph upcoming)
  (export)
  (import
    (guile)
    (ice-9 regex)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph io)
    (sph io read-write)
    (sph lang config)
    (sph list)
    (sph number)
    (sph one)
    (sph record)
    (sph time)
    (sph time string)
    (sph time utc)
    (sph vector))

  (define doc-example-config
    "28.8 meeting weekday 1
     24 eat interval 10 duration 0.6
     72 sleep
     ,(or login-time 25) work end 55 weekday (1 2 3 4)
     \"2017-12-13\" test title \"test title\"
     \"2017-12-13 28.8\" test-2 end 30 title \"test-2 title\"")

  (define doc-config-syntax
    "configuration format
     line:event
       time id option/value ...
     time-date: string:\"yyyy-mm-dd\"
     time-day: integer:day-ks
     time: time-day/time-date/\"time-date time-day\"
     id: symbol
     options
       end: time
       duration: integer
       interval: integer
       interval-unit: seconds/id/(id ...)
       weekday: integer/(integer ...)
       title: string
       depends: id/(id ...)
       start-depends: id/(id ...)
     available variables
       login-time")

  (define sph-upcoming-description
    "define when events will occur and filter events.
     event definition supports most natural qualifiers \"each second day\" \"only mondays\"
     and a configuration file syntax with one event definition per line")

  (define time-regexp (make-regexp "^([0-9]{4}-[0-1][0-9]-[0-3][0-9])( [0-9.]+)?$"))
  (define default-depends (list (q day)))
  (define default-start-base (list (q day)))
  (define default-interval-unit (q day))
  (define default-duration 200)
  (define default-lookahead 1)
  (define-record event id start end data)
  (define (ks->s a) (inexact->exact (* 1000 a)))
  (define (s->ks a) (/ a 1000))
  (define (config-read-time-error data) (raise (pair (q invalid-time-format) data)))

  (define (config-read-time a c)
    (cond ((not a) (c #f #f #f)) ((number? a) (c #f (ks->s a) #f))
      ( (string? a)
        (let (m (regexp-exec time-regexp a))
          (if m
            (c (ns->s (utc-from-ymd (match:substring m 1)))
              (let (m-2 (match:substring m 2))
                (if m-2 (ks->s (string->number (string-drop m-2 1))) 0))
              #t)
            (config-read-time-error a))))
      (else (config-read-time-error a))))

  (define (config-read-times a c) (apply c (append-map (l (a) (config-read-time a list)) a)))

  (define (config-read-line a)
    (and-let*
      ( (line-list (rw-string->list get-datum a))
        (line-data (and (not (null? line-list)) (eval (list (q quasiquote) line-list) config-env))))
      (apply (l (time id . options) (apply alist (q id) id (q start) time options)) line-data)))

  (define (config-read path)
    (compact (call-with-input-file path (l (port) (port-lines-map config-read-line port)))))

  (define (map-offsets start end proc) (map-integers (+ 1 (- end start)) proc))

  (define* (event-day now offset-start #:optional offset-end)
    "integer [integer/false] -> list:event-series"
    (map-offsets offset-start (or offset-end offset-start)
      (l (offset)
        (let (start (+ (* offset duration-day) (ns->s (utc-start-day now))))
          (vector (q day) start (+ start duration-day))))))

  (define duration-day utc-seconds-day)
  (define duration-week (* 7 utc-seconds-day))

  (define (event-weekday-proc weekday-offset)
    (l* (now offset-start #:optional offset-end) "integer [integer/false] -> list:event-series"
      (map-offsets offset-start (or offset-end offset-start)
        (l (offset)
          (let
            (start
              (+ (* weekday-offset duration-day) (* offset duration-week)
                (ns->s (utc-start-week now))))
            (vector (q day) start (+ start duration-day)))))))

  (define default-event-by-id
    (ht-create-symbol day event-day
      weekday-1 (event-weekday-proc 0)
      weekday-2 (event-weekday-proc 1)
      weekday-3 (event-weekday-proc 2)
      weekday-4 (event-weekday-proc 3)
      weekday-5 (event-weekday-proc 4)
      weekday-6 (event-weekday-proc 5) weekday-7 (event-weekday-proc 6)))

  (define default-interval-units
    (ht-create-symbol day duration-day week duration-week kilosecond 1000 second 1))

  (define create-event-functions
    (let*
      ( (get-weekday-depends
          (l (weekday)
            (if (and weekday (or (integer? weekday) (every integer? weekday)))
              (map (l (a) (string->symbol (string-append "weekday-" (number->string a))))
                (any->list weekday))
              null)))
        (get-interval-seconds (l (interval unit units) (* interval (ht-ref units unit))))
        (create-event-function
          (l
            (id config-start config-end
              interval interval-unit duration start-base depends get-event-by-id interval-units data)
            "_ ... -> procedure"
            (config-read-times (list config-start config-end)
              (l (start-date start-relative start-full? end-date end-relative end-full?)
                (if start-full?
                  (let (start (+ start-date start-relative))
                    (const
                      (list
                        (vector id start
                          (if config-end
                            (if end-full? (+ end-date end-relative) (+ start-date end-relative))
                            (+ start duration))
                          data))))
                  ; given start time is relative to another event
                  (let (start-base-ef (map get-event-by-id (any->list start-base)))
                    (l* (now offset-start #:optional offset-end)
                      (append-map
                        (l (ef)
                          (let (base-events (ef now offset-start offset-end))
                            (append-map
                              (l (base-event)
                                (let*
                                  ( (base-start (vector-second base-event))
                                    (start (+ base-start start-relative))
                                    (end
                                      (and config-end
                                        (if end-full? (+ end-date end-relative)
                                          (+ base-start end-relative)))))
                                  (if interval
                                    (let
                                      ( (end (or end (vector-third base-event)))
                                        (interval
                                          (get-interval-seconds interval interval-unit
                                            interval-units)))
                                      (let loop ((start start))
                                        (if (> start end) null
                                          (pair (vector id start (+ start duration) data)
                                            (loop (+ interval start))))))
                                    (list
                                      (vector id start
                                        (or end (+ base-start start-relative duration)) data)))))
                              base-events)))
                        start-base-ef))))))))
        (filter-depends
          (l (ef depends get-event-series-by-id)
            "event-function (id ...) -> event-function
          remove events that fall outside the duration of their dependent events"
            (l* a
              (let
                (depends-series (append-map (l (id) (apply get-event-series-by-id id a)) depends))
                (filter
                  (l (event)
                    (let ((start (event-start event)) (end (event-end event)))
                      (any
                        (l (depend-event)
                          (and (<= (event-start depend-event) (event-start event))
                            (>= (event-end depend-event) (event-end event))))
                        depends-series)))
                  (apply ef a)))))))
      (l (parsed-config event-by-id interval-units c)
        (let*
          ( (get-event-by-id (ht-object event-by-id))
            (get-event-series-by-id
              (procedure->cached-procedure (l (id . a) (apply (get-event-by-id id) a)))))
          (c
            (map
              (l (a)
                (alist-bind a
                  (id start end interval interval-unit duration weekday depends start-base)
                  (let*
                    ( (depends
                        (append (get-weekday-depends weekday)
                          (or (and depends (any->list depends)) default-depends)))
                      (ef
                        (filter-depends
                          (create-event-function id start
                            end interval
                            (or interval-unit (q kilosecond))
                            (or (and duration (ks->s duration)) default-duration)
                            (or start-base default-start-base) depends
                            get-event-by-id interval-units a)
                          depends get-event-series-by-id)))
                    (ht-set! event-by-id id ef) ef)))
              parsed-config)
            event-by-id)))))

  (define login-time 23)
  (define config-path (string-append (getenv "HOME") "/.config/sph/upcoming"))
  (define config-env (current-module))
  (define now (utc-current))
  (define now-s (ns->s now))

  (define (upcoming-events config)
    (create-event-functions (config-read config) default-event-by-id
      default-interval-units
      (l (ef-list event-by-id)
        (list-sort-with-accessor < event-start
          (filter (l (a) (> (event-start a) now-s))
            (append-map (l (ef) (ef now 0 default-lookahead)) ef-list))))))

  (define (upcoming-next-in config)
    (and-let* ((event (first-or-false (upcoming-events config))))
      (list (event-id event) (s->ks (- (event-start event) now-s)))))

  ; next-downcounter
  ; next-downcounter-multiple
  ; current-events
  ; next-events
  (display-line (upcoming-next-in config-path)))
