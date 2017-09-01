(library (sph upcoming)
  (export
    event-data
    event-end
    event-id
    event-start
    upcoming
    upcoming-config-path
    upcoming-config-variables
    upcoming-events)
  (import
    (ice-9 regex)
    (rnrs eval)
    (rnrs io simple)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph io)
    (sph list)
    (sph record)
    (sph server)
    (sph time)
    (sph time string)
    (sph time utc)
    (sph two)
    (only (sph io read-write) rw-string->list)
    (only (sph one) procedure->cached-procedure))

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

  (define (config-eval line-data variables config-env)
    (let*
      ( (variable-names (map first variables))
        (variable-values (map (compose (l (a) (if (procedure? a) (a) a)) tail) variables))
        (template
          (eval (list (q lambda) variable-names (list (q quasiquote) line-data)) config-env)))
      (apply template variable-values)))

  (define (config-read-line-data a variable-names variable-values)
    (and-let*
      ((line-list (rw-string->list get-datum a)) (get-line-data (and (not (null? line-list)))))))

  (define (config-read path config-variables config-env)
    (let*
      ( (line-data
          (compact
            (call-with-input-file path
              (l (port) (remove null? (port-lines-map (l (a) (rw-string->list get-datum a)) port))))))
        (line-data (config-eval line-data config-variables config-env)))
      (map (l (a) (apply (l (time id . options) (apply alist (q id) id (q start) time options)) a))
        line-data)))

  (define (map-offsets start end proc) (map-integers (+ 1 (- end start)) proc))

  (define* (event-day now offset-start #:optional offset-end)
    "integer [integer/false] -> list:event-series"
    (map-offsets offset-start (or offset-end offset-start)
      (l (offset)
        (let (start (+ (* offset duration-day) (ns->s (utc-start-day now))))
          (vector (q day) start (+ start duration-day))))))

  (define (event-weekday-proc weekday-offset)
    (l* (now offset-start #:optional offset-end) "integer [integer/false] -> list:event-series"
      (map-offsets offset-start (or offset-end offset-start)
        (l (offset)
          (let
            (start
              (+ (* weekday-offset duration-day) (* offset duration-week)
                (ns->s (utc-start-week now))))
            (vector (q day) start (+ start duration-day)))))))

  (define create-event-functions
    (let*
      ( (get-weekday-depends
          (l (weekday)
            "integer/(integer ...) -> (symbol:id ...)
            get the default event ids for weekdays identified by numbers in the weekday option"
            (if (and weekday (or (integer? weekday) (every integer? weekday)))
              (map (l (a) (string->symbol (string-append "weekday-" (number->string a))))
                (any->list weekday))
              null)))
        (get-interval-seconds
          (l (interval unit units)
            "number symbol hashtable -> number
            get the second duration of one interval in the given unit"
            (* interval (ht-ref units unit))))
        (filter-depends
          (l (ef depends get-event-series-by-id)
            "event-function (symbol:id ...) procedure -> event-function
            remove events that fall outside the duration of all their dependent events"
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
                  (apply ef a))))))
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
                                  ( (base-start (event-start base-event))
                                    (start (+ base-start start-relative))
                                    (end
                                      (and config-end
                                        (if end-full? (+ end-date end-relative)
                                          (+ base-start end-relative)))))
                                  (if interval
                                    (let
                                      ( (end (or end (event-end base-event)))
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
        (create-event-functions
          (l (config event-functions get-event-by-id get-event-series-by-id interval-units)
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
                    (ht-set! event-functions id ef) ef)))
              config))))
      (l (config event-functions interval-units c)
        (let*
          ( (event-functions (ht-copy event-functions #t))
            (get-event-by-id (ht-object event-functions))
            (get-event-series-by-id
              (procedure->cached-procedure (l (id . a) (apply (get-event-by-id id) a)))))
          (c
            (create-event-functions config event-functions
              get-event-by-id get-event-series-by-id interval-units)
            event-functions)))))

  (define*
    (upcoming-get-events config time past-n future-n #:key config-variables config-env
      interval-units
      event-functions)
    "list/string:path integer integer integer _ ... -> (vector:event ...)"
    (let
      (ef-list
        (create-event-functions (config-get config config-variables config-env)
          (or event-functions upcoming-event-functions) (or interval-units upcoming-interval-units)
          (compose first pair)))
      (append-map (l (ef) (ef time past-n future-n)) ef-list)))

  (define (upcoming-add-diff-start time events)
    (map (l (a) (pair (abs (- time (event-start a))) a)) events))

  (define-as upcoming-config-variables alist-q
    uptime (compose s->ks os-seconds-since-boot) uptime-start (compose s->ks os-seconds-at-boot))

  (define upcoming-config-path (string-append (getenv "HOME") "/.config/sph/upcoming"))
  (define time-regexp (make-regexp "^([0-9]{4}-[0-1][0-9]-[0-3][0-9])( [0-9.]+)?$"))
  (define default-depends (list (q day)))
  (define default-start-base (list (q day)))
  (define default-interval-unit (q day))
  (define default-duration 200)
  (define duration-day utc-seconds-day)
  (define duration-week (* 7 utc-seconds-day))
  (define default-config-env (environment (q (sph))))
  (define-record event id start end data)

  (define upcoming-event-functions
    (ht-create-symbol day event-day
      weekday-1 (event-weekday-proc 0)
      weekday-2 (event-weekday-proc 1)
      weekday-3 (event-weekday-proc 2)
      weekday-4 (event-weekday-proc 3)
      weekday-5 (event-weekday-proc 4)
      weekday-6 (event-weekday-proc 5) weekday-7 (event-weekday-proc 6)))

  (define upcoming-interval-units
    (ht-create-symbol day duration-day week duration-week kilosecond 1000 second 1))

  (define* (config-get config #:optional variables env)
    (if (string? config)
      (config-read config (or variables upcoming-config-variables) (or env default-config-env))
      config))

  (define*
    (upcoming proc interval #:key config config-variables config-env interval-units event-functions)
    "call proc in interval with a list of upcoming events.
     if proc returns false, stop"
    (let*
      ( (config (config-get (or config upcoming-config-path) config-variables config-env))
        (get-events
          (nullary
            (list-sort-with-accessor < event-start
              (upcoming-get-events config (ns->s (utc-current))
                0 1 #:interval-units interval-units #:event-functions event-functions)))))
      (let loop ((events (get-events)))
        (if (proc events) (begin (sleep interval) (loop (if (null? events) (get-events) events)))
          events)))))
