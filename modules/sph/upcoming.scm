(library (sph upcoming)
  (export
    event-data
    event-end
    event-id
    event-start
    upcoming
    upcoming-config-get
    upcoming-config-path
    upcoming-config-variables
    upcoming-doc-config-example
    upcoming-doc-config-syntax
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
    (except (srfi srfi-1) map)
    (only (sph io read-write) rw-string->list)
    (only (sph one) procedure->cached-procedure))

  (define upcoming-doc-config-example
    "28.8 meeting weekday 1
     24 eat interval 10 duration 0.6
     72 sleep
     ,(or uptime 25) work end 55 weekday (1 2 3 4)
     \"2017-12-13\" test title \"test title\"
     \"2017-12-13 28.8\" test-2 end 30 title \"test-2 title\"")

  (define upcoming-doc-config-syntax
    "line:event
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
     depends: id/(id ...)/(or/and/not depends ...)
     start-depends: id/(id ...)
       default variables
     uptime
     uptime-start")

  (define sph-upcoming-description
    "define when events will occur and filter events.
     event definition supports most natural qualifiers \"each second day\" \"only mondays\"
     and a configuration file syntax with one event definition per line")

  (define (ks->s a) (inexact->exact (truncate (* 1000 a))))
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

  (define* (upcoming-config-get #:optional config variables env)
    (let (config (or config upcoming-config-path))
      (if (string? config)
        (config-read config (or variables upcoming-config-variables) (or env upcoming-config-env))
        config)))

  (define (map-offsets start end proc)
    (map-integers (+ 1 (- end start)) (l (n) (proc (+ n start)))))

  (define* (event-day time offset-start #:optional offset-end)
    "integer [integer/false] -> list:event-series"
    (map-offsets offset-start (or offset-end offset-start)
      (l (offset)
        (let (start (+ (* offset duration-day) (ns->s (utc-start-day (s->ns time)))))
          (vector (q day) start (+ start duration-day))))))

  (define (event-weekday-proc weekday)
    (l* (time offset-start #:optional offset-end) "integer [integer/false] -> list:event-series"
      (map-offsets offset-start (or offset-end offset-start)
        (l (offset)
          (let
            (start
              (+ (* weekday duration-day) (* offset duration-week)
                (ns->s (utc-start-week (s->ns time)))))
            (vector (q day) start (+ start duration-day)))))))

  (define create-event-functions
    (let*
      ( (get-weekday-depends
          (l (weekday)
            "integer/(integer ...) -> (symbol:id ...)
            get the default event ids for weekdays identified by numbers in the weekday option"
            (if (and weekday (or (integer? weekday) (every integer? weekday)))
              (list
                (pair (q or)
                  (map (l (a) (string->symbol (string-append "weekday-" (number->string a))))
                    (any->list weekday))))
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
              (let (events (apply ef a))
                (filter
                  (l (event)
                    (let ((start (event-start event)) (end (event-end event)))
                      (list-set-match-iterate
                        (l (depends-id) "filter events that fit in the dependent event series"
                          (let (depends-series (apply get-event-series-by-id depends-id a))
                            ; it is sufficient if it falls into the duration of any occurence of the dependent event
                            (any
                              (l (depends-event)
                                (and (<= (event-start depends-event) start)
                                  (>= (event-end depends-event) end)))
                              depends-series)))
                        depends)))
                  events)))))
        (create-event-function
          (l
            (id config-start config-end
              interval interval-unit duration start-base get-event-by-id interval-units data)
            "_ ... -> procedure
            create a function that returns an event series for one event-source"
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
                    (l* (time offset-start #:optional offset-end)
                      (append-map
                        (l (ef)
                          (let (base-events (ef time offset-start offset-end))
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
            "for each event definition, create functions that create a series of events"
            (map
              (l (a)
                (alist-bind a
                  (id start end interval interval-unit duration weekday depends start-base)
                  (let*
                    ( (depends
                        (pairs (q and)
                          (append (get-weekday-depends weekday)
                            (if depends (list (any->list depends)) null))))
                      (ef
                        (filter-depends
                          (create-event-function id start
                            end interval
                            (or interval-unit (q kilosecond))
                            (or (and duration (ks->s duration)) default-duration)
                            (or start-base default-start-base) get-event-by-id interval-units a)
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

  (define (events-keep-only-n-of-ids a n)
    "((integer . vector:event) ...) -> ((integer . vector:event) ...)
     keep only the first n events for each unique id"
    (let (counter (ht-create-symbol))
      (filter
        (l (a)
          (let* ((id (event-id (tail a))) (count (ht-ref counter id)))
            (if count (and (< count n) (begin (ht-set! counter id (+ 1 count)) #t))
              (begin (ht-set! counter id 1) #t))))
        a)))

  (define (events-add-diff-start time a)
    "integer (vector:event ...) -> ((integer . vector:event) ...)
     prepend the difference between event-start and time to event as a pair"
    (map (l (a) (pair (- (event-start a) time) a)) a))

  (define (events-reduce-range a time prev-n next-n id-n)
    "events-with-diff _ ... -> events-with-diff
     only leave prev-n events before time and next-n events after time,
     and for each direction only id-n repetitions of the same event"
    (let (n-of-ids (if id-n (l (a) (events-keep-only-n-of-ids a id-n)) identity))
      (consecutive (l (a) (< (first a) 0)) (list-sort-with-accessor < first a)
        (l (previous next)
          (append (reverse (take* (abs prev-n) (n-of-ids (reverse previous))))
            (take* (abs next-n) (n-of-ids next)))))))

  (define (events-filter-ids ids a)
    (if (null? ids) a (filter (l (a) (containsq? ids (event-id a))) a)))

  (define*
    (upcoming-events time past-n future-n #:key event-ids config interval-units event-functions)
    "list/string:path integer:utc-seconds integer integer list:alist environment hashtable -> (vector:event ...)
     create a list of events in range past-n to future-n, which select the next and previous n occurences of each available event.
     example:
     (upcoming-events (ns->s (utc-current)) 0 1)"
    (let
      (ef-list
        (create-event-functions config (or event-functions upcoming-event-functions)
          (or interval-units upcoming-interval-units) (compose first pair)))
      (events-filter-ids (or event-ids null)
        (append-map (l (ef) (ef time past-n future-n)) ef-list))))

  (define* (upcoming-reduce events time past-n future-n id-n)
    "calculate new time differences and reselect the relevant range"
    (events-reduce-range (events-add-diff-start time events) time past-n future-n id-n))

  (define* (upcoming-config-cache c #:key config config-variables config-env)
    "procedure:{procedure:get-config:{cache -> cache} cache:initial -> any} -> any
     call c with a procedure that only returns a new configuration object when a corresponding configuration file was updated.
     the second argument to c is the initial cache object"
    (let
      (get-config
        (let
          ( (variables (or config-variables upcoming-config-variables))
            (env (or config-env upcoming-config-env)) (source (or config upcoming-config-path)))
          ; reload config if the file changed if config is read from a file.
          (if (string? source)
            (l (config)
              (let ((last-mtime (first config)) (mtime (stat:mtime (stat source))))
                (if (< last-mtime mtime) (pair mtime (config-read source variables env)) config)))
            (const (pair 0 config)))))
      (c get-config (pair 0 #f))))

  (define*
    (upcoming time past-n future-n #:key config event-ids id-n interval-units event-functions)
    "procedure:{procedure:{state -> (state (diff . event) ...)} initial-state -> any} integer integer _ ... -> any
     return a procedure that when called returns a state object and an event series,
     the event series is also cached and only relevant parts are recalculated where necessary.
     the resulting procedure creates much less processing overhead for repeated calls than \"upcoming-events\""
    (upcoming-reduce
      (upcoming-events time past-n
        future-n #:event-ids
        event-ids #:config config #:interval-units interval-units #:event-functions event-functions)
      time past-n future-n id-n))

  (define-as upcoming-config-variables alist-q
    uptime (compose s->ks os-seconds-since-boot) uptime-start (compose s->ks os-seconds-at-boot))

  (define upcoming-config-path (string-append (getenv "HOME") "/.config/sph/upcoming"))
  (define time-regexp (make-regexp "^([0-9]{4}-[0-1][0-9]-[0-3][0-9])( [0-9.]+)?$"))
  (define default-start-base (list (q day)))
  (define default-interval-unit (q day))
  (define default-duration 200)
  (define duration-day utc-seconds-day)
  (define duration-week (* 7 duration-day))
  (define upcoming-config-env (environment (q (sph))))
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
    (ht-create-symbol day duration-day week duration-week kilosecond 1000 second 1)))
