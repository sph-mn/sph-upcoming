(library (sph upcoming)
  (export
    config-cache-config
    config-cache-get
    config-cache-mtime
    config-cache-update
    event-data
    event-end
    event-id
    event-start
    upcoming
    upcoming-config-cached
    upcoming-config-cached-static
    upcoming-config-get
    upcoming-config-path
    upcoming-config-variables
    upcoming-doc-config-example
    upcoming-doc-config-syntax
    upcoming-events
    upcoming-table)
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
       time-day: integer:day-ks/\"hh[:mm[:ss]]\"
       time: time-day/time-date/\"time-date time-day\"/id
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
     event definition supports encoding most natural qualifiers \"each second day\" \"only mondays\"
     with a configuration file syntax with one event definition per line.
     terms in binding names:
       base: times without full dates are relative to this
       unit: intervals are specified as repetition of a unit of time duration")

  (define upcoming-config-path (string-append (getenv "HOME") "/.config/sph/upcoming"))
  (define hms-pattern "[0-9]{1,2}(:[0-9]{1,2}){0,2}")
  (define date-pattern "[0-9]{4}-[0-1][0-9]-[0-3][0-9]")
  (define ks-pattern "[0-9.]+")

  (define read-ymd-ks
    (let (regexp (make-regexp (string-append "^(" date-pattern ")( " ks-pattern ")?$")))
      (l (a) "string -> false/integer:utc-time"
        (and-let* ((m (regexp-exec regexp a)))
          (list
            ; epoch time
            (ns->s (utc-from-ymd (match:substring m 1)))
            ; relative time
            (let (m-2 (match:substring m 2))
              (if m-2 (ks->s (string->number (string-drop m-2 1))) 0))
            ; has epoch time?
            #t)))))

  (define read-hms
    (let (regexp (make-regexp (string-append "^" hms-pattern "$")))
      (l (a)
        "string -> (epoch-time relative-time has-epoch-time?)
        parse a hh[:mm[:ss]] time string"
        (and-let* ((m (regexp-exec regexp a))) (list #f (seconds-from-hms a) #f)))))

  (define read-ymd-hms
    (let (regexp (make-regexp (string-append "^(" date-pattern ")( " hms-pattern ")?$")))
      (l (a) "string -> false/integer:utc-time"
        (and-let* ((m (regexp-exec regexp a)))
          (list
            ; epoch time
            (ns->s (utc-from-ymd (match:substring m 1)))
            ; relative time
            (let (m-2 (match:substring m 2))
              (if m-2 (ks->s (string->number (string-drop m-2 1))) 0))
            ; has epoch time?
            #t)))))

  (define-as upcoming-datetime-readers list read-ymd-ks read-hms read-ymd-hms)
  (define default-start-base (list (q day)))
  (define default-interval-unit (q day))
  (define default-duration 200)
  (define duration-day utc-seconds-day)
  (define duration-week (* 7 duration-day))
  (define upcoming-config-env (environment (q (sph))))
  (define-record event id start end data)
  (define (ks->s a) (inexact->exact (truncate (* 1000 a))))
  (define (s->ks a) (/ a 1000))

  (define (map-offsets start end proc)
    "integer integer proc -> list
     map the range start..end including start and end"
    (map-integers (+ 1 (- end start)) (l (n) (proc (+ n start)))))

  (define-as upcoming-config-variables alist-q
    ; symbol -> any/procedure
    uptime (compose s->ks os-seconds-since-boot) uptime-start (compose s->ks os-seconds-at-boot))

  (define upcoming-interval-units
    (ht-create-symbol day duration-day week duration-week kilosecond 1000 ks 1000 second 1 s 1))

  (define-record upcoming-env ef-ht units readers get-ef get-events)
  (define-record config-cache get config mtime)
  (define (config-cache-update cc) ((config-cache-get cc) cc))
  ;
  ;-- config parsing
  ;
  (define (config-read-time-error data) (raise (pair (q invalid-time-format) data)))

  (define (config-read-time a datetime-readers get-ef event-time c)
    "any procedure procedure procedure -> any
     numbers: are day-start relative kiloseconds
     string: various date/time formats to be parsed
     symbol: event-id whose start or end time used depending on the given event-time procedure"
    (cond ((not a) (c #f #f #f)) ((number? a) (c #f (ks->s a) #f))
      ((string? a) (or (any (l (b) (b a)) datetime-readers) (config-read-time-error a)))
      ((symbol? a) (c (event-time (get-ef a)) 0 #t)) (else (config-read-time-error a))))

  (define (config-read-times start end u-env c)
    (apply c
      (let ((readers (upcoming-env-readers u-env)) (get-ef (upcoming-env-get-ef u-env)))
        (append (config-read-time start readers get-ef event-start list)
          (config-read-time end readers get-ef event-end list)))))

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

  (define* (config-load path config-variables config-env)
    "read and parse a configuration object from file"
    (let*
      ( (line-data
          (compact
            (call-with-input-file path
              (l (port) (remove null? (port-lines-map (l (a) (rw-string->list get-datum a)) port))))))
        (line-data (config-eval line-data config-variables config-env)))
      (map (l (a) (apply (l (time id . options) (apply alist (q id) id (q start) time options)) a))
        line-data)))

  (define* (upcoming-config-load #:key path variables env)
    "load configuration from file with defaults"
    (config-load (or path upcoming-config-path) (or variables upcoming-config-variables)
      (or env upcoming-config-env)))

  ;-- default event-functions
  ;

  (define* (ef-day time offset-start #:optional offset-end)
    "integer [integer/false] -> list:event-series"
    (map-offsets offset-start (or offset-end offset-start)
      (l (offset)
        (let (start (+ (* offset duration-day) (ns->s (utc-start-day (s->ns time)))))
          (vector (q day) start (+ start duration-day))))))

  (define (create-ef-weekday weekday) "create an eve"
    (l* (time offset-start #:optional offset-end) "integer [integer/false] -> list:event-series"
      (map-offsets offset-start (or offset-end offset-start)
        (l (offset)
          (let
            (start
              (+ (* weekday duration-day) (* offset duration-week)
                (ns->s (utc-start-week (s->ns time)))))
            (vector (q day) start (+ start duration-day)))))))

  (define upcoming-event-functions-ht
    (ht-create-symbol day ef-day
      weekday-1 (create-ef-weekday 0)
      weekday-2 (create-ef-weekday 1)
      weekday-3 (create-ef-weekday 2)
      weekday-4 (create-ef-weekday 3)
      weekday-5 (create-ef-weekday 4) weekday-6 (create-ef-weekday 5) weekday-7 (create-ef-weekday 6)))

  (define* (upcoming-create-env #:key ef-ht units readers)
    "create an upcoming-env record with defaults"
    (record upcoming-env (or ef-ht upcoming-event-functions-ht)
      (or units upcoming-interval-units) (or readers upcoming-datetime-readers)))

  (define upcoming-default-env (upcoming-create-env))

  (define config->event-functions
    (let*
      ( (get-weekday-depends
          (l (weekday)
            "integer/(integer ...) -> (symbol:id ...)
            translate numbers from the weekday option to the default event ids for the weekdays"
            (if (and weekday (or (integer? weekday) (every integer? weekday)))
              (list
                (pair (q or)
                  (map (l (a) (string->symbol (string-append "weekday-" (number->string a))))
                    (any->list weekday))))
              null)))
        (get-interval-seconds
          (l (interval unit units)
            "number symbol hashtable -> number
            calculate the duration of seconds for one interval in the given unit"
            (* interval (ht-ref units unit))))
        (filter-depends
          (l (ef depends get-events)
            "event-function (symbol:id ...) procedure -> event-function
            remove events that fall outside the duration of all their dependent events"
            (l* a
              (let (events (apply ef a))
                (filter
                  (l (event)
                    (let ((start (event-start event)) (end (event-end event)))
                      (list-set-match-iterate
                        (l (depends-id) "filter events that fit in the dependent event series"
                          (let (depends-series (apply get-events depends-id a))
                            ; it is sufficient if it falls into the duration of any occurence of the dependent event
                            (any
                              (l (depends-event)
                                (and (<= (event-start depends-event) start)
                                  (>= (event-end depends-event) end)))
                              depends-series)))
                        depends)))
                  events)))))
        (create-event-function
          (l (id config-start config-end interval interval-unit duration start-base u-env data)
            "_ ... -> procedure
            create a function that returns an event series for one event-source"
            (config-read-times config-start config-end
              u-env
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
                  (let (start-base-ef (map (upcoming-env-get-ef u-env) (any->list start-base)))
                    (l* (time offset-start #:optional offset-end)
                      "integer:current-time integer [integer] -> (event ...)
                      an event function"
                      (let (interval-units (upcoming-env-units u-env))
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
                          start-base-ef)))))))))
        (create-event-functions
          (l (config u-env)
            "for each event definition, create functions that create one or multiple events
            given an offset and range from the current time"
            (let ((ef-ht (upcoming-env-ef-ht u-env)) (get-events (upcoming-env-get-events u-env)))
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
                              (or start-base default-start-base) u-env a)
                            depends get-events)))
                      (ht-set! ef-ht id ef) ef)))
                config)))))
      (l (config u-env c)
        "list hashtable:all-event-functions hashtable:unit-config procedure -> any
        get event-functions for config and add them by id to event-functions-ht"
        (let*
          ( (ef-ht (ht-copy (upcoming-env-ef-ht u-env) #t)) (get-ef (ht-object ef-ht))
            (get-events (procedure->cached-procedure (l (id . a) (apply (get-ef id) a))))
            (u-env (record-update-b upcoming-env u-env ef-ht get-ef get-events)))
          (c (create-event-functions config u-env) ef-ht)))))

  ;-- event series'

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

  (define upcoming-table
    (let*
      ( (past-active-future
          (l (start-diff end-diff) (if (>= 0 start-diff) (if (< 0 end-diff) 0 -1) 1)))
        (past? (l (a) (< (first a) 0)))
        (add-diff-start
          (l (a time) "(pair ...) -> (pair ...)" (map (l (a) (pair (- (event-start a) time) a)) a)))
        (create-row
          (l (a) "(diff . event) -> vector"
            (pair-bind a (diff event)
              (let ((start (event-start event)) (end (event-end event)))
                (let* ((duration (- end start)) (end-diff (+ diff duration)))
                  (vector (past-active-future diff end-diff) diff
                    end-diff duration start end (event-id event) (event-data event))))))))
      (l (a time prev-n active-n next-n id-n ids)
        "list:events integer integer integer integer integer (symbol ...) -> (vector ...)
        re-calculate time differences and reselect the relevant range.
        only leave prev-n events before time and next-n events after time,
        and for each direction only id-n repetitions of the same event"
        (let*
          ( (n-of-ids (if id-n (l (a) (events-keep-only-n-of-ids a id-n)) identity))
            (of-ids
              (if (null? ids) identity
                (l (a) (filter (l (a) (containsq? ids (event-id (tail a)))) a))))
            (a (list-sort-with-accessor < first (add-diff-start a time)))
            (a
              (consecutive past? a
                (l (past future)
                  (append (reverse (take* (abs prev-n) (n-of-ids (reverse past))))
                    (take* (abs next-n) (n-of-ids future)))))))
          (map create-row (n-of-ids (of-ids a)))))))

  ;
  ;-- main exports

  (define* (upcoming-config-cached #:optional config-path variables config-env)
    "string [hashtable environment] -> (config . mtime):config-cache"
    ; todo: events are calculated in a range relative to current time. even if the file does not change
    ;   the events need to be recalculated at some point
    (let
      ( (variables (or variables upcoming-config-variables))
        (env (or config-env upcoming-config-env)) (config-path (or config-path upcoming-config-path)))
      (letrec*
        ( (get-config
            (l (cc) cc
              (let ((last-mtime (config-cache-mtime cc)) (mtime (stat:mtime (stat config-path))))
                (if (< last-mtime mtime)
                  (vector get-config (config-load config-path variables env) mtime) cc))))
          (cc (vector get-config #f 0)))
        (get-config cc))))

  (define* (upcoming-config-cached-static config-object)
    "create a config-cache object with an unchanging config object" (vector identity config-object 0))

  (define (upcoming-events time offset-start offset-end config u-env)
    "list/string:path integer:utc-seconds integer integer list:alist environment hashtable -> (vector:event ...)
     create a list of events in range past-n to future-n, which select n next or previous occurences of events.
     past: start and end are in the past
     active: start is in the past and end is in the future
     future: start and end are in the future
     example call: (upcoming-events (ns->s (utc-current)) 0 0 1)"
    (config->event-functions config u-env
      (l (ef-list ef-ht) (append-map (l (ef) (ef time offset-start offset-end)) ef-list))))

  (define* (upcoming config-cache time past-n active-n future-n #:key id-n ids u-env)
    "vector integer integer integer integer #:id-n integer #:ids (symbol ...) #:u-env vector -> list:(upcoming-table-row ...)
     upcoming-table-row: #(present-diff start-diff end-diff duration start end id)"
    (upcoming-table
      (upcoming-events time (max past-n active-n)
        future-n (config-cache-config config-cache) (or u-env upcoming-default-env))
      time past-n active-n future-n id-n ids)))
