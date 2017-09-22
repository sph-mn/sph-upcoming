(library (sph upcoming cli)
  (export
    upcoming-cli)
  (import
    (rnrs exceptions)
    (sph)
    (sph alist)
    (sph cli)
    (sph io)
    (sph io read-write)
    (sph lang indent-syntax)
    (sph list)
    (sph number)
    (sph record)
    (sph string)
    (sph time)
    (sph time string)
    (sph tree)
    (sph upcoming)
    (sph upcoming server))

  (define-as upcoming-cli-description-source list-qq
    "filter events in time with event definitions from a configuration file."
    "first start a server with \"upcoming --server\". if the server is running, use \"upcoming\" and other options to query events."
    (unquote
      (string-append "event definitions are loaded from the configuration file "
        (string-quote upcoming-config-path))))

  (define-as upcoming-cli-about-source list-qq
    ("description" (unquote-splicing upcoming-cli-description-source))
    ("configuration file syntax" (unquote upcoming-doc-config-syntax))
    ("example configuration file" (unquote upcoming-doc-config-example)))

  (define upcoming-cli-about (prefix-tree-text upcoming-cli-about-source))
  (define upcoming-cli-description (prefix-tree-text upcoming-cli-description-source))
  ; (strftime "%c" (localtime (current-time)))

  (define cli
    (cli-create #:description upcoming-cli-description
      #:about upcoming-cli-about
      #:options
      (list-q
        (next #:names #\n
          #:value-optional? #t #:type integer #:description "select up to n future events")
        (previous #:names #\p
          #:value-optional? #t #:type integer #:description "select up to n past events")
        (active #:names #\c
          #:value-optional? #t #:type integer #:description "select up to n active events")
        (config #:value-required? #t
          #:description "use a different configuration file for the server" #:type string)
        (limit #:value-required? #t
          #:description "include at most n repetitions of distinct event ids" #:type integer)
        (format #:value-required? #t #:description "hms, strptime, data-space, data-scm, data-csv")
        ((event-ids ...))
        (server #:names #\s #:description "start a server that answers event queries"))))

  (define-record u-row ppf diff-start diff-end duration start end id data)
  (define (format-time-ks-relative a) (s->ks-string a))
  (define (format-time-ks-date a) (utc->ymd-ks (s->ns a)))
  (define (format-time-hms-relative a) (s->hms-string a))
  (define (format-time-hms-date a) (utc->ymd-hms (s->ns a)))

  (define (text-format event-data current-date format-date format-relative)
    (let (ymd-prefix (string-append current-date ":"))
      (display-line "active diff-start diff-end duration start end id")
      (each
        (l (a)
          (display-line
            (string-join
              (list (let (ppf (u-row-ppf a)) (if (zero? ppf) "*" (if (= 1 ppf) "<" ">")))
                (format-relative (u-row-diff-start a)) (format-relative (u-row-diff-end a))
                (format-relative (u-row-duration a))
                (string-drop-prefix-if-exists ymd-prefix (format-date (u-row-start a)))
                (string-drop-prefix-if-exists ymd-prefix (format-date (u-row-end a)))
                (symbol->string (u-row-id a)))
              " ")))
        event-data)))

  (define (display-event-list a format format-options)
    "((diff:integer . vector:event) ...) -> unspecified
     start-diff end-diff duration start end id"
    (case format
      ((ks) (text-format a (utc-current-ymd) format-time-ks-date format-time-ks-relative))
      ((hms) (text-format a (utc-current-ymd) format-time-hms-date format-time-hms-relative))
      ( (strptime)
        ;(text-format a (utc-current-ymd) (format-time-strptime-date ) (format-time-strptime-relative))
        #t)
      ( (data-scm)
        (each
          (l (a) (put-datum (current-output-port) a) (put-char (current-output-port) #\newline)) a))
      ( (data-csv)
        (each
          (l (a)
            (put-string (current-output-port)
              (string-join
                (map (l (a) (if (number? a) (number->string a) (string-quote (any->string a))))
                  (vector->list a))
                ","))
            (put-char (current-output-port) #\newline))
          a))
      (else (raise (pair (q invalid-format-specified) format)))))

  (define (upcoming-cli . program-arguments)
    (alist-bind (apply cli program-arguments)
      (active config countdown countup event-ids format interval limit next previous server)
      (cond (server (upcoming-server))
        ( (file-exists? upcoming-server-path)
          (let*
            ( (format
                (if format
                  (let (a (string-split format #\:))
                    ; format-name and option-string
                    (pair (string->symbol (first a)) (string-join (tail a) ":")))
                  (list (q ks) "")))
              (previous (if previous (if (integer? previous) previous 1) 0))
              (next (if next (if (integer? next) next 1) 1)) (time (ns->s (utc-current)))
              (data
                (upcoming-client-upcoming time previous
                  active next limit (and event-ids (map string->symbol event-ids)))))
            (if (not (or (eof-object? data) (null? data)))
              (display-event-list data (first format) (tail format)))))
        (else (display-line "invalid call"))))))
