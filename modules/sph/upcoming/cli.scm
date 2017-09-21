(library (sph upcoming cli)
  (export
    upcoming-cli)
  (import
    (sph)
    (sph alist)
    (sph cli)
    (sph io)
    (sph io read-write)
    (sph lang indent-syntax)
    (sph list)
    (sph number)
    (sph string)
    (sph time)
    (sph time string)
    (sph tree)
    (sph upcoming)
    (sph upcoming server))

  ; possible enhancements
  ;   allow event ids as start/end, which uses the references events start or end respectively

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
  (define (s->ks-string a) (simple-format-number a 3 2))

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
        ((event-ids ...))
        (server #:names #\s #:description "start a server that answers event queries"))))

  (define (display-event-list a)
    ; passed-since-start left-till-end duration start end id
    (each (l (a) (debug-log a)) a))

  (define (upcoming-cli . program-arguments)
    (alist-bind (apply cli program-arguments)
      (limit server next previous active countdown countup config interval event-ids)
      (cond (server (upcoming-server))
        ( (file-exists? upcoming-server-path)
          (let*
            ( (previous (if previous (* -1 (if (integer? previous) previous 1)) 0))
              (next (if next (if (integer? next) next 1) 1)) (time (ns->s (utc-current)))
              (data
                (upcoming-client-upcoming time previous
                  active next limit (and event-ids (map string->symbol event-ids)))))
            (if (not (or (eof-object? data) (null? data))) (display-event-list data))))
        (else (display-line "invalid call"))))))
