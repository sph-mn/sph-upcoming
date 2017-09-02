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
  (define (client-get-one query) (upcoming-client (l (server) (write query server) (read server))))
  (define (s->ks-string a) (simple-format-number a 3 2))

  (define cli
    (cli-create #:description upcoming-cli-description
      #:about upcoming-cli-about
      #:options
      (list-q
        (next #:names #\n
          #:value-optional? #t #:type integer #:description "select up to n upcoming events")
        (previous #:names #\p
          #:value-optional? #t #:type integer #:description "select up to n previous events")
        (config #:names #\c
          #:value-required? #t #:description "use a different configuration file" #:type string)
        (limit #:value-required? #t
          #:description "return only n repetitions of each unique event id" #:type integer)
        ((event-ids ...))
        (server #:names #\s #:description "start a server that can answer event queries"))))

  (define (upcoming-cli . program-arguments)
    (let (options (apply cli program-arguments))
      (alist-bind options (limit server next previous countdown countup config interval event-ids)
        (if server (upcoming-server)
          (if (file-exists? upcoming-server-path)
            (let
              ( (previous (if previous (* -1 (if (integer? previous) previous 1)) 0))
                (next (if next (if (integer? next) next 1) 1)))
              (let*
                ( (time (ns->s (utc-current)))
                  (data
                    (client-get-one
                      (list (q upcoming) time
                        previous next
                        (or config upcoming-config-path)
                        (and event-ids (map string->symbol event-ids)) limit))))
                (if (or (eof-object? data) (null? data)) (display-line "")
                  (each
                    (l (a)
                      (let
                        ( (now-date-prefix (string-append (utc->ymd (s->ns time)) "_"))
                          (diff (first a)) (a (tail a)))
                        (display
                          (string-append (s->ks-string diff) " "
                            (string-drop-prefix-if-exists now-date-prefix
                              (utc->ymd-ks (s->ns (event-start a))))
                            " "
                            (string-drop-prefix-if-exists now-date-prefix
                              (utc->ymd-ks (s->ns (event-end a))))
                            " " (symbol->string (event-id a)) "\n"))))
                    data))))))))))
