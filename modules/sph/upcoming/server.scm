(library (sph upcoming server)
  (export
    upcoming-client
    upcoming-server
    upcoming-server-path)
  (import
    (guile)
    (ice-9 match)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph io)
    (sph list)
    (sph one)
    (sph server)
    (sph upcoming))

  (define upcoming-server-path "/tmp/upcoming-server")

  (define* (upcoming-server #:optional path)
    (display-line
      (string-append "start listening on " (or path upcoming-server-path) "\nexit with ctrl+c"))
    (server-listen
      (l (client)
        (let (query (get-datum client))
          (match query
            ( ( (quote upcoming) (? integer? time) (? integer? past-n)
                (? integer? future-n) config event-ids id-n)
              (write
                (upcoming time past-n
                  future-n #:config (upcoming-config-get config) #:event-ids event-ids #:id-n id-n)
                client))
            (else (put-datum client (pair (q invalid-query) query))))))
      (server-socket (or path upcoming-server-path)) #:parallelism 1))

  (define* (upcoming-client proc #:optional path)
    (let*
      ( (s (socket PF_UNIX SOCK_STREAM 0))
        (result (and (connect s AF_UNIX (or path upcoming-server-path)) (proc s))))
      (close-port s) result)))
