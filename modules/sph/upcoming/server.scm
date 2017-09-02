(library (sph upcoming server)
  (export
    upcoming-client
    upcoming-server)
  (import
    (ice-9 match)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph io)
    (sph list)
    (sph one)
    (sph server)
    (sph upcoming)
    (only (guile) port-closed?))

  (define upcoming-server-path "/tmp/upcoming-server")

  (define* (upcoming-server #:optional path)
    (display-line
      (string-append "start listening on " (or path upcoming-server-path) "\nexit with ctrl+c"))
    (server-listen
      (l (client)
        (let (query (read client))
          (match query
            ( ( (quote upcoming) (? integer? time) (? integer? past-n)
                (? integer? future-n) config event-ids id-n)
              (write
                (upcoming time past-n future-n #:config config #:event-ids event-ids #:id-n id-n)
                client))
            (else (write (pair (q invalid-query) query) client)))
          (close client)))
      (server-create-bound-socket (or path upcoming-server-path)) 0))

  (define* (upcoming-client proc #:optional path)
    (let*
      ( (socket (socket PF_UNIX SOCK_STREAM 0))
        (server (connect socket AF_UNIX (or path upcoming-server-path))) (result (proc socket)))
      (close socket) result)))
