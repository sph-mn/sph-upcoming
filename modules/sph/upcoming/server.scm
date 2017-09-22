(library (sph upcoming server)
  (export
    upcoming-client
    upcoming-client-query
    upcoming-client-upcoming
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
    (sph time)
    (sph upcoming))

  (define upcoming-server-path "/tmp/upcoming-server")

  (define (upcoming-server-upcoming cc time past-n active-n future-n id-n ids)
    (upcoming (config-cache-update cc) time past-n active-n future-n #:id-n id-n #:ids ids))

  (define (respond cc a)
    (match a (((quote upcoming) a ...) (apply upcoming-server-upcoming cc a))
      (else (list (q invalid-query) a))))

  (define* (upcoming-server #:optional path config-path)
    (let
      ( (cc (upcoming-config-cached config-path))
        (socket (server-socket (or path upcoming-server-path))))
      (display-line
        (string-append "start listening on " (or path upcoming-server-path) "\nexit with ctrl+c"))
      (server-listen (l (client) (put-datum client (respond cc (get-datum client)))) socket
        #:parallelism 1)))

  (define* (upcoming-client proc #:optional path)
    (let*
      ( (s (socket PF_UNIX SOCK_STREAM 0))
        (result (and (connect s AF_UNIX (or path upcoming-server-path)) (proc s))))
      (close-port s) result))

  (define (upcoming-client-query query)
    (upcoming-client (l (server) (put-datum server query) (get-datum server))))

  (define* (upcoming-client-upcoming #:optional time past-n active-n future-n id-n ids)
    (upcoming-client-query
      (list (q upcoming) (or time (utc-current))
        (or past-n 0) active-n (or future-n 1) id-n (or ids null)))))
