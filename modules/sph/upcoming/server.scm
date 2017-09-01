(library (sph upcoming server)
  (export
    upcoming-client
    upcoming-server)
  (import
    (rnrs eval)
    (sph)
    (sph alist)
    (sph io)
    (sph list)
    (sph server)
    (sph upcoming))

  (define upcoming-server-path "/tmp/upcoming-server")

  (define (server-eval options)
    (l (client) (each (l (a) (if (null? a) #f (eval a (current-module)))) (port->datums client))))

  (define (server-upcoming options)
    (alist-bind options (interval)
      (l (client)
        (upcoming (l (events) (let (next (first-or-false events)) (display-line next client)))
          (or interval 3)))))

  (define* (upcoming-server operation options)
    (alist-bind options (path)
      (server-listen
        (case operation ((upcoming) (server-upcoming options))
          ((eval) (server-eval options)) (else (l (client) (close client))))
        (server-create-bound-socket (or path upcoming-server-path)))))

  (define* (upcoming-client proc #:optional path)
    (let*
      ( (socket (socket PF_UNIX SOCK_STREAM 0))
        (server (connect socket AF_UNIX (or path upcoming-server-path))))
      (proc socket) (close socket))))
