#lang racket

(require
  gregr-misc/maybe
  gregr-misc/terminal
  )

(module+ main
  (with-stty-direct
    (let loop ((chars (read-chars-ready)))
      (with-stty-previous
        (displayln (format "chars: ~a\n~v" chars chars))
        )
      (sleep 1)
      (loop (read-chars-ready)))
    ))
