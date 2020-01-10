#lang racket

(provide render-tasks 
         task-list-container
         active)

(require website-js
         website-js/components/accordion-cards
         "./main.rkt")

(define active (tag "active"))

;Consider making this part of base?
(define current-pomos (make-parameter '()))

(define (task-li t)
  (accordion-card
    #:header (span (when (task-data t)
                     "[+] ")
                   (task-name t))
    (task-data t)))

(define (task-list-container tasks pomos)
  (current-pomos pomos)

  (define incomplete-tasks
    (incomplete tasks #:pomos (current-pomos)))

  (define incomplete:active
    (tag-filter 
      incomplete-tasks
      active))

  (define incomplete:inactive
    (tag-filter-not
      incomplete-tasks
      active))

  (parameterize ([current-pomos pomos])

    (container
      (h1 "Tasks")
      (h2 "Active")
      (map task-li incomplete:active)

      (h2 "Open")
      (map task-li incomplete:inactive)

      (h2 "Complete")
      (map task-li 
           (complete tasks
                     #:pomos (current-pomos))))))

(define (render-tasks tasks #:pomos (pomos '()))
  (render #:to "out" 
    (bootstrap
      (page index.html
            (content
              (task-list-container
                tasks 
                pomos))))))







