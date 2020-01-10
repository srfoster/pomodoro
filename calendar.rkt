#lang at-exp racket

(provide task-calendar
         schedule)

(require 
  pomodoro ;TODO: move structs in main to base. 
  website-js
  website-js/components/calendar
  website-js/components/accordion-cards
  gregor)

(struct scheduled (task start duration) #:transparent)

(define (schedule t #:start s #:duration d)
  (scheduled t s d))

(define (scheduled->event s)
  (define name (task-name (scheduled-task s)))
  (define data (task-data (scheduled-task s)))

  (enclose
    #;
    (accordion-card
      ;on-click: (callback 'main name)

      #:header (span
                 (~t (scheduled-start s)
                     "h:mm a") ": "
                 name)

      name data)

    (event
      #:on-click (callback 'main name)
      (~t (scheduled-start s)
          "h:mm a")
      name
      )
    (script ()
            (function (main s)
                      @js{alert(s)}))))

(define (events->hash ss)
  (define grouped (group-by 
                    ;Assumes same month, year, etc. (for now)
                    (compose ->day scheduled-start)
                    ss))

  (define (pair-with-day g)
    (cons
      (->day (scheduled-start (first g)))
      (map scheduled->event g)))

  (make-hash 
         (map pair-with-day grouped)))


(define (task-calendar events pomos)
  (define incomplete-tasks 
    (incomplete 
       (map scheduled-task events) 
       #:pomos pomos))

  (define incomplete-events
    (filter 
      (lambda (e)
        (member (scheduled-task e) incomplete-tasks))
      events)) 

  (list
   (calendar (date 2020 1)  ;January
             (events->hash incomplete-events)))
  )





