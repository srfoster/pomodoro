#lang at-exp racket

(provide task-calendar
         schedule
         (struct-out scheduled)
         overlaps-with?)

(require 
  pomodoro ;TODO: move structs in main to base. 
  website-js
  website-js/components/calendar
  website-js/components/accordion-cards
  gregor)

(struct scheduled (task start duration renderer data) #:transparent)

(define (overlaps-with? m s)
  (define start (scheduled-start s))

  (define hours
    (exact-floor (scheduled-duration s)))

  (define minutes
    (exact-round (* (- (scheduled-duration s) hours) 60)))

  (define end (+minutes (+hours start hours) minutes))

  (and (moment>=? m start)
       (moment<? m end)))

(define (schedule t #:start s #:duration d #:renderer (r badge-pill-dark) #:data (data #f))
  (scheduled t s d r data))

(define (scheduled->event s)
  (define name (task-name (scheduled-task s)))
  (define data (task-data (scheduled-task s)))

  (enclose
   (div
    (event
      #:on-click (callback 'main name)
     (string-downcase
      (~t (scheduled-start s)
       "h:mma"))

      name

      (curry (scheduled-renderer s)
             style: (properties width: "100%" cursor: "pointer" text-align: "left")
             'data-toggle: "modal"
             data-target: (ns# "modal")))
    (modal id: (ns "modal")
           (modal-dialog
            (modal-content
             (modal-body
              data
              (hr)
              (scheduled-data s))))))
    (script ()
            (function (main s)
                      ))))


(define (events->hash ss)
  (define grouped (group-by 
                    ;Assumes same month, year, etc. (for now)
                    (compose ->day scheduled-start)
                    ss))

  (define (pair-with-day g)
    (cons
      (->day (scheduled-start (first g)))
      (map scheduled->event g)))


  (define ret
    (make-hash 
     (map pair-with-day grouped)))

  ret)


(define (task-calendar events pomos)
  (define incomplete-tasks 
    (incomplete 
       (map scheduled-task events) 
       #:pomos pomos))

  (define incomplete-events
    (filter 
      (lambda (e)
        (member (scheduled-task e) incomplete-tasks eq?))
      events)) 

  (list
   (calendar (date 2020 1)  ;January
             (events->hash incomplete-events)))
  )





