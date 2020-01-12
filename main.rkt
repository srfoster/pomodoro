#lang racket

(provide
  (struct-out tag) 
  (except-out (struct-out task) task)
  (rename-out [make-task task])
  (struct-out task-list) 
  (except-out (struct-out pomo) pomo)
  (rename-out [make-pomo pomo])
  done
  incomplete
  complete
  tagged-with?
  task-complete?
  tag-filter
  tag-filter-not
  add-tag)


(struct tag (name))
(struct task (name data tags) #:transparent)
(struct task-list (name tags) #:transparent)
(struct pomo (task time completed?) #:transparent)


(define (make-task #:data (d #f) n . tags)
  (task n d tags))

(define (make-pomo t #:time (time 25))
  (pomo t time #f))

(define (done t #:time (time 25))
  (pomo t time #t))

(provide pomo-for)

(define (pomo-for p t)
  (eq? (pomo-task p) t))

(define (task-complete? t #:pomos (pomos '()))
  (define relevant
    (filter (curryr pomo-for t) 
            (flatten pomos)))

  (define completions
    (filter pomo-completed? relevant))

  (not (empty? completions)))

(define (incomplete l #:pomos (pomos '()))
  (filter-not (curry task-complete? #:pomos (flatten pomos))
              (flatten l)))

(define (complete l #:pomos (pomos '()))
  (filter (curry task-complete? #:pomos (flatten pomos))
          (flatten l)))

(define (tagged-with? t tag)
  (member tag (task-tags t)))

(define (tag-filter l tag)
  (filter (curryr tagged-with? tag)
          (flatten l)))

(define (tag-filter-not l tag)
  (filter-not (curryr tagged-with? tag) 
              (flatten l)))


(define (add-tag t ta)
  (struct-copy task ta
               [tags (cons t (task-tags ta))]))

