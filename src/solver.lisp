(defpackage :ppp.solver
  (:use #:cl
        #:alexandria)
  (:import-from :ppp.migration
                #:find-migration
                #:current-migration
                #:migration-range
                #:migration-revision)
  (:export #:solve))

(in-package :ppp.solver)

(defstruct plan
  steps)

(deftype plan-step ()
  '(cons t (member :up :down)))

(defun solve (start target)
  "Find a PLAN that lists steps to get from START to TARGET by calling
UP or DOWN on migrations."

  (let* (;; The revision that we are currently at, or 0 if there are no revisions in the database
         (start-revision (if (eq start :null)
                             0
                             start))
         ;; The revision we want to go up to (inclusive)
         (end-revision (cond
                         ((null target)     0)
                         ((eq target :head) most-positive-fixnum)
                         (t                 target)))
         (direction (if (> end-revision start-revision)
                        :up
                        :down)))

    (if (= start-revision end-revision)
        (error "Start and end revision are the same.  Nothing to do.")
        (make-plan
         :steps (mapcar
                 (lambda (m) (list m direction))
                 (sort (migration-range start-revision end-revision)
                       (if (eq direction :down)
                           #'>
                           #'<)
                       :key #'migration-revision))))))


(defun find-target-migration (direction target)
  "Find a migration based on relative information, like what direction
  and distance to go, or special keywords like :BASE or :HEAD."

  (declare (type (member :up :down) direction)
           (type (or (integer 0) (member :base :head)) target))

  (assert (not (or
                (and (eq direction :down) (eq target :head))
                (and (eq direction :up)   (eq target :base)))))

  (migration-revision
   (ecase direction
     (:up
      (let ((sorted-migrations (sort ppp.migration::*migrations* #'< :key #'migration-revision)))
        (etypecase target
          (integer (nth (+ target (position (current-migration) sorted-migrations :key #'migration-revision)) sorted-migrations))
          (symbol (ecase target
                    (:head (first (reverse sorted-migrations))))))))
     (:down
      (let ((sorted-migrations (sort ppp.migration::*migrations* #'> :key #'migration-revision)))
        (etypecase target
          (integer (nth (+ target (position (current-migration) sorted-migrations :key #'migration-revision)) sorted-migrations))
          (symbol (ecase target
                    (:base (first (reverse sorted-migrations)))))))))))
