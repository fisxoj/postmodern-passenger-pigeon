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

  (let* ( ;; The revision that we are currently at, or 0 if there are no revisions in the database
         (start-revision (if (eq start :null)
                             0
                             start))
         ;; The revision we want to go up to (inclusive)
         (end-revision (cond
                         ((eq target :base) 0)
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

  (assert (not (and (eq direction :down) (eq target :head))))
  (assert (not (and (eq direction :up)   (eq target :base))))

  (ecase direction
    (:up
     (let ((sorted-migrations (sort (copy-seq ppp.migration::*migrations*) #'< :key #'migration-revision)))
       (etypecase target
         (integer (migration-revision
                   (if (eq (current-migration) :null)
                       (first sorted-migrations)
                       (nth (+ target (or (position (current-migration)
                                                    sorted-migrations
                                                    :key #'migration-revision
                                                    :test #'=)
                                          -1))
                            sorted-migrations))))
         (symbol (ecase target
                   (:head most-positive-fixnum))))))
    (:down
     (if (eq (current-migration) :null)
         (error "At base revision.  Can't go any lower.")
         (let ((sorted-migrations (sort (copy-seq ppp.migration::*migrations*) #'> :key #'migration-revision)))
           (etypecase target
             (integer (migration-revision
                       (nth (+ target (position (current-migration)
                                                sorted-migrations
                                                :key #'migration-revision
                                                :test #'=))
                            sorted-migrations)))
             (symbol (ecase target
                       (:base 0)))))))))
