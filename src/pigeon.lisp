(defpackage :ppp
  (:use #:cl
        #:alexandria)
  (:import-from :ppp.migration
                #:current-migration
                #:migration-revision
                #:with-migrations
                #:migration-name)
  (:import-from :ppp.solver
                #:solve
                #:plan-steps
                #:find-target-migration)
  (:export #:migrate))

(in-package :ppp)

(defun migrate (&optional (direction :up) (target :head))

  (with-migrations ()
    (execute (solve (current-migration)
                    (find-target-migration direction target)))))

(defun execute (plan)
  (dolist (step (plan-steps plan))
    (format t "~&Migrating from ~a to ~a (~a)~%"
            (if (eq :null (current-migration)) :base (current-migration))
            (migration-revision (car step))
            (migration-name (car step)))
    (apply #'ppp.migration:migrate step)))
