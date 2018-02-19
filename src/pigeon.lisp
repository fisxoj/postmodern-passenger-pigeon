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
  (:export #:migrate
           #:main))

(in-package :ppp)

(defun migrate (&optional (direction :up) (target :head) dry-run)
  (with-migrations ()
    (execute (solve (current-migration)
                    (find-target-migration direction target))
             :dry-run dry-run)))

(defun execute (plan &key dry-run)
  (dolist (step (plan-steps plan))
    (format t "~&Migrating ~a from ~a to ~a ~a~%"
            (second step)
            (if (eq :null (current-migration)) :base (current-migration))
            (migration-revision (car step))
            (migration-name (car step)))
    (apply #'ppp.migration:migrate `(,@step :dry-run ,dry-run))))

(defun connect-from-url ()
  "Takes a URL of form postgres://username:pass@hostname[:port]/database and connects to it."

  (multiple-value-bind (proto auth hostname port db) (quri:parse-uri (ppp.configuration:database-url))
    (assert (string-equal proto "postgres"))
    (let ((colon-position (cl:position #\: auth :test #'char=)))
      (postmodern:connect-toplevel (subseq db 1)
                                   (subseq auth 0 colon-position)
                                   (if colon-position (subseq auth (1+ colon-position)) "")
                                   hostname
                                   :port (or port 5432)))))

(defmacro with-database (() &body body)
  `(unwind-protect
        (progn
          (connect-from-url)
          ,@body)
     (postmodern:disconnect-toplevel)))

(defun print-usage (&optional message)
  (format t "~&ppp [command]

~@[** ~a **~%~]

Commands:
    describe                     Describe the available migrations
    migrate [direction] [target] Migrates the database in DIRECTION (up or down) to TARGET (head, base, or a number delta)
    new \"[name]\"               Create a new migration named NAME in the migrations directory
    help                         Display this text."
          message))

(defun main ()
  (let ((args (uiop:raw-command-line-arguments)))
    (switch ((second args) :test #'string-equal)
      ("describe" (ppp.migration:describe-migrations))
      ("migrate"   (with-database ()
                     (migrate (alexandria:make-keyword (string-upcase (third args)))
                              (let ((target (fourth args)))
                                (switch (target :test #'string-equal)
                                  ("head" :head)
                                  ("base" :base)
                                  (t (parse-integer target)))))))
      ("new"      (format t "~&Created ~a.~%" (ppp.migration:new-migration (third args))))
      ("help"     (print-usage))
      ("--help"   (print-usage))
      (t          (print-usage (format nil "no such command ~S" (second args)))))))
