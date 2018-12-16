(defpackage :ppp.migration
  (:use #:cl
        #:alexandria)
  (:export #:current-migration
           #:with-migrations
           #:migrate
           #:new-migration

           #:list-migrations
           #:describe-migrations


           #:find-migration
           #:migration-range
           ;; Accessors
           #:migration-revision
           #:migration-name))

(in-package :ppp.migration)

(deftype action ()
  '(member :up :down))

(defstruct migration
  "A struct representing a change in the database state.  Is expected
  to have an UP and a DOWN function defined and may have a docstring.
  Corresponds to a lisp file in the migraton directory that has a name
  of the form `[universal time]-[migration name].lisp`."

  name
  revision
  docstring
  pathname
  package)

(defvar *migrations* nil
  "Holds a list of all the known migrations.")


(defun load-docstring (migration-pathname)
  "If the first form in the file pointed to by `migration-pathname` is
  a string, return it, otherwise return NIL."

  (handler-case
      (with-open-file (s migration-pathname)
        (when-let ((form (read s)))
          (when (stringp form) form)))
    (t () nil)))

(defun load-migration-into-package (migration)
  "Loads the lisp file that defines a migration in the package created
for it in LOAD-MIGRATION.  This likely causes [PACKAGE-NAME]:up and
[PACKAGE-NAME]:down to be defined."
  (declare (type migration migration))

  (let ((*package* (find-package (migration-package migration))))
    (load (migration-pathname migration))))

(defun load-migration (migration-pathname)
  (let* ((first-dash-position (position #\- (pathname-name migration-pathname) :test #'char=))
         (name                (subseq (pathname-name migration-pathname) (1+ first-dash-position)))
         (revision            (subseq (pathname-name migration-pathname) 0 first-dash-position)))
    (let ((migration (make-migration :name      name
                                     :revision  (parse-integer revision)
                                     :docstring (load-docstring migration-pathname)
                                     :pathname  migration-pathname
                                     :package   (concatenate 'string "PIGEON-" revision))))
      ;; Create a package for the revision to live in
      (uiop/package:ensure-package (migration-package migration)
                                   :use '(#:common-lisp #:ppp.operations))
      (load-migration-into-package migration)
      migration)))

(defun unload-migration (migration)
  (uiop/package:delete-package* (migration-package migration) :nuke t))

(defun find-migration (revision)
  "Locate a migratoin in *MIGRATIONS* (all known migrations in a
context where they have been loaded)."

  (find revision *migrations* :key #'migration-revision :test #'=))

(defun migration-range (start end &optional (migrations *migrations*))
  "Return a list of migratons which changes its inclusivity depending
on direction.  Downgrades are inclusive on the starting end (the
largest revision number) and upgrades are inclusive on the end (also
the largest revision).

This means that if you have a list of revisions 1-5 and are at 4,

UP 1 => apply revision #5

DOWN 1 => revert revision #4"

  (declare (type integer start end))

  (remove-if-not
   (if (< start end)
       ;; For up revisions, exclude the lower number as it's already
       ;; applied
       (lambda (r) (and (> r start)
                        (<= r end)))
       ;; For down revisions, exclude include the starting revision
       ;; since it must be reverted
       (lambda (r) (and (<= r start)
                        (< end r))))
   migrations
   :key #'migration-revision))

(defun migrate (migration action &key dry-run)
  "Perform ACTION from MIGRATION on the database."

  (declare (type action action)
           (type migration migration))

  (postmodern:with-transaction ()
    (unless dry-run
      (uiop:symbol-call (migration-package migration) action))
    (cond
      ((eq action :down)
       (if dry-run
           (format t "~&DRY-RUN: DELETING REVISION ~a" (migration-revision migration))
           (postmodern:query (:delete-from 'pigeon-revision :where (:= 'revision '$1))
                             (migration-revision migration)
                             :none)))
      ((eq action :up)
       (if dry-run
           (format t "~&DRY-RUN: INSERTING REVISION ~a" (migration-revision migration))
           (postmodern:query (:insert-into 'pigeon-revision :set 'revision '$1)
                             (migration-revision migration)
                             :none))))))

(defun list-migrations ()
  (uiop:directory-files (ppp.configuration:migrations-directory)
                        (make-pathname :directory nil :name :wild :type "lisp" :version :wild)))

(defun load-migrations ()
  (mapcar #'load-migration (list-migrations)))

(defmacro with-migrations (() &body body)
  `(let ((*migrations* (load-migrations)))
     (unwind-protect
          (progn ,@body)
       (map nil #'unload-migration *migrations*))))

(defun describe-migrations (&key (above 0) (below most-positive-fixnum))
  (format t "~% Revision   | Description                                                       ")
  (format t "~%------------+-------------------------------------------------------------------")
  (with-migrations ()
    (dolist (migration (remove-if-not (lambda (m) (< above (migration-revision m) below)) *migrations*))
      (format t "~&~11D | ~54A~%" (migration-revision migration) (or (migration-docstring migration) (migration-name migration))))))

(defun ensure-revision-table ()
  "Ensures that the revision table exists."

  (unless (postmodern:table-exists-p 'pigeon-revision)
    (postmodern:execute "CREATE TABLE pigeon_revision (revision BIGINT)")))

(defun current-migration ()
  (ensure-revision-table)
  (postmodern:query (:select (:max 'revision) :from 'pigeon-revision) :single))

(defun new-migration (name)

  (let* ((revision (get-universal-time))
         (slugified-name (format nil
                                 "~d-~a.lisp"
                                 revision
                                 (string-downcase
                                  (coerce (loop for c across name
                                                if (char= c #\Space)
                                                  collect #\-
                                                else
                                                  collect c)
                                          'string))))
         (migration-pathname (merge-pathnames slugified-name (ppp.configuration:migrations-directory))))

    (with-open-file (s migration-pathname :direction :output)
      (format s "~S~%~%(defun up ())~%~%(defun down ())~%" name))
    migration-pathname))
