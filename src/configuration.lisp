(defpackage :ppp.configuration
  (:use #:cl
        #:alexandria)
  (:export #:migrations-directory
           #:database-url))

(in-package :ppp.configuration)

(defvar *configuration* nil
  "The configuration hash table from a project's pigeon.toml")

;; (defvar *configuration-last-modified* 0
;;   "Time of the config file's last change, so we know whether to reload it.")

(defun ensure-configuration ()
  (setf *configuration*
        (pp-toml:parse-toml (alexandria:read-file-into-string (merge-pathnames "pigeon.toml")))))

(defun migrations-directory ()
  (ensure-configuration)
  (merge-pathnames (gethash "migration-directory" *configuration* "migrations/")))

(defun database-url ()
  (ensure-configuration)
  (gethash "database-url" *configuration* (uiop:getenv "DATABASE_URL")))
