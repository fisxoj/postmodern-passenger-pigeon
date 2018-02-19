"Set all the villagers on fire"

(defun up ()
  (execute (:alter-table "people" :add-column "on_fire" :type boolean :default t)))

(defun down()
  (execute (:alter-table "people" :drop-column "on_fire")))
