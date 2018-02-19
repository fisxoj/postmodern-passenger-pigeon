"Creates the pets table with association to people."

(defun up ()
  (execute (:create-table pets
               ((id :type integer)
                (type :type text)
                (human-id :type integer))
             (:foreign-key (human-id) (people id)))))

(defun down ()
  (execute (:drop-table 'pets)))
