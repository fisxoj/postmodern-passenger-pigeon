"Creates the pets table with association to people."

(defun up ()
  (query (:create-table pets
             ((id :type integer)
              (type :type text)
              (human-id :type integer))
           (:foreign-key (human-id) (people id)))))

(defun down ()
  (query (:drop-table pets)))
