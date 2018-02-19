"Creates the people table."

(defun up ()
  (query (:create-table people
             ((id :type integer :primary-key t)
              (name :type (string 20))))))

(defun down ()
  (query (:drop-table 'people)))
