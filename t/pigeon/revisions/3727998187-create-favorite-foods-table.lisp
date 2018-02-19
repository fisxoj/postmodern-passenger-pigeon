"Create favorite foods table"

(defun up ()
  (execute (:create-table favorite-food
               ((id :type integer :primary-key t)
                (name :type (string 50))
                (person-id :type integer))
             (:foreign-key (person-id) (people id)))))

(defun down()
  (execute (:drop-table 'favorite-food)))
