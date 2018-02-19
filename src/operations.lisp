(defpackage :ppp.operations
  (:use #:cl
        #:alexandria)
  (:import-from #:postmodern
                #:create-schema
                #:drop-schema
                #:query
                #:execute)
  (:export #:create-table
           #:add-column
           #:drop-column
           #:execute
           #:query
           #:create-schema
           #:drop-schema))

(in-package :ppp.operations)
