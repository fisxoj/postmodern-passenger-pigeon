(asdf:defsystem postmodern-passenger-pigeon
  :description "A migrations system for postmodern."
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :version "0.0.1"
  :depends-on ("alexandria"
               "postmodern"
               "pp-toml")
  :pathname #P"src/"
  :components ((:file "configuration")
               (:file "operations")
               (:file "migration")
               (:file "solver")
               (:file "pigeon")))
