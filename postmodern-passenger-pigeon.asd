(asdf:defsystem postmodern-passenger-pigeon
  :description "A migrations system for postmodern."
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :version "0.0.2"
  :depends-on ("alexandria"
               "postmodern"
               "pp-toml"
               "quri")
  :pathname #P"src/"
  :components ((:file "configuration")
               (:file "operations")
               (:file "migration")
               (:file "solver")
               (:file "pigeon"))

  :build-operation "program-op"
  :entry-point "ppp:main"
  :build-pathname "ppp")
