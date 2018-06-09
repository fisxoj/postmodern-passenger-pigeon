#+TITLE: Postmodern Passenger Pigeon
#+AUTHOR: Matt Novenstern

Postmodern passenger pigeon is a migration manager for [[http://marijnhaverbeke.nl/postmodern/][postmodern]].  It's similar to alembic for sqlalchemy.  It's probably much dumber, though.

* Setup
To set up a project to use ppp, make sure you define a ~pigeon.toml~ file in the root of the project.  Ppp will look for that file to find out where you want to store your migrations.

PPP uses a table in your database called ~pigeon_revision~ to keep track of what migration you're on.  If something goes wrong and you have to fix a migration manually, you can make sure PPP thinks a migration has been run by inserting the migration's id (timestamp) into this table.

* Configuration (~pigeon.toml~)
Your ~pigeon.toml~ file mostly exists to tell ppp where to find your migration definitions (~migration-directory~).  You can optionally tell it how to connect to the database here (~database-url~) but it will look at the ~DATABASE_URL~ environment variable if the database url isn't defined in the config file.  The latter is likely how you will use it in a hosted situation.

An simple ~pigeon.toml~ might look like this:
#+BEGIN_SRC toml
migration-directory = "revisions/"
database-url = "postgres://postgres@localhost/postgres"
#+END_SRC

* Writing migrations
Running the ~new-migration~ function will create a new migration definition in the path specified in your configuration file.  Files are named by a revision id, which is actually the universal time, followed by any arbitrary string.  PPP will kebab your migration name for the purpose of creating the name, so, for example ~Create People Table~ might be named something like ~3727984190-create-people-table.lisp~.

After creating a few migrations, the ~revisions/~ directory might look like this:

#+BEGIN_SRC
3727984190-create-people-table.lisp
3727989266-create-pet-table.lisp
3727998187-create-favorite-foods-table.lisp
3728002236-set-all-the-villagers-on-fire.lisp
#+END_SRC

An individual migration will look something like this:
#+BEGIN_SRC lisp
"Creates the people table."

(defun up ()
  (query (:create-table people
             ((id :type integer :primary-key t)
              (name :type (string 20))))))

(defun down ()
  (query (:drop-table 'people)))
#+END_SRC

A migration file starts with an optional docstring and then defines ~up~ and ~down~ functions.  ~up~ is responsible for migrating the database forward.  ~down~ is responsible for reversing the changes made by ~up~.  This is essentially the same scheme used by alembic or activerecord.

Migrations are lisp code files, and will be evaluated in their own package that's created for them and then destroyed after they run.  The package uses everything from ~common-lisp~ and the ~ppp.operations~ package, which reexports a few postmodern functions useful for migrations.  It's best not to assume anything else is loaded at migration run time unless you specifically design your migrations workflow around that assumption.
