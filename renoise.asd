(asdf:defsystem :renoise
  :description "Control renoise in Common Lisp with Open Sound Control."
  :version "1.0"
  :author "Brandon Hale <bthaleproductions@gmail.com>"
  :license "GNU General Public License v3"
  :depends-on ("cm-incudine"
	       #:incudine
	       #:cm)
  :components ((:file "renoise")))
