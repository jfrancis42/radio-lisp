;;;; radio.asd

(asdf:defsystem #:radio
  :description "Radio/electronics utility functions."
  :author "Jeff Francis <jeff@gritch.org>"
  :license  "MIT, see file LICENSE"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:jeffutils)
  :components ((:file "package")
               (:file "radio")))
