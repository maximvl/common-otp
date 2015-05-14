;;;; common-otp.asd

(asdf:defsystem #:common-otp
  :serial t
  :description "Describe common-otp here"
  :author "Maxvel loz.accs@gmail.com"
  :license "Specify license here"
  :depends-on (#:lparallel
               #:optima)
  :components ((:file "package")
               (:file "common-otp")))

