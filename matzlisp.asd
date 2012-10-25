;;;; $Id: matzlisp.asd,v 1.4 2007/09/27 14:10:01 matze Exp $

;; Matzes Lisp Extensions (mostly SBCL)


(in-package #:cl-user)

(defpackage #:matzlisp-system
  (:use #:cl #:asdf))

(in-package #:matzlisp-system)

(defsystem matzlisp
  :name "matzlisp"
  :author "Mathias Menzel-Nielsen"
  :version "1.7"
  :license "BSD"
  :description "Matzes personal Common Lisp extensions"
  :depends-on (:cl-ppcre :split-sequence :flexi-streams)
  :properties ((#:date . "$Date: 2007/09/27 14:10:01 $"))
  :components (
	       (:file "telnet-connection" :depends-on ("matzlisp"))
	       (:file "connector" :depends-on ("telnet-connection"))
	       (:file "fetchurl" :depends-on ("telnet-connection"))
	       (:file "matzlisp")
	       (:file "licenser" :depends-on ("matzlisp"))))
	       