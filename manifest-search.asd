;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :manifest-search.system)
    (defpackage :manifest-search.system
	(:use :common-lisp :asdf))))

(in-package manifest-search.system)

(defsystem :manifest-search
  :description "A library providing various collector type macros
   pulled from arnesi into its own library and stripped of dependencies"
  :licence "BSD"
  :version "0.1"
  :components ((:file "manifest-search"))
  :depends-on (:alexandria :collectors :iterate :manifest :montezuma))

(defsystem :manifest-search-test
  :description "A library providing various collector type macros
   pulled from arnesi into its own library"
  :licence "BSD"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "manifest-search"))))
  :depends-on (:manifest-search :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :manifest-search))))
  (asdf:oos 'asdf:load-op :manifest-search-test))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
