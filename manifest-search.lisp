;; -*- lisp -*-

(cl:defpackage :manifest-search
  (:use :cl :cl-user :iterate)
  (:shadowing-import-from :alexandria :ensure-list)
  (:export

   ))

(in-package :manifest-search)

(defparameter *cl-doc-index*
  (make-instance
   'montezuma:index
   :path "~/lisp/doc-index"
   :default-field "*"))

(defun join-strings (list
                     &optional (delim ", ")
                     (ignore-empty-strings-and-nil t))
  (collectors:with-string-builder-output
      (collect :delimiter delim
        :ignore-empty-strings-and-nil ignore-empty-strings-and-nil)
    (iter (for i in (ensure-list list)) (collect (%to-s i)))))

(defun doc-fn (type)
  (handler-case
      (fdefinition (intern (format nil "MAKE-~A-DOC" type) :manifest-search))
    (undefined-function ()
      #'make-default-doc)))

(defun doc-with-fields (&rest fields)
  (let ((doc (make-instance 'montezuma:document)))
    (iter (for f in fields)
      (when f (montezuma:add-field doc f)))
    doc))

(defun %to-s (thing)
  "Turns whatever we were given into an indexable string"
  (typecase thing
    (null "")
    (string thing)
    (package (package-name thing))
    (list (join-strings thing))
    (t (princ-to-string thing))))

(defun make-field (name value &optional (index? t))
  (montezuma:make-field
   (%to-s name)
   (%to-s value)
   :index (when index? :tokenized)))

(defun make-default-doc (thing
                         &optional type package
                         &aux (docs (manifest::docs-for thing type)))
  (doc-with-fields
   (make-field :name thing)
   (make-field :type type nil)
   (make-field :package package nil)
   (when docs (make-field :documentation docs))))

(defun make-package-doc (package &optional (type :package) package-package)
  (doc-with-fields
   (make-field :name (package-name package))
   (make-field :nicknames (package-nicknames package))
   (make-field :type type nil)
   (make-field :package package-package nil)
   (make-field :documentation (documentation package t))
   (make-field :readme (manifest::readme-text package))))

(defun add-to-index (thing type &optional
                     package (index *cl-doc-index*)
                     &aux (doc-fn (doc-fn type)))
  (montezuma:add-document-to-index
   index (funcall doc-fn thing type package)))

(defun index-package (package-name)
  (let ((package (find-package package-name)))
    (add-to-index package :package)
    (iter (for what in manifest::*categories*)
      (iter (for name in (manifest::names package what))
        (add-to-index name what package)))
    ))

(defun get-doc (idx)
  "Get a document for the thing passed in"
  (etypecase idx
    (montezuma:document idx)
    (integer
     (ignore-errors
      (montezuma:get-document *cl-doc-index* idx)))))

(defun doc-value (idx field
                  &aux (doc (get-doc idx)))
  "When we have a document get its value"
  (setf field (%to-s field))
  (when doc
    (when (montezuma:document-field doc field)
      ;; this breaks when the field doesnt exist
      (montezuma:document-value doc field))))

(defun print-index-contents ()
  (iter (for i upfrom 0)
    (for d = (get-doc i))
    (while d)
    (format T "~A:~A:~A~%"
            (doc-value d :package)
            (doc-value d :name)
            (doc-value d :type))))

(defun search-manifest (phrase)
  (montezuma:search-each
   *cl-doc-index*
   phrase
   (lambda (d score)
     (format T "~A:~A <~A : ~A> ~%"
             (doc-value d :package)
             (doc-value d :name)
             (doc-value d :type)
             score))))

