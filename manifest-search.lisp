;; -*- lisp -*-

(cl:defpackage :manifest-search
  (:use :cl :cl-user :iterate)
  (:shadowing-import-from :alexandria :ensure-list)
  (:export
   :index-package
   :index-pacakges
   :search-manifest
   :print-index-contents
   ))

(in-package :manifest-search)

(defparameter *cl-doc-index*
  (make-instance
   'montezuma:index
   :default-field "*"))

(defun join-strings (list
                     &optional (delim ", ")
                     (ignore-empty-strings-and-nil t))
  (collectors:with-string-builder-output
      (collect :delimiter delim
        :ignore-empty-strings-and-nil ignore-empty-strings-and-nil)
    (iter (for i in (ensure-list list)) (collect (%to-s i)))))

(defun doc-fn (type)
  "Try to find a specific make-doc function, failing that return make-default-doc"
  (handler-case
      (fdefinition (intern (format nil "MAKE-~A-DOC" type) :manifest-search))
    (undefined-function ()
      #'make-default-doc)))

(defun doc-with-fields (&rest fields)
  "A shortcut for making a new montezuma document with field data"
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
  ;; TODO: figure out how to filter by missing documentation then always return
  ;; the doc instead of not adding docs with missing documentation
  (when docs
    (doc-with-fields
     (make-field :name thing)
     (make-field :type type nil)
     (make-field :package package nil)
     (make-field :documentation docs))))

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
                     &aux (doc-fn (doc-fn type))
                     (document (funcall doc-fn thing type package)))
  "add documentation for a specific thing of type from a package "
  (when document
    (montezuma:add-document-to-index index document)))

(defun index-package (package-name)
  "Add package documentation and docs for all public symbols to the index"
  (let ((package (find-package package-name)))
    (add-to-index package :package)
    (iter (for what in manifest::*categories*)
      (iter (for name in (manifest::names package what))
        (add-to-index name what package)))
    ))

(defun index-packages (&key
                       (packages (list-all-packages))
                       (exclude-packages))
  "For every package in packages (defaults to all packages)
   excepting exclude-packages, index the documentation for all
   public symbols"
  (iter (for p in packages)
    (unless (member packages exclude-packages :key #'find-package)
      (index-package p))))

(defun get-doc (idx)
  "Get a document for the thing passed in"
  (etypecase idx
    (montezuma:document idx)
    (integer
     (ignore-errors
      (montezuma:get-document *cl-doc-index* idx)))))

(defun doc-value (idx field
                  &aux
                  (field (%to-s field))
                  (doc (get-doc idx))
                  (d-field (when doc (montezuma:document-field doc field))))
  "When we have a document and the doc has the field, get the field value "
  (when d-field
    ;; this breaks when the field doesnt exist
    (montezuma:document-value doc field)))

(defun print-index-contents ( &optional (limit 1000) )
  "Prints the package, name and type of every document in the index (up to limit)"
  (iter (for i upfrom 0)
    (when limit (while (< i limit)))
    (for d = (get-doc i))
    (while d)
    (format T "~A:~A <~A>~%"
            (doc-value d :package)
            (doc-value d :name)
            (doc-value d :type))))

(defun search-manifest (phrase
                        &key
                        (n 25)
                        (show-docs? t))
  "Searches through all of the known common lisp (in-process)
   documentation for the query and displays the results

   n: The n highest scoring documents will be printed to stdout

   if show-docs? is nil we will only print the package:name of
   the objects that match"
  (montezuma:search-each
   *cl-doc-index*
   phrase
   (lambda (d score)
     (format T "~A:~A <~A : ~A> ~%"
             (doc-value d :package)
             (doc-value d :name)
             (doc-value d :type)
             score)
     (when show-docs?
       (format T "~4T~A~%~%" (doc-value d :documentation))))
   (list :num-docs n)))

(defun ql-installed-systems ()
  (iter (for d in (ql-dist:enabled-dists))
    (appending (ql-dist:installed-systems d))))

(defun asdf-loaded-systems () asdf::*defined-systems*)




