;; -*- lisp -*-

(cl:defpackage :manifest-search
  (:use :cl :cl-user :iterate)
  (:shadowing-import-from :alexandria :ensure-list)
  (:export
   :index-package
   :index-packages
   :indexed-packages
   :search-manifest
   :print-index-contents
   :load-index
   :close-index
   :+index-path+
   :ql-installable-systems
   ))

(in-package :manifest-search)

;;
;; UTILS
;;

(defun join-strings (list
                     &optional (delim ", ")
                     (ignore-empty-strings-and-nil t))
  "Join a list of strings into a single longer string
   (optionally with a delimiter and ignoring empty/nil elements)
  "
  (collectors:with-string-builder-output
      (collect :delimiter delim
        :ignore-empty-strings-and-nil ignore-empty-strings-and-nil)
    (iter (for i in (ensure-list list)) (collect (%to-s i)))))

(defun %to-s (thing)
  "Turns whatever we were given into an indexable string"
  (typecase thing
    (null "")
    (string thing)
    (package (package-name thing))
    (list (join-strings thing))
    (t (princ-to-string thing))))

(defparameter +index-path+ #P"~/quicklisp/doc-index"
              "The location of the persistent document-index")

(defvar *cl-doc-index* )

(defun load-index ()
  (unless *cl-doc-index*
    (setf *cl-doc-index*
          (make-instance
           'montezuma:index
           :path +index-path+

           ;; :create-p T
           :create-if-missing-p T
   
           :default-field "*"
           :fields (mapcar #'%to-s '(:name :nicknames :type :package :documentation :readme))
           ))))

(load-index)

(defun close-index ()
  (montezuma:flush *cl-doc-index*)
  (montezuma:close *cl-doc-index*)
  (setf *cl-doc-index* nil))


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

(defun make-field (name value &optional
                              (tokenize? t))
  (montezuma:make-field
   (%to-s name)
   (%to-s value)
   :index (if tokenize? :tokenized :untokenized)))

(defun make-default-doc (thing
                         &optional type package
                         &aux (docs (manifest::docs-for thing type)))
  ;; TODO: figure out how to filter by missing documentation then always return
  ;; the doc instead of not adding docs with missing documentation
  (when docs
    (doc-with-fields
     (make-field :id (cl-doc-key package thing type) nil)
     (make-field :name thing)
     (make-field :type type nil)
     (make-field :package package nil)
     (make-field :documentation docs))))

(defun make-package-doc (package &optional (type :package) package-package)
  (doc-with-fields
   (make-field :id (cl-doc-key nil package type)  nil)
   (make-field :name (package-name package))
   (make-field :nicknames (package-nicknames package))
   (make-field :type type nil)
   (make-field :package package-package nil)
   (make-field :documentation (documentation package t))
   (make-field :readme (manifest::readme-text package))))

(defun add-to-index (thing type &optional
                     package (index *cl-doc-index*)
                     &aux (doc-fn (doc-fn type))
                     (new-doc (funcall doc-fn thing type package))
                     )
  "add documentation for a specific thing of type from a package "
  (when new-doc
    (montezuma:delete-document index (cl-doc-key-term package thing type))
    (montezuma:add-document-to-index index new-doc)
    (montezuma:flush index)))

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

(defun indexed-packages ()
  (collectors:with-collector-output (rtn)
    (%docs-for-term
     :type :package
     (lambda (d) (rtn (intern (doc-value d :name) :keyword))))))

(defun get-doc (idx)
  "Get a document for the thing passed in"
  (etypecase idx
    (montezuma:document idx)
    ((or integer montezuma::term string)
     (ignore-errors
      (montezuma:get-document *cl-doc-index* idx)))))

(defun %docs-for-term (n v fn)
  (let* ((term (make-term n v))
         (docs (montezuma::term-docs-for (montezuma:reader *cl-doc-index*) term)))
    (when docs
      (unwind-protect
           (iter
             (while (montezuma::next? docs))
             (funcall fn (get-doc (montezuma::doc docs))))
        (montezuma::close docs)))))

(defun docs-for-term (n v)
  (collectors:with-collector-output (rtn)
    (%docs-for-term n v #'rtn)))

(defun print-docs-for-term (n v &optional (stream t))
  (iter (for d in (docs-for-term n v))
    (print-doc d stream)))

(defun doc-value (idx field
                  &aux
                  (field (%to-s field))
                  (doc (get-doc idx))
                  (d-field (when doc (montezuma:document-field doc field))))
  "When we have a document and the doc has the field, get the field value "
  (when d-field
    ;; this breaks when the field doesnt exist
    (montezuma:document-value doc field)))

(defun print-default-search ( &optional (limit 1000) )
  "Prints the package, name and type of every document in the index (up to limit)"
  (search-manifest "*" :n limit :show-docs? nil))

(defun print-index-contents ( )
  "Prints the package, name and type of every document in the index (up to limit)"
  (iter (for what in (list* :package manifest::*categories*))
    (format T "~%~A:~%-------------" what )
    (print-docs-for-term :type what)
    (format T "~%-------------~%")))

(defun cl-doc-key (package name type)
  (format nil "~A:~A:~A"
          (typecase package
            (package (package-name package))
            (t package))
          name type))

(defun cl-doc-key-term (package name type)
  (make-term :id (cl-doc-key package name type)))

(defun make-term ( field value )
  (montezuma:make-term
   (%to-s field)
   (%to-s value)))

(defun find-doc-by-key (package name type)
  (get-doc (cl-doc-key-term package name type)))

(defun search-manifest (phrase
                        &key
                          (stream T)
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
     (print-doc d stream "{~a}" score)
     (when show-docs?
       (format stream "~4T~A~%~%" (doc-value d :documentation))))
   (list :num-docs n)))

(defun print-doc (d &optional (stream t) (message "") &rest args)
  (format stream "~%~A:~A <~A> ~?"
          (doc-value d :package)
          (doc-value d :name)
          (doc-value d :type)
          message args
          ))

(defun ql-installed-systems ()
  (iter (for d in (ql-dist:enabled-dists))
    (appending (ql-dist:installed-systems d))))

(defun ql-installable-systems ()
  (iter (for d in (ql-dist:enabled-dists))
    (appending (ql-dist:provided-systems d))))

(defun asdf-loaded-systems () asdf::*defined-systems*)




