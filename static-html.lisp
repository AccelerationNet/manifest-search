(in-package :manifest-search)
(cl-interpol:enable-interpol-syntax)

(defclass package-docs ()
  ((pname :accessor pname :initarg :pname :initform nil)
   (sections :accessor sections :initarg :sections :initform nil)))

(defclass doc-section ()
  ((pname :accessor pname :initarg :pname :initform nil)
   (sigil-type :accessor sigil-type :initarg :sigil-type :initform nil)
   (items :accessor items :initarg :items :initform nil)))

(defclass doc-item ()
  ((pname :accessor pname :initarg :pname :initform nil)
   (exported? :accessor exported? :initarg :exported? :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (docs :accessor docs :initarg :docs :initform nil)
   (sigil-type :accessor sigil-type :initarg :sigil-type :initform nil)
   (argslist :accessor argslist :initarg :argslist :initform nil)))

(defun sort-items (items)
  (stable-sort
   items
   #'string-lessp
   :key (lambda (x &aux (n (name x)))
          (typecase n
            ((or string symbol) (string n))
            (list (string (second n)))))))

(defun make-doc-section (pname sigil-type)
  (make-instance 'doc-section
                 :pname pname
                 :sigil-type sigil-type
                 :items (sort-items
                         (iter (for name in (manifest::names pname sigil-type))
                           (collect (make-doc-item name sigil-type pname))))))

(defun make-package-docs (pname)
  (make-instance
   'package-docs
   :pname pname
   :sections (iter (for what in manifest::*categories*)
               (collect (make-doc-section pname what)))))

(defun is-symbol-exported? (sym package)
  (etypecase sym
    (symbol (is-symbol-exported? (string sym) package))
    (string (eql :external (nth-value 1 (find-symbol sym package))))
    (list (if (eql 'setf (first sym))
              (is-symbol-exported? (second sym) package)
              nil))))

(defmethod documented? ((item doc-item)) (docs item))

(defun make-doc-item (name what package-name)
  (make-instance 'doc-item
                 :name name
                 :sigil-type what
                 :argslist (argslist-maybe name)
                 :pname package-name
                 :exported? (is-symbol-exported? name package-name)
                 :docs (manifest::docs-for name what)))

(defun %items-of (thing)
  (etypecase thing
    (doc-section (items thing))
    (list thing)))

(defun doc-items-matching (items
                            &key what
                            (documented? nil documented?-provided)
                            (exported? nil exported?-provided))
  (setf items (%items-of items))
  (iter (for item in items)
    (when (and what (not (eql what (sigil-type item))))
      (next-iteration))
    (when (and documented?-provided (alexandria:xor documented? (documented? item)))
      (next-iteration))
    (when (and exported?-provided (alexandria:xor exported? (exported? item)))
      (next-iteration))
    (collect item)))

(defmethod html-of ((it list))
  (mapcar #'html-of it))

(defmethod html-of ((item doc-item))
  (with-accessors ((name name) (argslist argslist) (docs docs))
      item
    (html5:section `(:class "item" :id ,(dom-id-of name))
      (html5:h1 `() name (when argslist #?" (@{argslist})"))
      (when docs
        (html5:div '(:class "doc") docs)))))

(defmethod html-of ((sec doc-section))
  (flet ((items-of (doc? exp?)
           (html-of
            (doc-items-matching
             sec :what (sigil-type sec)
             :documented? doc? :exported? exp?))))
    (let ((pub (items-of t t))
          (pub-un (items-of nil t))
          (priv (items-of t nil))
          (priv-un (items-of nil nil)))
      (when (or pub pub-un priv priv-un)
        (html5:section ()
          (html5:h1 () (sigil-type sec))
          (when (or pub pub-un)
            (html5:section `(:class "public")
              (html5:h1 () "Public")
              pub
              (when pub-un
                (html5:section `(:class "no-docs")
                  (html5:h1 () "Undocumented")
                  pub-un
                  (html5:div `(:class "clearer"))))))
          (when (or priv-un priv)
            (html5:section `(:class "private")
              (html5:h1 () "Private")
              priv
              (when priv-un
                (html5:section `(:class "no-docs")
                  (html5:h1 () "Undocumented")
                  priv-un
                  (html5:div `(:class "clearer")))))))))))

(defmethod html-of ((p package-docs))
  (page-template
   (pname p)
   (iter (for sec in (sections p))
     (collect (html-of sec)))))

(defun create-package-html (package &optional (overwrite? t))
  (let* ((pname (package-keyword package))
         (*print-case* :downcase)
         (path (index-html-path (package-to-file-name pname))))
    (when (or overwrite?
              (not (cl-fad:file-exists-p path)))
      (let ((docs (make-package-docs pname)))
        (buildnode:with-html5-document-to-file (path)
          (html-of docs))))))

(defun remove-html-index ()
  (cl-fad:delete-directory-and-files (index-html-path)))

(defun ensure-package-html (package)
  (create-package-html package nil))

;(defun make-package-json (package))

(defun index-html-path (&optional name (root +index-path+))
  (let ((dir (merge-pathnames #p"html/" root)))
    (cl:ensure-directories-exist dir)
    (if name
        (merge-pathnames name dir)
        dir)))

(defun index-json-path (package-name &optional (root +index-path+))
  (let ((dir (merge-pathnames (merge-pathnames #p"json/" root))))
    (cl:ensure-directories-exist dir)
    (merge-pathnames (format nil "~a.json" package-name) dir)))

(defun package-to-file-name (package)
  #?"${package}.html")

(defun dom-id-of (name)
  (cl-ppcre:regex-replace-all
   #?r"%|\$|\||\+|\*"
   (symbol-munger:english->underscores name)
   "_"))

(defun page-template (package &rest body)
  (html5:html ()
    (html5:head ()
      (html5:title () package " | Common Lisp Package")
      (html5:link '(:type "text/css" :href "style.css" :rel "stylesheet")))
    (html5:body ()
      (html5:div '(:class "page")
        (html5:header ()
          (html5:h1 '(:class "title")
            "Common Lisp Package: " package)
          (let ((url (web-address-for-package package)))
            (html5:div () "Quicklisp Source: " (html5:a `(:href ,url) url)))
          (html5:section '(:class "doc")
            (documentation (find-package package) t))
          (html5:section '(:class "readme")
            (html5:h1 () "README: ")
            (readme-html package)))
        (html5:article () body)
        (html5:footer ()
          (html5:a '(:href "index.html")
            "view the full quicklisp package index"))))))

(defun escape-for-html (txt)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (when txt
    (with-output-to-string (out)
      (with-input-from-string (in txt)
        (loop for char = (read-char in nil nil)
              while char
              do (case char
                   ((#\<) (write-string "&lt;" out))
                   ((#\>) (write-string "&gt;" out))
                   ((#\&) (write-string "&amp;" out))
                   (otherwise (write-char char out))))))))

(defun readme-html (package) 
  (let* ((pname (package-keyword package))
         (pth (manifest::find-readme pname))
         (rm (escape-for-html
              (manifest::readme-text pname))))
    (if (and pth (string-equal "md" (pathname-type pth)))
        ;; TODO: make buildnode / CXML:DOM be an available renderer for markdown
        (buildnode:inner-html
         (nth-value
          1 (cl-markdown:markdown rm :stream nil)))
        (html5:div `(:class "txt") rm))))

(defmethod create-stylesheet ()
  (alexandria:write-string-into-file
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :manifest-search "style.css"))
   (index-html-path "style.css")
   :if-exists :SUPERSEDE)
  (values))

(defun get-uri-for-source (src)
  (cl-ppcre:scan-to-strings #?r"\w*://[^\s]*" src))

(defun web-address-for-source (src)
  (let* ((uri (get-uri-for-source src))
         (github? (cl-ppcre:scan "github" uri)))
    (cond
      (github? (cl-ppcre:regex-replace "\.git$"
                (cl-ppcre:regex-replace "git://" uri "http://")
                ""))
      (t uri))))

(defun web-address-for-package (package)
  (web-address-for-source (quicklisp-origin package)))

(defun quicklisp-origin (ql-name)
  (let* ((ql-name (string-downcase (package-keyword ql-name)))
         (pth (asdf:system-relative-pathname
               :manifest-search #?"quicklisp-projects/${ql-name}/source.txt"))
         (content (ignore-errors (alexandria:read-file-into-string pth))))
    (values
     (web-address-for-source content)
     content pth)))

(defun index-list-items ()
  (iter (for file in (cl-fad:list-directory (index-html-path)))
    (for ext = (pathname-type file))
    (unless (string-equal ext "html")
      (next-iteration))
    (for name = (pathname-name file))
    (when (member name '("index") :test #'string-equal)
      (next-iteration))
    (for u = (web-address-for-package name))
    (collect (html5:li ()
               (html5:a `(:href ,#?"${name}.html") name)
               (when u (list " - origin: " (html5:a `(:href ,u) u)))))))

(defun create-index-html-from-files ()
  (buildnode:with-html5-document-to-file ((index-html-path "index.html"))
    (html5:html ()
      (html5:head ()
        (html5:title () "Quicklisp Documentation Index"))
      (html5:body ()
        (html5:h1 () "Quicklisp Documentation Index")
        (html5:ul () (index-list-items))))))
