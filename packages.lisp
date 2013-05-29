
(cl:defpackage :manifest-search
  (:use :cl :cl-user :iterate)
  (:shadowing-import-from :alexandria :ensure-list)
  (:export
   :index-package
   :index-packages
   :indexed-packages
   :package-document
   :search-manifest
   :search-manifest-collecting
   :print-index-contents
   :load-index
   :close-index
   :reload-index
   :switch-index
   :+index-path+
   :ql-installable-systems

   #:create-index-html-from-files
   #:create-stylesheet
   #:create-package-html
   #:ensure-package-html
   #:remove-html-index
   ))

