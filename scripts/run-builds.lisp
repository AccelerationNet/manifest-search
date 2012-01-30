(in-package :cl-user)

;;; INIT FROM SBCL.RC - running as --script prevents this from loading otherwise
(require 'asdf)
(require 'split-sequence)
(asdf:load-system :iterate)

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #-(or sbcl allegro lispworks) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
;;; END INIT

;;; FORWARD REFERENCE PACKAGE so I can load from the correct location
;;; after setting repository settings
(cl:defpackage :manifest-search
  (:use :cl :cl-user :iterate)
  (:export
   :index-package
   :index-packages
   :indexed-packages
   :search-manifest
   :print-index-contents
   :load-index
   :close-index
   :reload-index
   :switch-index
   :+index-path+
   :ql-installable-systems
   ))

;; Create this package and get in it
(defpackage #:ql-builder
  (:use #:cl #:cl-user #:iterate)
  (:export #:run-build #:load-system #:test-system))

(in-package :ql-builder)


;; removes quotes if the arg comes quoted (for some reason run-build does this)
(defun process-arg (arg)
  (let ((last (- (length arg) 1)))
    (if (and (char-equal (elt arg 0) #\")
	     (char-equal (elt arg last) #\"))
	(subseq arg 1 last)
	arg
	)))

(defun process-args (args)
  (mapcar #'process-arg args))

(defvar *args* (process-args sb-ext:*posix-argv*)
  "The command line arguments")

(defun has-arg (name)
  (member name *args* :test #'string-equal))

(defun arg-value (name)
  (cadr (has-arg name)))

(defun arg-system-list (name)
  (read-system-list-from-client-string
   (arg-value name)))

(defmacro with-load-system-error-handling (() &body body)
  `(let ((recompile-count 0)
	 (retry-count 0)
         (continue-count 0))
     (flet ((standard-error-handling (c)
              (when (and (< continue-count 5)
                         (find-restart 'continue))
                (continue))
	      (when (and (< recompile-count 5)
			 (find-restart 'asdf:try-recompiling))
		(incf recompile-count)
		(invoke-restart 'asdf:try-recompiling))
	      (when (and (< retry-count 5)
			 (find-restart 'asdf:retry))
		(incf retry-count)
		(invoke-restart 'asdf:retry))
	      (%log "~S:~A~%~S~%~S~% after ~A recompile attempts and after ~A retry attempts~%~%"
                    (type-of c)
                    c c
                    (compute-restarts)
                    recompile-count retry-count)))
       (handler-bind
	   ((asdf:missing-dependency
	     (lambda (c)
	       ;; if we are looking at local libs only then
	       ;; we dont want to auto quicklisp them because it probably means there is a
	       ;; dependancy error somewhere 
	       (when *quicklisp-only*
		 (%log "~%~%ATTEMPTING TO LOAD MISSING DEP ~S WITH QUICKLISP~%~%"
                       (asdf::missing-requires c))
		 (quicklisp:quickload (asdf::missing-requires c)))
	       (standard-error-handling c)))
            (asdf:compile-error
              (lambda (c) (standard-error-handling c)))
	    (error #'standard-error-handling))
	 ,@body
	 ))))

(defun %log (message &rest args)
  (force-output)
  (format t "~%~?" message args)
  (force-output))


;; if we only want to use dependancies specified and
;; pulled with quicklisp
(defvar *quicklisp-only* nil)

;; load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun setup-source-registry ()
  (%log "Using central source registry")
  (asdf:initialize-source-registry
   '(:source-registry
     (:tree "/usr/local/share/common-lisp/source")
     :ignore-inherited-configuration))
  ;; all fasls should go to the users directory
  (asdf:initialize-output-translations
   '(:output-translations
     :ignore-inherited-configuration
     (T (:user-cache :implementation)))))

(defun setup-quicklisp-source-registry ()
  ;; use only quicklisp and a directory full of fasls for the local libs you
  ;; are trying to test
  (%log "Using only QUICKLISP registry")
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(truename "~/quicklisp/"))
     :ignore-inherited-configuration))
  ;; all fasls go to the user directory
  (asdf:initialize-output-translations
   '(:output-translations
     :ignore-inherited-configuration
     (T (:user-cache :implementation)))))

(defun load-system (system)
  ;; to be run in the remote version
  (let ((old-packages (list-all-packages)))
    (with-load-system-error-handling ()
      (ql:quickload system))
    (let ((new-packages (set-difference (list-all-packages) old-packages)))
      (index-loaded-packages new-packages))))

(defun load-systems (systems)
  (mapcar #'load-system systems))

(defun test-system (system)
  ;; to be run in the remote version
  (load-system system)
  (with-load-system-error-handling ()
    (asdf:test-system system)))

(defun test-systems (systems)
  (mapcar #'test-system systems))

(defun package-keyword (it)
  (etypecase it
    (package (intern (package-name it) :keyword))
    (string (intern it :keyword))
    (keyword it)))

(defun index-loaded-packages (packages &optional (reindex? nil))
  (let ((indexed-packages
          (unless reindex?
            (manifest-search:indexed-packages))))
    (iter (for p in packages)
      (when (or reindex?
                (not (member (package-keyword p) indexed-packages)))
        (%log "Indexing Package ~A" (package-name p))
        (manifest-search:index-package p))
      )))

(defun run-build (system &key (test t) )
  "From a commotn lisp repl call out to a new sbcl
   process to run a build for you"  
  (sb-ext:run-program
   "sbcl" (list
           "--script" "run-builds.lisp"
           (if test
               "--test-system"
               "--load-system")
           (princ-to-string system))
   :search t
   :output *standard-output*))


(defun keywordize (s) (intern (string-upcase s) :keyword))
(defun read-system-list-from-client-string (s)
  "read in a space separated list of system names optionally surrounded by ()"
  (unless s (return-from read-system-list-from-client-string nil))
  (let ((last (- (length s) 1)))
    (when (and (char-equal (elt s 0) #\()
	       (char-equal (elt s last) #\)))
      (setf s (subseq s 1 last))))
   (mapcar
    #'keywordize
    (split-sequence:split-sequence
     #\space s
     :test #'char-equal :remove-empty-subseqs t)))


(defun rebuild-the-basic-index ()
  (%log "About to build everything in quicklisp.~% Closing Index")
  (manifest-search:close-index)
  (let ((dir (princ-to-string (truename manifest-search:+index-path+))))
    (%log "Removing index: ~A" dir)
    (sb-ext:run-program "rm" (list "-rf" dir) :search T))
  (%log "Recreating index")
  (manifest-search:load-index)
  (%log "Reloading basic packages index")
  (manifest-search:index-packages))

(defun build-all-quicklisp ()
  (let* ((core sb-int:*core-string*)
         (sbcl-name ;;(if core "sbcl" (truename "~/sbcl-run-build.sh"))
           "sbcl")
         args )
    (time
     (progn
       (unless (has-arg "--keep-index")
         (rebuild-the-basic-index))
       (manifest-search:close-index)
       (iter
         (for sys in (manifest-search:ql-installable-systems))
         (for n = (ql-dist:name sys))
         (setf args (append
                     (when core (list "--core" core))
                     (when (has-arg "--quicklisp-only")
                       (list "--quicklisp-only"))
                     (list "--keep-fasls" "--keep-index" "--load-system" n)))
         (%log " -- External LOADING: ~A - ~A ~s" n sbcl-name args )
         (time
          (sb-ext:run-program
           sbcl-name
           args
           :search T
           :output *standard-output*
           :error :output)
          ))
       (format T "Done Loading Everything!~% Reindex Base")
     
       (manifest-search:load-index)
       ))))

(defun run-test ()
  (let* ((core sb-int:*core-string*)
         (sbcl-name ;;(if core "sbcl" (truename "~/sbcl-run-build.sh"))
           "sbcl")
         (args (append
                (when core (list "--core" core))
                (when (has-arg "--quicklisp-only")
                  (list "--quicklisp-only"))
                (list "--keep-index" ))) )
    (%log " -- External LOADING: Running test - ~A ~s~%"  sbcl-name args )
    (time
     (sb-ext:run-program
      sbcl-name
      args
      :search T
      :output *standard-output*
      :error :output))
    (%log " -- END Running test - ~A ~s~%" sbcl-name args )))


(defun run-tests&builds ()
  (%log "Running Build test ARGS:~S~%------------" sb-ext:*posix-argv*)
  (let ((test-systems (arg-system-list "--test-system"))
        (load-systems (arg-system-list "--load-system")))
    (when load-systems
      (%log "Running load-system ~S" load-systems)
      (mapcar #'load-system load-systems))
    (when test-systems
      (%log "Running test-system ~S" test-systems)
      (mapcar #'test-system test-systems))
    ))

(defun need-to-load-manifest-search? ()
  (null
   (handler-case (fdefinition 'manifest-search:index-package)
     (undefined-function ()))))


(defun main () 
  "Handle this file having been run as a script "
  (setf *args* (process-args sb-ext:*posix-argv*))
  (%log ">> ARGS:~S" *args*)
  ;;(*quicklisp-only* (has-arg "--quicklisp-only"))
  (if (has-arg "--quicklisp-only")
      (setup-quicklisp-source-registry)
      (setup-source-registry))
  (if (need-to-load-manifest-search?)
    (with-load-system-error-handling ()
      (ql:quickload :manifest-search)))
  (manifest-search:reload-index)

  (if (has-arg "--make-core")
      (sb-ext:save-lisp-and-die
       "manifest-search-core"
       :toplevel #'main))
  
  (unless (has-arg "--keep-fasls")
    (%log "Deleting fasls")
    (sb-ext:run-program
     "delete-fasls.sh" (list (princ-to-string (truename "~/")))
     :search t))
  (cond
    ((has-arg "--build-world") (build-all-quicklisp))
    ((or (has-arg "--test-system")
         (has-arg "--load-system"))
     (run-tests&builds))
    ((has-arg "--run-test")
     (run-test))
    (t (print-help))))

(defun print-help ()
  (%log "
 --quicklisp-only A flag to say only use quicklisp files
                 (ie dont load the central registry)

 --keep-fasls A flag to say dont delete the fasls first thing

 --build-world Load every quicklisp system (which should index their docs)
  --keep-index When building world a flag to say dont rebuild the
    index from scratch. (Append only)


 --make-core slad after getting everything ready to run, needed to --build-world

 --test-system (list of-system names)
   runs asdf:test-op on a system after loading it

 --load-system (list of-system names)
   ql:quickload a list of systems system
 
"))

(funcall 'main)


#|

(in-package :ql-builder)
(load-system :alexandria :force t)

|#