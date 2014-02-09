
;;; Enable locally installed quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (directory-namestring (truename *default-pathname-defaults*)))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "Could not load quicklisp")))

;;; This prevents ssl loading [so we don't depend on libssl]
(push :hunchentoot-no-ssl cl:*features*)

;;; Load relevant quicklisp packages
(ql:quickload :hunchentoot)
(ql:quickload :cms-query)
(ql:quickload :sb-daemon)
(ql:quickload :cl-syslog)

(defun log-message (level message &rest args)
  "Send a message to the system log. The message is a Lisp format string, args
   will be applied.
   Facility will allways be :daemon.
   Level is one of (:EMERG :ALERT :CRIT :ERR :WARNING :NOTICE :INFO :DEBUG)"
  (cl-syslog:log "cms-queryd" :daemon level (apply #'format nil "[~A] ~?" level message (list  args))))

(defun main ()
  (let ((-done- nil)
        (log-path #p"/tmp/")
        (run-path #p"/tmp/")
        (pid-file #p"cms-queryd.pid"))
    (flet ((signal-handler  (signal)
             (format t "~A received~%" signal)
             (setf -done- t)))
      (sb-daemon:daemonize :exit-parent t
                           :output  (merge-pathnames "stdout.log" log-path)
                           :error   (merge-pathnames "stderr.log" log-path)
                           :pidfile (merge-pathnames pid-file run-path)
                           :sigterm #'signal-handler
                           :sigabrt #'signal-handler
                           :sighup  #'signal-handler
                           :sigint  #'signal-handler))

    (log-message :notice "User init ~A" (funcall  sb-ext:*userinit-pathname-function*))
    (log-message :notice "Starting server")
    #+NIL (setf *swank-server*
          (swank:create-server :port (parse-integer port)
                               :style :sigio
                               :dont-close t))
    (log-message :notice "Server started on ~A, port ~A" cms-query:*address* cms-query:*port*)
    (cms-query:start-server :verbose-p t) ; verbose, just for now
    (loop while (not -done-)
          do (progn
               (sleep 5)
               (log-message :notice "-done- is ~A" -done-)))
    (log-message :notice "Shutting down server")
    (cms-query:stop-server)
    (log-message :notice "Server shut down")
    (sb-ext:quit)))

;;; Hmm, let-sical binding allone doesn't work ...
(setf sb-ext:*sysinit-pathname-function*
      (lambda () #p"/etc/cms-queryd/queryd.conf"))

;;; FIXME: should we emit a warning in case we read from a project configuration?
(setf sb-ext:*userinit-pathname-function*
      (lambda ()
        (let ((user-provided (sb-posix:getenv "CMS_QUERYD_CONF"))
              (project-conf  (merge-pathnames ".cms-queryd.conf" (truename *default-pathname-defaults*)))
              (user-conf     (merge-pathnames ".cms-queryd.conf" (user-homedir-pathname))))

          (if user-provided
              user-provided
              (cond
                ((probe-file project-conf) project-conf)
                ((probe-file user-conf) user-conf))))))




(ensure-directories-exist
 (make-pathname :directory '(:relative  "build")))

;;; sayonara ...
(save-lisp-and-die (make-pathname :directory '(:relative  "build") :name "cms-queryd")
                   :toplevel 'main
                   :executable t)
