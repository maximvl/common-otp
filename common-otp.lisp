;;;; common-otp.lisp

(in-package #:common-otp)

;;; "common-otp" goes here. Hacks and glory await!
(defparameter *processes* (make-hash-table :synchronized t))

(defparameter *core-channel* (lparallel.kernel:make-channel))
(defparameter *next-pid* 0)

(defun make-next-pid ()
  (lparallel.kernel:submit-task *core-channel* (lambda () (incf *next-pid*)))
  (lparallel.kernel:receive-result *core-channel*))

(defclass process ()
  ((pid
    :accessor pid
    :initarg :pid
    :initform (make-next-pid))
   (mailbox
    :accessor mailbox
    :initarg :mailbox
    :initform (error "no mailbox"))
   (initial-func
    :accessor initial-func
    :initarg :initial-func
    :initform #'(lambda () nil))
   (links
    :accessor links
    :initarg :links
    :initform nil)
   (monitors
    :accessor monitors
    :initarg :monitors
    :initform nil)
   (name
    :accessor name
    :initarg :name
    :initform nil)))

(defmethod print-object ((object process) stream)
  (print-unreadable-object (object stream)
    (format stream "0.~a.0" (pid object))))

(defparameter *current-process* nil)

(defun wrap-initial-f (process f)
  (lambda () (let ((*current-process* process)) 
          (unwind-protect (funcall f)
            (notify-links process)
            (notify-monitors process)
            (remhash (pid process) *processes*)))))

(defun notify-links (process)
  (let ((pid (pid process)))
    (loop for l in (links process) do (send l (list 'exit pid)))))

(defun notify-monitors (process)
  (let ((pid (pid process)))
    (loop for m in (monitors process) do (send m (list 'exit pid)))))

(defun spawn (f)
  (let* ((channel (lparallel:make-channel))
         (mbox (lparallel.queue:make-queue))
         (process (make-instance 'process :initial-func f :mailbox mbox))
         (pid (pid process)))
    (lparallel.kernel:submit-task channel (wrap-initial-f process f))
    (setf (gethash pid *processes*) process)
    pid))

(defun pid-process (pid)
  (gethash pid *processes*))

(defmacro with-process (pid name &rest body)
  `(let ((,name (pid-process ,pid)))
     (when ,name ,@body)))

(defun send (pid message)
  (let ((p (pid-process pid)))
    (when p
      (let ((mbox (mailbox p)))
        (lparallel.queue:push-queue message mbox)))))

(defun self ()
  (pid *current-process*))

(defun is-process-alive (pid)
  (multiple-value-bind (p exist) (gethash pid *processes*)
    (declare (ignore p))
    exist))

(defun receive (&optional timeout)
  (let ((mbox (mailbox *current-process*)))
    (if timeout
        (lparallel.queue:try-pop-queue mbox :timeout timeout)
        (lparallel.queue:pop-queue mbox))))
