;;;; common-otp.lisp

(in-package #:common-otp)

;;; "common-otp" goes here. Hacks and glory await!
(defparameter *core-channel* (lparallel.kernel:make-channel))
(defparameter *next-pid* 0)
(defparameter *current-process* nil)

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
    :initform nil)
   (state
    :accessor state
    :initarg :state
    :initform :alive)))

(defun process-equal (process1 process2)
  (= (pid process1) (pid process2)))

(defmethod print-object ((object process) stream)
  (print-unreadable-object (object stream)
    (format stream "0.~a.0" (pid object))))

(defun wrap-initial-f (process f)
  (lambda () (let ((*current-process* process)) 
          (unwind-protect (funcall f)
            (notify-links process)
            (notify-monitors process)
            (setf (state process) :dead)))))

(defun notify-links (process)
  (loop for l in (links process) do (send l (list 'exit process))))

(defun notify-monitors (process)
  (loop for m in (monitors process) do (send m (list 'exit process))))

(defun spawn (f)
  (let* ((channel (lparallel:make-channel))
         (mbox (lparallel.queue:make-queue))
         (process (make-instance 'process :initial-func f :mailbox mbox)))
    (lparallel.kernel:submit-task channel (wrap-initial-f process f))
    process))

(defun send (process message)
  (when process
    (let ((mbox (mailbox p)))
      (lparallel.queue:push-queue message mbox))))

(defun self () *current-process*)

(defun is-process-alive (process)
  (eql (state process) :alive))

(defun receive (&optional timeout)
  (let ((mbox (mailbox *current-process*)))
    (if timeout
        (lparallel.queue:try-pop-queue mbox :timeout timeout)
        (lparallel.queue:pop-queue mbox))))
