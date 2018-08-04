;; -- Thread tests
(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
         (curr-thread-name (bt:thread-name curr-thread))
         (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(defparameter *counter* 0)

(defun test-update-global-variable ()
  (bt:make-thread
   (lambda ()
     (sleep 1)
     (incf *counter*)))
  *counter*)

(defun print-message-top-level-wrong ()
  (bt:make-thread
   (lambda ()
     (format *standard-output* "Hello from thread!")))
  nil)

(defun print-message-top-level-fixed ()
  (let ((top-level *standard-output*))
    (bt:make-thread
     (lambda ()
       (format top-level "Hello from thread!"))))
  nil)

(eval-when (:compile-toplevel)
  (defun print-message-top-level-reader-macro ()
    (bt:make-thread
     (lambda ()
       (format #.*standard-output* "Hello from thread!")))
    nil))


;;; Modify a shared resource from multiple threads
(defclass bank-account ()
  ((id :initarg :id
       :initform (error "id required")
       :accessor :id)
   (name :initarg :name
         :initform (error "name required")
         :accessor :name)
   (balance :initarg :balance
            :initform 0
            :accessor :balance)))

(defgeneric deposit (account amount)
  (:documentation "Deposit money into the account"))

(defgeneric withdraw (account amount)
  (:documentation "Withdraw amount from account"))

(defmethod deposit ((account bank-account) (amount real))
  (incf (:balance account) amount))

(defmethod withdraw ((account bank-account) (amount real))
  (decf (:balance account) amount))

