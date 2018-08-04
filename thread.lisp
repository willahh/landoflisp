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


(defparameter *rich*
  (make-instance 'bank-account
                 :id 1
                 :name "Rich"
                 :balance 0))

(defun demo-race-condition ()
  (loop repeat 100
     do
       (bt:make-thread
        (lambda ()
          (loop repeat 10000 do (deposit *rich* 100))
          (loop repeat 10000 do (withdraw *rich* 100))))))

(defvar *lock* (bt:make-lock))

(defun demo-race-condition-locks ()
  (loop repeat 100
     do
       (bt:make-thread
        (lambda ()
          (loop repeat 10000 do (bt:with-lock-held (*lock*)
                                  (deposit *rich* 100)))
          (loop repeat 10000 do (bt:with-lock-held (*lock*)
                                  (withdraw *rich* 100)))))))


(defmacro until (condition &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
          (if ,condition
              (return-from ,block-name nil)
              (progn
                ,@body))))))

(defun join-destroy-thread ()
  (let* ((s *standard-output*)
         (joiner-thread (bt:make-thread
                         (lambda ()
                           (loop for i from 1 to 10
                              do
                                (format s "~%[Joiner Thread]  Working...")
                                (sleep (* 0.01 (random 100)))))))
         (destroyer-thread (bt:make-thread
                            (lambda ()
                              (loop for i from 1 to 1000000
                                 do
                                   (format s "~%[Destroyer Thread] Working...")
                                   (sleep (* 0.01 (random 10000))))))))
    (format t "~%[Main Thread] Waiting on joiner thread...")
    (bt:join-thread joiner-thread)
    (format t "~%[Main Thread] Done waiting on joiner thread")
    (if (bt:thread-alive-p destroyer-thread)
        (progn
          (format t "~%[Main Thread] Destroyer thread alive... killing it")
          (bt:destroy-thread destroyer-thread))
        (format t "~%[Main Thread] Destroyer thread is already dead"))
    (until (bt:thread-alive-p destroyer-thread)
           (format t "[Main Thread] Waiting for destroyer thread to die..."))
    (format t "~%[Main Thread] Destroyer thread dead")
    (format t "~%[Main Thread] Adios!~%")))