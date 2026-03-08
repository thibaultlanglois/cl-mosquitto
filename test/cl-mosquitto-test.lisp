(defpackage :cl-mosquitto-test
  (:use #:common-lisp #:cl-mosquitto))

(in-package :cl-mosquitto-test)


;;;---Tests & examples----------------------------------------------------------

(defclass temperature-observer (device-observer)
  ()
  (:documentation "An example of observer that do something when temperture changes."))

(defmethod update ((self temperature-observer) value)
  "If temperature is above 19.5 switch smart plug on else switch off."
  (if (> (rest (assoc :temperature value)) 19.5)
      (progn
        (format t "Temperature OK~%")
        (switch-smart-plug1 :on))
      (progn
        (format t "It is cold here!~%")
        (switch-smart-plug1 :off))))


(defun switch-smart-plug1 (on-or-off)
  (publish "SmartPlug1" (list (cons "state" (if (eq on-or-off :on) "ON" "OFF")))))

(defclass smart-plug-observer (device-observer)
  ((data :initform nil :accessor data)
   (state :initform nil :accessor state)
   (last-state :initform nil :accessor last-state)))

(defmethod update ((self smart-plug-observer) value)
  (with-slots (state last-state data) self
    (setf last-state state)
    (setf state (rest (assoc :state value)))
    (unless (equal state last-state)
      (push (list (date-string (get-universal-time))
                  state)
            data))))

(defun test-observable-observer-setup ()
  (setq cl-mosquitto::*devices* nil) ;; just for testing...
  (let* ((temp/humid-sensor (start-device "TempSensor1"))
         (smart-plug (start-device "SmartPlug1"))
         (obs (make-instance 'temperature-observer)))
    (attach temp/humid-sensor obs)
    (values temp/humid-sensor obs smart-plug)))

(defun test-attach-new-observer ()
  (let ((plug-observer (make-instance 'smart-plug-observer)))
    (attach (find-device "SmartPlug1") plug-observer)
    (prog1
        plug-observer
      (format t "(defparameter plug-obs *)~%(print (data plug-obs)~%"))))

(defclass temperature-log (device-observer)
  ((data :accessor data :initform nil)))

(defmethod update ((obs temperature-log) value)
  (setf (data obs)
        (nconc (data obs)
               (list (list (date-string (get-universal-time)) (rest (assoc :temperature value))))))
  (format t "~A | ~A ~A~%" 
          (date-string (get-universal-time))
          (rest (assoc :device value))
          (rest (assoc :temperature value))))

(defun test-attach-new-register-temp-observer ()
  (let ((temp-register (make-instance 'temperature-log)))
    (attach (find-device "TempSensor1") temp-register)
    temp-register))


;;;--Utils---------------------------------------------------------------

(defun date-string (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (format nil "~2,'0d/~2,'0d/~4d ~2,'0d:~2,'0d"
            day month year hour min)))

;; (ql:quickload :sunrise-sunset)

;; (sunrise-sunset:sunrise
;;   :latitude 38.72
;;   :longitude -9.14
;;   :date '(2026 3 7))

;; (sunrise-sunset:sunset
;;   :latitude 38.72
;;   :longitude -9.14
;;   :date '(2026 3 7))
