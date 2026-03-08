
(defpackage :cl-mosquitto
  (:use #:common-lisp)
  (:nicknames #:mosquitto)
  (:export #:device-observer
           #:update
           #:publish
           #:attach
           #:detach
           #:start-device
           #:find-device))

(in-package :cl-mosquitto)

(defparameter *mosquitto-user* "mosquitto")
(defparameter *mosquitto-password* "mosquitto")

(defparameter *devices* nil
  "The list of managed devices as ((<device-name> . <device-instance>)*)")

(defclass device ()
  ((process :initarg :process :accessor process)
   (listening-thread :accessor listening-thread)
   (device-name :initarg :name :accessor device-name)
   (location :initform nil :accessor location)
   (observers :initform nil :accessor observers)))

(defclass plug-device (device)
  ())

(defclass device-observer ()
  ())

(defmethod publish ((self device) alist &key (qos 0))
  "Transforms ALIST into json and publish the result to device."
  (sb-ext:run-program "/usr/bin/mosquitto_pub"
                      (list "-h" "localhost" "-u" *mosquitto-user*
                            "-P" *mosquitto-password*
                            "-t" (format nil "zigbee2mqtt/~A/set" (device-name self))
                            "-q" (format nil "~A" qos)
                            "-m" (cl-json:encode-json-alist-to-string alist))))

(defmethod publish ((device-name string) alist &key (qos 0))
  "Transforms ALIST into json and publish the result to device."
  (sb-ext:run-program "/usr/bin/mosquitto_pub"
                      (list "-h" "localhost" "-u" *mosquitto-user*
                            "-P" *mosquitto-password*
                            "-t" (format nil "zigbee2mqtt/~A/set" device-name)
                            "-q" (format nil "~A" qos)
                            "-m" (cl-json:encode-json-alist-to-string alist))))

(defmethod attach ((self device) (observer device-observer))
  "Register a new observer."
  (pushnew observer (observers self)))

(defmethod detach ((self device) (observer device-observer))
  (setf (observers self)
        (remove observer (observers self))))

(defmethod terminate ((self device))
  "Shut down device closing and killing the mosquitto_sub process.
Terminates the associated listening process. "
  (sb-ext:process-close (process self))
  (sb-ext:process-kill (process self) 9)
  (setf (process self) nil)
  (sb-thread:terminate-thread (listening-thread self))
  (setf (listening-thread self) nil)
  ;; TODO: maybe remove device from *devices* ??
  )

(defmethod register ((self device))
  "Register DEVICE in *DEVICES*."
  (push (cons (device-name self) self)
        *devices*))

(defmethod start-listener ((self device))
  "Creates a listening thread that reads lines emitted by the mosquitto_sub 
command (see SUBSCRIBE)."
  (let ((thread
          (sb-thread:make-thread
           (lambda ()
             (loop
               for i from 1
               for val = (read-line (sb-ext:process-output (process self)) nil :eof)
               do
                  ;; (format t "~D ~A: ~A~%" i (device-name self) val)
                  (when (and val (not (eq val :eof)))
                    (notify-observers self val))
                  (sleep 2)
               until (eq val :eof)           
               finally (format t "EXIT ~A-LISTENER~%" (device-name self))))
           :name (format nil "~A-LISTENER" (device-name self)))))
    (setf (listening-thread self) thread)))

(defmethod subscribe ((self device) &key (qos 0))
  "Runs mosquitto_sub command for DEVICE. Returns a process instance."
  (sb-ext:run-program "/usr/bin/mosquitto_sub"
                      (list "-h" "localhost"
                            "-u" *mosquitto-user*
                            "-P" *mosquitto-password*
                            "-q" (format nil "~D" qos)
                            "-t" (format nil "zigbee2mqtt/~A" (device-name self)))
                      :output :stream
                      :wait nil))

(defmethod notify-observers ((self device) value)
  "Notify device's observers. VALUE (json) is converted to an alist."
  (let ((value
          (unless (equal value "done.")
            (cl-json:decode-json-from-string value))))
    (when value
      (setq value (append (list (cons :device (device-name self)))
                          value))
      (loop
        for obs in (observers self)
        do (update obs value)))))

(defmethod update ((self device-observer) value)
  (warn "specialize this to your needs !~%"))

(defun start-device (device-name)
  "Creates a DEVICE, runs a MQTT subscribe command to device DEVICE-NAME,
and start a listening thread (see START-LISTENER)."
  (let ((device (make-instance 'device :name device-name)))
    (register device)
    (setf (process device)
          (subscribe device))
    (start-listener device)
    device))

(defun find-device (device-name)
  (rest (assoc device-name *devices* :test #'equal)))

