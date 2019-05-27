(defpackage :cl-hidapi/examples/hidapi-app
  (:use :cl
        :cffi
        :alexandria
        :cl-hidapi/lib-hidapi)
  (:export :get-device-list
           :device-list-views))
(in-package :cl-hidapi/examples/hidapi-app)

(defun get-device-list (&optional (vendor-id #x0000) (product-id #x0000))
  (unwind-protect
       (let* ((devices (hid-enumerate vendor-id product-id))
              (result  (loop for ptr = devices then (getf device :next)
                             for device = (unless (null-pointer-p ptr)
                                            (mem-ref ptr '(:struct hid-device-info)))
                             while device
                             collect (remove-from-plist device :next))))
         (when devices
           (hid-free-enumeration devices))
         result)))

(defun device-list-views ()
  (let ((devices (get-device-list)))
    (loop for device in devices
          do (format t
                     "Device found~%  type: ~4,'0x ~4,'0x~%  path: ~s~%  serial number ~s~%"
                     (getf device :vendor-id)
                     (getf device :product-id)
                     (getf device :path)
                     (getf device :serial-number))
             (format t "  Manufacturer: ~s~%" (getf device :manufacturer))
             (format t "  Product: ~s~%" (getf device :product))
         (terpri))))
