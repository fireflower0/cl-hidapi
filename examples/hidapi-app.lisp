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
       do (format t "product_id:       ~4,'0x~%" (getf device :product-id))
          (format t "serial_number:    ~s~%"     (getf device :serial-number))
          (format t "manufacturer:     ~s~%"     (getf device :manufacturer))
          (format t "usage:            ~a~%"     (getf device :usage))
          (format t "interface_number: ~a~%"     (getf device :interface-number))
          (format t "product:          ~s~%"     (getf device :product))
          (format t "vendor_id:        ~4,'0x~%" (getf device :vendor-id))
          (format t "release_number:   ~a~%"     (getf device :release-number))
          (terpri))))
