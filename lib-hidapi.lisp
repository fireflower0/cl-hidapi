(defpackage :cl-hidapi/lib-hidapi
  (:use :cl
        :cffi
        :alexandria)
  (:export :hid-device-info
           :hid-init
           :hid-exit
           :hid-enumerate
           :hid-free-enumeration
           :hid-open
           :hid-open-path
           :hid-write
           :hid-read
           :hid-read-timeout
           :hid-set-nonblocking
           :hid-send-feature-report
           :hid-get-feature-report
           :hid-close
           :hid-get-manufacturer-string
           :hid-get-product-string
           :hid-get-serial-number-string
           :hid-get-indexed-string
           :hid-last-error))
(in-package :cl-hidapi/lib-hidapi)

(define-foreign-library hidapi
  (:unix (:or "/usr/local/lib/libhidapi-libusb.so"
              "/usr/local/lib/libhidapi-hidraw.so")))

(use-foreign-library hidapi)

(defcstruct hid-device-info
  (:path :string)
  (:vendor-id :unsigned-short)
  (:product-id :unsigned-short)
  (:serial-number :string)
  (:release-number :unsigned-short)
  (:manufacturer :string)
  (:product :string)
  (:usage-page :unsigned-short)
  (:usage :unsigned-short)
  (:interface-number :int)
  (:next (:pointer (:struct hid-device-info))))

(defcfun ("hid_init" hid-init) :void)

(defcfun ("hid_exit" hid-exit) :void)

(defcfun ("hid_enumerate" hid-enumerate) (:pointer (:struct hid-device-info))
  (vendor-id :unsigned-short)
  (product-id :unsigned-short))

(defcfun ("hid_free_enumeration" hid-free-enumeration) :void
  (devs (:pointer (:struct hid-device-info))))

(defcfun ("hid_open" hid-open) :pointer
  (vendor-id :unsigned-short)
  (product-id :unsigned-short)
  (serial-number :string))

(defcfun ("hid_open_path" hid-open-path) :pointer
  (path :string))

(defcfun ("hid_write" hid-write) :int
  (device :pointer)
  (data (:pointer :unsigned-char))
  (length :unsigned-long))

(defcfun ("hid_read" hid-read) :int
  (device :pointer)
  (data (:pointer :unsigned-char))
  (length :unsigned-long))

(defcfun ("hid_read_timeout" hid-read-timeout) :int
  (device :pointer)
  (data (:pointer :unsigned-char))
  (length :unsigned-long)
  (milliseconds :int))

(defcfun ("hid_set_nonblocking" hid-set-nonblocking) :int
  (device :pointer)
  (nonblock :int))

(defcfun ("hid_send_feature_report" hid-send-feature-report) :int
  (device :pointer)
  (data (:pointer :unsigned-char))
  (length :unsigned-long))

(defcfun ("hid_get_feature_report" hid-get-feature-report) :int
  (device :pointer)
  (data (:pointer :unsigned-char))
  (length :unsigned-long))

(defcfun ("hid_close" hid-close) :void
  (device :pointer))

(defcfun ("hid_get_manufacturer_string" hid-get-manufacturer-string) :int
  (device :pointer)
  (string :string)
  (maxlen :unsigned-long))

(defcfun ("hid_get_product_string" hid-get-product-string) :int
  (device :pointer)
  (string :string)
  (maxlen :unsigned-long))

(defcfun ("hid_get_serial_number_string" hid-get-serial-number-string) :int
  (device :pointer)
  (string :string)
  (maxlen :unsigned-long))

(defcfun ("hid_get_indexed_string" hid-get-indexed-string) :int
  (device :pointer)
  (index :int)
  (string :string)
  (maxlen :unsigned-long))

(defcfun ("hid_last_error" hid-last-error) :string
  (device :pointer))
