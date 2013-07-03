(in-package #:%hidapi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; fixme: grovel this or something
  (defconstant +wchar-encoding+
    #-windows :utf-32le
    #+windows :utf-16le))

(defctype wchar* (:string :encoding #.+wchar-encoding+))

;; fixme: grovel this too
(defctype size-t :unsigned-long)

(defctype device* :pointer)

(defcstruct device-info
  (:path :string)
  (:vendor-id :unsigned-short)
  (:product-id :unsigned-short)
  (:serial-number wchar*)
  (:release-number :unsigned-short)
  (:manufacturer wchar*)
  (:product wchar*)
  (:usage-page :unsigned-short)
  (:usage :unsigned-short)
  (:interface-number :int)
  (:next (:pointer #++(:struct device-info))))



(defmacro defhidfun (name return-type &body args)
  (flet ((hid-name (name)
           (if (consp name)
               name
               (list name (format nil "hid_~(~a~)" (substitute #\_ #\- (symbol-name name)))))))
    `(defcfun ,(hid-name name) ,return-type
       ,@args)))

(defhidfun init :void)
(defhidfun exit :void)

(defhidfun enumerate (:pointer (:struct device-info))
  (vendor-id :unsigned-short)
  (product-id :unsigned-short))


(defhidfun free-enumeration :void
  (devs (:pointer (:struct device-info))))

(defhidfun open device*
  (vendor-id :unsigned-short)
  (product-id :unsigned-short)
  (serial-number wchar*))

(defhidfun open-path device*
  (path :string))

(defhidfun write :int
  (device device*)
  (data (:pointer :unsigned-char))
  (length size-t))

(defhidfun read :int
  (device device*)
  (data (:pointer :unsigned-char))
  (length size-t))

(defhidfun read-timeout :int
  (device device*)
  (data (:pointer :unsigned-char))
  (length size-t)
  (milliseconds :int))

(defhidfun set-nonblocking :int
  (device device*)
  (nonblock :int))

(defhidfun send-feature-report :int
  (device device*)
  (data (:pointer :unsigned-char))
  (length size-t))

(defhidfun get-feature-report :int
  (device device*)
  (data (:pointer :unsigned-char))
  (length size-t))

(defhidfun close :void
  (device device*))

(defhidfun get-manufacturer-string :int
  (device device*)
  (string wchar*)
  (maxlen size-t))

(defhidfun get-product-string :int
  (device device*)
  (string wchar*)
  (maxlen size-t))

(defhidfun get-serial-number-string :int
  (device device*)
  (string wchar*)
  (maxlen size-t))

(defhidfun get-indexed-string :int
  (device device*)
  (index :int)
  (string wchar*)
  (maxlen size-t))

(defhidfun (last-error "hid_error") wchar*
  (device device*))
