(defpackage %hidapi
  (:use :cffi :cl)
  (:shadow #:open #:close #:read #:write )
  (:export
   #:enumerate
   #:device-info
   #:free-enumeration
   #:get-manufacturer-string
   #:get-product-string
   #:get-serial-number-string
   #:get-indexed-string
   #:last-error
   #:close
   #:get-feature-report
   #:send-feature-report
   #:set-nonblocking
   #:read-timeout
   #:read
   #:write
   #:open-path
   #:open
   #:+wchar-encoding+))


(defpackage hidapi
  (:use :cffi :cl)
  ;; not sure if these should be named directly after the lib,
  ;; or adjusted to not conflict with CL names? maybe add a -hid suffix?
  ;(:shadow #:open #:close #:read #:write )
  (:export
   #:enumerate
   #:with-hid-device
   #:get-feature-report
   #:send-feature-report
   #:read-timeout
   #:get-manufacturer-string
   #:get-product-string
   #:get-serial-number-string
   #:get-indexed-string
   #:last-error))
