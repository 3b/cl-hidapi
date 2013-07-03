(defpackage #:hidapi-test
  (:use #:cl))
(in-package #:hidapi-test)

;; partial port of example from http://www.signal11.us/oss/hidapi/
;; (not really tested since it requires specific hardware or libs or something)

(defun test ()
  ;; enumerate HID devices and print some info about them
  (let ((devices (hidapi:enumerate)))
    (loop for device in devices
          do (format t
                     "Device found~%  type: ~4,'0x ~4,'0x~%  path: ~s~%  serial number ~s~%"
                     (getf device :vendor-id) (getf device :product-id)
                     (getf device :path)
                     (getf device :serial-number))
             (format t "  Manufacturer: ~s~%" (getf device :manufacturer))
             (format t "  Product: ~s~%" (getf device :product))
             (terpri)))

  ;; open a device using Vendor ID, Product ID
  (hidapi:with-hid-device (dev #x4d8 #x3f)
    (format t "dev = ~s~%" dev)
    (when dev
      (format t "Manufacturer: ~s~%" (hidapi:get-manufacturer-string dev))
      (format t "Serial Number: ~s~%" (hidapi:get-serial-number-string dev))
      (format t "Product: ~s~%" (hidapi:get-product-string dev))

      ;; send a feature report to the device
      (hidapi:send-feature-report dev '(#x2 ;; first byte is report number
                                         #xa0 #xa0))

      ;; read a feature report from the device
      (let ((buf (hidapi:get-feature-report dev 1 64)))
        (format t "Feature Report~%  ~{~2,'0x~^ ~}~%" (coerce buf 'list))

        ;; todo: set read to non-blocking
        ;; (hidapi:set-nonblocking dev t)

        ;; rest of these reuse BUF and send more than they modify,
        ;; so not sure if they actually care about the extra or not.
        ;; no way to test writes anyway, so not implementing for now...

        ;; todo: send output report to toggle the LED
        ;; todo: send output report to request state
        ;; todo: read state, print it
        ))))

