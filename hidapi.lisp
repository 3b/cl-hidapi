(in-package #:hidapi)

(defun enumerate (&optional (vendor-id #x0000) (product-id #x0000))
  "Return a list of plists describing all detected HID devices
matching VENDOR-ID and PRODUCT-ID (0 in either matches anything).

Supported fields (should be present, but may not have useful info
depending on devices):
   :PATH :PRODUCT-ID :VENDOR-ID :SERIAL-NUMBER
   :PRODUCT :MANUFACTURER
   :USAGE :USAGE-PAGE :INTERFACE-NUMBER"
  (let ((devices nil)
        (ret nil))
    (unwind-protect
         (progn
           (setf devices (%hidapi:enumerate vendor-id product-id))
           (setf ret
                 (loop for p = devices then (getf d :next)
                       for d = (unless (cffi:null-pointer-p p)
                                 (cffi:mem-ref p '(:struct %hidapi:device-info)))
                       while d
                       collect (alexandria:remove-from-plist d :next))))
      (when devices
        (%hidapi:free-enumeration devices)))
    ret))


(defun call-with-hid-device (thunk vendor-id product-id serial-number path)
  ;; todo: make sure vendor/product IDs match when opening by path?
  (let ((dev nil))
    (unwind-protect
         (progn
           (setf dev (if path
                         (%hidapi:open-path path)
                         (%hidapi:open vendor-id product-id
                                       (or serial-number
                                           (cffi:null-pointer)))))
           (funcall thunk (if (null-pointer-p dev) nil dev)))
      (when dev
        (%hidapi:close dev)))))

(defmacro with-hid-device ((dev vendor-id product-id &key serial-number path)
                           &body body)
  "Evaluate BODY with DEV bound to a hidapi device handle for HID
device matching PATH if specified, or first device matching VENDOR-ID,
PRODUCT-ID and SERIAL-NUMBER, or NIL if no devices match or device
couldn't be opened.

VENDOR-ID and PRODUCT-ID = 0, and SERIAL-NUMBER = NIL match any device.

VENDOR-ID, PRODUCT-ID, and SERIAL-NUMBER are currently ignored when PATH
is set, but that may change, so they should be 0,0,NIL or actual values
when using PATH.

Interpretation of PATH is unspecified, use ENUMERATE to get valid
values if needed (for example to open multiple identical devices that
don't have serial numbers)."
  ;; not sure how serial-number = "" should be handled?
  `(call-with-hid-device (lambda (,dev) ,@body)
                         ,vendor-id ,product-id ,serial-number ,path))

(defmacro %hid-string (fun dev &optional i)
  ;; doesn't seem to be any way to query length of strings, and at
  ;; least some are apparently limited to 126 wide chars on USB, so
  ;; just picking something larger than that for now...
  `(cffi:with-foreign-pointer-as-string (s 1024 :encoding %hidapi:+wchar-encoding+)
     (setf (mem-aref s :unsigned-int) 0)
     (when (minusp (,fun ,dev ,@(when i (list i)) s 256))
       ;; fixme: proper errors...
       (error (format nil "hidapi ~a failed" ',fun)))))

(defun get-manufacturer-string (dev)
  "Return manufacturer name from an open hidapi handle DEV"
  (%hid-string %hidapi:get-manufacturer-string dev))

(defun get-product-string (dev)
  "Return product name from an open hidapi handle DEV"
  (%hid-string %hidapi:get-product-string dev))

(defun get-serial-number-string (dev)
  "Return serial number from an open hidapi handle DEV"
  (%hid-string %hidapi:get-serial-number-string dev))

(defun get-indexed-string (dev n)
  "Return string with index N from an open hidapi handle DEV"
  (%hid-string %hidapi:get-indexed-string dev n))

(defun last-error (dev)
  "Return last hidapi error for open handle DEV"
  (%hidapi:last-error dev))


(defun get-feature-report (device report-id size)
  "Get feature report REPORT-ID (0-255) from DEVICE, with SIZE octets
allocated for returned data. Returns (unsigned-byte 8) vector
containing response, including REPORT-ID in first octet."
  ;; fixme: should SIZE include space for report-id or not?
  ;; todo: fast path with less copying?
  ;; (maybe with-get-feature-report macro with accessors to read directly
  ;;  from foreign pointer, or allow passing a static-vector?)
  (with-foreign-pointer (v (1+ size))
    (setf (mem-aref v :unsigned-char 0) report-id)
    (let ((l (%hidapi:get-feature-report device
                                         v
                                         (1+ size))))
      (when (minusp l)
        (error (format nil "hidapi get-feature-report failed")))
      (loop with a = (make-array l :element-type '(unsigned-byte 8))
            for i below l
            do (setf (aref a i) (mem-aref v :unsigned-char i))
            finally (return a)))))


(defun send-feature-report (device buffer)
  "Send feature report in BUFFER to DEVICE. Buffer should be a sequence
of (unsigned-byte 8), formatted as required by DEVICE. First octet of
BUFFER should contain report id if needed, or 0.
Returns number of octets written?"
  (with-foreign-pointer (v (length buffer) size)
    (let ((i 0))
      (map nil (lambda (x)
                 (setf (mem-aref v :unsigned-char (shiftf i (1+ i))) x))
           buffer))
    (let ((l (%hidapi:send-feature-report device
                                          v
                                          size)))
      (when (minusp l)
        (error (format nil "hidapi send-feature-report failed")))
      l)))


(defun read-timeout (device octet-count timeout-ms)
  "Try to read OCTET-COUNT octets from DEVICE, waiting a maximum of
TIMEOUT-MS milliseconds. Returns (unsigned-byte 8) vector containing
any data read, or NIL if no data available before timeout."
  (with-foreign-pointer (v octet-count)
    (let ((c (%hidapi:read-timeout device v octet-count timeout-ms)))
      (cond
        ((minusp c)
         (error (format nil "hidapi read-timeout failed")))
        ((zerop c)
         nil)
        (t
         (loop with a = (make-array c :element-type '(unsigned-byte 8))
               for i below c
               do (setf (aref a i) (mem-aref v :unsigned-char i))
               finally (return a)))))))

;; todo: read-hid, write-hid, open, close, set-nonblocking
;; init, exit?