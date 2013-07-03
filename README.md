Common Lisp ([CFFI][]) bindings for [HIDAPI][].

Tested with at least 1 device on x86-64/Linux SBCL, but should work on other platforms supported by [HIDAPI][] and [CFFI][].

Not all of the API is wrapped yet, since I don't have devices to test them with, patches welcome.


### usage example:

```lisp
;; enumerate all HID devices
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

;; open and use a specific device

(hidapi:with-hid-device (dev #x2833 #x0001)
  (when dev ;; successfully opened device
    ;; read a feature report from device
    (format t "feature report #x02: ~{~2,'0x~^ ~}~%"
            (coerce (hidapi:get-feature-report dev 2 6) 'list))
    ;; send a feature report
    (hidapi:send-feature-report dev '(8 #x00 #x00 #xe8 #x03))
    ;; read some data
    (format t "read data: ~{~2,'0x~^ ~}~%"
            (coerce (hidapi:read-timeout dev 96 500) 'list))
    ;; and let WITH-HID-DEVICE close the device
    ))
```

### API
* *function* `enumerate &optional (vendor-id #x0000) (product-id #x0000)`

     Return a list of plists describing all detected HID devices
matching `VENDOR-ID` and `PRODUCT-ID` (0 in either matches anything).  
 Supported fields (should be present, but may not have useful info depending on devices):  
    `:PATH` `:PRODUCT-ID` `:VENDOR-ID` `:SERIAL-NUMBER`
    `:PRODUCT` `:MANUFACTURER`
    `:USAGE` `:USAGE-PAGE` `:INTERFACE-NUMBER`

* *macro* `with-hid-device ((dev vendor-id product-id &key serial-number path)  
 &body body)`  

     Evaluate `BODY` with `DEV` bound to a hidapi device handle for HID
device matching `PATH` if specified, or first device matching `VENDOR-ID`,
`PRODUCT-ID` and `SERIAL-NUMBER`, or NIL if no devices match or device
couldn't be opened.  

    `VENDOR-ID` = 0, `PRODUCT-ID` = 0, and `SERIAL-NUMBER` = NIL match any device.  

    `VENDOR-ID`, `PRODUCT-ID`, and `SERIAL-NUMBER` are currently ignored when `PATH` is set, but that may change, so they should be 0,0,NIL or actual values
when using `PATH`.  

     Interpretation of `PATH` is unspecified, use `ENUMERATE` to get valid
values if needed (for example to open multiple identical devices that
don't have serial numbers).


* *function* `get-feature-report (device report-id size)`  

    Get feature report `REPORT-ID` (0-255) from `DEVICE`, with `SIZE` octets allocated for returned data. Returns `(unsigned-byte 8)` vector containing response, including `REPORT-ID` in first octet.

* *function* `send-feature-report (device buffer)`  

    Send feature report in `BUFFER` to `DEVICE`. Buffer should be a sequence of `(unsigned-byte 8)`, formatted as required by `DEVICE`. First octet of `BUFFER` should contain report id if needed, or 0.  
    Returns number of octets written.

* *function*  `read-timeout (device octet-count timeout-ms)`  

    Try to read `OCTET-COUNT` octets from `DEVICE`, waiting a maximum of `TIMEOUT-MS` milliseconds. Returns `(unsigned-byte 8)` vector containing any data read, or NIL if no data available before timeout.


* *function* `get-manufacturer-string (dev)`  

    Return manufacturer name from an open hidapi handle `DEV`


* *function* `get-product-string (dev)`  

    Return product name from an open hidapi handle `DEV`

* *function* `get-serial-number-string (dev)`  

    Return serial number from an open hidapi handle `DEV`

* *function* `get-indexed-string (dev n)`  

    Return string with index `N` from an open hidapi handle `DEV`

* *function* `last-error (dev)`  

    Return last hidapi error for open handle `DEV`



[CFFI]: http://common-lisp.net/project/cffi/
[hidapi]: http://www.signal11.us/oss/hidapi/