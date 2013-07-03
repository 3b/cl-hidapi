(in-package #:%hidapi)

;; defaulting to hidapi-libusb for now, falling back to hidapi-hidraw
;; if that doesn't load... probably should add some sort of
;; configuration, or allow loading them separately

(define-foreign-library hidapi
  (:unix (:or "libhidapi-libusb.so"
              "libhidapi-hidraw.so")))

;; try loading libs from default dirs, otherwise try to load it from where
;; it is on my system (need a better way to handle this too)
(let ((local-dir (merge-pathnames "src/hidapi/" (user-homedir-pathname))))
  (let ((cffi::*foreign-library-directories*
          cffi::*foreign-library-directories*)
        (retry nil))
    (handler-bind
        ((cffi:load-foreign-library-error
           (lambda (e)
             (when (and (not retry) (probe-file local-dir))
               (push (merge-pathnames "linux/.libs/" local-dir)
                     cffi::*foreign-library-directories*)
               (push (merge-pathnames "libusb/.libs/" local-dir)
                     cffi::*foreign-library-directories*)
               (format t "restarts ~s~%" (compute-restarts e))
               (invoke-restart 'cffi::retry)))))
      (use-foreign-library hidapi))))

