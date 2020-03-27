;;;; nbox-tests.lisp

(defpackage #:nbox/tests
  (:use #:cl #:fiveam)
  (:export #:suite-tests))

(in-package :nbox/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Nested Box Client
;;;

(defclass desktop (nbox:nbox-root)
  ()
  (:default-initargs :type 'screen))

(defclass screen (nbox:nbox-item-wrapper)
  ()
  (:default-initargs :type 'group))

(defclass group (nbox:nbox-item-wrapper)
  ()
  (:default-initargs :type 'frame))

(defclass frame (nbox:nbox-item-wrapper)
  ()
  (:default-initargs :type 'window))

(defclass window (nbox:nbox-item)
  ())

;; Variables

(defparameter *desktop* (make-instance 'desktop))

(defparameter *empty-screen* (make-instance 'screen))
(defparameter *screen* (make-instance 'screen))

(defparameter *empty-group* (make-instance 'group))
(defparameter *group* (make-instance 'group))

(defparameter *empty-frame* (make-instance 'frame))
(defparameter *frame* (make-instance 'frame))

(defparameter *window1* (make-instance 'window))
(defparameter *window2* (make-instance 'window))
(defparameter *window3* (make-instance 'window))
(defparameter *window4* (make-instance 'window))

(defparameter *nested-box-root* *desktop*)

;; Register as ROOT
(nbox:nbox-root '*nested-box-root*)

(def-suite :nested-box)

(defun suite-tests ()
  (run! :nested-box))

(in-suite :nested-box)

(test nested-box-root
  (is-false (eq *nested-box-root* nbox::*nbox-root*))
  (is (eq *nested-box-root* (nbox:item-wrapper nil))))

(test populate-nested-box
  (nbox:add-item *empty-screen* nil)
  (nbox:add-item *screen* nil)
  (is (= 2 (length (nbox:list-items nil))))
  (is (eq *desktop* (nbox:item-wrapper *screen*)))
  (is (eq *screen* (nbox:current-item nil)))
  (let ((items (nbox:list-items nil)))
    (is (eq *screen* (first items)))
    (is (eq *empty-screen* (second items))))
  (nbox:move-item *screen* 1)
  (is-false (eq *screen* (nbox:current-item nil)))
  (nbox:move-item *screen* 0))
