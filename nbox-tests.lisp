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

(defclass tile-window (window)
  ())

(defclass float-window (window)
  ())

;; Variables
(defparameter *desktop* (make-instance 'desktop))

(defparameter *empty-screen* (make-instance 'screen))
(defparameter *screen* (make-instance 'screen))

(defparameter *empty-group* (make-instance 'group))
(defparameter *group* (make-instance 'group))

(defparameter *empty-frame* (make-instance 'frame))
(defparameter *frame* (make-instance 'frame))

(defparameter *window1* (make-instance 'float-window))
(defparameter *window2* (make-instance 'float-window))
(defparameter *window3* (make-instance 'tile-window))
(defparameter *window4* (make-instance 'tile-window))

;; Will be change to *desktop* in tests
(defparameter *nested-box-root* (make-instance 'desktop))

;; Register *nested-box-root* as ROOT
(nbox:nbox-root '*nested-box-root*)

(def-suite :nested-box)

(defun suite-tests ()
  (run! :nested-box))

(in-suite :nested-box)

(test nbox-root
  ;; Assign *desktop* as ROOT
  (setf (nbox:item-wrapper nil) *desktop*)
  (is (eq *desktop* (nbox:item-wrapper nil)))
  (is (eq *desktop* (nbox:current-item nil)))
  (is (eq *nested-box-root* (nbox:item-wrapper nil)))
  (is (eq *nested-box-root* (nbox:current-item nil)))
  (is-false (eq *nested-box-root* nbox::*nbox-root*)))

(test populate-with-nil
  ;; Add item x into ROOT
  (nbox:add-item *empty-screen* nil)
  (nbox:add-item *screen* nil)
  (is (= 2 (length (nbox:list-items nil)))))

(test populate-with-keyword
  ;; Add item x into current y
  (nbox:add-item *empty-group* :screen)
  (nbox:add-item *group* :screen)
  (nbox:add-item *empty-frame* :group)
  (nbox:add-item *frame* :group)
  (nbox:add-item *window1* :frame)
  (nbox:add-item *window2* :frame)
  (nbox:add-item *window3* :frame)
  (nbox:add-item *window4* :frame)
  (is (= 2 (length (nbox:list-items :desktop))))
  (is (= 2 (length (nbox:list-items :screen))))
  (is (= 2 (length (nbox:list-items :group))))
  (is (= 4 (length (nbox:list-items :frame)))))

(test nil-behavior
  (is (eql (nbox:list-items nil) (nbox:list-items *desktop*)))
  (is (eql (nbox:item-type nil) (nbox:item-type *desktop*)))
  (is (eql (nbox:item-wrapper nil) *desktop*))
  (is (eql (nbox:current-item nil) *desktop*)))

(test keyword-behavior
  (is (eql (nbox:list-items :desktop) (nbox:list-items nil)))
  (is (eql (nbox:list-items :screen) (nbox:list-items *screen*)))
  (is (eql (nbox:list-items :group) (nbox:list-items *group*)))
  (is (eql (nbox:list-items :frame) (nbox:list-items *frame*)))
  (is (eql (nbox:current-item :screen) *screen*))
  (nbox:move-item :screen -1) ; move current screen to last
  (is-false (eql (nbox:current-item :screen) *screen*))
  (nbox:add-item (make-instance 'group) :screen)
  (nbox:add-item (make-instance 'group) :screen)
  (nbox:add-item (make-instance 'group) :screen)
  (nbox:add-item (make-instance 'group) :screen)
  (nbox:add-item (make-instance 'group) :screen)
  (is (eql 5 (length (nbox:list-items :screen))))
  (nbox:remove-item :group :screen)
  (is (eql 4 (length (nbox:list-items :screen))))
  (nbox:clear-items :screen)
  (is (eql 0 (length (nbox:list-items :screen))))
  (nbox:move-item *screen* 0) ; move *screen* to first
  ;; Try access windows within old screen
  (is (eql 4 (length (nbox:list-items :frame)))))
