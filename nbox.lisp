;;;; nbox.lisp

(in-package :nbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Nested Box Protocol
;;;

(defclass nbox ()
  ()
  (:documentation "Superclass for all nested boxes."))

(defclass nbox-wrapper (nbox)
  ((%inner
    :initarg :inner
    :initarg :items
    :initform ()
    :accessor inner-box
    :accessor list-items)
   (%type
    :initarg :type
    :initarg :item-type
    :initform 'nbox
    :accessor inner-type
    :accessor item-type))
  (:documentation "Superclass for all wrappers."))

(defclass nbox-item (nbox)
  ((%outer
    :initarg :outer
    :initarg :wrapper
    :initform nil
    :accessor outer-box
    :accessor item-wrapper))
  (:documentation "Superclass for all items."))

(defclass nbox-item-wrapper (nbox-item nbox-wrapper)
  ()
  (:documentation "Superclass for item with wrapper functionality."))

(defclass nbox-root (nbox-wrapper)
  ()
  (:documentation "Superclass for outer wrapper as root."))

;;; Generic Functions
;;;
;;; list-items
;;; item-type
;;; item-wrapper
(defgeneric current-item (self))
(defgeneric add-item (self place))
(defgeneric remove-item (self place))
(defgeneric move-item (self place))
(defgeneric clear-items (place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Nested Box Implementation
;;;
;;; NIL for nested box ROOT
;;; KEYWORD for current/first items depends on keyword-type

;;;
;;; list-items
;;;

(defmethod list-items ((self symbol))
  (declare (keyword self))
  (list-items (current-item self)))

(defmethod list-items ((self null))
  (list-items (item-wrapper self)))

(defmethod (setf list-items) (new-value (self null))
  (setf (list-items (item-wrapper self)) new-value))

;;;
;;; item-type
;;;

(defmethod item-type ((self symbol))
  (declare (keyword self))
  (item-type (current-item self)))

(defmethod item-type ((self null))
  (item-type (item-wrapper self)))

(defmethod (setf item-type) (new-value (self null))
  (setf (item-type (item-wrapper self)) new-value))

(defmethod (setf item-type) :before (new-value (self nbox-wrapper))
  (declare (ignore new-value))
  (let ((items (list-items self)))
    (unless (null items)
      (error "Can not change un-empty items type.~2&~A" items))))

;;;
;;; item-wrapper
;;;

(defvar *root-name* "*NBOX-ROOT*")
(defvar *root-package* "NBOX")
(defvar *nbox-root* (make-instance 'nbox-root :inner nil :type 'nbox))
(declaim (nbox-root *nbox-root*) (string *root-name* *root-package*))

(defun nbox-root (symbol)
  "Function to change default root to new SYMBOL. Root value should be
instance of class NBOX-ROOT or it's subclasses."
  (unless (null symbol)
    (setf *root-name* (symbol-name symbol)
          *root-package* (package-name (symbol-package symbol)))))

(defun nbox-root-p (&optional (self *nbox-root*))
  (and (not (null self))
       (not (slot-exists-p self '%outer))
       (typep self 'nbox-root)))

(defmethod item-wrapper ((self symbol))
  (declare (keyword self))
  (item-wrapper (current-item self)))

(defmethod item-wrapper ((self null))
  (let ((symbol (find-symbol *root-name* *root-package*)))
    (if (nbox-root-p (symbol-value symbol))
        (symbol-value symbol)
        (error "Type error for symbol ~A value." symbol))))

(defmethod (setf item-wrapper) (new-value (self null))
  (let ((symbol (find-symbol *root-name* *root-package*)))
    (unless (null (symbol-value symbol))
      (setf (symbol-value symbol) new-value))))

;;;
;;; current-item
;;;

(defun keyword-type (self)
  (declare (keyword self))
  (let* ((symbol (find-symbol (string self)))
         (class (find-class symbol nil)))
    (if (null class)
        (error "There is no class named ~A" self)
        (class-name class))))

(defmethod current-item ((self symbol))
  (declare (keyword self))
  (loop with type = (keyword-type self)
        and item = (current-item nil)
        until (or (eql type (type-of item))
                  (null item))
        do (setf item (current-item item))
        finally (return item)))

(defmethod current-item ((self null))
  (item-wrapper self))

(defmethod current-item ((self nbox-wrapper))
  (first (list-items self)))

;;;
;;; add-item
;;;

(defmethod add-item (self (place symbol))
  (declare (keyword place))
  (add-item self (current-item place)))

(defmethod add-item (self (place null))
  (add-item self (item-wrapper place)))

(defmethod add-item ((self nbox-item) (place nbox-wrapper))
  (if (typep self (item-type place))
      (progn
        (setf (item-wrapper self) place)
        (pushnew self (list-items place)))
      (error "Wrapper ~A did not accept item ~A"
             (type-of place)
             (type-of self))))

;;;
;;; remove-item
;;;

(defmethod remove-item ((self symbol) place)
  (declare (keyword self))
  (remove-item (current-item self) place))

(defmethod remove-item (self (place symbol))
  (declare (keyword place))
  (remove-item self (current-item place)))

(defmethod remove-item (self (place null))
  (remove-item self (item-wrapper place)))

(defmethod remove-item ((self nbox-item) (place nbox-wrapper))
  (let ((new-value (remove self (list-items place))))
    (setf (list-items place) new-value)))

;;;
;;; move-item
;;;

(defmethod move-item ((self symbol) place)
  (declare (keyword self))
  (move-item (current-item self) place))

(defmethod move-item (self (place symbol))
  (declare (keyword place))
  (move-item self (current-item place)))

(defmethod move-item ((self nbox-item) (place (eql 0)))
  (move-item self (item-wrapper self)))

(defmethod move-item ((self nbox-item) (place integer))
  (when (> 0 place) (setf place most-positive-fixnum))
  (let* ((wrapper (item-wrapper self))
         (tmp-items (remove-item self wrapper))
         (limit (min place (length tmp-items)))
         (before (subseq tmp-items 0 limit))
         (after (subseq tmp-items limit)))
    (setf (list-items wrapper)
          (append before (list self) after))))

(defmethod move-item ((self nbox-item) (place nbox-wrapper))
  (remove-item self (item-wrapper self))
  (add-item self place))

;;;
;;; clear-items
;;;

(defmethod clear-items ((self symbol))
  (declare (keyword self))
  (clear-items (current-item self)))

(defmethod clear-items ((self null))
  (clear-items (item-wrapper self)))

(defmethod clear-items ((self nbox-wrapper))
  (setf (list-items self) nil))
