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

;;; list-items

(defmethod list-items ((self null))
  (list-items (item-wrapper self)))

(defmethod (setf list-items) (new-value (self null))
  (setf (list-items (item-wrapper self)) new-value))

;;; item-type

(defmethod item-type ((self null))
  (item-type (item-wrapper self)))

(defmethod (setf item-type) (new-value (self null))
  (setf (item-type (item-wrapper self)) new-value))

(defmethod (setf item-type) :before (new-value (self nbox-wrapper))
  (declare (ignore new-value))
  (let ((items (list-items self)))
    (unless (null items)
      (error "Can not change un-empty items type.~2&~A" items))))

;;; item-wrapper

(defvar *root-name* "*NBOX-ROOT*")
(defvar *root-package* "NBOX")
(defvar *nbox-root* (make-instance 'nbox-root :inner nil :type 'nbox))
(declaim (nbox-root *nbox-root*) (string *root-name* *root-package*))

(defun nbox-root (symbol)
  (setf *root-name* (symbol-name symbol)
        *root-package* (package-name (symbol-package symbol))))

(defun nbox-root-p (&optional (self *nbox-root*))
  (and (not (null self))
       (not (slot-exists-p self '%outer))
       (typep self 'nbox-root)))

(defmethod item-wrapper ((self null))
  (let ((symbol (find-symbol *root-name* *root-package*)))
    (if (nbox-root-p (symbol-value symbol))
        (symbol-value symbol)
        (error "Type error for symbol ~A value." symbol))))

(defmethod (setf item-wrapper) (new-value (self null))
  (let ((symbol (find-symbol *root-name* *root-package*)))
    (unless (null symbol)
      (setf symbol new-value))))

;;; current-item

(defmethod current-item ((self null))
  (current-item (item-wrapper self)))

(defmethod current-item ((self nbox-wrapper))
  (first (list-items self)))

;;; add-item

(defmethod add-item (self (place null))
  (add-item self (item-wrapper place)))

(defmethod add-item ((self nbox-item) (place nbox-wrapper))
  (let ((type1 (item-type place))
        (type2 (type-of self)))
    (if (eql type1 type2)
        (progn
          (setf (item-wrapper self) place)
          (pushnew self (list-items place)))
        (error "Wrapper ~A did not accept item ~A" type1 type2))))

;;; remove-item

(defmethod remove-item (self (place null))
  (remove-item self (item-wrapper place)))

(defmethod remove-item ((self nbox-item) (place nbox-wrapper))
  (let ((new-value (remove self (list-items place))))
    (setf (list-items place) new-value)))

;;; move-item

(defmethod move-item ((self nbox-item) (place (eql 0)))
  (move-item self (item-wrapper self)))

(defmethod move-item ((self nbox-item) (place integer))
  (declare (unsigned-byte place))
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

;;; clear-items

(defmethod clear-items ((self null))
  (clear-items (item-wrapper self)))

(defmethod clear-items ((self nbox-wrapper))
  (setf (list-items self) nil))
