;;;; package.lisp

(defpackage #:nbox
  (:nicknames #:nested-box)
  (:use #:cl)
  (:export #:nbox
           #:nbox-wrapper
           #:nbox-item
           #:nbox-item-wrapper
           #:nbox-root)
  (:export #:list-items
           #:item-type
           #:item-wrapper
           #:current-item
           #:add-item
           #:remove-item
           #:move-item
           #:clear-items))
