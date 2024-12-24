(defpackage #:moe.yana.mental-state
  (:use #:cl)
  (:export #:anxiety
           #:depression
           #:dissociation))

(in-package #:moe.yana.mental-state)

(defun walk-replace (form matcher replacer)
  (if (funcall matcher form)
      (funcall replacer form)
      (if (listp form)
          (mapcar (lambda (subform)
                    (walk-replace subform matcher replacer))
                  form)
          form)))

(defparameter *conditionals*
  '(if when unless cond
    case ccase ecase
    typecase ctypecase etypecase))

(defmacro anxiety (form)
  "What if?"
  (flet ((matcher (form)
          (find form *conditionals*))
         (replacer (form)
          (declare (ignore form))
          'progn))
    (walk-replace form #'matcher #'replacer)))

(defmacro depression (form)
  "Never quite"
  `(labels ((trap (value)
             (lambda () (trap value))))
     (trap ,form)))

(defmacro dissociation (form)
  ""
  `(progn ,form ()))
