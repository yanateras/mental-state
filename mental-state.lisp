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

(defmacro anxiety (form)
  "What if?"
  (flet ((matcher (form)
          (find form '(if when unless cond)))
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
