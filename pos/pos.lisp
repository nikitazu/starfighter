;;; Pos
;;; ===
(defclass pos () 
  ((x :initform 0 :initarg :x :accessor pos-x)
   (y :initform 0 :initarg :y :accessor pos-y)))

(defun pos (x y)
  (make-instance 'pos :x x :y y))

(defmethod pos-up ((p pos) &optional (v 1))
  (decf (slot-value p 'y) v))

(defmethod pos-down ((p pos) &optional (v 1))
  (incf (slot-value p 'y) v))

(defmethod pos-left ((p pos) &optional (v 1))
  (decf (slot-value p 'x) v))

(defmethod pos-right ((p pos) &optional (v 1))
  (incf (slot-value p 'x) v))

