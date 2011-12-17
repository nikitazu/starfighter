;;; Starship state
;;; ==============
(defclass starship-state ()
  ((attack :initform nil)
   (horizontal-move :initform nil)
   (vertical-move :initform nil)))

(defmethod starship-state-horizontal (direction (s starship-state))
  (setf (slot-value s 'horizontal-move) direction))

(defmethod starship-state-vertical (direction (s starship-state))
  (setf (slot-value s 'vertical-move) direction))

(defmethod starship-state-attack (attack (s starship-state))
  (setf (slot-value s 'attack) attack))


