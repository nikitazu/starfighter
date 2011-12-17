;;; Starship
;;; ========

(load "pos/starship-state.lisp")


(defclass starship (actor)
  ((boost :initform 5
	  :initarg :boost
	  :accessor starship-boost)

   (boost-p :initform nil
	    :reader starship-boost-p)))

(defmethod starship-speed ((ship starship))
  (if (starship-boost-p ship)
    (starship-boost ship)
    1))

(defmethod starship-move (x y (ship starship))
  (setf (pos-x ship) x)
  (setf (pos-y ship) y))

(defmethod starship-fly (pos-move (ship starship))
  (funcall pos-move
	   ship
	   (starship-speed ship)))

(defmethod starship-up ((ship starship))
  (starship-fly #'pos-up ship))

(defmethod starship-down ((ship starship))
  (starship-fly #'pos-down ship))

(defmethod starship-left ((ship starship))
  (starship-fly #'pos-left ship))

(defmethod starship-right ((ship starship))
  (starship-fly #'pos-right ship))

(defmethod starship-boost-on ((ship starship))
  (setf (slot-value ship 'boost-p) t))

(defmethod starship-boost-off ((ship starship))
  (setf (slot-value ship 'boost-p) nil))

(defmethod starship-apply-state ((ship starship)
                                 (state starship-state)
                                 on-attack)

  (let ((hm (slot-value state 'horizontal-move))
        (vm (slot-value state 'vertical-move))
        (attack (slot-value state 'attack)))

    (let ((hs (case hm
                ('left #'starship-left)
                ('right #'starship-right)
                (otherwise nil)))

          (vs (case vm
                ('up #'starship-up)
                ('down #'starship-down)
                (otherwise nil))))

      (when hs
        (funcall hs ship))

      (when vs
        (funcall vs ship))

      (when attack
        (funcall on-attack)))))


