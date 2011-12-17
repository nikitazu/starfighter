
;;; Actor
;;; =====
(defclass actor (pos)
  ((radius :initform 1 
	   :initarg :radius 
	   :reader actor-radius)))

(defun actor (x y r)
  (make-instance 'actor 
		 :x x :y y :radius r))

(defmethod actors-collide-p ((a1 actor) (a2 actor))
  (let ((x (abs (- (pos-x a1)
		   (pos-x a2))))
	
	(y (abs (- (pos-y a1)
		   (pos-y a2)))))
    
    (let ((radius (+ (actor-radius a1)
		     (actor-radius a2)))
	  
	  (length (sqrt (+ (* x x)
			   (* y y)))))
      
      (< length radius))))

(defmethod actors-collide-easy-p ((a1 actor) (a2 actor))
  (let ((radius (+ (actor-radius a1)
		   (actor-radius a2))))
    (and (>= radius (abs (- (pos-x a1)
			    (pos-x a2))))
	 (>= radius (abs (- (pos-y a1)
			    (pos-y a2)))))))

