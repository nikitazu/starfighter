(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "lispbuilder-sdl")

(defparameter *random-color* sdl:*blue*)


;;; Sky
;;; ===
(defparameter *sky-width*  800)
(defparameter *sky-height* 600)

(load "pos/pos.lisp")
(load "pos/actor.lisp")
(load "pos/starship.lisp")


(defun random-star ()
  (pos (random *sky-width*) 0))


;;; Emitter
;;; =======
(defclass emitter ()
  ((stars      :initform nil
	       :reader emitter-stars)

   (limit      :initarg :limit
	       :initform 30
	       :reader emitter-limit)

   (speed      :initarg :speed
	       :initform 2
	       :reader emitter-speed)

   (growth     :initform 0
	       :reader emitter-growth)

   (growth-max :initarg :growth-max
	       :initform 7
	       :reader emitter-growth-max)))

(defclass shaking-emitter (emitter) ())

(defmethod emitter-formula (star (e shaking-emitter))
  (pos-down star (emitter-speed e))
  (if (= 0 (random 2))
    (pos-left star)
    (pos-right star)))

(defmethod emitter-random-make ((e emitter))
  (pos (random *sky-width*) 0))

(defmethod emitter-formula (star (e emitter))
  (pos-down star (emitter-speed e)))

(defmethod emitter-step ((e emitter))
  (emitter-emit e)
  (emitter-move e))

(defmethod emitter-emit ((e emitter))
  (if (emitter-growth-max-reached-p e)
    (progn
      (setf (slot-value e 'growth) 0)
      (emitter-add e))
    (incf (slot-value e 'growth))))

(defmethod emitter-growth-max-reached-p ((e emitter))
  (= (emitter-growth-max e)
     (emitter-growth e)))

(defmethod emitter-add ((e emitter))
  (when (<= (list-length (emitter-stars e))
	    (emitter-limit e))
    (push (emitter-random-make e)
	  (slot-value e 'stars))))

(defmethod emitter-move ((e emitter))
  (dolist (star (emitter-stars e))
    (if (<= *sky-height* (pos-y star))
      (progn
	(setf (pos-y star) 0)
	(setf (pos-x star) (random *sky-width*)))
      (emitter-formula star e))))

(defmethod emitter-remove ((p pos) (e emitter))
  (let ((stars (slot-value e 'stars)))
    (setf (slot-value e 'stars)
	  (remove-if #'(lambda (s)
			 (and (= (pos-x s) (pos-x p))
			      (= (pos-y s) (pos-y p))))
		     stars))))

;;; Meteor emitter
;;; ==============
(defclass meteor-emitter (emitter) ())

(defmethod emitter-random-make ((e meteor-emitter))
  (actor (random *sky-width*) 0 (+ 5 (random 10))))

(defmethod emitter-remove-collisions ((e1 meteor-emitter) (e2 meteor-emitter))
  (let ((trash1 nil)
	(trash2 nil))
    (dolist (s1 (emitter-stars e1))
      (dolist (s2 (emitter-stars e2))
	(when (actors-collide-p s1 s2)
	  (push s1 trash1)
	  (push s2 trash2))))

    (dolist (t1 trash1)
      (emitter-remove t1 e1))

    (dolist (t2 trash2)
      (emitter-remove t2 e2))))

(defclass enemies-emitter (meteor-emitter)
  ())

(defmethod emitter-formula ((p pos) (e enemies-emitter))
  (pos-down p (emitter-speed e))
  (pos-left p (if (zerop (random 2))
			  1
			  -1)))


;;; Bullet emitter
;;; ==============
(defclass bullet-emitter (meteor-emitter)
  ((owner :initarg :owner)))

(defmethod emitter-random-make ((e bullet-emitter))
  (let ((owner (slot-value e 'owner)))
    (actor (pos-x owner)
	   (- (pos-y owner) 10)
	   (+ 4 (random 5)))))

(defmethod emitter-formula ((p pos) (e bullet-emitter))
  (pos-up p (emitter-speed e)))

(defmethod emitter-move ((e bullet-emitter))
  (let ((bullets (emitter-stars e)))
    (let ((bullets2 (remove-if-not #'(lambda (b)
				       (and (> (pos-x b) 0)
					    (< (pos-x b) *sky-width*)
					    (> (pos-y b) 0)
					    (< (pos-y b) *sky-height*)))
				   bullets)))
      (dolist (b bullets2)
	(emitter-formula b e))
      (setf (slot-value e 'stars) bullets2))))


(defparameter *starship*
  (make-instance 'starship :x 100 :y 100 :radius 20))

(defparameter *starship-state*
  (make-instance 'starship-state))

(defparameter *far-stars* (make-instance 'emitter))
(defparameter *mid-stars* (make-instance 'emitter :limit 10 :speed 4))
(defparameter *near-stars* (make-instance 'emitter :limit 5 :speed 8))
(defparameter *meteors* (make-instance 'meteor-emitter :limit 4 :speed 3))
(defparameter *enemies* (make-instance 'enemies-emitter :limit 3 :speed 4))
(defparameter *starship-weapon* (make-instance 'bullet-emitter :limit 20 :speed 15 :owner *starship*))

;;; Game
;;; ====
(defun game-logic ()
  (starship-apply-state *starship*
                        *starship-state*
                        #'(lambda () (emitter-emit *starship-weapon*)))

  (emitter-step *enemies*)
  (emitter-step *meteors*)
  (emitter-step *far-stars*)
  (emitter-step *mid-stars*)
  (emitter-step *near-stars*)
  (emitter-move *starship-weapon*)

  (emitter-remove-collisions *starship-weapon* *meteors*)
  (emitter-remove-collisions *starship-weapon* *enemies*)

  (when (find-collisions *starship* *enemies*)
    (setf *game-over* t))

  (when (find-collisions *starship* *meteors*)
    (setf *game-over* t)))

(defmethod find-collisions ((ship starship) (e meteor-emitter))
  (find-if #'(lambda (m)
	       (actors-collide-p m ship))
	   (emitter-stars e)))


(defparameter *sdl-brown* (sdl:color :r 140 :g 90 :b 90))
(defparameter *sdl-orange* (sdl:color :r 255 :g 200 :b 100))
(defparameter *sdl-yellow* (sdl:color :r 200 :g 100 :b 50))
(defparameter *game-over* nil)

(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window *sky-width* *sky-height* :title-caption "Starship battle")
    (setf (sdl:frame-rate) 60)
    (sdl:enable-key-repeat 10 10)

    (starship-move (/ *sky-width* 2)
		   (- *sky-height* 80)
		   *starship*)

    (sdl:with-events ()
      (:quit-event () t)

      ;; Handle keyboard events
      (:key-down-event (:key key)

	;; Quit on q
	(when (sdl:key= key :sdl-key-q)
	 (sdl:push-quit-event))

	;; Starship fires
	(when (sdl:key= key :sdl-key-space)
          (starship-state-attack 'fire *starship-state*))

	;; Starship boost on
	(when (sdl:key= key :sdl-key-lshift)
	  (starship-boost-on *starship*))

	;; Starship moves

	(when (sdl:key= key :sdl-key-up)
          (starship-state-vertical 'up *starship-state*))

	(when (sdl:key= key :sdl-key-down)
          (starship-state-vertical 'down *starship-state*))

	(when (sdl:key= key :sdl-key-right)
          (starship-state-horizontal 'right *starship-state*))

	(when (sdl:key= key :sdl-key-left)
          (starship-state-horizontal 'left *starship-state*)))

      (:key-up-event (:key key)

        ;; Starship move off
        (when (or (sdl:key= key :sdl-key-up)
                  (sdl:key= key :sdl-key-down))
          (starship-state-vertical nil *starship-state*))

        (when (or (sdl:key= key :sdl-key-right)
                  (sdl:key= key :sdl-key-left))
          (starship-state-horizontal nil *starship-state*))

        ;; Starship stop attack
        (when (sdl:key= key :sdl-key-space)
          (starship-state-attack nil *starship-state*))

	;; Starship boost off
        (when (sdl:key= key :sdl-key-lshift)
	  (starship-boost-off *starship*)))

      (:idle ()

       (game-logic)

       ;; Change the color of the box if the left mouse button is depressed
       (when (sdl:mouse-left-p)
         (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))

       ;; Clear the display each game loop
       (sdl:clear-display sdl:*black*)

       ;; Draw stars
       (emitter-draw sdl:*green* *far-stars*)
       (emitter-draw sdl:*cyan*  *mid-stars*)
       (emitter-draw sdl:*white* *near-stars*)
       (emitter-draw *sdl-brown* *meteors*)
       (emitter-draw *sdl-yellow* *enemies*)
       (emitter-draw sdl:*red* *starship-weapon*)

       (pos-draw *starship*)

       (when *game-over*
	 (sdl:with-color (sdl:*red*)
	   (sdl:draw-filled-circle (sdl:point :x 400 :y 300)
				   200)))

       ;; Redraw the display
       (sdl:update-display)))))

(defmethod pos-draw ((a actor))
  (sdl:draw-circle (sdl:point :x (pos-x a)
			      :y (pos-y a))
		   (actor-radius a)))

(defmethod pos-draw ((star pos))
  (sdl:draw-pixel
   (sdl:point :x (pos-x star)
	      :y (pos-y star))))

(defmethod pos-draw ((s starship))
  (let ((x (pos-x s))
	(y (pos-y s))
	(r (actor-radius s)))

    (sdl:with-color
     (*sdl-orange*)
     (sdl:draw-circle (sdl:point :x x :y (+ y 20))
		      (- (/ r 2) (random 20))))

    (sdl:with-color
     (*random-color*)

     (sdl:draw-trigon (sdl:point :x (- x 15) :y (+ y 10))
		      (sdl:point :x (+ x 15) :y (+ y 10))
		      (sdl:point :x x        :y (- y 5)))

     (sdl:draw-box
      (sdl:rectangle-from-midpoint-*
       x y (/ r 4) r)))))

(defmethod emitter-draw (color (e emitter))
  (sdl:with-color
   (color)
   (dolist (star (emitter-stars e))
     (pos-draw star))))

(mouse-rect-2d)


