(defparameter *game-root-dir* "~/projects/coding/ssod/")
(defparameter bullets (make-array 128 :adjustable t :fill-pointer 0))
(defparameter enemies (make-array 128 :adjustable t :fill-pointer 0))

(defstruct entity
  name
  position
  velocity
  acceleration
  hitbox
  dead)

(defstruct bullet
  position
  velocity
  acceleration
  color
  hitbox
  dead)

(defparameter player (make-entity :name 'player
				  :position '(320 240)
				  :velocity '(0 0)
				  :acceleration '(0 0)))

(defun spawn-enemy (&key position velocity angle)
  (vector-push (make-entity :name 'enemy01
			    :position position
			    :velocity `(,(* (cos angle)
					    velocity)
					,(* (sin angle)
					    velocity))
			    :acceleration (list 0 0)
			    :hitbox (list 32 32 32 32))
	       enemies))

(defmacro gfx-object (var-name graphics-file alpha)
  `(defparameter ,var-name (sdl-image:load-image ,(merge-pathnames
                                                  (string-downcase graphics-file)
                                                  (merge-pathnames "gfx/"
                                                                   *game-root-dir*))
                                                  :alpha ,alpha)))

(defmacro for-all (vector &rest body)
  `(loop for i from 0 to (1- (length vector))
      do (let ((current (elt vector i)))
           (progn ,@body))))

(defun key-down (key)
  (if (sdl:key-down-p key)
      T
      NIL))

(defun draw-player ()
  (sdl:draw-surface-at-* *player*
			 (round (- (elt (entity-position player) 0) 24))
			 (round (- (elt (entity-position player) 1) 24))))

(defun draw-bullets ()
  (for-all-bullets (symbol-macrolet ((x (elt (bullet-position bullet) 0))
				     (y (elt (bullet-position bullet) 1)))
		     (sdl:draw-box-* (round x)
				     (round y)
				     12 12
				     :color (eval (bullet-color bullet))))))

(defun draw-enemies ()
  (for-all-enemies (sdl:draw-surface-at-* *enemy*
					  (round (elt (entity-position enemy) 0))
					  (round (elt (entity-position enemy) 1)))))

(defun shoot ()
  "Spawn a bullet from player position."
  (vector-push (make-bullet :position (mapcar #'+ (copy-tree (entity-position player))
					      '(10 0))
			    :velocity (list 0 -1200)
			    :acceleration (list 0 0)
			    :color '(sdl:color :r 255 :g 0 :b 0))
	       bullets)
  (vector-push (make-bullet :position (mapcar #'- (copy-tree (entity-position player))
					      '(24 0))
			    :velocity (list 0 -1200)
			    :acceleration (list 0 0)
			    :color '(sdl:color :r 255 :g 0 :b 0))
	       bullets))

(defun spawn-bullet (&key position velocity angle)
  (vector-push (make-bullet :position position
			    :velocity `(,(* (cos angle)
					    velocity)
					,(* (sin angle)
					    velocity))
			    :acceleration (list 0 0)
			    :color '(sdl:color :r 255 :g 0 :b 255))
	       bullets))


(defun mark-dead-bullets ()
  "Mark each bullet dead if its position is outside screen area."
  (for-all-bullets (symbol-macrolet ((x (elt (bullet-position bullet) 0))
				     (y (elt (bullet-position bullet) 1)))
		     (if (OR (< x 0)
			     (> x 640)
			     (< y 0)
			     (> y 480))
			 (setf (bullet-dead bullet) 1)))))

(defun mark-dead-enemies ()
  "Mark each enemy dead if its position is far outside screen area."
  (for-all-enemies (symbol-macrolet ((x (elt (entity-position enemy) 0))
				     (y (elt (entity-position enemy) 1)))
		     (if (OR (< x -70)
			     (> x 710)
			     (< y -70)
			     (> y 540))
			 (setf (entity-dead enemy) 1)))))

(defun remove-dead-bullets ()
  "Remove dead bullets from bullets vector."
  (let ((temp-bullets bullets))
    (loop for i from (1- (length bullets)) downto 0
       do (if (eq (bullet-dead (elt bullets i)) 1)
	      (setf temp-bullets (delete-if (constantly t) temp-bullets
					    :start i
					    :count 1))))
    temp-bullets))

(defun remove-dead-enemies ()
  "Remove dead enemies from enemies vector."
  (let ((temp-enemies enemies))
    (loop for i from (1- (length enemies)) downto 0
       do (if (eq (entity-dead (elt enemies i)) 1)
	      (setf temp-enemies (delete-if (constantly t) temp-enemies
					    :start i
					    :count 1))))
    temp-enemies))

 (defun process-physics (vector)
  "Sets new positions and velocities."
   (for-all vector (multiple-value-bind (x_v y_v x y) (physics current)
		     (setf (elt (slot-value current 'position) 0) x)
		     (setf (elt (slot-value current 'position) 1) y)
		     (setf (elt (slot-value current 'velocity) 0) x_v)
		     (setf (elt (slot-value current 'velocity) 1) y_v))))
    
(defun process-player-physics ()
  (multiple-value-bind (x_v y_v x y) (physics player)
    (setf (elt (entity-position player) 0) (round x))
    (setf (elt (entity-position player) 1) (round y))
    (setf (elt (entity-velocity player) 0) x_v)
    (setf (elt (entity-velocity player) 1) y_v)))

(defun spawn-lots-of-bullets (count)
  (loop for i from 0 to (1- count)
       do (spawn-bullet :position (copy-tree (entity-position (elt enemies 0))) 
			:velocity 200
			:angle (* i (/ (* 2 pi) count)))))

(defmacro timer% (time &rest body)
  `(if (>= level-time time)
       (progn ,@body)))

(defun closure ()
  (let ((count 0))
    (lambda (msg)
      (case msg
	((:inc) (incf count))
	((:dec) (decf count))))))

;(events (timer 0 (spawn-enemy :route route001)
;	        (spawn-enemy :route route002))
;	(timer 30 (spawn-enemy :route route003)
;	         (spawn-enemy :route route004)))

(defun main ()
  (load (merge-pathnames "physics.lisp" *game-root-dir*))
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "sovereign state of dance")
    (load (merge-pathnames "gfx.lisp" *game-root-dir*))
    (setf (sdl:frame-rate) 30)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key)
       (case key
         (:sdl-key-escape (sdl:push-quit-event))
	 (:sdl-key-up (setf (elt (entity-velocity player) 1)
			    -100))
	 (:sdl-key-down (setf (elt (entity-velocity player) 1)
			      100))
	 (:sdl-key-left (setf (elt (entity-velocity player) 0)
			      -100))
	 (:sdl-key-right (setf (elt (entity-velocity player) 0)
			       100))
	 (:sdl-key-space (shoot))
	 (:sdl-key-a (spawn-bullet :position (list 320 240)
				   :velocity 200
				   :angle pi))
	 (:sdl-key-s (spawn-lots-of-bullets 20))))
      (:idle ()
      (sdl:clear-display sdl:*black*)
        (mark-dead-enemies)
	(remove-dead-enemies)
        (if (eq (length enemies) 0)
	    (let ((rn (random 2)))
	      (spawn-enemy :position (list (* rn 640) 80)
			   :velocity 100
			   :angle (* rn pi))
	      (setf counter 0)))
	(if (NOT (eq (length enemies) 0))
	    (incf counter (sdl:dt))
	    (setf counter 0))
	(if (> counter 1)
	    (progn (spawn-lots-of-bullets 25)
		   (setf counter 0)))
	;(process-enemy-physics)
	(process-physics enemies)
	(draw-enemies)
        (process-player-physics)
	(draw-player)
      
	;(process-bullet-physics)
	(process-physics bullets)
	(mark-dead-bullets)
	(remove-dead-bullets)
	(draw-bullets)

      (sdl:update-display)))))
