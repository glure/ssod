(defparameter *game-root-dir* "~/projects/coding/ssod/")
(defparameter bullets (make-array 128 :adjustable t :fill-pointer 0))

(defstruct entity
  name
  position
  velocity
  acceleration)

(defstruct bullet
  position
  velocity
  acceleration
  color
  dead)

(defparameter player (make-entity :name 'player
				  :position '(320 240)
				  :velocity '(0 0)
				  :acceleration '(0 0)))



(defmacro gfx-object (var-name graphics-file alpha)
  `(defparameter ,var-name (sdl-image:load-image ,(merge-pathnames
                                                  (string-downcase graphics-file)
                                                  (merge-pathnames "gfx/"
                                                                   *game-root-dir*))
                                                  :alpha ,alpha)))

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


(defun shoot ()
  (vector-push (make-bullet :position (copy-tree (entity-position player))
			    :velocity '(0 200)
			    :acceleration '(0 -15000)
			    :color '(sdl:color :r 255 :g 0 :b 0))
	       bullets))

(defmacro for-all-bullets (&rest body)
  `(loop for i from 0 to (1- (length bullets))
      do (let ((bullet (elt bullets i)))
           (progn ,@body))))

(defun mark-dead-bullets ()
  "Mark each bullet dead if its position is outside screen area."
  (for-all-bullets (symbol-macrolet ((x (elt (bullet-position bullet) 0))
				     (y (elt (bullet-position bullet) 1)))
		     (if (OR (< x 0)
			     (> x 640)
			     (< y 0)
			     (> y 480))
			 (setf (bullet-dead bullet) 1)))))

(defun remove-dead-bullets ()
  (let ((temp-bullets bullets))
    (loop for i from (1- (length bullets)) downto 0
       do (if (eq (bullet-dead (elt bullets i)) 1)
	      (setf temp-bullets (delete-if (constantly t) temp-bullets
					    :start i
					    :count 1))))
    temp-bullets))




(defun main ()
  (load (merge-pathnames "physics.lisp" *game-root-dir*))
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "sovereign state of dance")
    (load (merge-pathnames "gfx.lisp" *game-root-dir*))
    (setf (sdl:frame-rate) 60)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key)
       (case key
         (:sdl-key-escape (sdl:push-quit-event))
	 (:sdl-key-up (progn (setf (elt (entity-velocity player) 1)
			    -100) (format t "~D~%" (elt (entity-velocity player) 0))))
	 (:sdl-key-down (setf (elt (entity-velocity player) 1)
			      100))
	 (:sdl-key-left (setf (elt (entity-velocity player) 0)
			      -100))
	 (:sdl-key-right (setf (elt (entity-velocity player) 0)
			       100))
	 (:sdl-key-space (shoot))))
      (:idle ()
      (sdl:clear-display sdl:*black*)
      
      (physics player)
      (draw-player)
      
      (for-all-bullets (physics bullet))
      (mark-dead-bullets)
      (remove-dead-bullets)
      (draw-bullets)
      (sdl:update-display)))))
