(defun physics (ent)
  ; DESTRUCTIVE!! (make functional)

  ;(setf (x dot) (+ (x dot) (* (x_v dot) (sdl:dt))))
  ;(setf (y dot) (+ (y dot) (* (y_v dot) (sdl:dt)))))
  (symbol-macrolet ((x (elt (slot-value ent 'position) 0))
		    (y (elt (slot-value ent 'position) 1))
		    (x_v (elt (slot-value ent 'velocity) 0))
		    (y_v (elt (slot-value ent 'velocity) 1))
		    (x_a (elt (slot-value ent 'acceleration) 0))
		    (y_a (elt (slot-value ent 'acceleration) 1)))
    (if (OR (AND (< x_v 50) 
		 (> x_v 0))
	    (AND (> x_v -50) 
		 (< x_v 0)))
	() )
	;(setf x_a 
	 ;     0))

    (setf x_v 
	  (+ x_v 
	     (* x_a 
		(sdl:dt))))
    (setf y_v 
	  (+ y_v 
	     (* y_a 
		(sdl:dt))))
    (setf x 
	  (+ x 
	     (* x_v 
		(sdl:dt)) 
	     (* 1/2 
		x_a 
		(expt (sdl:dt) 
		      2))))
    (setf y 
	  (+ y 
	     (* y_v 
		(sdl:dt))
	     (* 1/2 
		y_a 
		(expt (sdl:dt) 
		      2))))
  ;(addVector (* 0.001 (x_v ent)) (* 0.001 (y_v ent)))

    (unless (OR (key-down :sdl-key-left)
		(key-down :sdl-key-right))
      (if (> (abs x_v) 0.1)
	  (setf x_v (* 0.75 x_v))
	  (setf x_v 0)))
    
    (unless (OR (key-down :sdl-key-down)
		(key-down :sdl-key-up))
      (if (> (abs y_v) 0.1)
	  (setf y_v (* 0.75 y_v))
	  (setf y_v 0)))
    
    ;(setf (elt (pos ent) 0) (x ent))
    ;(setf (elt (pos ent) 1) (y ent))
))
