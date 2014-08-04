(defun physics (ent)
  (let ((x (elt (slot-value ent 'position) 0))
	(y (elt (slot-value ent 'position) 1))
	(x_v (elt (slot-value ent 'velocity) 0))
	(y_v (elt (slot-value ent 'velocity) 1))
	(x_a (elt (slot-value ent 'acceleration) 0))
	(y_a (elt (slot-value ent 'acceleration) 1)))
    (if (equal ent player)
	(progn (unless (OR (key-down :sdl-key-left)
			   (key-down :sdl-key-right))
		 (if (> (abs x_v) 0.1)
		     (setf x_v (* 0.75 x_v))
		     (setf x_v 0)))	
	       (unless (OR (key-down :sdl-key-down)
			   (key-down :sdl-key-up))
		 (if (> (abs y_v) 0.1)
		     (setf y_v (* 0.75 y_v))
		     (setf y_v 0)))))
    (values (+ x_v 
	       (* x_a 
		  (sdl:dt)))
	    (+ y_v 
	       (* y_a 
		  (sdl:dt)))
	    (+ x 
	       (* x_v 
		  (sdl:dt)) 
	       (* 1/2 
		  x_a 
		  (expt (sdl:dt) 
			2)))
	    (+ y 
	       (* y_v 
		  (sdl:dt))
	       (* 1/2 
		  y_a 
		  (expt (sdl:dt) 
			2))))))
