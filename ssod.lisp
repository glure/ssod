(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "sovereign state of dance")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key)
       (case key
         (:sdl-key-escape (sdl:push-quit-event))))
     (:idle ()
      (sdl:clear-display sdl:*black*)
      ; GAME HAPPENS HERE
      (sdl:update-display)))))
