(in-package #:ecs-compare)

(defvar *mode* 'ecs)

(defparameter +window-size+ '(800 600))

(defparameter *asteroids-count* 5000)

(defvar *ecs-ticks* 0.0d0)
(defvar *ecs-dt* 0.0d0)

(declaim (inline update-ecs-ticks))
(defun update-ecs-ticks ()
  (let ((new-ticks (al:get-time)))
    (setf *ecs-dt* (- new-ticks *ecs-ticks*)
          *ecs-ticks* new-ticks)))


(defun main ()
  (lgame:init)
  (lgame.display:create-centered-window "Do you need ECS?" (first +window-size+) (second +window-size+))
  (lgame.display:create-renderer)

  ; replace if making a binary:
  (lgame.loader:create-texture-loader (merge-pathnames #p"assets/" (asdf:system-source-directory "ecs-compare")))

  (sdl2:pump-events)
  (lgame::sdl-raise-window lgame:*screen*)

  (ecs-version:init (first +window-size+) (second +window-size+) *asteroids-count*)
  (object-version:init *asteroids-count*)

  (lgame.time:clock-start)
  (setf *ecs-ticks* (/ lgame.time::*tick-us* 1d6))

  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (lgame:quit)))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (or
            (= (event-type event) lgame::+sdl-quit+)
            (and (= (event-type event) lgame::+sdl-keydown+)
                 (= (key-scancode event) lgame::+sdl-scancode-escape+)))
      (lgame.time:clock-stop))

    (when (= (event-type event) lgame::+sdl-keyup+)

      (when (= (key-scancode event) lgame::+sdl-scancode-e+)
        (setf *mode* 'ecs)
        (update-ecs-ticks))

      (when (= (key-scancode event) lgame::+sdl-scancode-o+)
        (setf *mode* 'object))))

  (lgame.render:clear)

  (cond
    ((eql *mode* 'ecs) (game-tick-ecs))
    ((eql *mode* 'object) (game-tick-object)))

  (lgame.render:present)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick))

(defun game-tick-ecs ()
  (update-ecs-ticks)
  (ecs-version:update *ecs-dt*)
  (ecs-version:render))

(defun game-tick-object ()
  (object-version:update)
  (object-version:render))

(eval-when (:execute)
  (main))
