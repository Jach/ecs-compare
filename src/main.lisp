(in-package #:ecs-compare)

; should probably just make a window class...
(defvar *ecs-window*)
(defvar *object-window*)

(defvar *ecs-window-closed?* nil)
(defvar *object-window-closed?* nil)

(defvar *ecs-window-paused?* nil)
(defvar *object-window-paused?* nil)

(defvar *ecs-renderer*)
(defvar *object-renderer*)

(defparameter +window-size+ '(800 600))

(defparameter *asteroids-count* 5000)

(defvar *ecs-ticks* 0.0d0)
(defvar *ecs-dt* 0.0d0)

(declaim (inline update-ecs-ticks))
(defun update-ecs-ticks ()
  (let ((new-ticks (al:get-time)))
    (setf *ecs-dt* (- new-ticks *ecs-ticks*)
          *ecs-ticks* new-ticks)))

(defun window-id (window)
  (lgame::sdl-get-window-id window))

(defun hide-window (id)
  (alexandria:eswitch (id)
    ((window-id *ecs-window*) (setf *ecs-window-closed?* t) (lgame::sdl-hide-window *ecs-window*))
    ((window-id *object-window*) (setf *object-window-closed?* t) (lgame::sdl-hide-window *object-window*))))

(defun main ()
  (lgame:init)
  (setf *ecs-window* (lgame.display:create-centered-window "ECS Simulation" (first +window-size+) (second +window-size+))
        *object-window* (lgame.display:create-centered-window "OOP Simulation" (first +window-size+) (second +window-size+))
        *ecs-window-closed?* nil
        *object-window-closed?* nil
        *ecs-window-paused?* nil
        *ecs-window-paused?* nil
        *ecs-renderer* (lgame.display:create-renderer *ecs-window*)
        *object-renderer* (lgame.display:create-renderer *object-window*))

  ; replace the asdf call with something more principled if making a binary,
  ; check other files too
  (lgame.loader:create-texture-loader (merge-pathnames #p"assets/" (asdf:system-source-directory "ecs-compare")))

  (sdl2:pump-events)
  (multiple-value-bind (x y) (sdl2:get-window-position *object-window*)
    (sdl2:set-window-position *object-window* x (+ y 50 (second +window-size+))))

  ;(lgame::sdl-raise-window *ecs-window*)
  ;(lgame::sdl-raise-window *object-window*)

  (let ((lgame:*renderer* *ecs-renderer*))
    (ecs-version:init (first +window-size+) (second +window-size+) *asteroids-count*))

  (let ((lgame:*renderer* *object-renderer*))
    (object-version:init *asteroids-count*))

  (lgame.time:clock-start)
  (setf *ecs-ticks* (/ lgame.time::*tick-us* 1d6))

  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (object-version:quit)
    (lgame:quit)))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (and (= (event-type event) lgame::+sdl-windowevent+)
               (= (lgame.event:ref event :window :event) lgame::+sdl-windowevent-close+))
      (let ((id (lgame.event:ref event :window :window-id)))
        (hide-window id)))

    (when (or
            (= (event-type event) lgame::+sdl-quit+)
            (and (= (event-type event) lgame::+sdl-keydown+)
                 (= (key-scancode event) lgame::+sdl-scancode-escape+))
            (and *ecs-window-closed?* *object-window-closed?*))
      (lgame.time:clock-stop))

    (when (= (event-type event) lgame::+sdl-keyup+)
      (when (= (key-scancode event) lgame::+sdl-scancode-e+)
        (setf *ecs-window-paused?* (not *ecs-window-paused?*))
        (update-ecs-ticks))

      (when (= (key-scancode event) lgame::+sdl-scancode-o+)
        (setf *object-window-paused?* (not *object-window-paused?*)))))

  (when (not *ecs-window-paused?*)
    (let ((lgame:*renderer* *ecs-renderer*))
      (lgame.render:clear)
      (game-tick-ecs)
      (lgame.render:present)))
  (when (not *object-window-paused?*)
    (let ((lgame:*renderer* *object-renderer*))
      (lgame.render:clear)
      (game-tick-object)
      (lgame.render:present)))

  (livesupport:update-repl-link)
  (lgame.time:clock-tick))

(defun game-tick-ecs ()
  (update-ecs-ticks)
  (ecs-version:update *ecs-dt*)
  (ecs-version:render))

(defun game-tick-object ()
  (object-version:tick))

(eval-when (:execute)
  (main))
