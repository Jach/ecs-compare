(in-package #:ecs-compare)

; should probably just make a window class...
(defvar *ecs-window* nil)
(defvar *object-window* nil)

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

(defun window-id (window)
  (lgame::sdl-get-window-id window))

(defun hide-window (id)
  (alexandria:eswitch (id)
    ((window-id *ecs-window*) (setf *ecs-window-closed?* t) (lgame::sdl-hide-window *ecs-window*))
    ((window-id *object-window*) (setf *object-window-closed?* t) (lgame::sdl-hide-window *object-window*))))

(defun main (&key only-ecs? only-oop? &aux both?)
  (setf both? (and (not only-ecs?) (not only-oop?)))
  (lgame:init)
  ;(fc:make-store :sample-on-frames (loop for i below 10 collect i))

  (setf *ecs-window-closed?* t
        *ecs-window-paused?* t
        *object-window-closed?* t
        *object-window-paused?* t)

  (when (or both? only-ecs?)
    (setf
      *ecs-window* (lgame.display:create-centered-window "ECS Simulation" (first +window-size+) (second +window-size+))
      *ecs-renderer* (lgame.display:create-renderer *ecs-window*)
      *ecs-window-closed?* nil
      *ecs-window-paused?* nil))
  (when (or both? only-oop?)
    (setf
      *object-window* (lgame.display:create-centered-window "OOP Simulation" (first +window-size+) (second +window-size+))
      *object-renderer* (lgame.display:create-renderer *object-window*)
      *object-window-closed?* nil
      *object-window-paused?* nil))

  ; replace the asdf call with something more principled if making a binary,
  ; check other files too
  (lgame.loader:create-texture-loader (merge-pathnames #p"assets/" (asdf:system-source-directory "ecs-compare")))

  (sdl2:pump-events)
  (when *object-window*
    (multiple-value-bind (x y) (sdl2:get-window-position *object-window*)
      (sdl2:set-window-position *object-window* x (+ y 50 (second +window-size+)))))

  ;(lgame::sdl-raise-window *ecs-window*)
  ;(lgame::sdl-raise-window *object-window*)

  (when *ecs-window*
    (let ((lgame:*renderer* *ecs-renderer*))
      (ecs-version:init (first +window-size+) (second +window-size+) *asteroids-count*)))

  (when *object-window*
    (let ((lgame:*renderer* *object-renderer*))
      (object-version:init *asteroids-count* :use-field? t)))

  (lgame.time:clock-start)
  (setf *ecs-ticks* (/ lgame.time::*tick-us* 1d6))

  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (when *object-window*
      (object-version:quit))
    (lgame:quit)))

(declaim (inline update-ecs-ticks))
(defun update-ecs-ticks ()
  (let ((new-ticks (al:get-time)))
    (setf *ecs-dt* (- new-ticks *ecs-ticks*)
          *ecs-ticks* new-ticks)))

(defun game-tick ()
  ;(fc:frame-tick)
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
        (setf *object-window-paused?* (not *object-window-paused?*))
        (lgame.time:clock-tick))))

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

#|
(defun query-stuff ()
  (fc:query nil)

  (fc:query :ecs-dt)
  (fc:query :ecs-ticks)

  (fc:query :obj-dt)
  (fc:query :ecs-dt-in-update)
  ; so ecs-dt-in-update starts with 0.0 at frame 0, then frame 1 and on roughly match obj-dt.
  ; obj-dt starts with 0.1 at frame 0... this is because lgame dt is using values that are only updated by
  ; clock-tick... either way, the frame0 dts are suspect, but 0 makes more sense I guess.let's do that.

  (fc:query :system-move-dt :frames 1)

  (take 3 (fc:query :system-move-new-pos :frames 0))
  (take 3 (fc:query :obj-new-pos :frames 0))

  (take 3 (fc:query :system-move-new-pos :frames 1))
  (take 3 (fc:query :obj-new-pos :frames 1))

  (take 3 (fc:query :system-accel-new-speed :frames 0))
  (take 3 (fc:query :obj-new-speed :frames 0))

  (take 3 (fc:query :system-accel-new-speed :frames 1))
  (take 3 (fc:query :obj-new-speed :frames 1))
  (fc:query :ecs-background-data)
  (fc:query :ecs-drawing :frames 0)
)

(defun take (n list)
  (when (and (plusp n)
             (listp list))
    (cons (first list) (take (1- n) (rest list)))))
|#

