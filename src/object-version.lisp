(in-package #:object-version)

(defvar *all-group* nil)

(declaim (inline dt))
(defun dt ()
  (min 0.25 (* (the double-float (+ lgame.time:*last-frame-duration* lgame.time:*last-any-delay*)) 1d-3)))

(defclass game-object (sprite add-groups-mixin cleaned-on-kill-mixin)
  ())



(defclass background (game-object)
  ())

(defmethod initialize-instance :after ((self background) &key image pos-x pos-y)
  (setf (.image self) image
        (.rect self) (get-texture-rect image))
  (move-rect (.rect self) pos-x pos-y))



(defclass planet (game-object)
  ((mass :accessor .mass :initform 500000.0)))

(defmethod initialize-instance :after ((self planet) &key)
  (setf (.image self) (get-texture "parallax-space-big-planet.png")
        (.rect self) (get-texture-rect (.image self)))
  (setf (rect-coord (.rect self) :center) (rect-coord lgame:*screen-rect* :center)))



(defclass asteroid (game-object)
  ((textures :accessor /textures :allocation :class
             :initform '("a10000.png"
                         "a10002.png"
                         "a10004.png"
                         "a10006.png"
                         "a10008.png"
                         "a10010.png"
                         "a10012.png"
                         "a10014.png"
                         "b10000.png"
                         "b10002.png"
                         "b10004.png"
                         "b10006.png"
                         "b10008.png"
                         "b10010.png"
                         "b10012.png"
                         "b10014.png"))

   (pos :accessor .position :initform (vector 0.0 0.0))
   (vel :accessor .velocity :initform (vector 0.0 0.0))
   (acc :accessor .acceleration :initform (vector 0.0 0.0))

   (planet :accessor .planet :initarg :planet)))

(defmethod initialize-instance :after ((self asteroid) &key)
  (let ((r (random 20.0))
        (angle (random (* 2 pi)))
        (planet-y (rect-coord (.rect (.planet self)) :centery))
        (image (get-texture (alexandria:random-elt (/textures self)))))
    (setf (aref (.position self) 0) (+ 200 (* r (cos angle)))
          (aref (.position self) 1) (+ planet-y (* r (sin angle)))
          (aref (.velocity self) 0) (+ -5 (random 15.0))
          (aref (.velocity self) 1) (+ 30 (random 30.0))
          (.image self) image)
    (let* ((rect (get-texture-rect image))
           (scale (+ 0.1 (random 0.9)))
           (scaled-w (* scale (sdl2:rect-width rect)))
           (scaled-h (* scale (sdl2:rect-height rect))))
      (set-rect rect
                :x (- (aref (.position self) 0) (* 0.5 scaled-w))
                :y (- (aref (.position self) 1) (* 0.5 scaled-h))
                :w scaled-w :h scaled-h)
      (setf (.rect self) rect))))

(defmethod update ((self asteroid))
  (let* ((dt (dt))
         (pos-x (aref (.position self) 0))
         (pos-y (aref (.position self) 1))
         (planet (.planet self))
         (planet-x (rect-coord (.rect planet) :centerx))
         (planet-y (rect-coord (.rect planet) :centery))
         (distance-x (- planet-x pos-x))
         (distance-y (- planet-y pos-y))
         (angle (atan distance-y distance-x))
         (distance-squared (+ (expt distance-x 2) (expt distance-y 2)))
         (accel (/ (.mass planet) distance-squared))
         (accel-x (* accel (cos angle)))
         (accel-y (* accel (sin angle))))
    (setf (aref (.acceleration self) 0) accel-x
          (aref (.acceleration self) 1) accel-y)

    (incf (aref (.velocity self) 0) (* dt accel-x))
    (incf (aref (.velocity self) 1) (* dt accel-y))

    (incf (aref (.position self) 0) (* dt (aref (.velocity self) 0)))
    (incf (aref (.position self) 1) (* dt (aref (.velocity self) 1)))

    (let ((new-x (aref (.position self) 0))
          (new-y (aref (.position self) 1))
          (planet-half-w (/ (sdl2:rect-width (.rect planet)) 2.0))
          (planet-half-h (/ (sdl2:rect-height (.rect planet)) 2.0)))
    (set-rect (.rect self) :x new-x :y new-y)
    (when (<= (+ (expt (/ (- new-x planet-x) planet-half-w) 2)
                 (expt (/ (- new-y planet-y) planet-half-h) 2))
              1.0)
      (kill self)))))



(defclass fps-display (sprite add-groups-mixin)
  ())

(defmethod draw ((self fps-display))
  (let* ((font (lgame.font:load-font (al::asset-path "inconsolata.ttf") 24))
         (msg (format nil "~d FPS" (round 1 (dt))))
         (texture (lgame.font:render-text font msg 255 255 255)))
    (lgame.rect:with-rect (r 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture))
      (sdl2:render-copy lgame:*renderer* texture :dest-rect r))
    (sdl2:destroy-texture texture)))


(defun init (asteroids)
  (setf *all-group* (make-instance 'lgame.sprite:ordered-group))
  (make-instance 'background
                 :image (get-texture "parallax-space-stars.png")
                 :pos-x 400 :pos-y 200
                 :groups *all-group*)
  (make-instance 'background :image (get-texture "parallax-space-far-planets.png")
                 :pos-x 100 :pos-y 100
                 :groups *all-group*)
  (let* ((planet (make-instance 'planet :groups *all-group*)))
    (dotimes (_ asteroids)
        (make-instance 'asteroid :planet planet :groups *all-group*)))
  (make-instance 'fps-display :groups *all-group*))


(defun tick ()
  (update *all-group*)
  (draw *all-group*))

(defun quit ()
  (lgame.sprite:do-sprite (s *all-group*)
    (kill s)))

