(in-package #:object-version)

(defvar *all-group* nil)

(defclass game-object (sprite add-groups-mixin cleaned-on-kill-mixin)
  ())



(defclass background (game-object)
  ())

(defmethod initialize-instance :after ((self background) &key image pos-x pos-y)
  (setf (.image self) image
        (.rect self) (get-texture-rect image))
  (move-rect (.rect self) pos-x pos-y))

(defmethod draw ((self background))
  "Draw the bg objects 'scaled' by 1 and shifted over"
  (let ((x (sdl2:rect-x (.rect self)))
        (y (sdl2:rect-y (.rect self)))
        (w (sdl2:rect-width (.rect self)))
        (h (sdl2:rect-height (.rect self))))
  (al:draw-scaled-bitmap (.image self)
                         0 0 w h ; src rect
                         (- x (* 0.5 w))
                         (- y (* 0.5 h))
                         w h ; dst rect
                         0)))


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
         (dt (dt))
         (msg (format nil "~d FPS" (if (zerop dt) 0 (round 1 dt))))
         (texture (lgame.font:render-text font msg 255 255 255)))
    (lgame.rect:with-rect (r 0 0 (sdl2:texture-width texture) (sdl2:texture-height texture))
      (sdl2:render-copy lgame:*renderer* texture :dest-rect r))
    (sdl2:destroy-texture texture)))


(defun init (asteroids &key (use-field? nil))
  ;(setf lparallel:*kernel* (lparallel:make-kernel 12))
  (setf *all-group* (make-instance 'lgame.sprite:ordered-group))
  (make-instance 'background
                 :image (get-texture "parallax-space-stars.png")
                 :pos-x 400 :pos-y 200
                 :groups *all-group*)
  (make-instance 'background
                 :image (get-texture "parallax-space-far-planets.png")
                 :pos-x 100 :pos-y 100
                 :groups *all-group*)
  (let* ((planet (make-instance 'planet :groups *all-group*)))
    (if (not use-field?)
        (dotimes (_ asteroids)
          (make-instance 'asteroid :planet planet :groups *all-group*))
        (make-instance 'asteroid-field :planet planet :asteroids asteroids :groups *all-group*)))
        ;(make-instance 'asteroid-field-soa :planet planet :asteroids asteroids :groups *all-group*)))
  (make-instance 'fps-display :groups *all-group*))

(defun dt ()
  (lgame.time:dt))

(defun tick ()
  (update *all-group*)
  (draw *all-group*))

(defun quit ()
  (lgame.sprite:do-sprite (s *all-group*)
    (kill s)))

; More optimized asteroid-field version added later:

(defclass asteroid-field (sprite add-groups-mixin)
  ((asteroids :accessor .asteroids)
   (planet :accessor .planet :initarg :planet)))

(defstruct simple-asteroid
  texture
  (pos-x 0.0)
  (pos-y 0.0)
  (vel-x 0.0)
  (vel-y 0.0)
  scaled-w
  scaled-h)

(defmethod initialize-instance :after ((self asteroid-field) &key planet asteroids &aux textures)
  (let ((tmp-asteroid (make-instance 'asteroid :planet planet)))
    (setf textures (/textures tmp-asteroid))
    (kill tmp-asteroid))

  (setf (.asteroids self) (make-array asteroids))
  (dotimes (i asteroids)
    (let ((r (random 20.0))
          (angle (random (* 2 pi)))
          (planet-y (rect-coord (.rect (.planet self)) :centery))
          (image (get-texture (alexandria:random-elt textures)))
          (asteroid (make-simple-asteroid)))
      (setf (simple-asteroid-texture asteroid) image
            (simple-asteroid-pos-x asteroid) (+ 200 (* r (cos angle)))
            (simple-asteroid-pos-y asteroid) (+ planet-y (* r (sin angle)))
            (simple-asteroid-vel-x asteroid) (+ -5 (random 15.0))
            (simple-asteroid-vel-y asteroid) (+ 30 (random 30.0)))

      (let* ((scale (+ 0.1 (random 0.9)))
             (scaled-w (* scale (sdl2:texture-width image)))
             (scaled-h (* scale (sdl2:texture-height image))))
        (setf (simple-asteroid-scaled-w asteroid) scaled-w
              (simple-asteroid-scaled-h asteroid) scaled-h)
        (decf (simple-asteroid-pos-x asteroid) (* 0.5 scaled-w))
        (decf (simple-asteroid-pos-y asteroid) (* 0.5 scaled-h)))

      (setf (aref (.asteroids self) i) asteroid))))

(defmethod update ((self asteroid-field))
  (let* ((dt (dt))
         (asteroids (.asteroids self))
         (planet (.planet self))
         (planet-x (rect-coord (.rect planet) :centerx))
         (planet-y (rect-coord (.rect planet) :centery))
         (planet-half-w (/ (sdl2:rect-width (.rect planet)) 2.0))
         (planet-half-h (/ (sdl2:rect-height (.rect planet)) 2.0)))
    (loop for asteroid across asteroids
          for i from 0
          when asteroid
          do
    ;(lparallel:pmapc
    ;  (lambda (i)
    ;    (alexandria:when-let ((asteroid (aref asteroids i)))
          (let* ((distance-x (- planet-x (simple-asteroid-pos-x asteroid)))
                 (distance-y (- planet-y (simple-asteroid-pos-y asteroid)))
                 (angle (atan distance-y distance-x))
                 (distance-squared (+ (expt distance-x 2) (expt distance-y 2)))
                 (accel (/ (.mass planet) distance-squared))
                 (accel-x (* accel (cos angle)))
                 (accel-y (* accel (sin angle))))
            (incf (simple-asteroid-vel-x asteroid) (* dt accel-x))
            (incf (simple-asteroid-vel-y asteroid) (* dt accel-y))

            (incf (simple-asteroid-pos-x asteroid) (* dt (simple-asteroid-vel-x asteroid)))
            (incf (simple-asteroid-pos-y asteroid) (* dt (simple-asteroid-vel-y asteroid)))

            (when (<= (+ (expt (/ (- (simple-asteroid-pos-x asteroid) planet-x) planet-half-w) 2)
                         (expt (/ (- (simple-asteroid-pos-y asteroid) planet-y) planet-half-h) 2))
                      1.0)
              ;; 'remove' it
              (setf (aref (.asteroids self) i) nil)))
    ;      )) (loop for i below (length asteroids) collect i))
          )
  ))


(defmethod draw ((self asteroid-field))
  (loop for asteroid across (.asteroids self)
        when asteroid
        do
        (lgame.rect:with-rect (r (simple-asteroid-pos-x asteroid)
                                 (simple-asteroid-pos-y asteroid)
                                 (simple-asteroid-scaled-w asteroid)
                                 (simple-asteroid-scaled-h asteroid))
          (sdl2:render-copy lgame:*renderer* (simple-asteroid-texture asteroid) :dest-rect r))))


; Another version, using a struct of arrays approach.

(defstruct pair
  (x 0.0)
  (y 0.0))

(defstruct soa-asteroids
  textures
  positions
  velocities
  scales)

(defclass asteroid-field-soa (sprite add-groups-mixin)
  ((asteroids :accessor .asteroids)
   (planet :accessor .planet :initarg :planet)))

(defmethod initialize-instance :after ((self asteroid-field-soa) &key planet asteroids &aux textures all-asteroids)
  (let ((tmp-asteroid (make-instance 'asteroid :planet planet)))
    (setf textures (/textures tmp-asteroid))
    (kill tmp-asteroid))

  (setf all-asteroids (make-soa-asteroids
                        :textures (make-array asteroids)
                        :positions (make-array asteroids)
                        :velocities (make-array asteroids)
                        :scales (make-array asteroids)))
  (setf (.asteroids self) all-asteroids)

  (dotimes (i asteroids)
    (let* ((r (random 20.0))
           (angle (random (* 2 pi)))
           (planet-y (rect-coord (.rect (.planet self)) :centery))
           (image (get-texture (alexandria:random-elt textures)))
           (pos-x (+ 200 (* r (cos angle))))
           (pos-y (+ planet-y (* r (sin angle))))
           (vel-x (+ -5 (random 15.0)))
           (vel-y (+ 30 (random 30.0)))
           (scale (+ 0.1 (random 0.9)))
           (scaled-w (* scale (sdl2:texture-width image)))
           (scaled-h (* scale (sdl2:texture-height image))))
      (decf pos-x (* 0.5 scaled-w))
      (decf pos-y (* 0.5 scaled-h))

      (setf (aref (soa-asteroids-textures all-asteroids) i) image
            (aref (soa-asteroids-positions all-asteroids) i) (make-pair :x pos-x :y pos-y)
            (aref (soa-asteroids-velocities all-asteroids) i) (make-pair :x vel-x :y vel-y)
            (aref (soa-asteroids-scales all-asteroids) i) (make-pair :x scaled-w :y scaled-h)))))

(defmethod update ((self asteroid-field-soa))
  (let* ((dt (dt))
         (all-asteroids (.asteroids self))
         (planet (.planet self))
         (planet-x (rect-coord (.rect planet) :centerx))
         (planet-y (rect-coord (.rect planet) :centery))
         (planet-half-w (/ (sdl2:rect-width (.rect planet)) 2.0))
         (planet-half-h (/ (sdl2:rect-height (.rect planet)) 2.0)))

    (loop for pos across (soa-asteroids-positions all-asteroids)
          for vel across (soa-asteroids-velocities all-asteroids)
          for i from 0
          when pos
          do
          (let* ((distance-x (- planet-x (pair-x pos)))
                 (distance-y (- planet-y (pair-y pos)))
                 (angle (atan distance-y distance-x))
                 (distance-squared (+ (expt distance-x 2) (expt distance-y 2)))
                 (accel (/ (.mass planet) distance-squared))
                 (accel-x (* accel (cos angle)))
                 (accel-y (* accel (sin angle))))
            (incf (pair-x vel) (* dt accel-x))
            (incf (pair-y vel) (* dt accel-y))

            (incf (pair-x pos) (* dt (pair-x vel)))
            (incf (pair-y pos) (* dt (pair-y vel)))

            (when (<= (+ (expt (/ (- (pair-x pos) planet-x) planet-half-w) 2)
                         (expt (/ (- (pair-y pos) planet-y) planet-half-h) 2))
                      1.0)
              ;; 'remove' it, for laziness just from pos
              (setf (aref (soa-asteroids-positions all-asteroids) i) nil))))))


(defmethod draw ((self asteroid-field-soa))
  (loop for image across (soa-asteroids-textures (.asteroids self))
        for pos across (soa-asteroids-positions (.asteroids self))
        for scaled across (soa-asteroids-scales (.asteroids self))
        when pos
        do
        (lgame.rect:with-rect (r (pair-x pos)
                                 (pair-y pos)
                                 (pair-x scaled)
                                 (pair-y scaled))
          (sdl2:render-copy lgame:*renderer* image :dest-rect r))))
