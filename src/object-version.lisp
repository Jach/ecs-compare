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
        ; if you want to use one of the soa variants, just comment out the next field line and uncomment the one you want
        (make-instance 'asteroid-field :planet planet :asteroids asteroids :groups *all-group*)))
        ;(make-instance 'asteroid-field-soa :planet planet :asteroids asteroids :groups *all-group*)))
        ;(make-instance 'asteroid-field-fast-soa :planet planet :asteroids asteroids :groups *all-group*)))
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

;;;;
; The following code was generated by Claude Sonnet 4
;;;;

;; Optimized struct-of-arrays implementation using typed arrays
;; This should deliver measurable performance gains over the naive version

(defstruct fast-soa-asteroids
  "Struct of arrays using specialized, contiguous memory layouts"
  ;; Position arrays - separate X and Y for better cache behavior
  (pos-x nil :type (simple-array single-float (*)))
  (pos-y nil :type (simple-array single-float (*)))

  ;; Velocity arrays
  (vel-x nil :type (simple-array single-float (*)))
  (vel-y nil :type (simple-array single-float (*)))

  ;; Rendering data
  (textures nil :type (simple-array t (*)))
  (scale-w nil :type (simple-array single-float (*)))
  (scale-h nil :type (simple-array single-float (*)))

  ;; Bitmask for alive/dead state - more cache efficient than nil checks
  (alive nil :type (simple-array bit (*)))

  ;; Current count of live asteroids for early loop termination
  (count 0 :type fixnum)
  (capacity 0 :type fixnum))

(defclass asteroid-field-fast-soa (sprite add-groups-mixin)
  ((asteroids :accessor .asteroids)
   (planet :accessor .planet :initarg :planet)))

(defmethod initialize-instance :after ((self asteroid-field-fast-soa) &key planet asteroids &aux textures)
  (let ((tmp-asteroid (make-instance 'asteroid :planet planet)))
    (setf textures (/textures tmp-asteroid))
    (kill tmp-asteroid))

  (let ((soa (make-fast-soa-asteroids
              :pos-x (make-array asteroids :element-type 'single-float :initial-element 0.0f0)
              :pos-y (make-array asteroids :element-type 'single-float :initial-element 0.0f0)
              :vel-x (make-array asteroids :element-type 'single-float :initial-element 0.0f0)
              :vel-y (make-array asteroids :element-type 'single-float :initial-element 0.0f0)
              :textures (make-array asteroids :initial-element nil)
              :scale-w (make-array asteroids :element-type 'single-float :initial-element 1.0f0)
              :scale-h (make-array asteroids :element-type 'single-float :initial-element 1.0f0)
              :alive (make-array asteroids :element-type 'bit :initial-element 1)
              :count asteroids
              :capacity asteroids)))

    (setf (.asteroids self) soa)

    ;; Initialize asteroid data
    (let ((planet-y (float (rect-coord (.rect planet) :centery) 0.0f0))
          (pos-x-arr (fast-soa-asteroids-pos-x soa))
          (pos-y-arr (fast-soa-asteroids-pos-y soa))
          (vel-x-arr (fast-soa-asteroids-vel-x soa))
          (vel-y-arr (fast-soa-asteroids-vel-y soa))
          (tex-arr (fast-soa-asteroids-textures soa))
          (scale-w-arr (fast-soa-asteroids-scale-w soa))
          (scale-h-arr (fast-soa-asteroids-scale-h soa)))

      (dotimes (i asteroids)
        (let* ((r (random 20.0f0))
               (angle (random (* 2.0f0 pi)))
               (image (get-texture (alexandria:random-elt textures)))
               (base-pos-x (float (+ 200.0f0 (* r (cos angle))) 0.0f0))
               (base-pos-y (float (+ planet-y (* r (sin angle))) 0.0f0))
               (vel-x (+ -5.0f0 (random 15.0f0)))
               (vel-y (+ 30.0f0 (random 30.0f0)))
               (scale (+ 0.1f0 (random 0.9f0)))
               (scaled-w (* scale (float (sdl2:texture-width image) 0.0f0)))
               (scaled-h (* scale (float (sdl2:texture-height image) 0.0f0))))

          ;; Adjust position for centered rendering
          (setf (aref pos-x-arr i) (- base-pos-x (* 0.5f0 scaled-w))
                (aref pos-y-arr i) (- base-pos-y (* 0.5f0 scaled-h))
                (aref vel-x-arr i) vel-x
                (aref vel-y-arr i) vel-y
                (aref tex-arr i) image
                (aref scale-w-arr i) scaled-w
                (aref scale-h-arr i) scaled-h))))))

(defmethod update ((self asteroid-field-fast-soa))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((dt (float (dt) 0.0f0))
         (soa (.asteroids self))
         (planet (.planet self))
         (planet-x (float (rect-coord (.rect planet) :centerx) 0.0f0))
         (planet-y (float (rect-coord (.rect planet) :centery) 0.0f0))
         (planet-mass (float (.mass planet) 0.0f0))
         (planet-half-w (float (/ (sdl2:rect-width (.rect planet)) 2.0) 0.0f0))
         (planet-half-h (float (/ (sdl2:rect-height (.rect planet)) 2.0) 0.0f0))

         ;; Extract arrays for better compiler optimization
         (pos-x-arr (fast-soa-asteroids-pos-x soa))
         (pos-y-arr (fast-soa-asteroids-pos-y soa))
         (vel-x-arr (fast-soa-asteroids-vel-x soa))
         (vel-y-arr (fast-soa-asteroids-vel-y soa))
         (alive-arr (fast-soa-asteroids-alive soa))
         (capacity (fast-soa-asteroids-capacity soa)))

    (declare (type single-float dt planet-x planet-y planet-mass planet-half-w planet-half-h)
             (type (simple-array single-float (*)) pos-x-arr pos-y-arr vel-x-arr vel-y-arr)
             (type (simple-array bit (*)) alive-arr)
             (type fixnum capacity))

    ;; Main physics loop - optimized for cache locality
    (loop for i of-type fixnum below capacity
          when (= 1 (aref alive-arr i))
          do
          (let* ((pos-x (aref pos-x-arr i))
                 (pos-y (aref pos-y-arr i))
                 (distance-x (- planet-x pos-x))
                 (distance-y (- planet-y pos-y))
                 (distance-squared (+ (* distance-x distance-x)
                                    (* distance-y distance-y))))
            (declare (type single-float pos-x pos-y distance-x distance-y distance-squared))

            ;; Avoid division by zero and infinite acceleration
            (when (> distance-squared 1.0f0)
              (let* ((angle (atan distance-y distance-x))
                     (accel (/ planet-mass distance-squared))
                     (accel-x (* accel (cos angle)))
                     (accel-y (* accel (sin angle))))
                (declare (type single-float angle accel accel-x accel-y))

                ;; Semi-implicit Euler integration
                ;; Update velocity first
                (incf (aref vel-x-arr i) (* dt accel-x))
                (incf (aref vel-y-arr i) (* dt accel-y))

                ;; Then update position using new velocity
                (incf (aref pos-x-arr i) (* dt (aref vel-x-arr i)))
                (incf (aref pos-y-arr i) (* dt (aref vel-y-arr i)))

                ;; Collision detection with planet (ellipse)
                (let ((new-pos-x (aref pos-x-arr i))
                      (new-pos-y (aref pos-y-arr i)))
                  (declare (type single-float new-pos-x new-pos-y))
                  (when (<= (+ (expt (/ (- new-pos-x planet-x) planet-half-w) 2)
                             (expt (/ (- new-pos-y planet-y) planet-half-h) 2))
                          1.0f0)
                    ;; Mark as dead
                    (setf (aref alive-arr i) 0)
                    (decf (fast-soa-asteroids-count soa))))))))))

(defmethod draw ((self asteroid-field-fast-soa))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((soa (.asteroids self))
         (pos-x-arr (fast-soa-asteroids-pos-x soa))
         (pos-y-arr (fast-soa-asteroids-pos-y soa))
         (tex-arr (fast-soa-asteroids-textures soa))
         (scale-w-arr (fast-soa-asteroids-scale-w soa))
         (scale-h-arr (fast-soa-asteroids-scale-h soa))
         (alive-arr (fast-soa-asteroids-alive soa))
         (capacity (fast-soa-asteroids-capacity soa)))

    (declare (type (simple-array single-float (*)) pos-x-arr pos-y-arr scale-w-arr scale-h-arr)
             (type (simple-array t (*)) tex-arr)
             (type (simple-array bit (*)) alive-arr)
             (type fixnum capacity))

    ;; Early exit if no asteroids alive
    (when (> (fast-soa-asteroids-count soa) 0)
      (loop for i of-type fixnum below capacity
            when (= 1 (aref alive-arr i))
            do
            (lgame.rect:with-rect (r (truncate (aref pos-x-arr i))
                                     (truncate (aref pos-y-arr i))
                                     (truncate (aref scale-w-arr i))
                                     (truncate (aref scale-h-arr i)))
              (sdl2:render-copy lgame:*renderer* (aref tex-arr i) :dest-rect r))))))

;; Convenience method to add this to your existing code:
;; Just replace asteroid-field-soa with asteroid-field-fast-soa in your init function

;; Key optimizations made:
;; 1. Separate X/Y arrays instead of pair structs (eliminates pointer indirection)
;; 2. Explicit single-float typing throughout (enables SIMD optimizations)
;; 3. Bit array for alive/dead state (more cache efficient than nil checks)
;; 4. Early termination when count reaches 0
;; 5. Compiler declarations for aggressive optimization
;; 6. Array extraction outside loops to help compiler optimization
;; 7. Proper numeric type declarations to avoid boxing/unboxing
