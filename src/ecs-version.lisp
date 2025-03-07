#|
MIT License

Copyright (c) 2023 Andrew Kravchuk <awkravchuk@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
(in-package #:ecs-version)

(defvar *font*)
(define-constant +font-path+ "inconsolata.ttf"
  :test #'string=)
(define-constant +font-size+ 24)

(ecs:define-component position
  "Determines the location of the object, in pixels."
  (x 0.0 :type single-float :documentation "X coordinate")
  (y 0.0 :type single-float :documentation "Y coordinate"))

(ecs:define-component speed
  "Determines the speed of the object, in pixels/second."
  (x 0.0 :type single-float :documentation "X coordinate")
  (y 0.0 :type single-float :documentation "Y coordinate"))

(ecs:define-component acceleration
  "Determines the acceleration of the object, in pixels/second^2."
  (x 0.0 :type single-float :documentation "X coordinate")
  (y 0.0 :type single-float :documentation "Y coordinate"))

(ecs:define-component image
  "Stores ALLEGRO_BITMAP structure pointer, size and scaling information."
  (bitmap nil :type (or null sdl2-ffi:sdl-texture))
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (scale 1.0 :type single-float))

(ecs:define-component planet
  "Tag component to indicate that entity is a planet.")

(declaim
 (type single-float
       *planet-x* *planet-y* *planet-width* *planet-height* *planet-mass*))
(defvar *planet-x*)
(defvar *planet-y*)
(defvar *planet-width*)
(defvar *planet-height*)
(defvar *planet-mass* 500000.0)

(ecs:define-system draw-images
  (:components-ro (position image)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (let ((scaled-width (* image-scale image-width))
        (scaled-height (* image-scale image-height)))
    (when (or (= position-x 400)
            (= position-y 100))
      (fc:record :ecs-drawing (list :image-scale image-scale
                                    :image-width image-width
                                    :image-height image-height
                                    :scaled-width scaled-width
                                    )))
    (al:draw-scaled-bitmap image-bitmap 0 0
                           image-width image-height
                           (- position-x (* 0.5 scaled-width))
                           (- position-y (* 0.5 scaled-height))
                           scaled-width scaled-height 0)))

(ecs:define-system move
  (:components-ro (speed)
   :components-rw (position)
   :arguments ((:dt single-float)))
  (incf position-x (* dt speed-x))
  (incf position-y (* dt speed-y))
  (fc:record :system-move-dt dt)
  (fc:record :system-move-new-pos (list position-x position-y))
  )

(ecs:define-system accelerate
  (:components-ro (acceleration)
   :components-rw (speed)
   :arguments ((:dt single-float)))
  (incf speed-x (* dt acceleration-x))
  (incf speed-y (* dt acceleration-y))
  (fc:record :system-accel-dt dt)
  (fc:record :system-accel-new-speed (list speed-x speed-y))
  )

(ecs:define-system pull
  (:components-ro (position)
   :components-rw (acceleration))
  (let* ((distance-x (- *planet-x* position-x))
         (distance-y (- *planet-y* position-y))
         (angle (atan distance-y distance-x))
         (distance-squared (+ (expt distance-x 2) (expt distance-y 2)))
         (acceleration (/ *planet-mass* distance-squared)))
    (setf acceleration-x (* acceleration (cos angle))
          acceleration-y (* acceleration (sin angle)))))

(ecs:define-system crash-asteroids
  (:components-ro (position)
   :components-no (planet)
   :with ((planet-half-width planet-half-height)
          :of-type (single-float single-float)
          := (values (/ *planet-width* 2.0)
                     (/ *planet-height* 2.0))))
  (when (<= (+ (expt (/ (- position-x *planet-x*) planet-half-width) 2)
               (expt (/ (- position-y *planet-y*) planet-half-height) 2))
            1.0)
    (ecs:delete-entity entity)))

(define-constant asteroid-images
    '("a10000.png";"ga10001.png"
      "a10002.png";"ga10003.png"
      "a10004.png";"ga10005.png"
      "a10006.png";"ga10007.png"
      "a10008.png";"ga10009.png"
      "a10010.png";"ga10011.png"
      "a10012.png";"ga10013.png"
      "a10014.png";"ga10015.png"
      "b10000.png";"gb10001.png"
      "b10002.png";"gb10003.png"
      "b10004.png";"gb10005.png"
      "b10006.png";"gb10007.png"
      "b10008.png";"gb10009.png"
      "b10010.png";"gb10011.png"
      "b10012.png";"gb10013.png"
      "b10014.png";"gb10015.png"
      )
  :test #'equalp)

(defun init (window-width window-height asteroids)
  (ecs:bind-storage)
  (let ((background-bitmap-1 (al:ensure-loaded
                              #'al:load-bitmap
                              "parallax-space-stars.png"))
        (background-bitmap-2 (al:ensure-loaded
                              #'al:load-bitmap
                              "parallax-space-far-planets.png")))
    (ecs:make-object
     `((:position :x 400.0 :y 200.0)
       (:image :bitmap ,background-bitmap-1
               :width ,(float (al:get-bitmap-width background-bitmap-1))
               :height ,(float (al:get-bitmap-height background-bitmap-1)))))
    (ecs:make-object
     `((:position :x 100.0 :y 100.0)
       (:image :bitmap ,background-bitmap-2
               :width ,(float (al:get-bitmap-width background-bitmap-2))
               :height ,(float (al:get-bitmap-height background-bitmap-2)))))
  (fc:record :ecs-background-data (list :x1 400.0 :y1 200.0
                                        :width1 (float (al:get-bitmap-width background-bitmap-1))
                                        :height1 (float (al:get-bitmap-height background-bitmap-1))
                                        :x2 100.0 :y2 100.0
                                        :width2 (float (al:get-bitmap-width background-bitmap-2))
                                        :height2 (float (al:get-bitmap-height background-bitmap-2)))))
  (let ((planet-bitmap (al:ensure-loaded
                        #'al:load-bitmap
                        "parallax-space-big-planet.png")))
    (setf *planet-width* (float (al:get-bitmap-width planet-bitmap))
          *planet-height* (float (al:get-bitmap-height planet-bitmap))
          *planet-x* (/ window-width 2.0)
          *planet-y* (/ window-height 2.0))
    (ecs:make-object `((:planet)
                       (:position :x ,*planet-x* :y ,*planet-y*)
                       (:image :bitmap ,planet-bitmap
                               :width ,*planet-width*
                               :height ,*planet-height*))))
  (let ((asteroid-bitmaps
          (map 'list
               #'(lambda (filename)
                   (al:ensure-loaded #'al:load-bitmap filename))
               asteroid-images)))
    (dotimes (_ asteroids)
      (let ((r (random 20.0))
            (angle (float (random (* 2 pi)) 0.0)))
        (ecs:make-object `((:position :x ,(+ 200.0 (* r (cos angle)))
                                      :y ,(+ *planet-y* (* r (sin angle))))
                           (:speed :x ,(+ -5.0 (random 15.0))
                                   :y ,(+ 30.0 (random 30.0)))
                           (:acceleration)
                           (:image
                            :bitmap ,(alexandria:random-elt asteroid-bitmaps)
                            :scale ,(+ 0.1 (random 0.9))
                            :width 64.0 :height 64.0))))))

  (setf *font* (al:load-ttf-font +font-path+ +font-size+ 0))
  )

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(defun update (dt)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (fc:record :ecs-dt-in-update (float dt 0.0))
  (ecs:run-systems :dt (float dt 0.0)))

(defun render ()
  (al:draw-text *font* (al:map-rgba 255 255 255 255) 0 0 0
                (format nil "~d FPS" *fps*)))

