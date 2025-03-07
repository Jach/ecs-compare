(in-package #:al)
#|
This file is to wrap the allegro methods used by the ecs code to instead use lgame methods
|#

(defun asset-path (basename)
  "Converts basename path, presumed to refer to a resource in the assets/ folder,
   to an absolute path. Replace this if deploying a binary."
  (asdf:system-relative-pathname "ecs-compare" (uiop:strcat "assets/" basename)))

(declaim (inline hold-bitmap-drawing))
(defun hold-bitmap-drawing (_)
  (declare (ignore _)))

(defun load-bitmap ())

(declaim (inline draw-scaled-bitmap))
(defun draw-scaled-bitmap (image-bitmap src-x src-y src-w src-h dst-x dst-y dst-w dst-h flags)
  (declare (ignore flags))
  (lgame.rect:with-rect (src-r src-x src-y src-w src-h)
    (lgame.rect:with-rect (dst-r dst-x dst-y dst-w dst-h)
      (lgame::sdl-render-copy lgame:*renderer* image-bitmap src-r dst-r))))

(declaim (inline ensure-loaded))
(defun ensure-loaded (_ basename)
  "Bitmap load -> sdl texture load"
  (declare (ignore _))
  (lgame.loader:load-texture (asset-path basename)))

(declaim (inline get-bitmap-width))
(defun get-bitmap-width (texture)
  (sdl2:texture-width texture))

(declaim (inline get-bitmap-height))
(defun get-bitmap-height (texture)
  (sdl2:texture-height texture))

(declaim (inline map-rgba))
(defun map-rgba (r g b a)
  (vector r g b a))

(declaim (inline draw-text))
(defun draw-text (font color x y flags str)
  (declare (ignore flags))
  (let ((texture (lgame.font:render-text font str
                                         (aref color 0)
                                         (aref color 1)
                                         (aref color 2)
                                         (aref color 3))))
    (lgame.rect:with-rect (r x y (sdl2:texture-width texture) (sdl2:texture-height texture))
      (sdl2:render-copy lgame:*renderer* texture :dest-rect r))
    (sdl2:destroy-texture texture)))

(declaim (inline load-ttf-font))
(defun load-ttf-font (path size flags)
  (declare (ignore flags))
  (lgame.font:load-font (asset-path path) size))

(declaim (inline get-time))
(defun get-time ()
  (the double-float (/ lgame.time::*tick-us* 1d6)))

