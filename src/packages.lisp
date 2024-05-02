(defpackage #:al
  (:documentation
    "Proxy package replacing the allegro functions called by ecs-version with lgame ones instead
     to minimize edits to its original code.")
  (:use #:cl)
  (:export #:hold-bitmap-drawing #:load-bitmap #:draw-scaled-bitmap #:ensure-loaded #:get-bitmap-width #:get-bitmap-height #:map-rgba #:draw-text #:load-ttf-font #:get-time))

(defpackage #:ecs-version
  (:use #:cl)
  (:export #:init #:update #:render)
  (:import-from #:alexandria #:define-constant))

(defpackage #:object-version
  (:use #:cl)
  (:export #:init #:update #:render))

(defpackage #:ecs-compare
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.event
                #:event-type
                #:key-scancode))

