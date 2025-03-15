(defsystem "ecs-compare"
  :description "Comparison of using ECS or a more traditional game object model"
  :author "Jach <jach@thejach.com>"
  :license "MIT"
  :depends-on ("lgame"
               "cl-fast-ecs"
               ;"lparallel"
               )
  :components ((:module "src/"
                :serial t
                :components ((:file "packages")
                             (:file "al-wrapper")
                             (:file "ecs-version")
                             (:file "object-version")
                             (:file "main")))))

