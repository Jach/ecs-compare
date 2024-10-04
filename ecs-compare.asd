(defsystem "ecs-compare"
  :description "Comparison of using ECS or a more traditional game object model"
  :author "Jach <jach@thejach.com>"
  :license "Public Domain/Unlicense"
  :depends-on ("lgame"
               "cl-fast-ecs")
  :components ((:module "src/"
                :serial t
                :components ((:file "packages")
                             (:file "al-wrapper")
                             (:file "ecs-version")
                             (:file "object-version")
                             (:file "main")))))

