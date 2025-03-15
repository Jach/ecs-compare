Demonstration of ECS vs. more traditional game object style of code

A full writeup I made for this project is here: [You might not need ECS](https://www.thejach.com/view/2025/3/you_might_not_need_ecs)

# Running

If you have my [lgame](https://github.com/Jach/lgame) project and this project in your
~/quicklisp/local-projects/ folder or otherwise known to ql, you can run this
project from the command line with:

`sbcl --eval '(ql:quickload "ecs-compare")' --eval '(progn (ecs-compare:main) (quit))'`

This will launch two windows, one below the other. You can press Escape to close
them both, "e" to toggle pausing the ECS version (top), and "o" to toggle
pausing the OOP version (bottom).

If you just want to run one or the other individually, you can give the optional
keyword arguments `:only-ecs? t` or `:only-oop? t` to `main`.

# License

`ecs-version.lisp` is under the MIT license with copyright to Andrew Kravchuk,
see original [ecs-tutorial-1](https://github.com/lockie/ecs-tutorial-1) project.

Assets are copied from the same project and governed by their own licenses. They
are:

* [Inconsolata font](https://fonts.google.com/specimen/Inconsolata/about)
* [Tango icons](http://tango.freedesktop.org)
* [Asteroids](https://opengameart.org/content/asteroids)
* [Space Background](https://opengameart.org/content/space-background-3)

The rest of the new code is similarly released under the MIT license with copyright to Jach.
