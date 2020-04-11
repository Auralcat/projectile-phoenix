## Dependencies
This project uses [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) for implementing behavior-driven tests and [Cask](https://github.com/cask/cask) for dependency management.

## How to run tests
Go to the root of the project and run `make test`. This should run Buttercup for you even if you've just cloned the project:

``` shell
$ cd ~/projectile-phoenix
$ make test
```

## Writing new tests
When you want to test something related to the Phoenix project itself, use the `projectile-test-with-sandbox` and `projectile-test-with-files` macros like this:

``` emacs-lisp
(projectile-test-with-sandbox
  (projectile-test-with-files
  ("sample/"
   "sample/lib/"
   "sample/lib/sample_web/"
   "sample/.projectile"
   "sample/mix.exs"
   "sample/lib/sample_web.example"
  )
  (cd "sample")
(expect (function) :to-equal "Something")))
```

It's important to mention that you need to *list each directory explicitly* in `projectile-test-with-files`.
This macro is not smart enough to pick up on compound paths.
For example, if you want to create `foo/bar.py`, you need to declare it like this:

``` emacs-lisp
(projectile-test-with-files
(
  "foo/"
  "foo/bar.py"
))
```

Instead of just:

``` emacs-lisp
(projectile-test-with-files
(
  "foo/bar.py"
))
```
