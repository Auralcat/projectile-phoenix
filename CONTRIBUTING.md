# Project structure
Currently, all the implementation code is in `projectile-phoenix.el`. The file is separated into the external functions (as of this moment, just the navigation functions) and the utility functions.

The functions named like `projectile-phoenix--this` are intended as private functions. The end-user should not call them, despite Emacs showing them in the function list when calling `describe-function`, for example. The external API for the package is noted as the external functions in the file.

To write tests, we can only check what is returned by the auxiliary functions, since most external functions deal with side effects: opening new buffers, running processes and so on.

## Dependencies
This project uses [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) for implementing behavior-driven tests and [Cask](https://github.com/cask/cask) for dependency management.

## Elisp References
If you are new to Emacs Lisp, we recommend going through the `GNU Emacs Lisp Reference Manual` (`M-x info` then search for `Emacs Lisp Intro`) and later consulting the `Emacs Lisp Reference Manual` (`M-x info` then search for `Elisp`).
This project makes heavy use of file-related functions.

## Glossary
A `resource` is a component of a Phoenix project.
Examples: controllers, views, templates, routers, migrations.

We define `web resources` as resources that are under the `<project_name>_web`
directory:

- Controllers
- Views
- Routers
- Channels
- Templates

For example, if we have a project structure like this:

``` shell
# ...
|-- lib
|   |-- sample_project
|   |   |-- application.ex
|   |   `-- repo.ex
|   |-- sample_project.ex
|   |-- sample_project_web
|   |   |-- channels
|   |   |   `-- user_socket.ex
|   |   |-- controllers
|   |   |   `-- page_controller.ex
|   |   |-- endpoint.ex
|   |   |-- gettext.ex
|   |   |-- router.ex
|   |   |-- templates
|   |   |   |-- layout
|   |   |   |   `-- app.html.eex
|   |   |   `-- page
|   |   |       `-- index.html.eex
|   |   `-- views
|   |       |-- error_helpers.ex
|   |       |-- error_view.ex
|   |       |-- layout_view.ex
|   |       `-- page_view.ex
|   `-- sample_project_web.ex
|-- mix.exs
|-- mix.lock
|-- priv
|   |-- gettext
|   |   |-- en
|   |   |   `-- LC_MESSAGES
|   |   |       `-- errors.po
|   |   `-- errors.pot
|   |-- repo
|   |   |-- migrations
|   |   `-- seeds.exs
# ...
```

The controller would be `page_controller.ex`, the views would be `error_view.ex`, `layout_view.ex` and `page_view.ex`, the templates would be `app.html.eex` and `index.html.eex`, the channel is `user_socket.ex` and the router is `router.ex`.

A `private resource` is defined as a resource under the `priv` directory. This includes:
- Migrations
- Translation files

# Tests
## How to run
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
