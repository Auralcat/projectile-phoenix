# Projectile-phoenix
This project is inspired by [Projectile Rails](https://github.com/asok/projectile-rails) and takes some extra code from [Projectile](https://github.com/bbatsov/projectile).
This is also an early alpha version, so expect a couple bugs.
For now the package implements navigation between some Phoenix resources.

## Problem proposition
The reason this package came to be was:

- I started working with Phoenix projects
- I liked the experience that I had in [Projectile Rails](https://github.com/asok/projectile-rails)
- There were no similar minor modes for working with Phoenix projects.
  While Alchemist has `alchemist-phoenix-mode`, the last version covers the
  previous version of Phoenix, with a different project structure.
  To update `alchemist` would require more work to get around the code, and that package does a lot of stuff already.

I wanted something that concerns itself only with Phoenix projects, and nothing else.
So we're going to delegate whatever auxiliary features we can to other projects, such as `alchemist` itself for `alchemist-iex-mode`, `mix`-related stuff to `mix.el` and so on.

This project aims to offer quicker navigation to Phoenix-related files at first:

- [X] Find controller
- [X] Find view
- [X] Find template
- [X] Jump to test
- Run mix tasks

## Why not use Alchemist?
Alchemist offers navigation between Phoenix files with a minor mode, but it has been unmaintained for some time now.

## Installation
This package is not in MELPA yet, so you need to clone this repository into your local machine:

``` shell
$ git clone git@github.com:Auralcat/projectile-phoenix.git
```

In your Emacs configuration (usually `init.el`), include this snippet:

``` emacs-lisp
;; Location of the cloned repository, in this example it is ~/projectile-phoenix
(add-to-list 'load-path "~/projectile-phoenix")
(require "projectile-phoenix")
```
## Configuration
To use the package's functions through keybindings, you need to provide a prefix binding in your `init.el` like so.
This example uses "C-c .", but you're free to use any other chord you like.
``` emacs-lisp
(define-key projectile-phoenix-mode-map (kbd "C-c .") 'projectile-phoenix-command-map)
```

As an alternative, if you use [evil-mode](https://github.com/emacs-evil/evil) and [evil-leader](https://github.com/emacs-evil/evil), you can set a binding to `projectile-phoenix-command-map` directly like this:
``` emacs-lisp
(evil-leader/set-key-for-mode 'elixir-mode "." 'projectile-phoenix-command-map)
```

To activate `projectile-phoenix-mode` automatically for Phoenix projects, include this in your `init.el`:
``` emacs-lisp
(projectile-phoenix-global-mode)
```

## Usage
When inside a Phoenix project, press the key prefix that you configured and select a function:
| Function        | Key |
| Find controller | c   |
| Find migration  | n   |
| Find mix task   | m    |
| Find seed file  | s   |
| Find template   | l   |
| Find test       | t   |
| Find view       | v   |

## Contributing
For detailed instructions about how the code is structured and project goals, please check `CONTRIBUTING.md`.
If you've found a bug or want to suggest new features, please open an issue in this repository, and thank you in advance! 💜
