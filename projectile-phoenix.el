;;; projectile-phoenix.el --- Minor mode for Phoenix projects based on projectile-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020 Miriam Retka

;; Author:            Miriam Retka <miriamretka@tutanota.com>
;; URL:               https://github.com/Auralcat/projectile-phoenix
;; Package-Version:   0.1
;; Version:           0.1
;; Keywords:          elixir, phoenix, projectile
;; Package-Requires:  ((emacs "24.3") (projectile "0.12.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This project is a helper extension meant for Phoenix projects, much like
;; projectile-rails.
;; Currently, it implements functions to navigate between Phoenix resources in a faster way.

;;; Code:
(require 'projectile)
(require 'inflections)

;; Keybindings
(defgroup projectile-phoenix nil
  "Phoenix mode based on projectile"
  :prefix "projectile-phoenix-"
  :group 'projectile)

(defcustom projectile-phoenix-keymap-prefix nil
  "Keymap prefix for function `projectile-phoenix-mode'."
  :group 'projectile-phoenix
  :type 'string)

(defvar projectile-phoenix-command-map
  (let ((map (make-sparse-keymap)) )
    (define-key map (kbd "c") 'projectile-phoenix-find-controller)
    (define-key map (kbd "t") 'projectile-phoenix-find-test)
    (define-key map (kbd "l") 'projectile-phoenix-find-template)
    (define-key map (kbd "v") 'projectile-phoenix-find-view)
    (define-key map (kbd "s") 'projectile-phoenix-find-seed-file)
    (define-key map (kbd "n") 'projectile-phoenix-find-migration)
    (define-key map (kbd "m") 'projectile-phoenix-find-mix-task)
    (define-key map (kbd "w") 'projectile-phoenix-find-worker)
    map)
  "Keymap after `projectile-phoenix-keymap-prefix'.")

(defvar projectile-phoenix-mode-map
  (let ((map (make-sparse-keymap)))
    (when projectile-phoenix-keymap-prefix
      (define-key map projectile-phoenix-keymap-prefix 'projectile-phoenix-command-map))
    map
    ))

(fset 'projectile-phoenix-command-map projectile-phoenix-command-map)

;; Mode declaration
;;;###autoload
(define-minor-mode projectile-phoenix-mode
  "Toggle Projectile Phoenix mode.

When Projectile Phoenix mode is enabled, it adds specific keybindings to
navigation functions designed for working with Phoenix projects."
  :initial-value nil
  ;; Mode line indicator
  :lighter " Phx"
  )

;;;###autoload
(defun projectile-phoenix-on ()
  "Enable variable `projectile-phoenix-mode' inside a Phoenix project.

This function is meant to be used with `define-globalized-minor-mode' in order
to enable `projectile-phoenix-mode' automatically, without needing to add a hook
to Elixir mode or activating the mode manually."
  (when (projectile-phoenix-project-p)
      (projectile-phoenix-mode +1)
    ))

(defun projectile-phoenix-off ()
  "Disable variable `projectile-phoenix-mode'."
  (projectile-phoenix-mode -1)
    )

;;;###autoload
(define-globalized-minor-mode projectile-phoenix-global-mode
  projectile-phoenix-mode
  projectile-phoenix-on)

;;; External functions
;; TODO: Improve the coverage of resource regexps.
(defun projectile-phoenix-find-controller ()
  "Search for a controller inside the project and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "controller" "_controller.ex$")
  )

(defun projectile-phoenix-find-view ()
  "Search for a view inside the project and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "view" "_view.ex$")
  )

(defun projectile-phoenix-find-template ()
  "Search for a template inside the project and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "template" ".html.eex$"))

(defun projectile-phoenix-find-migration ()
  "Search for a migration inside the project and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-migration)
  )

(defun projectile-phoenix-find-mix-task ()
  "Search for a mix task in the project and open it in a new buffer."
  (interactive)
  (projectile-phoenix--find-mix-task)
  )

(defun projectile-phoenix-find-worker ()
  "Search for a worker in the project and open it in a new buffer."
  (interactive)
  (projectile-phoenix--find-worker)
  )

(defun projectile-phoenix-find-test ()
  "This is a wrapper function for `projectile-find-test-file'.

Opens the test directory in the project and shows a list of candidates
to the user. When the user chooses one, open it in a new buffer."
  (interactive)
  (projectile-find-test-file)
  )

(defun projectile-phoenix-find-seed-file ()
   "Open the seeds.exs file in the project.

This is a convention used in Phoenix projects to denote seed files, usually
located in /priv/repo.
When the project doesn't have a seeds.exs file, prompt the user to create one
if they wish so."
   (interactive)
   (let* (
          (seeds-file-location (expand-file-name "priv/repo/seeds.exs" (projectile-project-root)))
          )
     (if (file-exists-p seeds-file-location)
         (find-file seeds-file-location)
       (if (y-or-n-p "The seeds.exs file could not be found in this project. Create one? ")
           (find-file seeds-file-location))
       )
     )
   )

;;; Utilities
(defun projectile-phoenix--find-web-resource (resource resource-regexp)
  "Show a list of candidates for the required web RESOURCE.
It should match the provided RESOURCE-REGEXP and open the chosen candidate
in a new buffer.

Web resources include:

- Controllers
- Views
- Templates
- Channels"
  (let* (
         (prompt (concat (capitalize resource) ": "))
         (choices-hash (projectile-phoenix-hash-web-resource-choices resource resource-regexp))
         )
    (if (projectile-phoenix-project-p)
        (projectile-completing-read
         prompt
         (hash-table-keys choices-hash)
         :action (lambda (candidate)
                   (find-file (gethash candidate choices-hash)
                    )))
      (message "Please call this function inside a Phoenix project."))))

(defun projectile-phoenix--find-migration ()
  "Show a list of migrations to the user and open the candidate in a new buffer."
  (let* (
         (prompt "Migration: ")
         (choices-hash (projectile-phoenix-hash-migration-choices))
         )
    (if (projectile-phoenix-project-p)
        (projectile-completing-read
         prompt
         (hash-table-keys choices-hash)
         :action (lambda (candidate)
                   (find-file (gethash candidate choices-hash)
                    )))
      (message "Please call this function inside a Phoenix project."))))

(defun projectile-phoenix--find-mix-task ()
  "Show a list of mix tasks to the user and open the candidate in a new buffer."
  (let* (
         (prompt "Task: ")
         (choices-hash (projectile-phoenix-hash-mix-task-choices))
         )
    (if (projectile-phoenix-project-p)
        (projectile-completing-read
         prompt
         (hash-table-keys choices-hash)
         :action (lambda (candidate)
                   (find-file (gethash candidate choices-hash)
                              )))
      (message "Please call this function inside a Phoenix project."))))

(defun projectile-phoenix--find-worker ()
  "Show a list of workers to the user and open the candidate in a new buffer."
  (let* (
         (prompt "Worker: ")
         (choices-hash (projectile-phoenix-hash-worker-choices))
         )
    (if (projectile-phoenix-project-p)
        (projectile-completing-read
         prompt
         (hash-table-keys choices-hash)
         :action (lambda (candidate)
                   (find-file (gethash candidate choices-hash)
                              )))
      (message "Please call this function inside a Phoenix project."))))

(defun projectile-phoenix-web-resources-directory (web-resource)
  "Return the directory of the queried WEB-RESOURCE inside the Phoenix project."
  (expand-file-name (inflection-pluralize-string web-resource)
                    (projectile-phoenix--web-directory)))

(defun projectile-phoenix--web-directory ()
  "Return the absolute path of the web directory for the current project."
  (expand-file-name
   (concat (projectile-project-name) "_web")
   (expand-file-name "lib" (projectile-project-root)))
  )

(defun projectile-phoenix--logic-directory ()
  "Return the absolute path of the lib/<project-name> directory.

This is where Phoenix stores all the logic-related modules of the project.
There is a separation between the framework layer, responsible for the wiring of the project, and the specific logic it uses."
  (expand-file-name (format "lib/%s" (projectile-project-name)) (projectile-project-root)))

(defun projectile-phoenix-web-resource-candidates (resource resource-regexp)
  "Return a list of base RESOURCE candidates for selection.

These candidates should match the provided RESOURCE-REGEXP."
  (let ((web-resource-choices
         (directory-files-recursively (projectile-phoenix-web-resources-directory resource) resource-regexp)))
    (mapcar (lambda (c) (file-name-nondirectory c)) web-resource-choices)
    )
  )

(defun projectile-phoenix-hash-web-resource-choices (resource resource-regexp)
  "Create a hash table linking the base file name and the file's absolute path.

The table is generated based on the provided web RESOURCE which matches the
provided web RESOURCE-REGEXP.

It generates a relation like this for a controller, for instance:

- sample:
  <project_base>/lib/<project_base>_web/controllers/sample_controller.ex
- cogs:
  <project_base>/lib/<project_base>_web/controllers/cogs_controller.ex
- nested/sprockets:
  <project_base>/lib/<project_base>_web/controllers/nested/sprockets_controller.ex

This function focuses on the processing of web resources, like controllers,
views and templates. For resources like tests and migrations, please check
projectile-phoenix-hash-resource-choices."
  (let* (
         (base-hash (make-hash-table :test 'equal))
         (file-collection (directory-files-recursively (projectile-phoenix-web-resources-directory resource) resource-regexp))
         )
    (dolist (path file-collection)
      (puthash (projectile-phoenix-context-resource-name path resource resource-regexp) path base-hash))
    base-hash
    ))

(defun projectile-phoenix-hash-migration-choices ()
  "Create a hash-table linking the base migration name and the migration's absolute path."
  (let* (
         (base-hash (make-hash-table :test 'equal))
         (migrations-dir (expand-file-name "priv/repo/migrations" (projectile-project-root)))
         (file-collection (directory-files-recursively migrations-dir "^[[:alnum:]].*\.exs$"))
         )
    (dolist (path file-collection)
      (puthash (file-name-base path) (expand-file-name path migrations-dir) base-hash))
    (print base-hash)
    base-hash
    ))

(defun projectile-phoenix-hash-mix-task-choices ()
  "Create a hash-table linking the base task name and the task's absolute path."
  (let* (
         (base-hash (make-hash-table :test 'equal))
         (mix-tasks-dir (expand-file-name "lib/mix/tasks" (projectile-project-root)))
         (file-collection (directory-files-recursively mix-tasks-dir ".*\.ex$"))
         )
    (dolist (path file-collection)
      (puthash
       (replace-regexp-in-string "\.ex$"
                                 ""
                                 (file-relative-name path mix-tasks-dir))
       (expand-file-name path mix-tasks-dir)
       base-hash))
    base-hash
    ))

(defun projectile-phoenix-hash-worker-choices ()
  "Create a hash-table linking the base worker name and the worker's absolute path."
  (let* (
         (base-hash (make-hash-table :test 'equal))
         (logic-dir (projectile-phoenix--logic-directory))
         (file-collection (directory-files-recursively logic-dir ".*_worker\.ex$"))
         )
    (dolist (path file-collection)
      (puthash
       (replace-regexp-in-string "_worker\.ex$"
                                 ""
                                 (file-relative-name path logic-dir))
       (expand-file-name path logic-dir)
       base-hash))
    base-hash
    ))

;; TODO: Add examples to the documentation of the functions in this file.
;; Maybe we could also give this a better name?
(defun projectile-phoenix-context-resource-name (resource-path resource resource-trim-regexp)
  "Return a name for the RESOURCE based on its context in the project.

It takes the RESOURCE-PATH to trim the absolute path of the files and an
additional RESOURCE-TRIM-REGEXP to reduce the path to a context-aware name
for the type of RESOURCE in the project.

Example:

A controller in
<project_name>/lib/<project_name>_web/controllers/nested/foo_controller.ex
will be named 'nested/foo'."
  (let* ((resource-base-dir (projectile-phoenix-web-resources-directory resource)))
    (replace-regexp-in-string resource-trim-regexp
                            ""
                            (file-relative-name resource-path resource-base-dir))))

(defun projectile-phoenix--goto-file (filename base-directory)
  "Opens the file given by FILENAME starting from BASE-DIRECTORY."
  (find-file (expand-file-name filename base-directory))
  )

(defun projectile-phoenix-project-p ()
  "Return t if called inside a Phoenix project.
It will return nil otherwise.

One of the details of Phoenix projects is that they have a
<project_name>_web.ex file under <project_name>/lib/.

So if that file is present in the project and there is also a
<project_name>_web directory as well, chances that that we're working
with a Phoenix project."
  (let* (
         (phoenix-web-file (concat (projectile-project-name) "_web.ex")))
    (and
     (file-exists-p (concat (projectile-project-root) "/" "lib/" phoenix-web-file))
     (file-exists-p (projectile-phoenix--web-directory))
     )
    ))

(provide 'projectile-phoenix)
;;; projectile-phoenix.el ends here
