;;; projectile-phoenix.el --- Minor mode for Phoenix projects based on projectile-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020 Miriam Retka

;; Author:            Miriam Retka <miriamretka@tutanota.com>
;; URL:               <insert_url_here>
;; Package-Version:   <insert_version_here>
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
;; This project is a helper thingy for Phoenix projects, much like
;; projectile-rails. But for Phoenix projects.

;;; Code:
(require 'projectile)
(require 'inflections)

;;; External functions
;; TODO: Improve the coverage of resource regexps.
(defun projectile-phoenix-find-controller ()
  "Search for a controller inside the controllers directory and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "controller" "_controller.ex$")
  )

(defun projectile-phoenix-find-view ()
  "Search for a view inside the views directory and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "view" "_view.ex$")
  )

(defun projectile-phoenix-find-template ()
  "Search for a template inside the templates directory and open it in a buffer."
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

(defun projectile-phoenix-find-test ()
    "This is a wrapper function for projectile-find-test-file."
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
       (if (y-or-n-p "The seeds.exs file could not be found in this project. Create one?")
           (find-file seeds-file-location))
       )
     )
   )

;;; Utilities
(defun projectile-phoenix--find-web-resource (resource resource-regexp)
  "Show a list of candidates for the required web RESOURCE matching RESOURCE-REGEXP to the user and open the chosen candidate in a new buffer.

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
  "Show a list of candidates for migrations to the user and open the chosen candidate in a new buffer."
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
  "Show a list of candidates for mix tasks to the user and open the chosen candidate in a new buffer."
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

(defun projectile-phoenix-web-resource-candidates (web-resource web-resource-regexp)
  "Return a list of base WEB-RESOURCE candidates for selection that match the provided WEB-RESOURCE-REGEXP."
  (let ((web-resource-choices
         (directory-files-recursively (projectile-phoenix-web-resources-directory web-resource) web-resource-regexp)))
    (mapcar (lambda (c) (file-name-nondirectory c)) web-resource-choices)
    )
  )

(defun projectile-phoenix-hash-web-resource-choices (resource resource-regexp)
  "Generate a key-pair relationship between the base file name (without the extension) and the file's absolute path.

It generates a relation like this for a controller, for instance:

- sample -> <project_base>/lib/<project_base>_web/controllers/sample_controller.ex
- cogs -> <project_base>/lib/<project_base>_web/controllers/cogs_controller.ex
- nested/sprockets -> <project_base>/lib/<project_base>_web/controllers/nested/sprockets_controller.ex

This function focuses on the processing of web resources, like controllers, views and templates.
For resources like tests and migrations, please check projectile-phoenix-hash-resource-choices."
  (let* (
         (base-hash (make-hash-table :test 'equal))
         (file-collection (directory-files-recursively (projectile-phoenix-web-resources-directory resource) resource-regexp))
         )
    (dolist (path file-collection)
      (puthash (projectile-phoenix-context-resource-name path resource resource-regexp) path base-hash))
    base-hash
    ))

(defun projectile-phoenix-hash-migration-choices ()
  "Generate a key-pair relationship between the base migration name and the migration's absolute path."
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
  "Generate a key-pair relationship between the base task name and the task's absolute path."
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

;; TODO: Add examples to the documentation of the functions in this file.
;; Maybe we could also give this a better name?
(defun projectile-phoenix-context-resource-name (resource-path resource resource-trim-regexp)
  "Return the context-aware name of the resource given its RESOURCE-BASE-DIR and the RESOURCE-REGEXP it should match."
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
A Phoenix project is defined by:

- A <project_name>_web directory
- A <project_name>_web.ex file.
- A mix.exs file, since it's a class of Mix project."
  (let* (
         (phoenix-web-file (concat (projectile-project-name) "_web.ex")))
    (file-exists-p (concat (projectile-project-root) "/" "lib/" phoenix-web-file))))

(provide 'projectile-phoenix)
;;; projectile-phoenix.el ends here
