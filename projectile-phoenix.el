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
(defun projectile-phoenix-find-controller ()
  "Search for a controller inside the controllers directory and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "controller" ".*_controller.ex$")
  )

(defun projectile-phoenix-find-view ()
  "Search for a view inside the views directory and open it in a buffer."
  (interactive)
  (projectile-phoenix--find-web-resource "view" ".*_view.ex$")
  )

;;; Utilities
(defun projectile-phoenix--find-web-resource (web-resource web-resource-regexp)
  "Show a list of candidates for the required WEB-RESOURCE matching WEB-RESOURCE-REGEXP to the user and open the chosen candidate in a new buffer."
  (let* ((prompt (concat (capitalize web-resource) ": ")))
    (if (projectile-phoenix-project-p)
        (projectile-completing-read
         prompt
         (projectile-phoenix-web-resource-candidates web-resource web-resource-regexp)
         :action (lambda (candidate)
                   (projectile-phoenix--goto-file
                    candidate
                    (projectile-phoenix-web-resources-directory web-resource))))
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
         (directory-files (projectile-phoenix-web-resources-directory web-resource))))
    (seq-filter (lambda (c) (string-match web-resource-regexp c)) web-resource-choices)
    )
  )

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
