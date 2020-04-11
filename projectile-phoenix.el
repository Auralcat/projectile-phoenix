;;; projectile-phoenix.el --- Minor mode for Phoenix projects based on projectile-mode

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

;;; Commentary
;; This project is a helper thingy for Phoenix projects, much like
;; projectile-rails. But for Phoenix projects.

;; Utilities
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
