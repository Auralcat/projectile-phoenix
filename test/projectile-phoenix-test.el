;;; projectile-phoenix-test.el

;; Copyright © 2020 Miriam Retka

;; Author: Miriam Retka <miriamretka@tutanota.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of projectile-phoenix.
;; It's where we put our specs.

;; Load the files you want to test
(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "projectile-phoenix" source-directory))
  (setq projectile-test-path (expand-file-name "test" source-directory)))

;;; Test utilities taken from Projectile
(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox (expand-file-name
                   (convert-standard-filename "test/sandbox/")
                   (file-name-directory (locate-library "projectile-phoenix.el" t)))))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  "Evaluate BODY in the presence of FILES.
You'd normally combine this with `projectile-test-with-sandbox'."
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     ,@(mapcar (lambda (file)
                 (if (string-suffix-p "/" file)
                     `(make-directory ,file t)
                   `(with-temp-file ,file)))
               files)
     ,@body))

;;; Tests
(describe "projectile-phoenix-project-p"
  (it "returns t when called inside a Phoenix project"
(projectile-test-with-sandbox
      (projectile-test-with-files
       ("sample/"
        "sample/lib/"
        "sample/lib/sample_web/"
        "sample/.projectile"
        "sample/mix.exs"
        "sample/lib/sample_web.ex"
        )
       (cd "sample")
       (print default-directory)
      (expect (projectile-phoenix-project-p) :to-be-truthy))))

  (it "returns nil when the user is not in a Phoenix project"
      (expect (projectile-phoenix-project-p) :not :to-be-truthy)))
