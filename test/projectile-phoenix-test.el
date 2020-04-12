;;; projectile-phoenix-test.el -*- lexical-binding: t -*-

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

;;; Code:
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
(describe "projectile-phoenix-web-resource-candidates"
          (it "returns a list of valid controller files"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/controllers/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/controllers/example_controller.ex"
                 "sample/lib/sample_web/controllers/cogs_controller.ex"
                 "sample/lib/sample_web/controllers/sprockets_controller.ex"
                 "sample/lib/sample_web/controllers/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "example_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "cogs_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "sprockets_controller.ex")
                )))
          (it "returns a list of valid view files"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/views/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/views/example_view.ex"
                 "sample/lib/sample_web/views/cogs_view.ex"
                 "sample/lib/sample_web/views/sprockets_view.ex"
                 "sample/lib/sample_web/views/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "example_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "cogs_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "sprockets_view.ex")
                ))))

(describe "projectile-phoenix-web-resources-directory"
          (it "returns the base directory of the controllers in the project"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/controllers/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/controllers/example_controller.ex"
                 "sample/lib/sample_web/controllers/cogs_controller.ex"
                 "sample/lib/sample_web/controllers/sprockets_controller.ex"
                 "sample/lib/sample_web/controllers/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resources-directory "controller")
                        :to-equal
                        (expand-file-name "lib/sample_web/controllers"))
                )))
          (it "returns the base directory of the views in the project"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/views/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/views/example_view.ex"
                 "sample/lib/sample_web/views/cogs_view.ex"
                 "sample/lib/sample_web/views/sprockets_view.ex"
                 "sample/lib/sample_web/views/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resources-directory "view")
                        :to-equal
                        (expand-file-name "lib/sample_web/views"))
                ))))

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
                (expect (projectile-phoenix-project-p) :to-be-truthy))))

          (it "returns nil when the user is not in a Phoenix project"
              (expect (projectile-phoenix-project-p) :not :to-be-truthy)))
