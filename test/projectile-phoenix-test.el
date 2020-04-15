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
                 "sample/lib/sample_web/controllers/nested/"
                 "sample/lib/sample_web/controllers/nested/subnested/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/controllers/nested/first_level_controller.ex"
                 "sample/lib/sample_web/controllers/nested/subnested/second_level_controller.ex"
                 "sample/lib/sample_web/controllers/example_controller.ex"
                 "sample/lib/sample_web/controllers/cogs_controller.ex"
                 "sample/lib/sample_web/controllers/sprockets_controller.ex"
                 "sample/lib/sample_web/controllers/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "example_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "cogs_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "sprockets_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "first_level_controller.ex")
                (expect (projectile-phoenix-web-resource-candidates "controller" ".*_controller.ex$") :to-contain "second_level_controller.ex")
                )))
          (it "returns a list of valid view files"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/views/"
                 "sample/lib/sample_web/views/nested/"
                 "sample/lib/sample_web/views/nested/subnested/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/views/example_view.ex"
                 "sample/lib/sample_web/views/nested/first_level_view.ex"
                 "sample/lib/sample_web/views/nested/subnested/second_level_view.ex"
                 "sample/lib/sample_web/views/cogs_view.ex"
                 "sample/lib/sample_web/views/sprockets_view.ex"
                 "sample/lib/sample_web/views/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "example_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "cogs_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "sprockets_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "first_level_view.ex")
                (expect (projectile-phoenix-web-resource-candidates "view" ".*_view.ex$") :to-contain "second_level_view.ex")
                )))
          (it "returns a list of valid template files"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/templates/"
                 "sample/lib/sample_web/templates/users/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/templates/users/show.html.eex"
                 "sample/lib/sample_web/templates/users/edit.html.eex"
                 "sample/lib/sample_web/templates/users/new.html.eex"
                 "sample/lib/sample_web/templates/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-web-resource-candidates "template" ".*.html.eex$") :to-contain "new.html.eex")
                (expect (projectile-phoenix-web-resource-candidates "template" ".*.html.eex$") :to-contain "show.html.eex")
                (expect (projectile-phoenix-web-resource-candidates "template" ".*.html.eex$") :to-contain "edit.html.eex")
                )))
          )

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

(describe "projectile-phoenix-hash-web-resource-choices"
           (it "generates a key-pair relationship between the file name and the file's absolute path"
               (projectile-test-with-sandbox
                (projectile-test-with-files
                 ("sample/"
                  "sample/lib/"
                  "sample/lib/sample_web/"
                  "sample/lib/sample_web/controllers/"
                  "sample/lib/sample_web/controllers/homepage/"
                  "sample/lib/sample_web/controllers/blog/"
                  "sample/.projectile"
                  "sample/mix.exs"
                  "sample/lib/sample_web.ex"
                  "sample/lib/sample_web/controllers/example_controller.ex"
                  "sample/lib/sample_web/controllers/blog/sample_controller.ex"
                  "sample/lib/sample_web/controllers/homepage/yet_another_controller.ex"
                  "sample/lib/sample_web/controllers/cogs_controller.ex"
                  "sample/lib/sample_web/controllers/sprockets_controller.ex"
                  "sample/lib/sample_web/controllers/trashfile.ex"
                  )
                 (cd "sample")
                 (expect (gethash "homepage/yet_another" (projectile-phoenix-hash-web-resource-choices "controller" "_controller.ex$"))
                         :to-equal
                         (expand-file-name "lib/sample_web/controllers/homepage/yet_another_controller.ex"
                                           (projectile-project-root)))
                 (expect (gethash "blog/sample" (projectile-phoenix-hash-web-resource-choices "controller" "_controller.ex$"))
                         :to-equal
                         (expand-file-name "lib/sample_web/controllers/blog/sample_controller.ex"
                                           (projectile-project-root)))
                 (expect (gethash "example" (projectile-phoenix-hash-web-resource-choices "controller" "_controller.ex$"))
                         :to-equal
                         (expand-file-name "lib/sample_web/controllers/example_controller.ex"
                                           (projectile-project-root)))
                 ))))

(describe "projectile-phoenix-hash-migration-choices"
           (it "generates a key-pair relationship between the migration name and the migration's absolute path"
               (projectile-test-with-sandbox
                (projectile-test-with-files
                 ("sample/"
                  "sample/lib/"
                  "sample/priv/"
                  "sample/priv/repo/"
                  "sample/priv/repo/migrations/"
                  "sample/priv/repo/migrations/20200319173652_sample_migration.exs"
                  "sample/priv/repo/migrations/20200320173952_another_migration.exs"
                  "sample/.projectile"
                  "sample/mix.exs"
                  "sample/lib/sample_web.ex"
                  )
                 (cd "sample")
                 (expect (gethash "20200319173652_sample_migration" (projectile-phoenix-hash-migration-choices))
                         :to-equal
                         (expand-file-name "priv/repo/migrations/20200319173652_sample_migration.exs"
                                           (projectile-project-root)))
                 (expect (gethash "20200320173952_another_migration" (projectile-phoenix-hash-migration-choices))
                         :to-equal
                         (expand-file-name "priv/repo/migrations/20200320173952_another_migration.exs"
                                           (projectile-project-root)))
                 ))))

(describe "projectile-phoenix-hash-mix-task-choices"
           (it "generates a key-pair relationship between the task name and the task's absolute path"
               (projectile-test-with-sandbox
                (projectile-test-with-files
                 ("sample/"
                  "sample/lib/"
                  "sample/lib/mix/"
                  "sample/lib/mix/tasks/"
                  "sample/lib/mix/tasks/nested/"
                  "sample/lib/mix/tasks/nested/another_nesting/"
                  "sample/lib/mix/tasks/example.ex"
                  "sample/lib/mix/tasks/nested/sample_thing.ex"
                  "sample/lib/mix/tasks/nested/another_nesting/another_thing.ex"
                  "sample/.projectile"
                  "sample/mix.exs"
                  "sample/lib/sample_web.ex"
                  )
                 (cd "sample")
                 (expect (gethash "example" (projectile-phoenix-hash-mix-task-choices))
                         :to-equal
                         (expand-file-name "lib/mix/tasks/example.ex"
                                           (projectile-project-root)))
                 (expect (gethash "nested/sample_thing" (projectile-phoenix-hash-mix-task-choices))
                         :to-equal
                         (expand-file-name "lib/mix/tasks/nested/sample_thing.ex"
                                           (projectile-project-root)))
                 (expect (gethash "nested/another_nesting/another_thing" (projectile-phoenix-hash-mix-task-choices))
                         :to-equal
                         (expand-file-name "lib/mix/tasks/nested/another_nesting/another_thing.ex"
                                           (projectile-project-root)))
                 ))))

(describe "projectile-phoenix-context-resource-name"
          (it "generates a context name for the resource"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("sample/"
                 "sample/lib/"
                 "sample/lib/sample_web/"
                 "sample/lib/sample_web/controllers/"
                 "sample/lib/sample_web/controllers/homepage/"
                 "sample/lib/sample_web/controllers/blog/"
                 "sample/.projectile"
                 "sample/mix.exs"
                 "sample/lib/sample_web.ex"
                 "sample/lib/sample_web/controllers/example_controller.ex"
                 "sample/lib/sample_web/controllers/blog/sample_controller.ex"
                 "sample/lib/sample_web/controllers/homepage/yet_another_controller.ex"
                 "sample/lib/sample_web/controllers/cogs_controller.ex"
                 "sample/lib/sample_web/controllers/sprockets_controller.ex"
                 "sample/lib/sample_web/controllers/trashfile.ex"
                 )
                (cd "sample")
                (expect (projectile-phoenix-context-resource-name
                         (expand-file-name "lib/sample_web/controllers/cogs_controller.ex"
                                           (projectile-project-root))
                         "controller"
                         "_controller.ex$")
                        :to-equal
                        "cogs")
                (expect (projectile-phoenix-context-resource-name
                         (expand-file-name "lib/sample_web/controllers/homepage/yet_another_controller.ex"
                                           (projectile-project-root))
                         "controller"
                         "_controller.ex$")
                        :to-equal
                        "homepage/yet_another")
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
