;;; related-files-recipe-test.el --- Tests for related-files-recipe  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.2"))
;; Created: 25 Sep 2022
;; URL: https://www.gnu.org/software/emacs/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for related-files-recipe.el.

;;; Code:

(require 'related-files-recipe)


;;; Customization Options

(ert-deftest related-files-recipe-test-jumpers-safe-values ()
  (should (safe-local-variable-p 'related-files-jumpers '((recipe :remove-suffix ".el" add-suffix "-tests.el")))))


;;; Utility Functions

(ert-deftest related-files-recipe-test-apply-filename-jumper ()
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (_) t)))
    (let* ((place "/emacs-src/lisp/Abbrev.el")
           (places (related-files-recipe--apply-filename-jumper
                    place
                    :remove-suffix ".el"
                    :add-suffix "-tests.el"
                    :case-transformer 'uncapitalize
                    :add-directory "test"
                    :filler 'foo)))
      (should (seq-set-equal-p
               places
               '("/test/emacs-src/lisp/abbrev-tests.el"
                 "/emacs-src/test/lisp/abbrev-tests.el"
                 "/emacs-src/lisp/test/abbrev-tests.el"))))))

(ert-deftest related-files-recipe-test-unapply-filename-jumper ()
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (_) t)))
    (let* ((place "/emacs-src/test/lisp/abbrev-tests.el")
           (places (related-files-recipe--unapply-filename-jumper
                    place
                    :remove-suffix ".el"
                    :add-suffix "-tests.el"
                    :case-transformer 'uncapitalize
                    :add-directory "test"
                    :filler 'foo)))
      (should (seq-set-equal-p places '("/emacs-src/lisp/Abbrev.el"))))))

(ert-deftest related-files-recipe-test-add-directory-to-path ()
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (_) t)))
    (let ((result (related-files-recipe--add-directory-to-path "/emacs-src/lisp/abbrev.el" "test")))
      (should (seq-set-equal-p
               result
               '("/test/emacs-src/lisp/abbrev.el"
                 "/emacs-src/test/lisp/abbrev.el"
                 "/emacs-src/lisp/test/abbrev.el"))))))

(ert-deftest related-files-recipe-test-add-directory-to-path-filter-non-existing-directories ()
  "To reduce the number of candidates, the directories must already exist."
  (let ((existing-directory "/emacs-src/test/lisp/"))
    (cl-letf (((symbol-function 'file-exists-p)
               (apply-partially #'string= existing-directory)))
      (let ((result (related-files-recipe--add-directory-to-path "/emacs-src/lisp/abbrev.el" "test")))
        (should (equal
                 result
                 (list (concat existing-directory "abbrev.el"))))))))

(ert-deftest related-files-recipe-test-remove-directory-from-path ()
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (_) t)))
    (let ((result (related-files-recipe--remove-directory-from-path "/test/emacs-src/test/lisp/test/abbrev-tests.el" "test")))
      (should (seq-set-equal-p
               result
               '("/emacs-src/test/lisp/test/abbrev-tests.el"
                 "/test/emacs-src/lisp/test/abbrev-tests.el"
                 "/test/emacs-src/test/lisp/abbrev-tests.el"))))))

(ert-deftest related-files-recipe-test-remove-directory-from-path-filter-non-existing-directories ()
  "To reduce the number of candidates, the directories must already exist."
  (let ((existing-directory "/test/emacs-src/lisp/test/"))
    (cl-letf (((symbol-function 'file-exists-p)
               (apply-partially #'string= existing-directory)))
      (let ((result (related-files-recipe--remove-directory-from-path "/test/emacs-src/test/lisp/test/abbrev-tests.el" "test")))
        (should (equal
                 result
                 (list (concat existing-directory "abbrev-tests.el"))))))))

(ert-deftest related-files-recipe-test-apply-to-filename ()
  (should (equal (related-files-recipe--apply-to-filename "/foo/bar" #'upcase) "/foo/BAR"))
  (should (equal (related-files-recipe--apply-to-filename "/foo/bar/BAZ.EL" #'downcase) "/foo/bar/baz.el")))

(ert-deftest related-files-recipe-test-apply-case-transformer ()
  (should (equal (related-files-recipe--apply-case-transformer 'capitalize "foo") "Foo"))
  (should (equal (related-files-recipe--apply-case-transformer 'uncapitalize "Foo") "foo"))
  (should (equal (related-files-recipe--apply-case-transformer nil "foo") "foo"))
  (should-error (related-files-recipe--apply-case-transformer 'unknown "foo")))

(ert-deftest related-files-recipe-test-unapply-case-transformer ()
  (should (equal (related-files-recipe--unapply-case-transformer 'capitalize "Foo") "foo"))
  (should (equal (related-files-recipe--unapply-case-transformer 'uncapitalize "foo") "Foo"))
  (should (equal (related-files-recipe--unapply-case-transformer nil "foo") "foo"))
  (should-error (related-files-recipe--unapply-case-transformer 'unknown "foo")))

(ert-deftest related-files-recipe-test-suffix-can-be-changed-p ()
  (should-not (related-files-recipe--suffix-can-be-changed-p "/a/b.el" ".el" "-tests.el"))
  (should-not (related-files-recipe--suffix-can-be-changed-p "/a/b-tests.el" "-tests.el" ".el"))
  (should (related-files-recipe--suffix-can-be-changed-p "/a/b-tests.el" ".el" "-tests.el"))
  (should (related-files-recipe--suffix-can-be-changed-p "/a/b.el" "-tests.el" ".el"))
  (should (related-files-recipe--suffix-can-be-changed-p "/a/b.less" ".js" ".less")))

(ert-deftest related-files-recipe-test-seq-positions ()
  (should (equal '(0 3) (related-files-recipe--seq-positions '("a" "b" "c" "a" "d") "a")))
  (should (equal '() (related-files-recipe--seq-positions '("a" "b" "c" "a" "d") "Z"))))

(ert-deftest related-files-recipe-test-seq-remove-at-position ()
  (let ((letters '(a b c d)))
    (should (equal '(a b d) (related-files-recipe--seq-remove-at-position letters 2)))
    (should (equal '(b c d) (related-files-recipe--seq-remove-at-position letters 0)))
    (should (equal '(a b c) (related-files-recipe--seq-remove-at-position letters 3)))))

(provide 'related-files-recipe-test)
;;; related-files-recipe-test.el ends here
