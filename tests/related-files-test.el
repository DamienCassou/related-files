;;; related-files-test.el --- Tests for related-files             -*- lexical-binding: t; -*-

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

;; Tests for related-files.el.

;;; Code:
(require 'related-files)
(require 'ert)
(require 'cl-lib)
(require 'seq)


;;; Customization Options

(ert-deftest related-files-test-jumpers-safe-values ()
  (should (safe-local-variable-p 'related-files-jumpers nil))
  (should-not (safe-local-variable-p 'related-files-jumpers (list (lambda (place) place)))))


;;; Jumpers Public API

(ert-deftest related-files-test-apply-function-jumper ()
  (let* ((place 'place)
         (jumperIdentity #'identity)
         (jumperConst (lambda (_) place)))
    (should (equal (related-files-apply jumperIdentity "/foo/bar") "/foo/bar"))
    (should (equal (related-files-apply jumperConst "/foo/bar") place))))

(defun related-files-test-jumper-with-filler (_)
  "A jumper returning a constant place."
  "new-place")

(ert-deftest related-files-test-get-filler ()
  (let ((filler "my filler"))
    (put #'related-files-test-jumper-with-filler 'related-files-filler filler)
    (should (equal (related-files-get-filler #'related-files-test-jumper-with-filler) filler))))


;;; Functions Manipulating Places

(ert-deftest related-files-test-format-place ()
  (cl-letf (((symbol-function 'file-exists-p)
             (apply-partially #'equal "/project/foo/exists.el")))
    (should (equal (related-files--format-place "/project/foo/" "/project/foo/exists.el") "exists.el"))
    (should (equal (related-files--format-place "/project/bar/" "/project/foo/exists.el") "../foo/exists.el"))
    (should (equal (related-files--format-place "/project/foo/" "/project/foo/non-existing.el") "non-existing.el (create it!)"))))


;;; Utility Functions

(ert-deftest related-files-test-collect-existing-places-does-not-return-current-place ()
  (cl-letf (((symbol-function 'file-exists-p)
             (apply-partially #'seq-contains-p '("/bar" "/foo"))))
    (let* ((current-place "/bar")
           (new-place "/foo")
           (jumper1 (lambda (_) new-place)))
      (should (equal
               (related-files--collect-existing-places (list jumper1) current-place)
               (list new-place))))))

(ert-deftest related-files-test-collect-existing-places-returns-uniq-results ()
  "If 2 jumpers produce the same place, the place should only appear once."
  (cl-letf (((symbol-function 'file-exists-p)
             (apply-partially #'seq-contains-p '("/bar" "/foo"))))
    (let* ((current-place "/bar")
           (new-place "/foo")
           (jumper1 (lambda (_) new-place))
           (jumper2 (lambda (_) new-place)))
      (should (seq-set-equal-p
               (related-files--collect-existing-places (list jumper1 jumper2) current-place)
               (list new-place))))))

(ert-deftest related-files-test-collect-existing-places-avoids-calling-same-jumper-with-current-place-twice ()
  "A jumper must only be called once per place."
  (cl-letf (((symbol-function 'file-exists-p) #'identity))
    (let* ((current-place "/bar")
           (jumper1-arguments nil)
           (jumper1 (lambda (place) (push place jumper1-arguments) (list current-place "/place"))))
      (related-files--collect-existing-places (list jumper1) current-place)
      (should (seq-set-equal-p
               jumper1-arguments
               (list current-place current-place "/place"))))))

(ert-deftest related-files-test-collect-existing-places-returns-no-place-when-no-current-place ()
  "If there is no current place, there shouldn't be any destination place."
  (should-not (related-files--collect-existing-places '(jumper) nil)))

(ert-deftest related-files-test-call-jumpers ()
  (let* ((jumperAtom (lambda (_) "/foo"))
         (jumperList (lambda (_) (list "/bar1" "/bar2")))
         (jumperSingleton (lambda (_) (list "/baz")))
         (jumperNil (lambda (_)))
         (jumperIdentity #'identity))
    (should (seq-set-equal-p (related-files--call-jumpers
                              (list jumperAtom jumperList)
                              "/")
                             '("/foo" "/bar1" "/bar2")))
    (should (seq-set-equal-p (related-files--call-jumpers
                              (list jumperAtom jumperSingleton)
                              "/")
                             '("/foo" "/baz")))
    (should (seq-set-equal-p (related-files--call-jumpers
                              (list jumperAtom jumperNil)
                              "/")
                             '("/foo")))
    (should (seq-set-equal-p (related-files--call-jumpers
                              (list jumperAtom jumperIdentity)
                              '"/")
                             '("/foo" "/")))
    (should (seq-set-equal-p (related-files--call-jumpers
                              (list jumperAtom jumperList jumperSingleton jumperNil jumperIdentity)
                              '"/")
                             '("/foo" "/bar1" "/bar2" "/baz" "/")))))

(ert-deftest related-files-test-test--call-jumpers-attach-jumper-to-all-places ()
  (let* ((jumper (lambda (_) "/foo"))
         (place (car (related-files--call-jumpers (list jumper) "/"))))
    (should (eq (get-text-property 0 :related-files-jumper place) jumper))))

(provide 'related-files-test)
;;; related-files-test.el ends here
