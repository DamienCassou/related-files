;;; related-files-recipe.el --- Provide a recipe DSL to define related-files jumpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1.0
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

;; The code below defines a file name recipe DSL to create related-files
;; jumpers.  Such a jumper should be defined as a list starting with the
;; symbol 'recipe.

;;; Code:

(require 'subr-x)
(require 'map)
(require 'related-files)


;;; Overrides of Public Methods

(cl-defmethod related-files-apply ((jumper (head recipe)) place)
  "Return a list of new places built by applying recipe JUMPER to PLACE."
  (append
   (apply #'related-files-recipe--apply-filename-jumper place (cdr jumper))
   (apply #'related-files-recipe--unapply-filename-jumper place (cdr jumper))))

(cl-defmethod related-files-get-filler ((jumper (head recipe)))
  "Return the filler of recipe JUMPER."
  (map-elt (cdr jumper) :filler))


;;; Utility Functions

(cl-defun related-files-recipe--apply-filename-jumper
    (place &key (remove-suffix "") (add-suffix "") case-transformer add-directory
           &allow-other-keys)
  "Return places built after applying some modifications to PLACE.

Modifications are applied in the order below.

REMOVE-SUFFIX is a string (e.g., \".el\") that PLACE should
end with and that is going to be removed from it.

ADD-SUFFIX is a string (e.g., \"-tests.el\") that will be
added at the end.

CASE-TRANSFORMER is one of the kind of tranformers defined by
`related-files-recipe--apply-case-transformer' and is used to change
the case of the filename.

ADD-DIRECTORY is a string (e.g., \"test\") that is added next to
directory names in PLACE."
  (when-let* (((related-files-recipe--suffix-can-be-changed-p place add-suffix remove-suffix))
              (path-without-suffix (substring place 0 (- (length remove-suffix))))
              (path-with-suffix (concat path-without-suffix add-suffix))
              (path-with-changed-case (related-files-recipe--apply-to-filename
                                       path-with-suffix
                                       (apply-partially #'related-files-recipe--apply-case-transformer case-transformer))))
    (if add-directory
        (related-files-recipe--add-directory-to-path path-with-changed-case add-directory)
      (list path-with-changed-case))))

(cl-defun related-files-recipe--unapply-filename-jumper (place &key (add-suffix "") (remove-suffix "") case-transformer add-directory &allow-other-keys)
  "Return places built after un-applying some modifications to PLACE.

The meaning of ADD-SUFFIX, REMOVE-SUFFIX, CASE-TRANSFORMER and
ADD-DIRECTORY is the opposite of the one of
`related-files-recipe--apply-filename-jumper'.  For example, ADD-SUFFIX
should already be present in PLACE and will be removed from it."
  (when-let* (((related-files-recipe--suffix-can-be-changed-p place remove-suffix add-suffix))
              (path-without-suffix (substring place 0 (- (length add-suffix))))
              (path-with-suffix (concat path-without-suffix remove-suffix))
              (path-with-changed-case (related-files-recipe--apply-to-filename
                                       path-with-suffix
                                       (apply-partially #'related-files-recipe--unapply-case-transformer case-transformer))))
    (if add-directory
        (related-files-recipe--remove-directory-from-path path-with-changed-case add-directory)
      (list path-with-changed-case))))

(defun related-files-recipe--add-directory-to-path (file add-directory)
  "Return the paths to files looking like FILE but with ADD-DIRECTORY inside it.

The file-system is searched for existing directories but the
returned paths don't have to exist."
  (cl-labels
      ((parent-directory (directory) (file-name-directory (directory-file-name directory)))
       (root-p (directory) (string= directory (parent-directory directory))))
    (cl-loop
     for current-directory = (file-name-directory file) then (parent-directory current-directory)
     for candidate = (expand-file-name
                      (substring file (length (expand-file-name current-directory)))
                      (expand-file-name add-directory current-directory))
     if (file-exists-p (file-name-directory candidate)) collect candidate into result
     if (root-p current-directory) return result)))

(defun related-files-recipe--remove-directory-from-path (file remove-directory)
  "Return the paths to files looking like FILE but with REMOVE-DIRECTORY removed.

The file-system is searched for existing directories but the
returned paths don't have to exist."
  (when-let* ((path-segments (split-string file "/"))
              (positions (related-files-recipe--seq-positions path-segments remove-directory)))
    (cl-loop
     for position in positions
     for candidate = (string-join (related-files-recipe--seq-remove-at-position path-segments position) "/")
     if (file-exists-p (file-name-directory candidate)) collect candidate)))

(defun related-files-recipe--apply-to-filename (path fn)
  "Apply FN to the filename part of PATH."
  (let* ((filename (file-name-nondirectory path))
         (directory (file-name-directory path)))
    (expand-file-name (funcall fn filename) directory)))

(defun related-files-recipe--apply-case-transformer (transformer string)
  "Return the result of applying TRANFORMER to STRING.

TRANSFORMER should be either nil, \\='capitalize or \\='uncapitalize.
If nil, this function just returns STRING."
  (cl-case transformer
    (capitalize (concat (upcase (substring string 0 1)) (substring string 1)))
    (uncapitalize (concat (downcase (substring string 0 1)) (substring string 1)))
    (t (if transformer
           (user-error "Unknown transformer %s" transformer)
         string))))

(defun related-files-recipe--unapply-case-transformer (transformer string)
  "Return the result of un-applying TRANFORMER to STRING.

TRANSFORMER should be either nil, \\='capitalize or \\='uncapitalize.
If nil, this function just returns STRING."
  (let ((untransformer (cl-case transformer
                         (capitalize 'uncapitalize)
                         (uncapitalize 'capitalize)
                         (t transformer))))
    (related-files-recipe--apply-case-transformer untransformer string)))

(defun related-files-recipe--suffix-can-be-changed-p (path add-suffix remove-suffix)
  "Return nil if REMOVE-SUFFIX cannot be replaced with ADD-SUFFIX in PATH.

The function also returns nil if ADD-SUFFIX is already present in
PATH.  This avoids adding the same suffix again.  For example,
the function returns nil if -tests.el is added to
/project/foo-tests.el to avoid getting
/project/foo-tests-tests.el as candidate."
  (and
   (string-suffix-p remove-suffix path)
   (or (not (string-suffix-p add-suffix path))
       (string-suffix-p add-suffix remove-suffix))))

;; NOTE: This is in Emacs 29 already under the name `seq-positions'
(defun related-files-recipe--seq-positions (seq elt &optional testfn)
  "Return the positions of ELT in SEQ.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (cl-loop for i from 0 below (length seq)
           if (funcall (or testfn #'equal) (nth i seq) elt) collect i))

;; NOTE: This is in Emacs 29 already under the name `seq-remove-at-position'
(defun related-files-recipe--seq-remove-at-position (seq position)
  "Return a copy of SEQ where the element at POSITION got removed."
  (append
   (cl-subseq seq 0 position)
   (cl-subseq seq (1+ position))))

(related-files-add-jumper-type
 '(cons
   :tag "Transformation recipe"
   (const recipe)
   (set
    :tag "Transformations"
    (list :inline t
          :format "%t: %v\n%d"
          :tag "Remove a string from the end of the filename, e.g., \".el\""
          (const :remove-suffix)
          (string :tag "Suffix to remove" :value ".c"))
    (list :inline t
          :format "%t: %v\n%d"
          :tag "Add a string at the end of the filename, e.g., \"-tests.el\""
          (const :add-suffix)
          (string :tag "Suffix to add" :value ".h"))
    (list :inline t
          :tag "Case transformer"
          :format "%t: %v%h\n"
          :doc "Useful when a file and its related files have names with different case"
          (const :case-transformer)
          (choice
           :value capitalize
           (const :tag "Capitalize the filename" capitalize)
           (const :tag "Uncapitalize the filename" uncapitalize)))
    (list :inline t
          :tag "String that is added next to directory names in PLACE"
          :format "%t: %v\n%h\n"
          :doc "Useful when a related file is in a parallel file hierarchy.\nFor example, with a value of \"test\", the user could jump from\n\"/project/src/lisp/calendar/parse-time.el\" to\n\"/project/src/test/lisp/calendar/parse-time.el\" and back.\nThe directory must already exist."
          (const :add-directory)
          (string :tag "Directory name to add" :value "test"))
    (list :inline t
          :tag "Filler"
          :format "%t: %v\n"
          (const :filler)
          related-files-filler))))

;;;###autoload
(add-hook 'related-files-jumper-safety-functions (lambda (jumper) (when (eq (car jumper) 'recipe) 'safe)))

(provide 'related-files-recipe)
;;; related-files-recipe.el ends here

;; LocalWords:  tranformers el
