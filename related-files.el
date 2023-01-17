;;; related-files.el --- Easily find files related to the current one  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.2"))
;; Created: 25 Sep 2022
;; URL: https://www.gnu.org/software/emacs/

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: tools

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

;; Thousands times a day you want to jump from a file to its test file
;; (or to its CSS file, or to its header file, or any other related
;; file) and just as many times you want to go back to the initial
;; file.  Jumping to related files is what this package is about.

;; The question is: how does a user specify that a file is related to
;; a set of other files? One way is to create a function that takes a
;; file as argument and returns a list of related filenames:
;;
;; (defun my/related-files-jumper (file)
;;   (let ((without-ext (file-name-sans-extension file)))
;;     (list
;;      (concat without-ext ".js")
;;      (concat without-ext ".css"))))
;;
;; (setq related-files-jumpers (list #'my/related-files-jumper))
;;
;; `my/related-files-jumper' is called a 'jumper.  With this setup,
;; `related-files-jump' will let the user jump from Foo.js to Foo.css and
;; back.
;;
;; This is working good but has several limitations:
;;
;; 1. If Foo.css is not in the same directory as Foo.js or if you want
;; to include test files which end with "-tests.js",
;; `my/related-files-jumper' has to be modified in a non-obvious way or a
;; complicated new jumper must be written and added to
;; `related-files-jumpers';
;;
;; 2. The function `my/related-files-jumper' has to be shared with all Emacs
;; users working on the same project

;; So related-files recommends another approach that is less powerful but
;; much simpler.  Here is another way to define the same jumper:
;;
;; (recipe :remove-suffix ".js" :add-suffix ".css")
;;
;; This list must replace `my/related-files-jumper' in
;; `related-files-jumpers'.  This jumper lets the user go from Foo.js
;; to Foo.css.  related-files will automatically inverse the meaning
;; of :remove-suffix and :add-suffix arguments so the user can also go
;; from Foo.css to Foo.js with this jumper.  See
;; `related-files-jumpers' and THE MANUAL (TODO) for more information.
;;
;; This kind of jumper can easily be shared with the members of a team
;; through a .dir-locals.el file.  See (info "(Emacs) Directory Variables").
;;
;; `related-files-make' also makes it easy to create a related file and fill
;; it with some content.  If the content is always the same, a string
;; can be used to specify it:
;;
;; (recipe :remove-suffix ".js" :add-suffix ".css" :filler "Fill the CSS file")
;;
;; There is also an `auto-insert'-based way to fill new files and new
;; kinds of fillers can easily be implemented.  See the manual for
;; more information.

;; If you want to add a new kind of jump, override `related-files-apply' and
;; optionally `related-files-get-filler', call `related-files-add-jumper-type' and
;; add a function to `related-files-jumper-safety-functions'.
;;
;; If you want to add a new kind of filler, override `related-files-fill'
;; and call `related-files-add-filler-type'.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'map)


;;; Customization Options

(defgroup related-files nil
  "Facilitate navigation between the current file and related files."
  :group 'tools)

(define-widget 'related-files-jumper 'lazy
  "A description of how two files relate to each other."
  :tag "Jumper"
  :type '(choice))

(define-widget 'related-files-filler 'lazy
  "A description of how to fill a new file."
  :format "%v"
  :type '(choice :value ""))

;;;###autoload
(defvar related-files-jumper-safety-functions nil
  "Functions checking if a given jumper is safe or not.

Each function should take a jumper as argument and should return
either nil, \\='safe or \\='unsafe.  Nil must be returned if the
function doesn't know if the jumper is safe.

The first function returning non-nil will determine the safety of
the jumper and other functions won't be executed.")

;;;###autoload (put 'related-files-jumpers 'safe-local-variable (lambda (jumpers) (seq-every-p (lambda (jumper) (eq 'safe (run-hook-with-args-until-success 'related-files-jumper-safety-functions jumper))) jumpers)))
(defcustom related-files-jumpers nil
  "List of jumpers to consider to go from the current file to related files.

There are different kinds of jumpers:

- A jumper can be a function.  In this case, the function should
accept the current place as argument (a filename) and should
return a (possibly-empty) list of (existing and non-existing)
places the user might want to go to or create from the current
place.  Instead of returning a list, the jumper may also just
return a place.

- A jumper can also be a list (recipe [:remove-suffix
REMOVE-SUFFIX] [:add-suffix ADD-SUFFIX] [:add-directory
ADD-DIRECTORY] [:case-transformer TRANSFORMATION]).  Such a
jumper defines transformations to apply to the current file name
to get related file names.  A :filler keyword can also be added
to the list to specify how to create a missing file.  Such a
jumper has the advantage that is works both ways: you can go from
a file to its related files but also from any related file to the
initial file and other related files.

Other kinds of jumpers can be created by writing Emacs Lisp.
Defining a new kind of jumper requires overriding
`related-files-apply' and optionally `related-files-get-filler'.
It also requires calling `related-files-add-jumper-type' and
adding a function to `related-files-jumper-safety-functions'.

Get more information about jumper types defined above, new
jumpers and fillers through the customization interface and THE
MANUAL (TODO)."
  :type '(repeat :tag "Jumpers" related-files-jumper)
  :safe (lambda (jumpers) (seq-every-p (lambda (jumper) (eq 'safe (run-hook-with-args-until-success 'related-files-jumper-safety-functions jumper))) jumpers)))

;;;###autoload
(defcustom related-files-current-place-finders '(buffer-file-name
						 current-buffer)
  "List of functions returning the current place.

Each function should return the current place in a different
form, or nil if called when the function is not applicable to the
current context (e.g., the function `buffer-file-name' returns
nil when the current buffer is not associated with a file)."
  :group 'related-files
  :type '(repeat function))

(defcustom related-files-place-category-tests
  '(((pred stringp) 'file)
    ((pred bufferp) 'buffer))
  "List of tests for getting type of a place.

This variable is passed as an argument list to `pcase', so must
take the relevant form.  The return value of each clause must be a
symbol for a category.  See info node `(elisp)Programmed
Completion' for more information.  The customization type is an
approximation of this."
  :type '(repeat (list (choice symbol integer string cons sexp)
		       symbol))
  :group 'related-files)


;;; Public Functions

;;;###autoload
(defun related-files-jump (&optional jumpers current-places)
  "Let the user choose where to go from the current place by asking JUMPERS.

Each element of JUMPERS is asked for a list of candidates and the
resulting lists are concatenated with duplicates removed.  The
resulting list of candidates is shown to the user so one can be
selected.  If the resulting list is empty, the user will get an
error message with some ideas on what to configure to get
candidates.  If the resulting list contains only one item, this
item is automatically selected.

Only existing places are presented to the user.  Look at
`related-files-make' and `related-files-jump-or-make' if you also
want to be able to create new places.

If JUMPERS is not provided, use `related-files-jumpers'.  If
CURRENT-PLACES is not provided, try different places from
`related-files-current-place-finders'.

Interactively, a numeric prefix argument selects the jumper at
the specified position (zero-based index) in
`related-files-jumpers'."
  (interactive (list (when (numberp current-prefix-arg)
                       (list (seq-elt related-files-jumpers current-prefix-arg)))))
  
  (related-files--jump-or-make jumpers current-places :include-existing-places t))

;;;###autoload
(defun related-files-make (&optional jumpers current-places)
  "Let the user choose where to go from the current place by asking JUMPERS.

Each element of JUMPERS is asked for a list of candidates and the
resulting lists are concatenated with duplicates removed.  The
resulting list of candidates is shown to the user so one can be
selected.  If the resulting list is empty, the user will get an
error message with some ideas on what to configure to get
candidates.  If the resulting list contains only one item, this
item is automatically selected.

Only non-existing places are presented to the user so the user
can easily create them.  This is useful to create a test file for
the current file for example.  Look at `related-files-jump' and
`related-files-jump-or-make' if you also want to be able to jump
to existing places.

If JUMPERS is not provided, use `related-files-jumpers'.  If
CURRENT-PLACES is not provided, use
`related-files-current-place-finders'.

Interactively, a numeric prefix argument selects the jumper at
the specified position (zero-based index) in `related-files-jumpers'."
  (interactive (list (when (numberp current-prefix-arg)
                       (list (seq-elt related-files-jumpers current-prefix-arg)))))
  (related-files--jump-or-make jumpers current-places :include-non-existing-places t))

;;;###autoload
(defun related-files-jump-or-make (&optional jumpers current-places)
  "Let the user choose where to go from the current place by asking JUMPERS.

Each element of JUMPERS is asked for a list of candidates and the
resulting lists are concatenated with duplicates removed.  The
resulting list of candidates is shown to the user so one can be
selected.  If the resulting list is empty, the user will get an
error message with some ideas on what to configure to get
candidates.  If the resulting list contains only one item, this
item is automatically selected.

Both existing and non-existing places are presented to the user
so the user can easily jump to existing files or create missing
ones.  Look at `related-files-jump' and `related-files-make' if
you don't want to mix existing and non-existing files in the same
list.

If JUMPERS is not provided, use `related-files-jumpers'.  If
CURRENT-PLACES is not provided, use
`related-files-current-place-finders'.

Interactively, a numeric prefix argument selects the jumper at
the specified position (zero-based index) in `related-files-jumpers'."
  (interactive (list (when (numberp current-prefix-arg)
                       (list (seq-elt related-files-jumpers current-prefix-arg)))))
  (related-files--jump-or-make jumpers current-places
                         :include-existing-places t
                         :include-non-existing-places t))


;;; Jumpers and Places Public API

(cl-defgeneric related-files-apply (_jumper _place)
  "Apply JUMPER to PLACE and return related places or nil.

PLACE can be anything supported by the methods defined for the
generic API.  By default methods are provided for
filenames (strings).  The result must be a possibly-empty list of
filenames.

The generic implementation just returns nil.  This ensures that
calling a jumper/place pair for which there is no appropriately
typed method does not break anything -- it just doesn't return
any places."
  nil)

(cl-defmethod related-files-apply ((jumper symbol) place)
"Call JUMPER on PLACE."
  (funcall jumper place))

(cl-defgeneric related-files-get-filler (jumper)
  "Return a filler associated with JUMPER.")

(cl-defmethod related-files-get-filler ((jumper symbol))
  "`get' JUMPER's property `related-files-filler'."
  (get jumper 'related-files-filler))

(cl-defgeneric related-files-place-exists-p (_place)
  "Return non-nil if PLACE exists.

The default implementation returns nil.  This means that related-
files assumes any place without an appropriate method does not
exist."
  nil)

(cl-defgeneric related-files-goto-place (place)
  "Go to an existing PLACE.")

(cl-defgeneric related-files-attach-jumper-to-place (jumper place)
  "Attach JUMPER to PLACE.

Return the modified version of PLACE.")

(cl-defgeneric related-files-retrieve-jumper-from-place (place)
  "Retrieve jumper attached to PLACE.")

(cl-defgeneric related-files-format-place (_initial-places place &optional _annotate)
  "Format PLACE, relative to INITIAL-PLACES.

If ANNOTATE is non-nil, it is ok for this function to return an
`annotated' or `decorated' version of PLACE (with extra
information).  If ANNOTATE is nil,a `bare' version must be
returned.

The default implementation ignores INITIAL-PLACES and ANNOTATE,
and prints PLACE with `format'."
  (format "%s" place))


;;; Filler Public API

(cl-defgeneric related-files-fill (filler &allow-other-keys &rest)
  "Use FILLER to fill the current buffer with some content.

The current buffer is empty when this function is called.

Beyond the filler, this function is called with the :jumper and
:place keyword arguments.")


;;; Functions Manipulating Places

(defun related-files--get-current-places ()
  "Return a list of different forms of the current place."
  (delete-dups
   (seq-remove
    #'null
    (mapcar #'funcall related-files-current-place-finders))))

(defun related-files--choose-place (places initial-places)
  "Let the user pick one of PLACES and return it.

PLACES is a list of filenames and INITIAL-PLACES is a list of
places.

INITIAL-PLACE is the place that was current when the user started
related-files.  It is used to format each place in PLACES."
  (cond
   ((length= places 0) (user-error "No place to go to.  Consider configuring `related-files-jumpers' or using `related-files-make'") nil)
   ((length= places 1) (car places))
   (t (related-files--completing-read "Place: " places (apply-partially #'related-files--format-place initial-places)))))

(defun related-files--act-on-place (place)
  "Either open or create PLACE, a filename."
  (if (related-files-place-exists-p place)
      (related-files-goto-place place)
    (related-files--make-place place)))

(defun related-files--get-place-category (place)
  "Get completion category for PLACE.

Use `pcase' to check PLACE against the patterns in
`related-files-place-category-tests'.  If none succeed, return
`type-of' PLACE."
  (eval
   `(pcase ,place
      ,@related-files-place-category-tests
      (_ (type-of ,place)))))

(defun related-files--format-place (initial-places place &optional annotate)
  "Return a string representing PLACE.

INITIAL-PLACES is a list different objects, each representing the
initial place in a different way (e.g. a filename, a buffer
object, etc.). It is used to format PLACE relatively.

If PLACE doesn't exist (as determined by
`related-files-place-exists-p'), append \"(create it!)\" to the
return value."
  (when-let* ((name (related-files-format-place initial-places place annotate)))
    (if (or (not annotate) (related-files-place-exists-p place))
        name
      (format "%s (create it!)" name))))

(defun related-files--make-place (place)
  "Create the file at PLACE.

If a jumper is attached to PLACE and if this jumper has a filler,
use the filler to populate the new file with initial content."
  (related-files-goto-place place)
  (when-let* ((jumper (related-files-retrieve-jumper-from-place place))
              (filler (related-files-get-filler jumper)))
    (related-files-fill filler :jumper jumper :place place)))


;;; Fillers

(cl-defmethod related-files-fill ((filler string) &allow-other-keys &rest)
  "Fill the current buffer with FILLER, a string."
  (insert filler))

(cl-defmethod related-files-fill ((_filler (eql auto-insert)) &allow-other-keys &rest)
  "Fill the current buffer by calling `auto-insert'."
  (auto-insert))

(declare-function yas-expand-snippet "ext:yasnippet.el")
(declare-function yas-lookup-snippet "ext:yasnippet.el")

(cl-defmethod related-files-fill ((filler (head yasnippet)) &allow-other-keys &rest)
  "Fill the current buffer with yasnippet-based FILLER."
  (when-let* (((require 'yasnippet nil t))
              (snippet (map-elt (cdr filler) :name)))
    (yas-expand-snippet (yas-lookup-snippet snippet major-mode))))


;;; Utility Functions

(cl-defun related-files--jump-or-make (jumpers current-places &key include-existing-places include-non-existing-places)
  "Let the user choose where to go from the current-place by asking JUMPERS.

Existing files are presented to the user if
INCLUDE-EXISTING-PLACES is non-nil.  Non-existing files are
presented to the user if INCLUDE-NON-EXISTING-PLACES is non-nil.

If JUMPERS is not provided, use `related-files-jumpers'.  If
CURRENT-PLACES is not provided, use `related-files-current-place-finders'."
  (let* ((jumpers (or jumpers related-files-jumpers))
         (current-places (or current-places (related-files--get-current-places))))
    (cond ((not jumpers)
           (user-error "No jumpers.  Consider configuring `related-files-jumpers'"))
          ((not current-places)
           (user-error "Related-Files only works from file-based buffers"))
          (t
           (let ((existing-places (when include-existing-places
                                    (related-files--collect-existing-places jumpers current-places)))
                 (non-existing-places (when include-non-existing-places
                                        (related-files--collect-non-existing-places jumpers current-places))))
             (when-let* ((place (related-files--choose-place (append existing-places non-existing-places) current-places)))
               (related-files--act-on-place place)))))))

(defun related-files--collect-existing-places (jumpers current-places)
  "Return a list of places that can accessible from the current place with JUMPERS.

Each jumper in JUMPERS is not only called with each element of
CURRENT-PLACES as argument but also with all places generated by
other jumpers, recursively.  Only existing places are considered
and returned.

The returned value doesn't contain CURRENT-PLACE."
  (when current-places
    (let* ((places-result nil)
           (places-tried nil)
           (places-queue (copy-sequence current-places)))
      (while places-queue
        (when-let* ((place (pop places-queue))
		    ((related-files-place-exists-p place))
                    ((not (seq-contains-p places-tried place))))
          (unless (member place current-places) (push place places-result))
          (let ((new-places (related-files--call-jumpers jumpers `(,place))))
            (push place places-tried)
            (setq places-queue (nconc places-queue new-places)))))
      places-result)))

(defun related-files--collect-non-existing-places (jumpers current-places)
  "Return a list of places accessible from the current place with JUMPERS.

Only non-existing places are considered and returned.  The
returned value doesn't contain any places in CURRENT-PLACES."
  (cl-delete-if
   (lambda (place) (or (member place current-places)
                       (related-files-place-exists-p place)))
   (related-files--call-jumpers jumpers current-places)))

(defun related-files--call-jumpers (jumpers places)
  "Return a list of places that can be accessed from PLACES with JUMPERS."
  (mapcan (lambda (place)
	    (mapcan
	     (apply-partially #'related-files--call-jumper place)
	     jumpers))
	  places))

(defun related-files--call-jumper (place jumper)
  "Return a list of places that can be accessed from PLACE with JUMPER."
  (when-let* ((place-or-places (related-files-apply jumper place))
              (places (if (proper-list-p place-or-places)
                          place-or-places
                        (list place-or-places))))
    (related-files--attach-jumper-to-places jumper places)))

(defun related-files--attach-jumper-to-places (jumper places)
  "Return PLACES with JUMPER attached to each.

Each item of the return value remembers it was created with
JUMPER."
  (mapcar (apply-partially #'related-files-attach-jumper-to-place jumper) places))

(defun related-files--completing-read (prompt entities formatter)
  "Display PROMPT and let the user choose one of ENTITIES in the minibuffer.

FORMATTER is a function for formating ENTITIES.  It should return
a string for presentation to the user.  It takes one entity, and
one optional argument ANNOTATE.  If ANNOTATE is non-nil, it is
acceptable (though not necessary) to return a string with
annotations or `decorations'.  It ANNOTATE is nil, only a `bare'
version should be returned.

Before being presented to the user, each entity is formatted by
FORMATTER with ANNOTATE set to t, then the result has a property
`multi-category' attached.  The value of this property is a cons
cell: its car is the category of the entity (as determined by
`related-files--get-place-category') and its cdr is the string
returned by FORMATTER when ANNOTATE is nil."
  (let* ((entity-string-to-entity (make-hash-table :test 'equal :size (length entities)))
	 (format-function
	  (lambda (entity) (propertize
		       (funcall formatter entity 'annotate)
		       'multi-category
		       (cons (related-files--get-place-category entity)
			     (funcall formatter entity)))))
         (entity-strings (mapcar format-function entities)))
    (cl-loop
     for entity in entities
     for entity-string in entity-strings
     do (puthash entity-string entity entity-string-to-entity))
    (when-let* ((entity-table (lambda (str pred flag)
				(pcase flag
				  ('nil (try-completion str entity-strings pred))
				  ('t (all-completions str entity-strings pred))
				  ('lambda (test-completion str entity-strings pred))
				  ((and (pred consp)
					(app car 'boundaries))
				   `(boundaries 0 . ,(length (cdr flag))))
				  ('metadata
				   `(metadata (category . multi-category))))))
		(entity-string (completing-read prompt entity-table nil t)))
      (gethash entity-string entity-string-to-entity))))

(defun related-files-add-jumper-type (customization-type)
  "Add CUSTOMIZATION-TYPE choice to `related-files-jumper' widget.

This function should be called when creating a new kind of jumper
to add an alternative customization type to the `customize'
interface of `related-files-jumpers'.

CUSTOMIZATION-TYPE describes what the new kind of jumper should
look like and should contain the same kind of data as the :type
argument of `defcustom'.  See Info node `(elisp) Customization
Types' for more information."
  (related-files--add-choice-to-type 'related-files-jumper customization-type))

(defun related-files-add-filler-type (customization-type)
  "Add CUSTOMIZATION-TYPE choice to `related-files-filler' widget.

This function should be called when creating a new kind of filler
to add an alternative customization type to the `customize'
interface of `related-files-jumpers'.

CUSTOMIZATION-TYPE describes what the new kind of filler should
look like and should contain the same kind of data as the :type
argument of `defcustom'.  See Info node `(elisp) Customization
Types' for more information."
  (related-files--add-choice-to-type 'related-files-filler customization-type))

(defun related-files--add-choice-to-type (widget-symbol customization-type)
  "Add CUSTOMIZATION-TYPE to the choice type of WIDGET-SYMBOL.

CUSTOMIZATION-TYPE is only added if absent from the type
alternatives."
  (when-let* ((widget (get widget-symbol 'widget-type))
              (choice (widget-get widget :type))
              ((not (seq-contains-p (cdr choice) customization-type))))
    (widget-put widget :type `(,@choice ,customization-type))))

(related-files-add-jumper-type
 '(function
   :format "%t: %v\n%h\n"
   :doc "Should accept a place as argument and return a list of related places."))

(related-files-add-filler-type '(string :tag "Fill with pre-defined content" :value "Replace me with a better default"))
(related-files-add-filler-type '(const :tag "Use `auto-insert'" auto-insert))

(provide 'related-files)
;;; related-files.el ends here

;; LocalWords:  minibuffer related-files
