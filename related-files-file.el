;;; related-files-file.el --- Add support for file-based places to related-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@undertown>
;; Package-Requires: ((emacs "28.2"))
;; Version: 1.0
;; Keywords: files
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

;; Provide methods for the `related-files' public API for PLACEs as
;; files.  A file-based place is a string containing the path to the file.

;;; Code:

(require 'cl-lib)



;;;###autoload
(cl-defmethod related-files-place-exists-p ((place string))
  "Call `file-exists-p' on PLACE."
  (file-exists-p place))

;;;###autoload
(cl-defmethod related-files-goto-place ((place string))
  "Call `find-file' on PLACE."
  (find-file place))

;;;###autoload
(cl-defmethod related-files-attach-jumper-to-place (jumper (place string))
  "Set PLACE :related-files-jumper property to JUMPER."
  (propertize place :related-files-jumper jumper))

;;;###autoload
(cl-defmethod related-files-retrieve-jumper-from-place ((place string))
  "Get value of :related-files-jumper text property in PLACE."
  (get-text-property 0 :related-files-jumper place))

;;;###autoload
(cl-defmethod related-files-format-place (initial-places (place string) &optional _annotate)
  "Format PLACE relative to the first string in INITIAL-PLACES.

If INITIAL-PLACES contains no strings, just return PLACE."
  (if-let ((initial-file (seq-find #'stringp initial-places))
	   (initial-directory (file-name-directory initial-file)))
      (file-relative-name place initial-directory)
    place))

(provide 'related-files-file)
;;; related-files-file.el ends here
