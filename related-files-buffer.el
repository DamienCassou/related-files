;;; related-files-buffer.el --- Add support for buffer-based places to related-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@undertown>
;; Package-Requires: ((emacs "28.2"))
;; Version: 1.0
;; Keywords: convenience
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

;; Provide buffers as a type of place for `related-files.el'.  A
;; buffer-based place is the buffer.

;;; Code:

(require 'cl-lib)



(cl-defmethod related-files-place-exists-p ((place buffer))
  "Call `buffer-live-p' on PLACE."
  (buffer-live-p place))

(cl-defmethod related-files-goto-place ((place buffer))
  "Call `switch-to-buffer' on PLACE."
  (switch-to-buffer place))

(defvar-local related-files-buffer--jumper nil
  "Remember which jumper was used to reach the current buffer.")

(cl-defmethod related-files-attach-jumper-to-place (jumper (place buffer))
  "Set `related-files-jumper' to JUMPER, locally in PLACE."
  (with-current-buffer place
    (setq-local related-files-buffer--jumper jumper))
  place)

(cl-defmethod related-files-retrieve-jumper-from-place ((place buffer))
  "Get the value of `related-files-jumper' in PLACE."
  (with-current-buffer place
    related-files-buffer--jumper))

(cl-defmethod related-files-format-place (_initial-places (place buffer) &optional _annotate)
  "Call `buffer-name' on PLACE.

Ignore INITIAL-PLACES and ANNOTATE."
  (buffer-name place))

(provide 'related-files-buffer)
;;; related-files-buffer.el ends here
