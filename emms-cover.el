;;; emms-cover.el --- Dispay the cover of the currently playing album in EMMS. Based on mview-image.
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/mview-image-and-emms-cover

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The project is hosted at https://github.com/sabof/mview-image-and-emms-cover
;; The latest version, and all the relevant information can be found there.

;;; Code:

(require 'cl)
(require 'mview-image)

(defvar emms-cover-nocover-image nil)

(defun emms-cover-buffers-with-mode (mode)
  (remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (eq major-mode mode)))
   (buffer-list)))

(defun* emms-cover-path ()
  (unless emms-player-playing-p
    (return-from emms-cover-path))
  (let* (( track (emms-playlist-current-selected-track))
         ( track-name (cdr (assq 'name (rest track))))
         ( folder (file-name-directory track-name)))
    (expand-file-name
     (or (first (directory-files
                 folder t "folder\\..\\{2,4\\}$"))
         (first (directory-files
                 folder t
                 (mapconcat
                  'identity
                  (mapcar
                   'extension-to-regex
                   image-extensions)
                  "\\|")))
         emms-cover-nocover-image))))

(defun* emms-cover-refresh (&rest ignore)
  (let* (( cover-buffer
           (or (first (emms-cover-buffers-with-mode 'emms-cover-mode))
               (return-from emms-cover-refresh)))
         ( cover-path
           (or (emms-cover-path)
               (progn
                 (mview-image-clear cover-buffer)
                 (return-from emms-cover-refresh)))))
    (mview-image-set-image cover-path cover-buffer)))

(define-derived-mode emms-cover-mode mview-image-mode
  "EMMS Cover Mode"
  "Dispay the cover of the currently playing album in EMMS. Based on
mview-image."
  (add-hook 'emms-player-finished-hook 'emms-cover-refresh)
  (add-hook 'emms-player-paused-hook 'emms-cover-refresh)
  (add-hook 'emms-player-started-hook 'emms-cover-refresh)
  (add-hook 'emms-player-stopped-hook 'emms-cover-refresh)
  (setq revert-buffer-function 'emms-cover-refresh))

(defun emms-cover-pop-to ()
  "Pop to the emms-cover buffer."
  (interactive)
  (pop-to-buffer "*EMMS Cover*")
  (emms-cover-mode)
  (emms-cover-refresh))

(defalias 'emms-cover-show 'emms-cover-pop-to)

(defun emms-cover-switch-to ()
  "Switch to the emms-cover buffer."
  (interactive)
  (switch-to-buffer "*EMMS Cover*")
  (emms-cover-mode)
  (emms-cover-refresh))

(provide 'emms-cover)

;;; emms-cover.el ends here