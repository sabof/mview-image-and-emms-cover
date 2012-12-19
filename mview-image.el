;;; mview-image.el --- Fit and center an image in an emacs buffer. Responsive to window resizing. Requires ImageMagick.  -*- lexical-binding: t -*-
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

;; GLOBAL VARS

(defvar mvi-current-image-file nil)
(make-variable-buffer-local 'mvi-current-image-file)

(defvar mvi-resize-timer nil)
(make-variable-buffer-local 'mvi-resize-timer)

(defvar mvi-is-mvi-buffer nil)
(make-variable-buffer-local 'mvi-is-mvi-buffer)

(defvar mvi-last-image nil)
(make-variable-buffer-local 'mvi-last-image)

(defvar mvi-buffer-tmp-file nil)
(make-variable-buffer-local 'mvi-buffer-tmp-file)

(defvar mvi-buffer-lock nil)
(make-variable-buffer-local 'mvi-buffer-lock)

(defvar mvi-buffer-queue nil)
(make-variable-buffer-local 'mvi-buffer-queue)

;; MACROS

(defmacro mvi-with-window-or-buffer (window-or-buffer &rest rest)
  (let (( win-sym (gensym "win-"))
        ( buf-sym (gensym "buf-")))
    `(let (( ,win-sym
             (cond ( (windowp ,window-or-buffer)
                     ,window-or-buffer)
                   ( (bufferp ,window-or-buffer)
                     (first (mvi-windows-with-buffer ,window-or-buffer)))
                   ( t (selected-window))))
           ( ,buf-sym
             (cond ( (windowp ,window-or-buffer)
                     (window-buffer ,window-or-buffer))
                   ( (bufferp ,window-or-buffer)
                     ,window-or-buffer)
                   ( t (current-buffer))))
           ( inhibit-read-only t))
       (when (and (windowp ,win-sym)
                  (bufferp ,buf-sym)
                  (mvi-mvi-buffer-p ,buf-sym))
         (with-selected-window ,win-sym
           (with-current-buffer ,buf-sym
             ,@rest))))))
(put 'mvi-with-window-or-buffer 'common-lisp-indent-function
     '(2 &body))

(defmacro mvi-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))


(defmacro mvi-back-pop (symbol)
  (let ( (result (gensym)))
    `(let ( (,result (first (last ,symbol))))
       (setq ,symbol (butlast ,symbol))
       ,result)))

;; FUNCTIONS

(defun* mvi-mvi-buffer-p (&optional (buf (current-buffer)))
  ;; Maybe check active keymaps instead?
  (with-current-buffer buf mvi-is-mvi-buffer))

(defun* mvi-mvi-window-p (&optional (win (selected-window)))
  (mvi-mvi-buffer-p (window-buffer win)))

(defun* mvi-define-keys (keymap &rest bindings)
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))
(put 'mvi-define-keys 'common-lisp-indent-function
     '(4 &body))

(defun mvi-windows-with-buffer (buffer)
  (remove-if-not
   (lambda (window)
     (eq (window-buffer window) buffer))
   (loop for frame in (frame-list)
         append (window-list frame))))

(defun mvi-full-window-list ()
  (loop for frame in (remove terminal-frame (frame-list))
        nconcing (with-selected-frame frame
                   (window-list))))

(defun* mvi-buffer-has-window-p (buf)
  (some (lambda (win)
          (eq (window-buffer win) buf))
        (mvi-full-window-list)))

(defun mvi-main-area-dimensions ()
  (let (( margin 2)
        ( char-dim (mvi-character-dimensions)))
    (list (- (* (first char-dim) (window-width))
             margin)
          (- (* (second char-dim)
                (- (window-height)
                   (if header-line-format 1 0)
                   (if mode-line-format 1 0)))
             margin))))

(defun* mvi-fit-image-async (image-loc width height callback)
  (assert (file-exists-p image-loc))
  (let* (( new-file (or mvi-buffer-tmp-file
                        (setq mvi-buffer-tmp-file
                              (make-temp-file "mvi-image" nil ".png"))))
         ( args (list image-loc "-resize" (format "%sx%s>" width height)))
         ( ---
           ;; Emacs can get bitchy with transparent PNGs. There is a possibility
           ;; Emacs colors won't always being understood by ImageMagick. There
           ;; might also be some method to convert any Emacs color to hex.
           (progn
             (when (stringp (face-attribute 'default :background))
               (setq args (append
                           args (list "-compose" "over" "-background"
                                      (face-attribute 'default :background)
                                      "-flatten"))))
             (setq args (append args (list new-file)))))
         ( process
           (apply 'start-process "convert" "*Messages*" "convert"
                  args)))
    (set-process-sentinel
     process (lambda (&rest ignore)
               (funcall callback new-file)))))

(defun mvi-buffers ()
  (remove-if-not 'mvi-mvi-buffer-p (buffer-list)))

(defun mvi-mvi-window-pixel-dimensions ()
  (let ((pixel-edges (window-pixel-edges)))
    (list (- (third pixel-edges) (first pixel-edges))
          (- (fourth pixel-edges) (second pixel-edges)))))

(defun mvi-character-dimensions ()
  (let (( window-dimensions (mvi-mvi-window-pixel-dimensions)))
    (list (/ (first window-dimensions) (window-width))
          (/ (second window-dimensions) (window-height)))))

(defun mvi-image-dimensions-async (source callback)
  (assert (file-exists-p source))
  (setq source (expand-file-name source))
  (let* (( process
           (start-process "identify" "*Messages*" "identify" source))
         ( result-string ""))
    (set-process-filter
     process (lambda (process output)
               (setq result-string
                     (concat result-string output))))
    (set-process-sentinel
     process (lambda (&rest ignore)
               (setq result-string
                     (replace-regexp-in-string "^[^ ] " "" result-string))
               (string-match "\\([0-9]+\\)x\\([0-9]+\\)" result-string)
               (funcall callback (list (string-to-int (match-string 1 result-string))
                                       (string-to-int (match-string 2 result-string))))))))

(defun mvi-refresh-all ()
  (let (( mvi-buffers
          (remove-if-not
           'mvi-buffer-has-window-p
           (mvi-buffers))))
    (mapc 'mview-image-refresh mvi-buffers)))

(defun* mvi-center-insert-image-async (image &optional (margin-in-chars 1) callback)
  (assert (file-exists-p image))
  (setq image (expand-file-name image))
  (let* (( buffer (current-buffer))
         ( char-dim (mvi-character-dimensions))
         ( win-dim (mvi-main-area-dimensions))
         ( max-dim
           (mapcar* (lambda (win char) (- win (* 2 margin-in-chars char)))
                    win-dim
                    char-dim)))
    (mvi-fit-image-async
     image (first max-dim) (second max-dim)
     (lambda (file)
       (mvi-image-dimensions-async
        file (lambda (image-fitted-dim)
               (let* (( pixel-margins
                        (mapcar* (lambda (win image) (/ (- win image) 2))
                                 win-dim
                                 image-fitted-dim))
                      ( char-margins
                        (mapcar* (lambda (char mar) (/ mar char))
                                 char-dim
                                 pixel-margins)))
                 (mvi-with-window-or-buffer buffer
                   (erase-buffer)
                   (insert (make-string (second char-margins) ?\n))
                   (insert (make-string (first char-margins) ?\s))
                   (insert-image-file file)
                   ;; (delete-file file)
                   (goto-char (point-min))
                   (funcall callback)))))))))

(defun* mvi-window-configuration-change-hook (&rest ignore)
  (when (some 'mvi-mvi-window-p (window-list))
    (when (timerp mvi-resize-timer)
      (cancel-timer mvi-resize-timer))
    (setq mvi-resize-timer
          (run-with-timer 1 nil 'mvi-refresh-all))))

(defun mvi-save-data-to-temp (string-or-buffer)
  (let (( temp-file-name (make-temp-file "mvi-image-data"))
        buffer)
    (cond ( (bufferp string-or-buffer)
            (setq buffer string-or-buffer))
          ( (stringp string-or-buffer)
            (setq buffer (generate-new-buffer " mview-image-temp"))
            (with-current-buffer buffer
              (insert string-or-buffer))))
    (with-current-buffer buffer
      (write-region nil nil temp-file-name))
    (when (stringp string-or-buffer)
      (kill-buffer buffer))
    temp-file-name))

(defun mvi-get-url (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    (current-buffer)))

;; INTERFACE

(defun* mview-image-set-image (image &optional window-or-buffer)
  "Set the image of WINDOW-OR-BUFFER. Clear it if the IMAGE is nil."
  (assert (or (null image) (file-exists-p image)))
  (mvi-with-window-or-buffer window-or-buffer
    (when mvi-buffer-lock
      (when mvi-buffer-queue
        (setq mvi-buffer-queue
              (remove image mvi-buffer-queue)))
      (push image mvi-buffer-queue)
      (return-from mview-image-set-image))
    (setq mvi-buffer-lock t)
    (when (null image)
      (progn ; Erase
        (erase-buffer)
        (setq mvi-current-image-file nil))
      (progn ; Handle queue
        (setq mvi-buffer-lock nil)
        (when mvi-buffer-queue
          (mview-image-set-image
           (mvi-back-pop mvi-buffer-queue))))
      (return-from mview-image-set-image))
    (setq mvi-current-image-file image)
    (setq default-directory (file-name-directory image))
    (mvi-center-insert-image-async
     image 1 (lambda ()
               (mvi-with-window-or-buffer window-or-buffer
                 (setq mvi-buffer-lock nil)
                 (when mvi-buffer-queue
                   (mview-image-set-image
                    (mvi-back-pop mvi-buffer-queue))))))))

(defun* mview-image-set-image-from-data (string-or-buffer &optional window-or-buffer)
  (let ((temp (mvi-save-data-to-temp  string-or-buffer)))
    (mview-image-set-image temp window-or-buffer)
    (delete-file temp)))

(defun* mview-image-set-image-from-url (url &optional window-or-buffer)
  (let ((url-buffer (mvi-get-url url)))
    (mview-image-set-image-from-data  url-buffer  window-or-buffer)
    (kill-buffer url-buffer)))

(defun mview-image-clear (&optional window-or-buffer)
  (mview-image-set-image nil window-or-buffer))

(defun mview-image-refresh (&optional window-or-buffer)
  (mvi-with-window-or-buffer window-or-buffer
    (when (and (stringp mvi-current-image-file)
               (file-exists-p mvi-current-image-file))
      (mview-image-set-image mvi-current-image-file window-or-buffer))))

(define-derived-mode mview-image-mode fundamental-mode
  "MView Image Mode"
  "Fit and center an image in an emacs buffer. Responsive to window resizing.
Requires ImageMagick."
  (add-hook 'post-command-hook
            (lambda (&rest ignore)
              (deactivate-mark)
              (goto-char (point-min)))
            t t)
  (add-hook 'window-configuration-change-hook
            'mvi-window-configuration-change-hook)
  (setq revert-buffer-function
        (lambda (&rest ignore)
          (mview-image-refresh)))
  (mvi-define-keys mview-image-mode-map
    [remap smooth-scroll-up] 'ignore
    [remap smooth-scroll-down] 'ignore
    [remap cua-scroll-up] 'ignore
    [remap cua-scroll-down] 'ignore
    [remap scroll-up-command] 'ignore
    [remap scroll-down-command] 'ignore
    (kbd "<down-mouse-1>") 'ignore
    (kbd "<drag-mouse-1>") 'ignore
    (kbd "<mouse-5>") 'ignore
    (kbd "<mouse-4>") 'ignore
    (kbd "<next>") 'ignore
    (kbd "<prior>") 'ignore
    (kbd "<vertical-scroll-bar>") 'ignore
    (kbd "C-v") 'ignore
    (kbd "M-v") 'ignore
    (kbd "g") 'revert-buffer)
  (setq cursor-type nil
        buffer-read-only t
        mvi-is-mvi-buffer t
        indicate-empty-lines nil))

(defun mview-image-switch-to-url (url)
  "Pop to a new buffer showing the image from URL"
  (interactive (list (read-file-name "View url: ")))
  (switch-to-buffer (generate-new-buffer "MView Image"))
  (mview-image-mode)
  (mview-image-set-image-from-url url))

(defun mview-image-pop-to-image (filename)
  "Pop to a new buffer showing the image at FILENAME."
  (interactive (list (read-file-name "View image: ")))
  (pop-to-buffer (generate-new-buffer "MView Image"))
  (mview-image-mode)
  (mview-image-set-image filename))

(defun mview-image-switch-to-image (filename)
  "Switch to a new buffer showing the image at FILENAME."
  (interactive (list (read-file-name "View image: ")))
  (switch-to-buffer (generate-new-buffer "MView Image"))
  (mview-image-mode)
  (mview-image-set-image filename))

(provide 'mview-image)

;;; mview-image.el ends here