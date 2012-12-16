;;; mview-image.el --- Fit and center an image in an emacs buffer. Responsive to window resizing. Requires ImageMagick.

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'cl))

(defvar mvi-fit-image-caching-hash
  (make-hash-table
   :test 'equal))

(defvar mvi-current-image-file nil)
(make-variable-buffer-local 'mvi-current-image-file)

(defvar mvi-resize-timer nil)
(make-variable-buffer-local 'mvi-resize-timer)

(defvar mvi-is-mvi-buffer nil)
(make-variable-buffer-local 'mvi-is-mvi-buffer)

(defvar mvi-last-image nil)
(make-variable-buffer-local 'mvi-last-image)

;; ----------------------------------------------------------------------------

(defun* mvi-buffer-p (&optional (buf (current-buffer)))
  ;; Maybe check active keymaps instead?
  (with-current-buffer buf mvi-is-mvi-buffer))

(defun* mvi-window-p (&optional (win (selected-window)))
  (mvi-buffer-p (window-buffer win)))

(defun* mvi-define-keys (keymap &rest bindings)
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))
(put 'mvi-define-keys 'common-lisp-indent-function
     '(4 &body))

(defmacro mvi-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))

(defun mvi-windows-with-buffer (buffer)
  (remove-if-not
   (lambda (window)
     (eq (window-buffer window) buffer))
   (loop for frame in (frame-list)
         append (window-list frame))))

(defmacro mvi-with-window-or-buffer (window-or-buffer &rest rest)
  (let ((win-sym (gensym "win-"))
        (buf-sym (gensym "buf-")))
    `(let (( ,win-sym (cond ( (windowp ,window-or-buffer)
                              ,window-or-buffer)
                            ( (bufferp ,window-or-buffer)
                              (first (mvi-windows-with-buffer ,window-or-buffer)))
                            ( t (selected-window))))
           ( ,buf-sym (cond ( (windowp ,window-or-buffer)
                              (window-buffer ,window-or-buffer))
                            ( (bufferp ,window-or-buffer)
                              ,window-or-buffer)
                            ( t (current-buffer)))))
       (when (and (windowp ,win-sym)
                  (bufferp ,buf-sym)
                  (mvi-buffer-p ,buf-sym))
         (with-selected-window ,win-sym
           (with-current-buffer ,buf-sym
             ,@rest))))))
(put 'mvi-with-window-or-buffer 'common-lisp-indent-function
     '(2 &body))


(defun mvi-full-window-list ()
  (loop for frame in (remove terminal-frame (frame-list))
        nconcing (with-selected-frame frame
                   (window-list))))


(defun* mvi-fit-image (image-loc &optional width height)
  "Should be renamed/split into mvi-fit-image and mvi-fit-image to window"
  (assert (file-exists-p image-loc))
  (let* (( file (make-temp-file "emacs-image" nil ".png"))
         ( dimensions (mvi-writable-window-dimensions))
         ( command (progn
                     (when width
                       (setf (first dimensions)
                             width))
                     (when height
                       (setf (second dimensions)
                             height))
                     (format "convert %s -resize %sx%s\\> %s"
                             (shell-quote-argument image-loc)
                             (first dimensions)
                             (second dimensions)
                             file))))
    (mvi-silence-messages
     (shell-command command))
    file))

(defun mvi-fit-image-caching (image-loc width height)
  (let (( hash-val (gethash
                    (list image-loc width height)
                    mvi-fit-image-caching-hash)))
    (when (and hash-val (file-exists-p hash-val))
      (return-from mvi-fit-image hash-val)))
  (puthash (list image-loc width height)
           (mvi-fit-image image-loc width height)
           mvi-fit-image-caching-hash))

(defun mvi-buffers ()
  (remove-if-not 'mvi-buffer-p (buffer-list)))

(defun mvi-window-pixel-dimensions ()
  (let ((pixel-edges (window-pixel-edges)))
    (list (- (third pixel-edges)
             (first pixel-edges)
             )
          (- (fourth pixel-edges)
             (second pixel-edges)
             ))))

(defun mvi-character-dimensions ()
  (let ((window-dimensions (mvi-window-pixel-dimensions)))
    (list (/ (first window-dimensions) (window-width))
          (/ (second window-dimensions) (window-height)))))

(defun mvi-window-characters-dimensions ()
  (let ((window-dimensions (mvi-window-pixel-dimensions)))
    (list (/ (first window-dimensions) (window-width))
          (/ (second window-dimensions) (window-height)))))

(defun mvi-writable-window-dimensions ()
  (let (( margin 2)
        ( char-dim (mvi-character-dimensions)))
    (list (- (* (first char-dim)
                (window-width))
             margin)
          (- (* (second char-dim)
                (- (window-height)
                   (if header-line-format
                       1 0)
                   (if mode-line-format
                       1 0)))
             margin))))

(defun mvi-image-dimensions (source)
  (assert (file-exists-p source))
  (flet (( message (&rest ignore)))
    (let* ((command (concat "identify " (shell-quote-argument source)))
           (command-result (substring (shell-command-to-string command)
                                      (length source))))
      (save-match-data
        (string-match "\\([0-9]+\\)x\\([0-9]+\\)" command-result)
        (list (string-to-int (match-string 1 command-result))
              (string-to-int (match-string 2 command-result)))))))

(defun* mvi-center-insert-image
    (image
     &optional
     (margin-in-chars 1)
     use-cache)
  (setq image (expand-file-name image))
  (assert (file-exists-p image))
  (let* (( char-dim (mvi-window-characters-dimensions))
         ( win-dim (mvi-writable-window-dimensions))
         ( max-dim
           (mapcar* (lambda (win char) (- win (* 2 margin-in-chars char)))
                    win-dim
                    char-dim))
         ( image-fitted
           (funcall (if use-cache
                        'mvi-fit-image
                        'mvi-fit-image-caching)
                    image
                    (first max-dim)
                    (second max-dim)))
         ( image-fitted-dim (mvi-image-dimensions image-fitted))
         ( pixel-margins
           (mapcar* (lambda (win image) (/ (- win image) 2))
                    win-dim
                    image-fitted-dim))
         ( char-margins
           (mapcar*
            (lambda (char mar) (/ mar char))
            char-dim
            pixel-margins)))
    (erase-buffer)
    (insert (make-string (second char-margins) ?\n))
    (insert (make-string (first char-margins) ?\s))
    (insert-image-file image-fitted)
    (goto-char (point-min))))

(defun* mvi-buffer-has-window-p (buf)
  (some (lambda (win)
          (eq (window-buffer win) buf))
        (mvi-full-window-list)))

(defun mvi-refresh-all ()
  (let (( mvi-buffers
          (remove-if-not
           'mvi-buffer-has-window-p
           (mvi-buffers))))
    (mapc 'mview-image-refresh mvi-buffers)))

(defun* mvi-window-configuration-change-hook (&rest ignore)
  (when (some 'mvi-window-p (window-list))
    (when (timerp mvi-resize-timer)
      (cancel-timer mvi-resize-timer))
    (setq mvi-resize-timer
          (run-with-timer 1 nil 'mvi-refresh-all))))

(defun* mview-image-set-image (image &optional window-or-buffer)
  (assert (file-exists-p image))
  (let (( inhibit-read-only t))
    (mvi-with-window-or-buffer window-or-buffer
      (setq mvi-current-image-file image)
      (setq default-directory (file-name-directory image))
      (mvi-center-insert-image image 1 'use-cache))))

(defun* mview-image-clear (&optional window-or-buffer)
  (mvi-with-window-or-buffer window-or-buffer
    (let (( inhibit-read-only t))
      (erase-buffer)
      (setq mvi-current-image-file nil))))

(defun mview-image-refresh (&optional window-or-buffer)
  (mvi-with-window-or-buffer window-or-buffer
    (when (and (stringp mvi-current-image-file)
               (file-exists-p mvi-current-image-file))
      (mview-image-set-image
       mvi-current-image-file
       window-or-buffer))))

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
    [remap mmouse-scroll-up] 'ignore
    (kbd "<down-mouse-1>") 'ignore
    (kbd "<drag-mouse-1>") 'ignore
    [remap mmouse-scroll-down] 'ignore
    [remap smooth-scroll-up] 'ignore
    [remap smooth-scroll-down] 'ignore
    [remap cua-scroll-up] 'ignore
    [remap cua-scroll-down] 'ignore
    (kbd "g") 'revert-buffer)
  (setq cursor-type nil
        buffer-read-only t
        mvi-is-mvi-buffer t
        indicate-empty-lines nil))

(defun mview-image-pop-to-image (filename)
  "Pop to a new buffer showing the image at FILENAME."
  (interactive (list (read-file-name "Open image:")))
  (pop-to-buffer (generate-new-buffer "MView Image"))
  (mview-image-mode)
  (mview-image-set-image filename))

(provide 'mview-image)

;;; mview-image.el ends here