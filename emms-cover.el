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
                 (s-join
                  "\\|"
                  (mapcar
                   'extension-to-regex
                   image-extensions))))
         (mmake-path "default-cover.png")))))

(defun* emms-cover-refresh (&rest ignore)
  (let* (( cover-buffer
           (or (first (buffers-with-mode 'emms-cover-mode))
               (return-from emms-cover-refresh)))
         ( cover-path
           (or (emms-cover-path)
               (progn
                 (mview-image-clear cover-buffer)
                 (return-from emms-cover-refresh)))))
    (mview-image-set-image cover-path cover-buffer)))

(define-derived-mode emms-cover-mode mview-image-mode
  "Emms Cover Mode"
  "Dispay the cover of the currently playing album in EMMS. Based on
mview-image."
  (add-hook 'emms-player-finished-hook 'emms-cover-refresh)
  (add-hook 'emms-player-paused-hook 'emms-cover-refresh)
  (add-hook 'emms-player-started-hook 'emms-cover-refresh)
  (add-hook 'emms-player-stopped-hook 'emms-cover-refresh)
  (setq revert-buffer-function 'emms-cover-refresh))

(defun emms-cover-show ()
  "Show the emms-cover buffer."
  (interactive)
  (pop-to-buffer "*EMMS Cover*")
  (emms-cover-mode)
  (emms-cover-referesh))

(provide 'emms-cover)