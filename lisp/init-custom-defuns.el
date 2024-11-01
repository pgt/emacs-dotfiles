;;; init-custom-defuns.el -- Adds simple and useful functions that have nowhere else to go.
;;; Commentary:
;;; Code:

;;open the spec of a class
(defun senny-ruby-open-spec-other-buffer ()
  (interactive)
  (when (featurep 'rspec-mode)
    (let ((source-buffer (current-buffer))
          (other-buffer (progn
                          (rspec-toggle-spec-and-target)
                          (current-buffer))))
      (switch-to-buffer source-buffer)
      (pop-to-buffer other-buffer))))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c , ,") 'senny-ruby-open-spec-other-buffer)))

;; String interpolation
(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))
;; (put 'dired-find-alternate-file 'disabled nil)

;;; Open line above
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Comment in initial of file
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; Strip whitespaces
(defun rr-strip-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "[\s\t]+" " " nil (point-min) (point-max)))
  (indent-region (point-min) (point-max)))

;; Copying without select the line
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
	   (interactive (if mark-active (list (region-beginning) (region-end)) (message
										"Copied line") (list (line-beginning-position) (line-beginning-position
																2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; Duplicate lines
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;;; Move text over the current document
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)


;; beggining of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Close buffer in other window
(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

;; Colapse namespaces in ruby
(defun rr/split-module-nesting ()
  (interactive)
  (save-excursion
    (when (re-search-forward "\\(class\\|module\\|describe\\).*::" nil t)
      (backward-delete-char 2)
      (set-mark (point))
      (backward-sexp)
      (kill-region (point) (mark))
      (beginning-of-buffer)
      (insert "module ")
      (yank)
      (insert "\n")
      (end-of-buffer)
      (insert "end\n")
      (indent-region (point-min) (point-max)))))

;; vcr toggle
(defun custom/vcr-toggle ()
  (interactive)
  (if (getenv "VCR_OFF")
      (progn
        (setenv "VCR_OFF" nil)
        (message "VCR is ON"))
    (progn
      (setenv "VCR_OFF" "true")
      (message "VCR is OFF"))))

;; Indent all buffer
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; Not push to kill-ring the killed words
(defun kill-word (arg)
  ;; -- This monkeypatch fixes the behavior of kill word --
  ;; now, it will not push to the kill-ring the killed words.
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; -- window management --
(defun vsplit-last-buffer ()
  "Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  "Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
	 (other (next-window))
	 (this-buffer (window-buffer this))
	 (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun insert-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
	  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
	  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
	  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
	  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
	  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))

(defun sudo-edit (&optional arg)
  "Edit file as sudo. ARG as point."
  (interactive "p")
  (find-file (concat "/sudo:root@localhost:" (helm-read-file-name "File: "))))

(defun rr-show-file-name ()
  "Show the full path filename in the minibuffer."
  (interactive)
  (let ((text (format "%s:%i" (buffer-file-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun custom/class-from-file-name (file-name)
  "Guess the name of the class given a filename."
  (let* ((name (file-relative-name file-name (projectile-project-root)))
         (class (capitalize name))
         (rules '(("\\.rb"        . "")
                  ("app\\/.+?\\/" . "")
                  ("lib\\/"       . "")
                  ("/"            . "::")
                  ("_"            . ""))))
    (dolist (rule rules)
      (setq class (replace-regexp-in-string (car rule) (cdr rule) class nil t)))
    class))

;; (defun custom/run-mutant-from-dired ()
;;   "Run mutant over all marked files."
;;   (interactive)
;;   (let* ((file-names (dired-get-marked-files))
;;          (class-names (mapcar 'custom/class-from-file-name file-names))
;;          (joined-classes (mapconcat 'identity class-names " "))
;; 	 (joined-names (mapconcat 'identity file-names " ")))
;;     (compile (concat "bundle exec mutant -r " joined-names " --use rspec -j 1 " joined-classes))))

(defun close-all-buffers ()
  "Close all buffers!"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun rr/kill-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun rr/backward-kill-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Delete file and buffer
(defun pgt-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(defalias 'pgt-delete-buffer-and-file #'pgt-delete-file-and-buffer)

;; Rename file and buffer
(defun pgt-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(defalias 'pgt-rename-buffer-and-file #'pgt-rename-file-and-buffer)

;; Open URL source code
(defun pgt-view-url-source-code ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (set-auto-mode)))

(defun git-add-current-buffer ()
  "call 'git add [current-buffer]'"

  (interactive)
  (let* ((buffile (buffer-file-name))
	 (output (shell-command-to-string
		  (concat "git add " (buffer-file-name)))))
    (message (if (not (string= output ""))
		 output
	       (concat "Added " buffile)))))

(provide 'init-custom-defuns)
;;; init-custom-defuns.el ends here
