(smex-initialize) ;; To usando isso?

;;; Open line above
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'smart-open-line-above)

;;; Delete trailing space automatically on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Auto reload buffer when file change
;;; Necessary when switch between branches of git
(global-auto-revert-mode t)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Comment in initial of file
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; Highlight current line
(global-hl-line-mode 1)

;; Highligh the lines
;; TODO: JOGAR ISSO SOMENTE PARA O HOOK DE RUBY PARA PARAR DE TER O PROBLEMA DE CONFLITO DE CORES COM O MAGIT
;; (hl-highlight-mode 1) ;; See later better

;; Strip whitespaces
(defun rr-strip-whitespace ()
 (interactive)
 (save-excursion
   (goto-char (point-min))
   (replace-regexp "[\s\t]+" " " nil (point-min) (point-max)))
 (indent-region (point-min) (point-max)))

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)


;;; Smartscan
(package-install 'smartscan)
(smartscan-mode 1)


;; Move between splits with arrows
;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

;; Set font
(set-default-font "Source Code Pro")


;; Delete on selected text
(delete-selection-mode 1)

;; Cursor type
(setq default-cursor-type 'bar)

;; Undo and Redo
(winner-mode 1)

;; Google this
(require 'google-this)
(google-this-mode 1)

;; Undo tree
(require 'undo-tree)
(undo-tree-mode 1)

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

;; Ruby-mode :: Dont add utf-8 coding to files
(setq ruby-insert-encoding-magic-comment nil)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)


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
(global-set-key (kbd "s-d") 'duplicate-line)


;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; kill sexp
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-hybrid-sexp)

;;
;;; navigation
;;
(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;;
;;; barf and slurp
;;
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;
;;; splicing ;; Remove the brackets of the current block
;;
(define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
(define-key sp-keymap (kbd "M-r") 'sp-splice-sexp-killing-around)

;;
;;; pairs
;;
(define-key sp-keymap (kbd "C-(") 'sp-rewrap-sexp)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "/" "/" "/")
  (sp-local-tag "_" "_" "_")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(org-mode)
  (sp-local-pair "/" "/")
  (sp-local-pair "_" "_")
  (sp-local-pair "*" "*")
  (sp-local-pair "=" "=")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-pair "$" "$")
  (sp-local-tag "i" "\"<" "\">"))

(sp-with-modes '(ruby-mode)
  (sp-local-pair "|" "|"))

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(provide 'init-smartparens)
;;; init-smartparens.el ends here


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
(define-key global-map [remap move-beginning-of-line]
  (defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.

  Move point to the first non-whitespace character on this line.
  If point was already at that position, move point to beginning of line."
    (interactive)
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line)))))


;; Better view-mode
(require 'view)
(global-set-key (kbd "C-x C-q") 'view-mode)

;; simpler navigation
(define-key view-mode-map "p" 'previous-line)
(define-key view-mode-map "n" 'next-line)
(define-key view-mode-map "f" 'forward-char)
(define-key view-mode-map "k" 'forward-word)
(define-key view-mode-map "b" 'backward-char)
(define-key view-mode-map "j" 'backward-word)
(define-key view-mode-map "l" 'recenter-top-bottom)
(define-key view-mode-map "e" 'move-end-of-line)
(define-key view-mode-map "a" 'smart-beginning-of-line)
(define-key view-mode-map "v" 'scroll-up-command)

;; Yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)

;; Dont exit directly
(setq-default confirm-kill-emacs (quote y-or-n-p))

;; Spelling
(setq ispell-program-name "/usr/local/bin/aspell")

;; Auto update logs like tail -f
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Browser ring
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

;; Multiple cursors
(eval-after-load "multiple-cursors-autoloads"
  '(progn
     (when (require 'multiple-cursors nil t)
       (defun mc/mark-all-dispatch ()
         "- add a fake cursor at current position

- call mc/edit-lines if multiple lines are marked

- call mc/mark-all-like-this if marked region is on a single line"
         (interactive)
         (cond
          ((not (region-active-p))
           (mc/create-fake-cursor-at-point)
           (mc/maybe-multiple-cursors-mode))
          ((> (- (line-number-at-pos (region-end))
                 (line-number-at-pos (region-beginning))) 0)
           (mc/edit-lines))
          (t
           (mc/mark-all-like-this))))

       (defun mc/align ()
         "Aligns all the cursor vertically."
         (interactive)
         (let ((max-column 0)
               (cursors-column '()))
           (mc/for-each-cursor-ordered
            (mc/save-excursion
             (goto-char (overlay-start cursor))
             (let ((cur (current-column)))
               (setq cursors-column (append cursors-column (list cur)))
               (setq max-column (if (< max-column cur) cur max-column)))))

           (defun mc--align-insert-times ()
             (interactive)
             (dotimes (_ times)
               (insert " ")))
           (mc/for-each-cursor-ordered
            (let ((times (- max-column (car cursors-column))))
              (mc/execute-command-for-fake-cursor 'mc--align-insert-times cursor))
            (setq cursors-column (cdr cursors-column)))))

       (setq mc/list-file "~/.mc-lists.el")
       (load mc/list-file t) ;; load, but no errors if it does not exist yet please

       (global-set-key (kbd "C->")  'mc/mark-next-like-this)
       (global-set-key (kbd "C-<")  'mc/mark-previous-like-this)

       (global-set-key (kbd "M-@") 'mc/mark-all-dispatch)
       (global-set-key (kbd "M-#") 'mc/insert-numbers)
       (global-set-key (kbd "M-'") 'mc/align))))

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

(global-set-key (kbd "C-x 9") 'other-window-kill-buffer)

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
(global-set-key (kbd "C-c e") 'rr/split-module-nesting)

;; History minibuffer navigation
(define-key minibuffer-local-map (kbd "C-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-complete-history-element)


;; Projectile enable caching
(setq projectile-enable-caching t)
