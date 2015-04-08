;;; init-defaults.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:

;;;;;;;; Set numbers to lines
(global-linum-mode t)

;;;;;;; Save ~ file in a different place
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))


;;; TODO: I dont remember this
(custom-set-variables
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(custom-safe-themes
   (quote
    ("75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(initial-buffer-choice "~/code"))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Highlight current line
(global-hl-line-mode 1)

;; Delete on selected text
(delete-selection-mode 1)

;; Undo and Redo
(winner-mode 1)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

;; Yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)

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

;; Dont exit directly
(setq-default confirm-kill-emacs (quote y-or-n-p))

;; Auto update logs like tail -f
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Browser ring
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

(smex-initialize) ;; To usando isso?


;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; =======================
;; -- git-gutter config --
;; =======================
(require 'git-gutter)
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

;; Google this
(require 'google-this)
(google-this-mode 1)

;; Undo tree
(require 'undo-tree)
(undo-tree-mode 1)

;; Spelling
(setq ispell-program-name "/usr/local/bin/aspell")

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

(provide 'init-defaults)
;;; init-defaults.el ends here
