;;; init-defaults.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:

;; import PATH environment variable
(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append (split-string-and-unquote path ":") exec-path)))

;;;;;;;; Set numbers to lines
(add-hook 'prog-mode-hook 'linum-mode)

;;;;;;; Save ~ file in a different place
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Initializing in ~/code
(custom-set-variables
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(custom-safe-themes
   (quote
    ("75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(initial-buffer-choice t))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Highlight current line
(global-hl-line-mode 1)

;; Delete on selected text
(delete-selection-mode 1)

;; Undo and Redo
(winner-mode 1)

;; Global company-mode; autocomplete
(global-company-mode)

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

;; Anzu
(global-anzu-mode t)

;; ==========================
;; -- Expand-region config --
;; ==========================
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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


;; Reuse buffers between frames
(setq-default display-buffer-reuse-frames t)

;;
;; ace jump mode major function
;;
(add-to-list 'load-path "~/.emacs.d/vendor/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c j") 'ace-jump-mode)

;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode-pop-mark)

;; Helm AG
(setq helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-insert-at-point 'symbol)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;;; Guide-key configurations
(require 'guide-key)
(setq guide-key/guide-key-sequence 't)
(guide-key-mode 1)  ; Enable guide-key-mode
(setq guide-key/highlight-command-regexp "rectangle")
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))
(setq guide-key/idle-delay 0.8)

;;; Scroll settings
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Recent files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;; When you visit a file, point goes to the last place where it was
;;; when you previously visited the same file.
(require 'saveplace)
(setq-default save-place t)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-enable-key "r")

;;; Setting some envs
(setenv "LC_ALL" "en_US.utf-8")
(setenv "LANG" "en_US.utf-8")

;;; Back-button
(require 'back-button)
(back-button-mode 1)

;; Highlight-symbol - vim imitation feature
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(provide 'init-defaults)
;;; init-defaults.el ends here
