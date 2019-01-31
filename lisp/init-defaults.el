;;; init-defaults.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:

;; import PATH environment variable
(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append (split-string-and-unquote path ":") exec-path)))

;;;;;;;; Set numbers to lines
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;;;;; Save ~ file in a different place
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Initializing in ~/code
(custom-set-variables
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(custom-safe-themes
   (quote
    ("75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(initial-buffer-choice t))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

;; Yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)

;; Dont exit directly
(setq-default confirm-kill-emacs (quote y-or-n-p))

;; Auto update logs like tail -f
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Browser ring
(global-set-key (kbd "s-V") 'helm-show-kill-ring)

;; ==========================
;; -- Expand-region config --
;; ==========================
(global-set-key (kbd "C-=") 'er/expand-region)

;; Spelling
(setq ispell-program-name "/usr/local/bin/aspell")

;; Reuse buffers between frames
(setq-default display-buffer-reuse-frames t)

;;; Scroll settings
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Recent files
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;; When you visit a file, point goes to the last place where it was
;;; when you previously visited the same file.
(setq-default save-place t)

(setq wgrep-auto-save-buffer t)
(setq wgrep-enable-key "r")

;;; Setting some envs
(setenv "LC_ALL" "en_US.utf-8")
(setenv "LANG" "en_US.utf-8")

;; Highlight-symbol - vim imitation feature
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Diff-hl no dired
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; Set a better column size
(setq fill-column 80)

;; This for open all files in the current emacs section
(setq ns-pop-up-frames nil)

;;; TODO: Put this in a frontend file or CSS file
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(defun squiter/get-window-name ()
  "Get a project name or buffer name."
  (if (projectile-project-p)
      (projectile-project-name)
    (buffer-name)))

(setq frame-title-format
      '("emacs@"
        (:eval (squiter/get-window-name))))

(provide 'init-defaults)
;;; init-defaults.el ends here
