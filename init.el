;;; package ---- Summary;

;;; Code
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(
    magit
    git-timemachine ;; Travel for versions of a file on git
    flycheck ;; Syntax checking for over 60 programming languages
    ido-vertical-mode ;; Easy navigation on commands
    fuzzy ;; Completion
    s ;; TODO: need search
    ag ;; Search
    smartscan ;; Move between symbols (* of vim)
    helm ;; Help in navigation
    smex ;; Better interface for M-x
    projectile ;; Project interaction
    helm-projectile ;; Navigation for Projectile
    smooth-scrolling ;; Scroll
    auto-complete ;; Auto-complete
    anzu ;; Better search mode of file with syntax highlight
    wrap-region ;; Highlight the selected region
    yaml-mode ;; Help on YAML editing
    neotree ;; Like NerdTree on Vim, F8
    paredit ;; TODO
    idle-highlight-mode
    gruvbox-theme
    find-file-in-project
    popup
    inf-ruby
    rvm
    rspec-mode
    rhtml-mode
    rubocop
    google-this
    smartparens
    undo-tree
    robe
    restclient)
  "A list of packages to ensure are installed at launch.")

(defvar libs-to-require
  '(cl
    uniquify
    linum
    paredit
    whitespace
    wrap-region
    ffap
    find-file-in-project
    recentf
    saveplace
    ansi-color
    dired-x
    s
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode))

(setq packaged-contents-refreshed-p nil)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case ex
     (package-install p)
      ('error (if packaged-contents-refreshed-p
            (error ex)
          (package-refresh-contents)
          (setq packaged-contents-refreshed-p t)
          (package-install p))))))

(dolist (lib libs-to-require)
  (require lib))

;;;;; Helm config
(require 'helm-config)

(helm-mode t)

(setq helm-split-window-in-side-p t
      helm-exit-idle-delay 0.01
      helm-ff-transformer-show-only-basename nil
      helm-ls-git-show-abs-or-relative 'relative
      helm-buffer-max-length 45)

;; helpers for more familiar helm find-file navigation
(defun helm-find-files-sensitive-backspace ()
  "Deletes whole directory in helm find files mode on backspace."
  (interactive)
  (if (char-equal ?/ (char-before))
      (helm-find-files-up-one-level 1)
    (backward-delete-char 1)))

;; -- keybindings --
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-N") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)
(global-set-key (kbd "s-t") 'helm-projectile-find-file)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-/") 'comment-dwim-line)

(global-set-key (kbd "C-j") 'backward-word) ;; navigation
(global-set-key (kbd "C-k") 'forward-word) ;; navigation

(global-set-key (kbd "C-o") 'kill-line) ;; kill line

(global-set-key (kbd "C-c y") 'magit-status)


;; helm better navigation
(define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "<DEL>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-sensitive-backspace)

(define-key helm-map (kbd "C-h") 'delete-backward-char)


;;;;;; project
(require 'helm-projectile)

(projectile-global-mode 1)

;; variables
(defvar default-project-source
  "~/code")

(defvar project-sources
  (list
   default-project-source
   "~/code/locaweb/"))

;; helm integration for opening projects

(defun helm-rr-open-project ()
  "Bring up a Project search interface in helm."
  (interactive)
  (helm :sources '(helm-source-list-projects)
     :buffer "*helm-list-projects*"))

(defvar helm-source-list-projects
  '((name . "Open Project")
    (volatile)
    (delayed)
    (candidates . rr-list-projects)
    (action-transformer . rr-open-project)))

(defun rr-list-projects ()
  "Lists all projects given project sources."
  (cl-labels ((dir-to-files (dir)
                   (if (file-exists-p dir)
                    (directory-files dir t directory-files-no-dot-files-regexp)))
           (flatten (x)
                 (cond ((null x) nil)
                    ((listp x) (append (car x) (flatten (cdr x)))))))
    (progn (flatten (mapcar #'dir-to-files  project-sources)))))

(defun rr-open-project (actions path)
  "Do nothing with ACTIONS. Open project given PATH."
  ;; TODO: Add default file get.
  (cl-flet ((find-default-file () (if (file-exists-p (expand-file-name "Gemfile" path))
                          (expand-file-name "Gemfile" path)
                        path)))
    (find-file (find-default-file))))

;; Creating new project
(defun rr-new-git-project ()
  (interactive)
  (let* ((source (ido-completing-read "create new project in which source?: " project-sources))
      (project-name (read-input "new project name: "))
      (project-dir (file-name-as-directory (expand-file-name project-name source))))
    (condition-case nil
     (mkdir project-dir)
      (error nil))

    (shell-command (format "cd %s; git init" project-dir))
    (rr-add-gitignore-file project-dir)))

(defun rr-add-gitignore-file (repo-path)
  (interactive (list
          (read-directory-name
           "Which repository?: "
           (if (projectile-project-root)
               (projectile-project-root)
             (file-name-directory (buffer-file-name))))))
  (let* ((gitignore-dir (expand-file-name "gitignore/" default-project-source))
      (gitignore-files (directory-files
                  gitignore-dir
                  nil
                  directory-files-no-dot-files-regexp))
      (gitignore-file (ido-completing-read "choose gitignore file: " gitignore-files)))
    (if gitignore-file
     (copy-file
      (expand-file-name gitignore-file gitignore-dir)
      (expand-file-name ".gitignore" repo-path)
      t))))

;; ===============
;; -- ag config --
;; ===============
(setq
 ag-highlight-search t ;; highlight the matches
 ag-reuse-window nil   ;; do not use the same window for the search result
 ag-reuse-buffers t)   ;; use the same buffer for many searches

;; ====================
;; -- neotree config --
;; ====================
(setq
 neo-persist-show nil
 neo-keymap-style 'concise)

(require 'neotree)

(define-key neotree-mode-map (kbd "C-x C-s") 'noop)

(defun neotree-git-project ()
  "Open dirtree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

;; =====================================
;; -- extensions to projectile keymap --
;; =====================================
(let ((map projectile-command-map))
  ;; general utils
  (define-key map "o" 'helm-rr-open-project)
  (define-key map "n" 'rr-show-file-name)
  (define-key map "\C-n" 'rr-new-git-project)
  (define-key map "\C-g" 'rr-add-gitignore-file)

  (define-key map "m" 'git-timemachine)

  ;; ag
  (define-key map "s" 'ag-project)
  (define-key map "\C-s" 'ag-project-regexp)

  ;; neotree
  (define-key map "d" 'neotree-git-project)
  (define-key map "x" 'neotree-find)

  (define-key map "h" 'hl-highlight-thingatpt-local)
  (define-key map "u" 'hl-unhighlight-all-local)

  (define-key map "y" 'projectile-find-implementation-or-test-other-window)
  (define-key map "a" 'projectile-test-project)
  (define-key map "F" 'helm-projectile-find-file-in-known-projects))

(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)


;;;;; theme
(load-theme 'gruvbox t)

;;;;;; defaults
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(smex-initialize) ;; To usando isso?

(global-anzu-mode t)

;;;;;; keybindings
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

;;; RVM
(require 'rvm)
;; use rvm’s default ruby for the current Emacs session
(rvm-use-default)

;;;;;; ruby config
(require 'rspec-mode)
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(setq-default rspec-use-rvm t)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;;;;;;;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case nil)

(add-hook 'ruby-mode-hook
          (lambda ()
	    (rvm-activate-corresponding-ruby)
            (make-local-variable 'ac-ignores)
            ;; ruby keywords
            (add-to-list 'ac-ignores "do")
            (add-to-list 'ac-ignores "end")
            (add-to-list 'ac-ignores "begin")
            (add-to-list 'ac-ignores "true")
            (add-to-list 'ac-ignores "false")
            (add-to-list 'ac-ignores "for")
            (add-to-list 'ac-ignores "rescue")
            (add-to-list 'ac-ignores "fail")
            (add-to-list 'ac-ignores "while")))

(define-key ac-complete-mode-map "\C-n" 'ac-next)

(global-set-key (kbd "M-/") 'auto-complete)

;;;;;;;; Set numbers to lines
(global-linum-mode t)

;;;;;;; Save ~ file in a different place
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Put autosave files (ie #foo#) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(custom-safe-themes
   (quote
    ("75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(initial-buffer-choice "~/code"))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

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

;;; Change winddow with C-tab
(global-set-key [C-tab]
    (lambda ()
      (interactive)
      (other-window -1)))

;;; Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")
(setq powerline-arrow-shape 'arrow)   ;; give your mode-line curves

(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Rubocop
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; Rainbow-mode
;; (require 'rainbow-mode)
;; (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
;;   (lambda () (rainbow-mode 1)))

;; (my-global-rainbow-mode 1)

;; Rainbow-delimiters
;;(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


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

;; Using robe-mode
(robe-mode 1)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

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
