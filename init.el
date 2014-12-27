;;;;; Package config

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
    find-file-in-project
    popup
    solarized-theme
    inf-ruby
    rspec-mode
    rhtml-mode
    rubocop
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
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

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
  "Bring up a Spotify search interface in helm."
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
(load-theme 'solarized-light t)

;;;;;; defaults
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(smex-initialize)

(global-anzu-mode t)


;;;;;; keybindings
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)


;;;;;; ruby config
(require 'rspec-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(setq rspec-use-rake-when-possible nil)

(setq ruby-insert-encoding-magic-comment nil)

;;;;;;;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case nil)

(add-hook 'ruby-mode-hook
          (lambda ()
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
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; complete with tab, return and \C-m
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\C-m" 'ac-complete)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(global-set-key (kbd "M-/") 'auto-complete)

;;;;;;;; Set numbers to lines
(global-linum-mode t)

;;;;;;; Save backup~ file in a different place
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
