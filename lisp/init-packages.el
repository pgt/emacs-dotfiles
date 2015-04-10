;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
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
    gruvbox-theme ;; My theme
    popup
    inf-ruby
    rvm
    rspec-mode
    rhtml-mode
    rubocop
    google-this
    diminish
    bundler
    auto-package-update
    smartparens
    undo-tree
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
    recentf
    saveplace
    ansi-color
    dired-x
    s
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode))

;; package loading
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

;; vendor loading
(dolist (lib libs-to-require)
  (require lib))

;; Automagically updating packages
(require 'auto-package-update)
(auto-package-update-maybe)

(provide `init-packages)
;;; init-packages.el ends here
