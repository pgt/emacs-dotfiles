;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)

;; package-specific
(require 'init-package-helm)
(require 'init-package-magit)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)
(require 'init-package-helm-ag)
(require 'init-package-ag)
(require 'init-package-swiper)
(require 'init-package-undo-tree)

(require 'init-project-utils)
(require 'init-view-mode)
(require 'init-ruby)
(require 'init-ssh)
(require 'init-elixir)
(require 'init-clojure)
(require 'init-rust)
(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-eshell)
(require 'init-git-pr)
(require 'init-sql)
(require 'init-docker)
(require 'init-restclient)
(require 'init-parinfer)
(require 'init-web-mode)

(require 'init-appearance)
(require 'init-keybindings)
(require 'init-org)

(require 'init-package-diminish) ;; Should stay at last position

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(custom-safe-themes
   (quote
    ("75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default)))
 '(initial-buffer-choice t)
 '(package-selected-packages
   (quote
    (indium gradle-mode jira-markup-mode applescript-mode sass-mode swift-mode yaml-mode wrap-region wgrep web-mode undo-tree tern-auto-complete swiper ssh-config-mode solarized-theme smartscan smartparens scss-mode rubocop rspec-mode rjsx-mode rhtml-mode restclient rbenv php-mode parinfer paredit org-bullets mutant mode-icons markdown-mode magit-gh-pulls kotlin-mode js2-refactor ido-vertical-mode hl-anything highlight-symbol helm-projectile helm-ag haml-mode gruvbox-theme gotest google-translate google-this git-timemachine git-gutter fuzzy font-lock+ flycheck-mix flycheck-kotlin expand-region dockerfile-mode docker diminish diff-hl company-go coffee-mode clojure-mode-extra-font-locking cider calfw-gcal calfw bundler auto-package-update atom-one-dark-theme atom-dark-theme anzu android-mode all-the-icons-dired alchemist ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
