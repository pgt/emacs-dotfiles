;;; init-package-undo-tree.el --
;;; Commentary:
;;; Code:

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(provide 'init-package-undo-tree)
;;; init-package-undo-tree.el ends here
