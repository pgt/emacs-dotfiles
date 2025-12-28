;;; init-terraform.el -- Terraform tooling configuration.
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.tf\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\'" . terraform-mode))

(defun my/terraform-setup ()
  "Configure Terraform buffers."
  (terraform-format-on-save-mode 1)
  (lsp-deferred))

(add-hook 'terraform-mode-hook #'my/terraform-setup)

(provide 'init-terraform)
;;; init-terraform.el ends here
