;;; Commentary:
;;; Commentary:
;;; Code:

(require 'highlight)

(defvar pgt/highlight-region-colors
  '("#FFF59D" "#FFCC80" "#EF9A9A" "#F48FB1" "#CE93D8" "#90CAF9"
    "#80CBC4" "#C5E1A5")
  "Palette of soft highlight colors cycled for region highlighting.")

(defun pgt/highlight-region-with-cycle ()
  "Highlight all occurrences of the active region using cycled colors."
  (interactive)
  (unless (use-region-p)
    (user-error "Select a region to highlight"))
  (let* ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
         (pattern (regexp-quote selection))
         (hlt-auto-face-backgrounds pgt/highlight-region-colors)
         (hlt-auto-faces-flag t))
    (unless (string= pattern "")
      (hlt-highlight-regexp-region (point-min) (point-max) pattern nil t)))
  (deactivate-mark))

(global-set-key (kbd "C-c p h") #'pgt/highlight-region-with-cycle)

(provide 'init-highlight)
;;; init-highlight.el ends here
