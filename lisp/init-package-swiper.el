;;; init-package-swiper.el --
;;; Commentary:
;;; Code:

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)

(provide 'init-package-swiper)
;;; init-package-swiper.el ends here
