;;; init-org.el --
;;; Commentary:
;;; Code:
(require 'org)

(defconst *user-org-cache-directory*
  (path-join *user-dropbox-directory* "org")
  "Path to user's org cache store.")

(defun path-abs-buffer ()
  "Get the current buffer absolute path."
  (file-truename (or (buffer-file-name) default-directory)))

(defun path-join (root &rest dirs)
  "Join paths together starting at ROOT and proceeding with DIRS.
Ex: (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun getenv-or (env value)
  "Fetch the value of ENV or, if it is not set, return VALUE."
  (if (getenv env)
      (getenv env)
    value))

;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(defconst *user-dropbox-directory*
  (path-join *user-home-directory* "Dropbox")
  "Path to Dropbox on user's machine.")

(setq org-capture-directory (path-join *user-dropbox-directory* "org/captures"))

(add-to-list 'load-path (expand-file-name "conf/orgmode" user-emacs-directory))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Agenda setup
(setq org-agenda-files (quote ("~/Dropbox/org")))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

;; Navigation
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-j" 'backward-word)
            (local-set-key "\C-k" 'forward-word)))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; Tasks keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(defun squiter/oc-template (file)
  "Get org template using a FILE."
  (get-string-from-file (path-join org-capture-directory file)))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file (path-join *user-org-cache-directory* "refile.org"))
         ,(squiter/oc-template "todo.org")
         :clock-in t
         :clock-resume t)

        ("n" "note" entry (file (path-join *user-org-cache-directory* "refile.org"))
         ,(squiter/oc-template "note.org")
         :clock-in t
         :clock-resume t)

	("j" "Journal" entry (file+datetree (path-join *user-org-cache-directory* "diary.org"))
	 "* %?\n%(oc/inc \"Things that I learned\" \"** Three things that I learn today\n\")"
	 :clock-in t
	 :clock-resume t)

	("e" "Evening Journal" entry (file+datetree (path-join *user-org-cache-directory* "diary.org"))
	 ,(squiter/oc-template "evening-journal.org")
	 :clock-in t
	 :clock-resume t)

	("w" "Weekly Review" entry (file+datetree (path-join *user-org-cache-directory* "diary.org"))
	 ,(squiter/oc-template "weekly-review.org")
	 :clock-in t
	 :clock-resume t)

	("m" "Morning routine" entry (file+datetree (path-join *user-org-cache-directory* "morning-routine.org"))
	 ,(squiter/oc-template "morning-routine-template.org")
	 :clock-in t
	 :clock-resume t)

	("s" "Code Snippet" entry
	 (file (path-join *user-org-cache-directory* "snippets.org"))
	 ;; Prompt for tag and language
	 "* %? :NOTE:\t\n%U\n#+BEGIN_SRC %(eval custom/org-mode-memory)\n%c\n#+END_SRC")

	("h" "Habit" entry (file (path-join *user-org-cache-directory* "refile.org"))
	 ,(squiter/oc-template "habit.org")
	 :clock-in t
	 :clock-resume t)))

;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Use Ctrl-TAB to change buffer
(add-hook 'org-mode-hook (lambda () (local-set-key [(control tab)] 'other-window)))

(require 'f)
(defvar custom/org-dir (f-long "~/Dropbox/org"))
(setq org-startup-folded nil)

(defun custom/org-get-filenames ()
  (when (file-directory-p custom/org-dir)
    (--> custom/org-dir
         (f-entries it (lambda (file) (string-match-p "\\.org$" file)) t)
         (--map (f-base it) it)
         (sort it 'string-lessp))))

(defun custom/org-file-path (name)
  (let ((org-file (format "%s.org" name)))
    (f-join custom/org-dir org-file)))

(defun custom/org-open-project-file (&optional name)
  (interactive)
  (let ((filename (or name (projectile-project-name))))
    (find-file (custom/org-file-path filename))))

; create a helm buffer displaying all my .org files
(defun custom/helm-org-files ()
  "Display all files under `custom/org-dir` inside a helm buffer."
  (interactive)
  (helm :sources (custom/helm-org-files-source)
        :buffer "*My Org Files*"))

(defun custom/helm-org-files-source ()
  (helm-build-sync-source "My Org Files"
    :candidates (custom/org-get-filenames)
    :action '(("Open file" . custom/org-open-project-file))))

(global-set-key (kbd "C-c p O") 'custom/helm-org-files)

(provide 'init-org)
;;; init-org.el ends here
