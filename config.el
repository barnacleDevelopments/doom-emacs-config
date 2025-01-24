;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; PROJECTILE CONFIG
(setq projectile-project-search-path '("~/WebDev/"))

;;REST CLIENT CONFIG
(use-package! restclient)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;;WEB MODE
(use-package! web-mode
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq web-mode-content-types-alist
        '(("html" . "\\.ejs\\'")))
  (setq web-mode-engines-alist
        '(("ejs" . "\\.ejs\\'"))))

;;Shortcuts
(map! "C-c r"   #'eshell-isearch-backward)

(setq avy-all-windows t)
;;QC CLIENT CONFIG
(use-package! counsel-jq)
(setq display-line-numbers-type 'relative)
(setq doom-theme 'doom-palenight)
(setq display-line-numbers-type t)

(setq org-directory "~/org/")

(defun my-org-babel-tangle-on-save ()
  "Automatically tangle Org Babel code blocks after saving."
  (add-hook 'after-save-hook #'org-babel-tangle :append :local))
(add-hook 'org-mode-hook #'my-org-babel-tangle-on-save)

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
(setq org-agenda-files '(
        "~/org/inbox.org"
        "~/org/warriertech.org"
        "~/org/personal.org"
))
(setq org-agenda-start-with-follow-mode t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(setq org-capture-templates
      '(("w" "Warriertech Todo" entry (file+headline "~/org/warriertech.org" "Inbox")
         "* TODO %?\n  %U\n %a %i")
        ("p" "Personal Todo" entry (file+headline "~/org/personal.org" "Inbox")
         "* TODO %?\n  %U\n  %i")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %U - %^{Title}\n  %?\n  %i")
        ("c" "Cookbook" entry (file "~/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)))

(setq org-refile-targets '(("~/org/personal.org" :level . 1)
                            ("~/org/warriertech.org" :maxlevel . 2)))

(use-package! prettier
  :hook (
         (js-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (css-mode . prettier-mode)
         (html-mode . prettier-mode)
         (markdown-mode . prettier-mode)
         (terraform-mode . prettier-mode)))

(setq lsp-idle-delay 0.500)  ; Increase delay to half a second (default is 0.1)
(setq lsp-enable-on-type-formatting nil)  ; Disable auto-formatting on typing
(setq lsp-file-watch-ignored-directories
      '("[/\\\\]\\.git$"
        "[/\\\\]node_modules$"
        "[/\\\\]build$"
        "[/\\\\]dist$"))
(setq lsp-file-watch-threshold 1000)  ;; Increase threshold to 1000 files
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil) ;; Disable symbol highlighting
  (setq lsp-enable-on-type-formatting nil)  ;; Disable on-type formatting
  (setq lsp-signature-auto-activate nil)    ;; Disable signature help
  (setq lsp-modeline-code-actions-enable nil) ;; Disable code actions in modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Disable diagnostics in modeline
  (setq lsp-lens-enable nil)) ;; Disable CodeLens
(after! lsp-mode
  (setq lsp-typescript-auto-import-completions nil)) ;; Disable auto-imports

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook! 'compilation-mode-hook 'my-compilation-mode-hook)

(setq gpt-api-key (getenv "CHAT_GPT_API_KEY"))
(use-package! gptel
 :config
 (setq! gptel-api-key gpt-api-key))

(map! :leader
      :prefix ("o" . "open")
      "c" #'gptel)

(use-package! elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))
(setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
(setq elfeed-score-serde-score-file "/home/devindavis/.doom.d/score.el")
(map! :leader
      :prefix ("o" . "open")
      "r" #'elfeed
      )

(map! :map elfeed-search-mode-map
      :prefix ("C-c" . "Update Feeds")
      "u" #'elfeed-update)

(elfeed-search-set-filter  "@3-days-ago")

;; Docs: https://kubernetes-el.github.io/kubernetes-el/
(use-package! kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(map! :leader
      :prefix ("o" . "Kubernetes")
      "k" #'kubernetes-overview)

(map! :map kubernetes-overview-mode-map
      :prefix ("C-c" . "Kubernetes Refresh")
      "r" #'kubernetes-refresh)

(map! :map kubernetes-overview-mode-map
      :prefix ("C-c" . "Kubernetes Display Pod")
      "P" #'kubernetes-display-pod)


(map! :map kubernetes-overview-mode-map
      :prefix ("C-c" . "Kubernetes Display Service")
      "s" #'kubernetes-display-service)
