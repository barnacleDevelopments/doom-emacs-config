;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; PROJECTILE CONFIG
(setq projectile-project-search-path '("~/WebDev/"))

;;WEB MODE
(use-package! web-mode
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq web-mode-content-types-alist
        '(("html" . "\\.ejs\\'")))
  (setq web-mode-engines-alist
        '(("ejs" . "\\.ejs\\'"))))
(setq! doom-themes-treemacs-theme "doom-colors")

(setq doom-font (font-spec :size 20))
;;Shortcuts
(setq avy-all-windows t)

;;QC CLIENT CONFIG
(setq display-line-numbers-type 'relative)
(setq doom-theme 'doom-palenight)
(setq display-line-numbers-type t)

; Mac Config
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
(setenv "GIT_SSH_COMMAND" "ssh -v")
(setq lsp-disabled-clients '(rubocop-ls))

(setq org-directory "~/org/")

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
(setq org-agenda-files '(
        "~/org/inbox.org"
        "~/org/warriertech.org"
        "~/org/personal.org"
        "~/org/professional.org"
        "~/org/projects.org"
        "~/org/main.org"
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

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)

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

(gptel-make-ollama "Ollama"
  :host "127.0.0.1:11434"
  :stream t
  :models '(mistral:latest deepseek-coder-v2:latest))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(map! :leader
      :prefix ("o" . "open")
      "c" #'gptel)

(map! :leader
      :prefix ("l" . "GPT")
      "r" #'gptel-rewrite
      "a" #'gptel--rewrite-accept)

(map! :localleader
      "c" #'gptel--infix-context-add-file
      "m" #'gptel-menu
      "r" #'gptel-context-remove-all)

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
      "r" #'elfeed)

(after! elfeed
  (map! :localleader
        :map elfeed-search-mode-map
        "u" #'elfeed-update
        "e" #'elfeed-score-explain "s" #'elfeed-search-set-filter
        "y" #'elfeed-search-yank
        "f" #'elfeed-search-live-filter
        "b" #'elfeed-search-browse-url))

(elfeed-search-set-filter  "@3-days-ago")

;;Docs: https://kubernetes-el.github.io/kubernetes-el/
(use-package! kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(map! :leader
      :prefix "o"
      "k" #'kubernetes-overview)

(after! kubernetes
  (map! :localleader
        :map kubernetes-overview-mode-map
        "s" #'kubernetes-display-service
        "p" #'kubernetes-display-pod
        "r" #'kubernetes-refresh
        "l" #'kubernetes-logs
        "e" #'kubernetes-edit
        "d" #'kubernetes-describe
        "n" #'kubernetes-set-namespace))

(setq! ledger-schedule-file "~/org/schedual.ledger")
(with-eval-after-load 'ledger-mode
  (add-to-list 'ledger-reports
               '("budget" "ledger bal --budget Expenses -f ~/org/budget.ledger")))
(defun ledger-analytic-start ()
  "Start the 'ledger-analytics' server on port 3000."
  (interactive)
  (let ((buffer-name "*Ledger Analytics Server*"))
    (if (get-buffer buffer-name)
        (message "Ledger Analytics server is already running.")
      (progn
        (start-process "ledger-analytics-process" buffer-name
                       "ledger-analytics" "-f" "~/org/budget.ledger")
        (message "Ledger Analytics server started on port 3000.")))))

(map! :leader
      :prefix "c"
      "R" #'query-replace)

(map! :localleader
      :map terraform-mode-map
      "d" #'terraform-open-doc)

(setq logview-additional-submodes
      '(("Pino JSON Logs"
         (format . "JSON")
         (levels . "level")
         (timestamp . "time"))))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(setq! message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.mailfence.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-auth-supported '(plain login)
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))

(set-email-account! "devin@devdeveloper.ca"
  '((mu4e-sent-folder . "/Sent Items")
    (mu4e-drafts-folder . "/Drafts")
    (mu4e-get-mail-command . "offlineimap -o")
    (mu4e-update-interval . 60)
    (mu4e-trash-folder . "/Trash")
    (smtpmail-smtp-user . "devin")
    (mail-host-address . "devdeveloper.ca")
    (user-full-name . "Devin")
    (user-mail-address . "devin@devdeveloper.ca")
    (smtpmail-smtp-server   . "smtp.mailfence.com")
    (mu4e-compose-signature . "---\nRegards,\nDev"))
  t)

(map! :leader
      :prefix ("o" . "open")
      "m" #'mu4e)

(map! :localleader
      :map diredfl-mode
      "R" #'query-replace)
