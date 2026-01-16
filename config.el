;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq avy-all-windows t)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-type t)

;; Relative Line Numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; Disable line numbers in specific modes
(dolist (mode '(pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(after! flycheck
  ;; Use Biome checker for TypeScript/JavaScript
  ;; Note: Biome integrates linting and formatting
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)

  (defun my/biome-fix-file ()
    "Fix and format current file with Biome."
    (interactive)
    (when buffer-file-name
      (save-buffer)  ; Save buffer first to ensure file is up to date
      (let* ((file (shell-quote-argument buffer-file-name))
             (cmd (format "npx @biomejs/biome check --write --unsafe %s" file))
             (result (shell-command cmd)))
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)
        (message "Biome: Fixed %s" (file-name-nondirectory buffer-file-name)))))

  (defun my/biome-check-file ()
    "Check current file with Biome (no fixes applied)."
    (interactive)
    (when buffer-file-name
      (compile (format "npx @biomejs/biome check %s"
                      (shell-quote-argument buffer-file-name)))))

  (defun my/biome-check-project ()
    "Check entire project with Biome."
    (interactive)
    (if-let ((project-root (projectile-project-root)))
        (let ((default-directory project-root))
          (compile "npx @biomejs/biome check ."))
      (message "Not in a projectile project")))

  (defun my/biome-fix-project ()
    "Fix and format entire project with Biome."
    (interactive)
    (if-let ((project-root (projectile-project-root)))
        (when (y-or-n-p (format "Run Biome fix on entire project at %s? " project-root))
          (let ((default-directory project-root))
            (compile "npx @biomejs/biome check --write --unsafe .")))
      (message "Not in a projectile project")))

  (map! :map (tsx-ts-mode-map typescript-ts-mode-map js-ts-mode-map)
        :localleader
        (:prefix ("b" . "biome")
         "f" #'my/biome-fix-file
         "c" #'my/biome-check-file
         "F" #'my/biome-fix-project
         "C" #'my/biome-check-project))

  )

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-modern-table-vertical 1
          org-modern-table-horizontal 0.2
          org-modern-list '((43 . "➤")
                            (45 . "–")
                            (42 . "•"))
          org-modern-block-fringe nil
          org-modern-block-name '("" . "")
          org-modern-keyword nil
          org-modern-footnote (cons nil (cadr org-script-display))
          org-modern-priority nil
          org-modern-todo nil))

; Mac Config
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
(setenv "GIT_SSH_COMMAND" "ssh -v")

(map! :leader
      :desc "Comment Region"
      :prefix ("c" . "+code")
       (:prefix-map ("f" . "format")
                    "c" #'comment-region))

(map! :leader
      :desc "Uncomment Region"
      :prefix ("c" . "+code")
       (:prefix-map ("f" . "format")
                    "C" #'uncomment-region))

;; Query replace commands
(map! :leader
      (:prefix ("s" . "search")
       (:prefix-map ("r" . "replace")
        :desc "Query replace"                    "r" #'query-replace
        :desc "Query replace regexp"            "R" #'query-replace-regexp
        :desc "Replace string"                   "s" #'replace-string
        :desc "Replace regexp"                   "S" #'replace-regexp
        :desc "Projectile replace"               "p" #'projectile-replace
        :desc "Projectile replace regexp"        "P" #'projectile-replace-regexp)))

(setq! doom-themes-treemacs-theme "doom-colors")
(setq! treemacs-width 60)

(setq! default-abbrev-mode t)
(setq! abbrev-file-name "./abbrev.el")
(quietly-read-abbrev-file)

(setq! doom-font (font-spec :size 16))
(setq! doom-theme 'doom-palenight)

(setq org-directory "~/my-org-roam/")

(after! org
    ;; Enable company in org-src blocks

  (defun +org-src-company-setup ()
    "Enable company-mode in org-src blocks."
    (company-mode +1))

    (add-hook! 'org-src-mode-hook #'+org-src-company-setup))

(defun org-get-title ()
  "Get the #+TITLE of the current buffer's file."
  (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
      (file-name-nondirectory (buffer-file-name))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-12(org-get-title) ") 
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-clock-sound "~/my-org-roam/ding.wav")


(setq org-clock-idle-time 15)

(map! :map org-mode-map
      :localleader
      (:prefix ("l" . "insert link")
        "i" #'my/org-insert-info-link))

(defun my/org-insert-package-link ()
  "Insert an org-mode link to package documentation with completion."
  (interactive)
  (let* ((packages (mapcar #'car package-alist))
         (package (intern (completing-read "Package: " packages))))
    (insert (format "[[elisp:(describe-package '%s)][%s]]" package package))))
(defun my/org-insert-info-link ()
  "Insert an org-mode link to open info documentation with completion."
  (interactive)
  (require 'info)
  (let* ((topic (info-lookup-guess-default 'symbol 'emacs-lisp-mode))
         (completions (progn
                        (info-initialize)
                        (let ((manuals '()))
                          (dolist (dir Info-directory-list)
                            (when (file-directory-p dir)
                              (dolist (file (directory-files dir nil "\\.info\\(?:\\.gz\\|\\.bz2\\)?$"))
                                (when (string-match "\\`\\([^.]+\\)" file)
                                  (push (match-string 1 file) manuals)))))
                          (delete-dups manuals))))
         (manual (completing-read "Info manual: " completions nil nil nil nil topic))
         (node (read-string "Node (optional): "))
         (description (read-string "Description: " nil nil manual))
         (command (if (string-empty-p node)
                      (format "(info \"%s\")" manual)
                    (format "(info \"(%s)%s\")" manual node))))
    (insert (format "[[elisp:%s][%s]]" command description))))

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
(setq org-agenda-files '(
        "~/my-org-roam/projects"
        "~/my-org-roam/daily"
        "~/my-org-roam/work-org-roam/daily"
        "~/my-org-roam/work-org-roam/tickets"
        "~/my-org-roam/sources"
        "~/my-org-roam/mobile-notes"
        "~/doom/config.org"
))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/my-org-roam/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)))

(defun my/org-md-filter-sub-to-underscore (text backend info)
  "Replace <sub>...</sub> with _... in GFM export."
  (when (eq backend 'gfm)
    (replace-regexp-in-string
     "<sub>\\([^<]+\\)</sub>" "_\\1" text)))

(defun my/org-md-filter-remove-anchors (text backend info)
  "Remove <a id=\"...\"></a> tags from export TEXT when exporting to GFM."
  (when (eq backend 'gfm)
    (replace-regexp-in-string "<a id=\"[^\"]+\"></a>\n?" "" text)))

(defun org-export-to-markdown-and-copy-clean ()
  "Export Org buffer to GFM Markdown, clean via filters, and copy to clipboard."
  (interactive)
  (require 'ox-gfm)
  (let ((org-export-filter-final-output-functions
         '(my/org-md-filter-remove-anchors
           my/org-md-filter-sub-to-underscore))
        (org-export-with-toc nil)) ;; Disable TOC
    (let ((clean-md (org-export-as 'gfm)))
      (with-temp-buffer
        (insert clean-md)
        (clipboard-kill-region (point-min) (point-max)))
      (message "Clean GFM Markdown copied to clipboard."))))

(setq org-roam-directory "~/my-org-roam")
(org-roam-db-autosync-mode)

(setq org-roam-dailies-capture-templates
      `(("d" "default" plain
         "%?"
         :target (file+head "%<%Y-%m-%d>.org"
                            ,(concat "#+title: %<%Y-%m-%d>\n\n"
                                     ""))
         :unnarrowed t
         )
        ("e" "Event Temple" plain
         "%?"
         :target (file+head "~/my-org-roam/work-org-roam/daily/%<%Y-%m-%d>-et.org"
                            ,(concat "#+title: %<%Y-%m-%d>\n\n"
                                     "#+filetags: :event_temple:\n"
                                     "* Standup\n** Yesterday\n** Today\n** Blockers\n** Action Items"))
         :unnarrowed t
         )))

(setq org-roam-capture-templates
      `(("g" "Generic" plain
         "%?"
         :target (file+head "work-org-roam/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :%^{tag}:\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "- \n\n"
                                     "** TODO Review \n"))
         :unnarrowed t)
        ("t" "Ticket" plain
         "%?"
         :target (file+head "work-org-roam/tickets/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :ticket:\n"
                                     "#+jira_ticket_url: %^{JiraTicketURL}\n"
                                     "#+figma_url: %^{FigmaDesignURL}\n"
                                     "#+pull_request_url: \n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* Pull Request\n"
                                     "** Description\n"
                                     "** How to test\n\n"
                                     "* Code\n"
                                     "- \n\n"
                                     "* Checklist\n"
                                     "** TODO Complete [0/6]\n"
                                     "*** [ ] Write tests\n"
                                     "*** [ ] Create pull request\n"
                                     "*** [ ] Apply feedback if any\n"
                                     "*** [ ] Deploy to staging\n"
                                     "*** [ ] Deploy to production\n"
                                     "*** [ ] Create release note using template in Slack\n"))
         :unnarrowed t)
        ("p" "ET Project" plain
         "%?"
         :target (file+head "work-org-roam/projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :project:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* Checklist\n"
                                     "** TODO Complete [0/6]\n"
                                     "*** [ ] Write tests\n"
                                     "*** [ ] Create pull request\n"
                                     "*** [ ] Apply feedback if any\n"
                                     "*** [ ] Deploy to staging\n"
                                     "*** [ ] Deploy to production\n"
                                     "*** [ ] Create release note using template in Slack\n"))
         :unnarrowed t)
        ("i" "Project" plain
         "%?"
         :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :project:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* Checklist\n"
                                     "** TODO Complete [0/6]\n"
                                     ""))
         :unnarrowed t)
        ("b" "Post" plain
         "%?"
         :target (file+head "posts/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :post:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* Checklist\n"
                                     "** TODO Post to website\n"
                                     "** TODO Make LinkedIn Post\n"))
         :unnarrowed t)
        ("s" "Source" plain
         "%?"
         :target (file+head "sources/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :%^{tag}:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* References\n"
                                     "- \n"
                                     ))
         :unnarrowed t)
("P" "Person" plain
         "%?"
         :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :%^{tag}: :person:\n\n"
                                     "* Description\n\n"
                                     "- \n"
                                     ))
         :unnarrowed t)
        ))

(setq org-export-show-temporary-export-buffer nil)
(defun my/org-to-md-on-save ()
  "Export Org file to Hugo-compatible Markdown cleanly, strip heading IDs, and copy it to the destination directory."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string-prefix-p (expand-file-name "~/org-roam/posts/")
                              (expand-file-name (buffer-file-name))))
    ;; Don't show temporary export buffer
      (let* ((base-name (file-name-base (buffer-file-name)))
             (exported-md (org-hugo-export-as-md)))
        (when (buffer-live-p exported-md)
          (let* ((destination-dir (expand-file-name "~/WebDev/Projects/PersonalSite/content/blog/"))
                 (title (replace-regexp-in-string "[[:digit:]]\\{14\\}-" "" base-name))
                 (destination-file (expand-file-name (concat title ".mdx") destination-dir)))
            (with-current-buffer exported-md
              ;; 🧹 Strip {#id} before saving
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward " {#\\([^}]+\\)}" nil t)
                  (replace-match "")))
              (write-region (point-min) (point-max) destination-file))
            (kill-buffer exported-md))))))
(add-hook 'after-save-hook 'my/org-to-md-on-save)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; Note: Biome handles linting through Apheleia integration
;; ESLint can still be used for projects that require it
(add-hook! 'typescript-mode
  (lambda ()
    (flycheck-select-checker 'javascript-eslint)))

(map! :leader
      :prefix ("c" . "+code")
      (:prefix-map ("x" . "errors")
       :desc "List errors"            "l" #'flycheck-list-errors
       :desc "Select Checker"         "s" #'flycheck-select-checker
       :desc "Next error"             "n" #'flycheck-next-error
       :desc "Previous error"         "p" #'flycheck-previous-error
       :desc "Check buffer"           "c" #'flycheck-buffer
       :desc "Clear errors"           "C" #'flycheck-clear
       :desc "Explain error at point" "e" #'flycheck-explain-error-at-point
       :desc "Verify setup"           "v" #'flycheck-verify-setup))

(use-package! web-mode
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq web-mode-content-types-alist
        '(("html" . "\\.ejs\\'")))
  (setq web-mode-engines-alist
        '(("ejs" . "\\.ejs\\'"))))

(setq projectile-project-search-path '("~/WebDev/"))
(use-package! ripgrep
  :config
  (setq ripgrep-arguments '("-C2")))  ;; -C2 is --context=2

(after! lsp-mode
  ;; Disable rubocop-ls
  (setq lsp-disabled-clients '(rubocop-ls))
  (setq lsp-ruby-lsp-use-bundler t)
  (setq lsp-ruby-lsp-formatter "auto")
  
  ;; Register ruby-lsp (Shopify's language server)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "ruby-lsp")
    :activation-fn (lsp-activate-on "ruby")
    :priority 1
    :server-id 'ruby-lsp
    :download-server-fn nil)))

(use-package! rspec-mode
  :hook ((ruby-mode . rspec-mode)
         (ruby-ts-mode . rspec-mode))
  :config
  ;; Use bundle exec for running RSpec
  (setq rspec-use-bundler-when-possible t)

  ;; Use rake for running specs (alternative to rspec command)
  (setq rspec-use-rake-when-possible nil)

  ;; Compilation mode settings for better output
  (setq compilation-scroll-output t))

(map! :localleader
      :map (ruby-mode-map ruby-ts-mode-map)
      (:prefix ("t" . "test/rspec")
       :desc "Run all specs"                    "a" #'rspec-verify-all
       :desc "Run current spec file"            "v" #'rspec-verify
       :desc "Run spec at point"                "s" #'rspec-verify-single
       :desc "Re-run last spec"                 "r" #'rspec-rerun
       :desc "Toggle between code and spec"     "t" #'rspec-toggle-spec-and-target
       :desc "Find spec file"                   "f" #'rspec-find-spec-file
       :desc "Toggle example pending"           "p" #'rspec-toggle-example-pendingness))

(use-package! rake
  :after ruby-mode
  :config
  ;; Use compilation mode for better output handling
  (setq rake-completion-system 'default))

(map! :localleader
      :map (ruby-mode-map ruby-ts-mode-map)
      (:prefix ("k" . "rake")
       :desc "Run rake task"                    "k" #'rake
       :desc "Rerun last rake task"             "r" #'rake-rerun
       :desc "Find and run rake task"           "f" #'rake-find-task
       :desc "Regenerate task cache"            "c" #'rake-regenerate-cache))

;; Enable projectile-rails-mode in tree-sitter Ruby buffers
;; Add hook directly without after! to ensure it runs before buffers are opened
(add-hook 'ruby-ts-mode-hook #'projectile-rails-mode)

(use-package! apheleia
  :config
  ;; Biome formatter using stdin/stdout to avoid "file changed on disk" prompts
  ;; Note: Uses 'format' subcommand (not 'check') and --stdin-file-path for config resolution
  (setf (alist-get 'biome apheleia-formatters)
        '("npx" "@biomejs/biome" "format" "--stdin-file-path" filepath))

  ;; TypeScript/TSX formatting with Biome
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'biome)
  (add-hook 'typescript-mode-hook #'apheleia-mode)
  (add-hook 'typescript-ts-mode-hook #'apheleia-mode)
  (add-hook 'tsx-ts-mode-hook #'apheleia-mode)

  ;; Ruby formatting with RuboCop
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)
  (add-hook 'ruby-mode-hook #'apheleia-mode)
  (add-hook 'ruby-ts-mode-hook #'apheleia-mode)

  (setq apheleia-formatters-respect-indent-level nil)
)

(after! lsp-mode
  (setq lsp-enable-on-type-formatting nil)  ;; Disable on-type formatting
  (setq lsp-signature-auto-activate nil)    ;; Disable signature help
  (setq lsp-modeline-code-actions-enable nil) ;; Disable code actions in modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Disable diagnostics in modeline
    (setq lsp-idle-delay 0.500)  ; Increase delay to half a second (default is 0.1)
    (setq lsp-enable-on-type-formatting nil)  ; Disable auto-formatting on typing
    (setq lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$"
            "[/\\\\]node_modules$"
            "[/\\\\]build$"
            "[/\\\\]dist$"))
    (setq lsp-file-watch-threshold 1000)  ;; Increase threshold to 1000 files
  (setq lsp-typescript-auto-import-completions nil) ;; Disable auto-imports
   (setq lsp-diagnostics-provider :flycheck)
        )

(map! :leader
      (:prefix ("c" . "+code")
       (:prefix-map ("l" . "+lsp")
        "r" #'lsp-javascript-remove-unused-imports)))

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook! 'compilation-mode-hook 'my-compilation-mode-hook)

(setq gptel-backend (gptel-make-openai "Venice"
                       :host "api.venice.ai"
                       :endpoint "/api/v1/chat/completions"
                       :stream t
                       :key (lambda () (auth-source-pick-first-password :host "venice.ai"))
                       :models '(
                        claude-opus-45
                        gpt-5.2
                        gemini-3-pro-preview
                        grok-4.1-fast
                        kimi-k2
                        deepseek-3.2
                        qwen3-235b
                        llama-3.3-70b
                        venice-uncensored
                        mistral-31-24b))
                    gptel-model "llama-3.3-70b")

(setq gptel-default-backend "Venice")

(defun my/gptel-context-add-folder (dir)
  "Add all files in DIR (recursively) to gptel context."
  (dolist (file (directory-files-recursively dir ".*" t))
    (when (file-regular-p file)
      (gptel-context-add-file file))))

(defun my/gptel-context-remove-all ()
  (let ((project-name (projectile-project-name))
        (project-root (projectile-project-root)))
    (gptel-context-remove-all)
    (cond
     ((string= project-name "eventtemple")
      (message "Setting up eventtemple BE project environment")
      (gptel-context-add-file (expand-file-name "ai-context.org" project-root))
      (my/gptel-context-add-folder (expand-file-name ".github/instructions" project-root))
      (find-file (expand-file-name "README.md" project-root)))

     ((string= project-name "eventtemple-frontend")
      (message "Setting up eventtemple FE project environment")
      (gptel-context-add-file (expand-file-name "pnpm-workspace.yaml" project-root))
      (gptel-context-add-file (expand-file-name "ai-context.org" project-root))
      (my/gptel-context-add-folder (expand-file-name ".github/instructions" project-root))
     )))
 )

(defun my/projectile-switch-project-action ()
  "Custom actions based on the project name or path."
  (let ((project-name (projectile-project-name))
        (project-root (projectile-project-root)))
    (gptel-context-remove-all)
    (cond
     ((string= project-name "eventtemple")
      (message "Setting up eventtemple BE project environment")
      (gptel-context-add-file (expand-file-name "ai-context.org" project-root))
      (my/gptel-context-add-folder (expand-file-name ".github/instructions" project-root))
      (find-file (expand-file-name "README.md" project-root)))

     ((string= project-name "eventtemple-frontend")
      (message "Setting up eventtemple FE project environment")
      (gptel-context-add-file (expand-file-name "pnpm-workspace.yaml" project-root))
      (gptel-context-add-file (expand-file-name "ai-context.org" project-root))
      (my/gptel-context-add-folder (expand-file-name ".github/instructions" project-root))
     )))
)

(add-hook 'projectile-after-switch-project-hook #'my/projectile-switch-project-action)

(map! :leader
      (:prefix ("o" . "open") "c" #'gptel)
      (:prefix ("l" . "GPT")
       "a" #'gptel-add
       "r" #'gptel-rewrite
       "m" #'gptel-menu
       "s" #'gptel-send
       "x" #'my/gptel-context-remove-all
       "a" #'gptel--rewrite-accept))

;; (use-package! gptel-magit
;;   :ensure t
;;   :hook (magit-mode . gptel-magit-install))





(use-package! elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map))
  (add-hook 'kill-emacs-hook #'elfeed-db-save)
  (run-at-time nil (* 8 60 60) #'elfeed-db-save)

  )
(setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
(setq elfeed-score-serde-score-file "/home/devindavis/.doom.d/score.el")
(map! :leader
      :prefix ("o" . "open")
      "r" #'elfeed)

(after! elfeed
  (map! :localleader
        :map elfeed-search-mode-map
        "u" #'elfeed-update
        "e" #'elfeed-score-explain
        "s" #'elfeed-search-set-filter
        "y" #'elfeed-search-yank
        "f" #'elfeed-search-live-filter
        "b" #'elfeed-search-browse-url))

(elfeed-search-set-filter  "@3-days-ago")

(defun my-elfeed-entries-last-3-days ()
  "Collect Elfeed entries from the past 3 days."
  (interactive)
  (let* ((now (float-time))
         (cutoff (- now (* 3 24 60 60)))
         (entries '()))
    (with-elfeed-db-visit (entry feed)
      (when (> (elfeed-entry-date entry) cutoff)
        (push entry entries)))
    entries))

(defun my-elfeed-format-entries (entries)
  "Format Elfeed ENTRIES into a plain text string with just the titles."
  (mapconcat
   (lambda (entry)
     (format "- %s" (elfeed-entry-title entry)))
   (nreverse entries)
   "\n"))

(defun my-elfeed-summarize-by-tag-org (days)
  "Summarize Elfeed entries from the past DAYS days, grouped by tag, in Org-mode format."
  (interactive (list (read-number "Days back: " 3)))
  (let* ((tag (completing-read "Tag: " (mapcar #'symbol-name (elfeed-db-get-all-tags)) nil t))
         (filter-str (format "@%d-days-ago +%s" days tag))
         entries)
    ;; Make list of enties
    (with-elfeed-db-visit (entry feed)
      (when (elfeed-search-filter (elfeed-search-parse-filter filter-str)
                                  entry feed nil)
        (push entry entries)))

    ;
    (setq entries (nreverse entries))

    (let ((buf (get-buffer-create "*elfeed-org-summary*"))
       ; Create the prompt
       (prompt (if entries
                      (format "Summary of these %d '%s'-tagged entries from the last %d days:\n\n%s. Provide only a point form list that summurizes in a couple sentences. Highlight the key elements using bold. Seperate each list item with a empty line. Make it highly readable. After each list entry insert the link to the entries."
                              (length entries) tag days
                              (my-elfeed-format-entries entries))
                    (format "No entries tagged '%s' in the last %d days." tag days))))

      ; Begin append buffer contents
      (with-current-buffer buf
        (org-mode)
        (read-only-mode -1)
        (erase-buffer)
        (insert prompt)
        (insert (format "#+TITLE: Elfeed Summary of Tag: %s\n#+DATE: %s\n\n"
                        tag
                        (format-time-string "%Y-%m-%d")))

        ; make a link entry for each article
        (when entries
          (insert "* Entries\n")
          (dolist (e entries)
            (let ((link (elfeed-entry-link e))
                  (title (org-no-properties (elfeed-entry-title e))))
          (insert (org-make-link-string link title))))
          (insert "\n"))

      ;; insert the fitler used
      (insert (format "- Filter syntax: `@%d‑days‑ago +%s`\n" days tag))

        (org-cycle '(64))
        (read-only-mode 1))


      (display-buffer buf)

      ;; Send to GPTel
      (if (> (length entries) 0)
      (gptel-request
       prompt
       :callback (lambda (response info)
                   (with-current-buffer buf
                     (read-only-mode -1)
                     (goto-char (point-max))
                     (insert "\n* Summary: ")
                     (insert "\n" (or response (format "No response; info: %S" info)))
                     (org-cycle '(64))
                     (read-only-mode 1)
                     (goto-char (point-min))
                     (display-buffer buf)))))))
  )

;;Docs: https://kubernetes-el.github.io/kubernetes-el/
(use-package! kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
    (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
    (map! :localleader
        :map kubernetes-overview-mode-map
        "s" #'kubernetes-display-service
        "p" #'kubernetes-display-pod
        "r" #'kubernetes-refresh
        "l" #'kubernetes-logs
        "e" #'kubernetes-edit
        "d" #'kubernetes-describe
        "n" #'kubernetes-set-namespace)
    (map! :leader
        :prefix "o"
        "k" #'kubernetes-overview)
 )

(setq! current-year-ledger-file "~/Documents/Personal/Finance/Banking/Ledger/2025.ledger")
(setq! ledger-schedule-file "~/Documents/Personal/Finance/Banking/Ledger/schedule.ledger")
(setq! ledger-default-journal "~/Documents/Personal/Finance/Banking/Ledger/2025.ledger")

(map! :localleader
      :map ledger-mode-map
      (:prefix ("r" . "reports")
       :desc "Balance report"           "b" #'ledger-report
       :desc "Register report"          "r" #'ledger-report
       :desc "Account report"           "a" #'ledger-report-goto
       :desc "Reconcile"                "c" #'ledger-reconcile)
      (:prefix ("t" . "toggle/transaction")
       :desc "Toggle pending"           "p" #'ledger-toggle-current-transaction
       :desc "Toggle cleared"           "c" #'ledger-toggle-current-transaction
       :desc "Delete transaction"       "d" #'ledger-delete-current-transaction
       :desc "Copy transaction"         "y" #'ledger-copy-transaction-at-point)
      (:prefix ("s" . "sort/schedule")
       :desc "Sort region"              "r" #'ledger-sort-region
       :desc "Sort buffer"              "b" #'ledger-sort-buffer
       :desc "Align transaction"        "a" #'evil-ledger-align)
      (:prefix ("i" . "insert")
       :desc "Add transaction"          "t" #'ledger-add-transaction
       :desc "Set effective date"       "d" #'ledger-set-effective-date)
      "." #'ledger-occur
      "n" #'ledger-navigate-next-xact-or-directive
      "p" #'ledger-navigate-prev-xact-or-directive)

(map! :leader
      :prefix "c"
      "R" #'projectile-replace)

(setq logview-additional-submodes
      '(("Pino JSON Logs"
         (format . "JSON")
         (levels . "level")
         (timestamp . "time"))))

(after! dirvish
  ;; Define quick-access bookmarks for frequently used directories
  (setq! dirvish-quick-access-entries
    `(("h" "~/"                        "Home")
      ("e" ,user-emacs-directory       "Emacs user directory")
      ("p" "~/WebDev/Projects"         "Projects")
      ("f" "~/Documents"               "Documents")
      ("d" "~/Downloads/"              "Downloads")
      ("m" "/mnt/"                     "Mounted drives")
      ("t" "~/.local/share/Trash/files/" "Trash"))))

;; Dirvish mode-specific keybindings
(map! :localleader
      :map dirvish-mode-map
      "R" #'query-replace              ; Replace in file names
      "w" #'wdired-change-to-wdired-mode) ; Enter writable dired mode

;; Global quick-access keybinding
(map! :leader
      "d" #'dirvish-quick-access)      ; Open quick-access menu

(use-package! claude-code
  :config
  ;; Use vterm as the terminal backend for better compatibility
  (setq claude-code-terminal-backend 'vterm)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))


;; Configure window display for Claude Code buffers using Doom's popup system
;; Opens Claude sessions in a right-side window at 45% width
(set-popup-rule! "^\\*claude:.+:.+\\*$"
  :side 'right
  :size 0.45
  :select t
  :quit nil
  :ttl nil)

;; Global leader keybindings for Claude Code
(map! :leader
      (:prefix ("l" . "++GPT")
        (:prefix-map ("c" . "claude-code")
          "c" #'claude-code                     ; Start/switch to Claude session
          "r" (lambda () (interactive)          ; Reset/interrupt Claude
                (claude-code-send-escape)
                (claude-code-send-escape))
          "o" #'claude-code-toggle              ; Toggle Claude window
          "u" #'claude-code-continue            ; Toggle Claude window
          "/" #'claude-code-slash-commands      ; Access slash commands
          "s" #'claude-code-send-command        ; Send command to Claude
          "b" #'claude-code-send-buffer         ; Send current buffer
          "k" #'claude-code-kill                ; Kill current session
          "K" #'claude-code-kill-all            ; Kill all sessions
          "RET" #'claude-code-send-return       ; Send return/continue
          "e" #'claude-code-send-escape         ; Send escape
          "l" #'claude-code-list-context)))     ; List context files

(use-package! claude-code
  :config
  ;; Enable Monet mode globally
  (monet-mode 1)

  ;; Hook Monet server startup into Claude Code's process lifecycle
  ;; This ensures the WebSocket server is available when Claude needs it
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)

  ;; Activate Claude Code mode
  (claude-code-mode))

(defun my/start-services (services)
  "Start multiple Prodigy SERVICES by name.
Opens the Prodigy buffer and starts each service in SERVICES list."
  (prodigy)
  (dolist (service-name services)
    (let ((service (prodigy-find-service service-name)))
      (if service
          (prodigy-start-service service)
        (message "Service '%s' not found" service-name)))))

(defun my/stop-services (services)
  "Stop multiple Prodigy SERVICES by name.
Opens the Prodigy buffer and stops each service in SERVICES list."
  (prodigy)
  (dolist (service-name services)
    (let ((service (prodigy-find-service service-name)))
      (if service
          (prodigy-stop-service service)
        (message "Service '%s' not found" service-name)))))

(defun my/restart-services (services)
  "Restart multiple Prodigy SERVICES by name.
Opens the Prodigy buffer and restarts each service in SERVICES list."
  (prodigy)
  (dolist (service-name services)
    (let ((service (prodigy-find-service service-name)))
      (if service
          (prodigy-restart-service service)
        (message "Service '%s' not found" service-name)))))

(defconst my/eventtemple-services
  '("core-web" "core-jobs" "frontends" "caddy")
  "Services for EventTemple: Rails backend, Sidekiq jobs, frontend, and Caddy proxy.")

(defconst my/portfolio-services
  '("portfolio-website")
  "Services for portfolio website development.")

(defconst my/farmers-map-services
  '("farmers-map")
  "Services for Farmers Truck Map development.")

(defun my/start-eventtemple-dev-environment ()
  "Start all EventTemple development services: Rails server, Sidekiq, frontend, and Caddy."
  (interactive)
  (my/start-services my/eventtemple-services))

(defun my/stop-eventtemple-dev-environment ()
  "Stop all EventTemple development services."
  (interactive)
  (my/stop-services my/eventtemple-services))

(defun my/restart-eventtemple-dev-environment ()
  "Restart all EventTemple development services."
  (interactive)
  (my/restart-services my/eventtemple-services))

(defun my/start-portfolio-dev-environment ()
  "Start portfolio website development server."
  (interactive)
  (my/start-services my/portfolio-services))

(defun my/stop-portfolio-dev-environment ()
  "Stop portfolio website development server."
  (interactive)
  (my/stop-services my/portfolio-services))

(defun my/restart-portfolio-dev-environment ()
  "Restart portfolio website development server."
  (interactive)
  (my/restart-services my/portfolio-services))

(defun my/start-farmers-map-dev-environment ()
  "Start Farmers Truck Map development server."
  (interactive)
  (my/start-services my/farmers-map-services))

(defun my/stop-farmers-map-dev-environment ()
  "Stop Farmers Truck Map development server."
  (interactive)
  (my/stop-services my/farmers-map-services))

(defun my/restart-farmers-map-dev-environment ()
  "Restart Farmers Truck Map development server."
  (interactive)
  (my/restart-services my/farmers-map-services))

(defun my/start-paisa-dev-environment ()
  "Start Paisa financial visualization server."
  (interactive)
  (my/start-services '("paisa")))

(defun my/stop-paisa-dev-environment ()
  "Stop Paisa financial visualization server."
  (interactive)
  (my/stop-services '("paisa")))

(defun my/restart-paisa-dev-environment ()
  "Restart Paisa financial visualization server."
  (interactive)
  (my/restart-services '("paisa")))

(after! prodigy
  :config
  (setq prodigy-view-buffer-maximum-size 10000
        prodigy-view-truncate-by-default t)

  ;; Rails backend server with debugging enabled
  (prodigy-define-service
    :name "core-web"
    :command "bundle"
    :args '("exec" "rails" "server")
    :cwd "~/Projects/eventtemple"
    :url "https://client.eventtempledev.com"
    :env '(("RUBY_DEBUG_SESSION_NAME" "core-web")
           ("RUBY_DEBUG_OPEN" "true"))
    :tags '(dev rails))

  ;; Sidekiq background job processor
  (prodigy-define-service
    :name "core-jobs"
    :command "bundle"
    :args '("exec" "sidekiq")
    :cwd "~/Projects/eventtemple"
    :env '(("RUBY_DEBUG_SESSION_NAME" "core-jobs")
           ("RUBY_DEBUG_OPEN" "true"))
    :tags '(dev rails))

  ;; Frontend development server with Node.js debugging
  (prodigy-define-service
    :name "frontends"
    :command "npm"
    :args '("run" "dev")
    :cwd "~/Projects/eventtemple-frontend"
    :url "https://app.eventtempledev.com"
    :env '(("NODE_OPTIONS" "--inspect"))
    :tags '(dev node))

  ;; Caddy reverse proxy for local HTTPS
  (prodigy-define-service
    :name "caddy"
    :command "caddy"
    :args '("run")
    :cwd "~/Projects/eventtemple"
    :tags '(dev))

(prodigy-define-service
  :name "portfolio-website"
  :command "npm"
  :args '("run" "develop")
  :cwd "~/WebDev/Projects/PersonalSite"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t
  :tags '(dev))

(prodigy-define-service
    :name "farmers-map"
    :command "npm"
    :args '("run" "dev")
    :cwd "~/WebDev/Projects/farmers-truck-map"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(dev))
)

(map! :leader
      :prefix ("r" . "+prodigy")
      (:prefix-map ("e" . "Event Temple")
        "s" #'my/start-eventtemple-dev-environment
        "x" #'my/stop-eventtemple-dev-environment
        "r" #'my/restart-eventtemple-dev-environment)
      (:prefix-map ("f" . "Farmers Truck Maps")
        "s" #'my/start-farmers-map-dev-environment
        "x" #'my/stop-farmers-map-dev-environment
        "r" #'my/restart-farmers-map-dev-environment)
      (:prefix-map ("p" . "Portfolio Website")
        "s" #'my/start-portfolio-dev-environment
        "x" #'my/stop-portfolio-dev-environment
        "r" #'my/restart-portfolio-dev-environment)
      (:prefix-map ("$" . "Paisa")
        "s" #'my/start-paisa-dev-environment
        "x" #'my/stop-paisa-dev-environment
        "r" #'my/restart-paisa-dev-environment)
      )

(defun my/magit-prompt-tag-on-master-push ()
  "Prompt to create a tag when pushing to the master branch."
  (when (and (equal (magit-get-current-branch) "master")
             (y-or-n-p "Pushing to master. Create a release tag? "))
    (call-interactively #'magit-tag-create)))

(add-hook 'magit-pre-push-hook #'my/magit-prompt-tag-on-master-push)

;; Global leader keybindings for Forge operations
(map! :leader
      (:prefix ("g" . "git")
        (:prefix-map ("f" . "forge")
          :desc "Forge dispatch"              "f" #'forge-dispatch
          :desc "Browse pull requests"        "p" #'forge-list-pullreqs
          :desc "Browse issues"               "i" #'forge-list-issues
          :desc "Browse topic"                "t" #'forge-browse-topic
          :desc "Browse remote"               "r" #'forge-browse-remote
          :desc "Browse commit"               "c" #'forge-browse-commit
          :desc "Browse branch"               "b" #'forge-browse-branch
          :desc "Browse issues & PRs"         "n" #'forge-list-notifications
          :desc "Copy URL at point"           "y" #'forge-copy-url-at-point-as-kill)))

;; Magit status buffer keybindings for Forge
(map! :map magit-status-mode-map
      :localleader
      (:prefix ("f" . "forge")
        :desc "Forge dispatch"                "f" #'forge-dispatch
        :desc "Pull forge data"               "y" #'forge-pull
        :desc "Create pull request"           "p" #'forge-create-pullreq
        :desc "Create issue"                  "i" #'forge-create-issue
        :desc "Checkout pull request"         "c" #'forge-checkout-pullreq
        :desc "Visit topic"                   "v" #'forge-visit-topic
        :desc "Browse topic"                  "b" #'forge-browse-topic
        :desc "Copy URL at point"             "Y" #'forge-copy-url-at-point-as-kill
        :desc "List pull requests"            "P" #'forge-list-pullreqs
        :desc "List issues"                   "I" #'forge-list-issues
        :desc "List notifications"            "n" #'forge-list-notifications))

(after! pdf
  (setq-default pdf-view-display-size 'fit-page)

  ;; PDF view mode keybindings
  (map! :map pdf-view-mode-map
        :n "j" #'pdf-view-next-line-or-next-page
        :n "k" #'pdf-view-previous-line-or-previous-page
        :n "J" #'pdf-view-next-page
        :n "K" #'pdf-view-previous-page
        :n "h" #'image-backward-hscroll
        :n "l" #'image-forward-hscroll
        :n "gg" #'pdf-view-first-page
        :n "G" #'pdf-view-last-page
        :n "gt" #'pdf-view-goto-page
        :n "d" #'pdf-view-scroll-up-or-next-page
        :n "u" #'pdf-view-scroll-down-or-previous-page
        :n "/" #'isearch-forward
        :n "?" #'isearch-backward
        :n "+" #'pdf-view-enlarge
        :n "-" #'pdf-view-shrink
        :n "0" #'pdf-view-scale-reset
        :n "W" #'pdf-view-fit-width-to-window
        :n "H" #'pdf-view-fit-height-to-window
        :n "P" #'pdf-view-fit-page-to-window
        :n "r" #'pdf-view-rotate
        :n "m" #'pdf-view-midnight-minor-mode)

  ;; Local leader keybindings for PDF operations
  (map! :localleader
        :map pdf-view-mode-map
        (:prefix ("a" . "annotations")
          "h" #'pdf-annot-add-highlight-markup-annotation
          "u" #'pdf-annot-add-underline-markup-annotation
          "s" #'pdf-annot-add-strikeout-markup-annotation
          "q" #'pdf-annot-add-squiggly-markup-annotation
          "t" #'pdf-annot-add-text-annotation
          "d" #'pdf-annot-delete
          "l" #'pdf-annot-list-annotations)
        (:prefix ("o" . "outline")
          "o" #'pdf-outline
          "i" #'pdf-outline-imenu)
        (:prefix ("s" . "search/slice")
          "s" #'pdf-occur
          "r" #'pdf-view-slice-to-region
          "R" #'pdf-view-reset-slice)
        "p" #'pdf-misc-print-document
        "m" #'pdf-view-midnight-minor-mode))

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;Add local development package to load-path
(let ((read-later-path "/home/devindavis/WebDev/Projects/read-later"))
  (when (file-exists-p read-later-path)
    (add-to-list 'load-path read-later-path)
    (require 'read-later)))

(after! notmuch
  (setq notmuch-hello-auto-refresh nil)
  
  (defun notmuch ()
    "Launch notmuch directly to unread inbox."
    (interactive)
    (notmuch-search "tag:inbox and tag:unread -tag:spam -tag:deleted -tag:sent -tag:draft -tag:sentry"))
  
  (setq notmuch-search-oldest-first nil
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-kill-buffer-on-exit t)
  
  (setq +notmuch-sync-backend 'mbsync)
  (setq +notmuch-mail-folder "~/Mail") 
  
  (setq notmuch-saved-searches
        '((:name "Inbox" :query "tag:inbox -tag:deleted -tag:sentry -tag:sent" :key "i")
          (:name "Unread" :query "tag:inbox and tag:unread -tag:deleted -tag:sentry -tag:sent" :key "u")
          (:name "All Mail" :query "*" :key "a")
          (:name "Finances" :query "tag:finance and -tag:deleted" :key "f")
          (:name "MyMail" :query "folder:mymail/** -tag:deleted" :key "m")
          (:name "Gmail" :query "folder:gmail/** -tag:deleted" :key "g")
          (:name "Deleted" :query "tag:deleted" :key "D")))
  
  (setq user-full-name "Devin Davis"
        user-mail-address "devin@devdeveloper.ca"))

(map! :map notmuch-search-mode-map
      :n "a" #'notmuch-search-add-tag
      :n "r" #'notmuch-search-remove-tag
      :n "J" #'notmuch-jump-search
      :n "gr" #'notmuch-refresh-this-buffer)
