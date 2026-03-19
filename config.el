;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq! avy-all-windows t)

;; Relative Line Numbers
(setq! display-line-numbers-type 'relative)

;; Disable line numbers in specific modes
(defun my/disable-line-numbers ()
  "Disable display-line-numbers-mode in the current buffer."
  (display-line-numbers-mode -1))
(add-hook! 'pdf-view-mode-hook #'my/disable-line-numbers)

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

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

; Mac Config
(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
(setenv "GIT_SSH_COMMAND" "ssh -v")

(when (eq system-type 'darwin)
  ;; Org clock sound via macOS afplay
  (setq! org-clock-sound t)
  (defvar dd/org-clock-sound-file (expand-file-name "ding.wav" doom-user-dir)
    "Path to the sound file played when an org clock timer finishes.")
  (defun dd/play-org-clock-sound ()
    "Play the org clock sound using macOS afplay."
    (when (file-exists-p dd/org-clock-sound-file)
      (start-process "org-clock-sound" nil "afplay" dd/org-clock-sound-file)))
  (add-hook! 'org-clock-out-hook #'dd/play-org-clock-sound)
  (add-hook! 'org-timer-done-hook #'dd/play-org-clock-sound)

  ;; Only show event_temple tasks in the global agenda todo list
  (setq! org-agenda-tag-filter-preset '("+event_temple"))

  ;; Only register EventTemple Prodigy services on macOS
  (after! prodigy
    (prodigy-define-service
      :name "core-web"
      :command "bundle"
      :args '("exec" "rails" "server")
      :cwd "~/Projects/eventtemple"
      :url "https://client.eventtempledev.com"
      :env '(("RUBY_DEBUG_SESSION_NAME" "core-web")
             ("RUBY_DEBUG_OPEN" "true"))
      :tags '(dev rails))

    (prodigy-define-service
      :name "core-jobs"
      :command "bundle"
      :args '("exec" "sidekiq")
      :cwd "~/Projects/eventtemple"
      :env '(("RUBY_DEBUG_SESSION_NAME" "core-jobs")
             ("RUBY_DEBUG_OPEN" "true"))
      :tags '(dev rails))

    (prodigy-define-service
      :name "frontends"
      :command "npm"
      :args '("run" "dev")
      :cwd "~/Projects/eventtemple-frontend"
      :url "https://app.eventtempledev.com"
      :env '(("NODE_OPTIONS" "--inspect"))
      :tags '(dev node))

    (prodigy-define-service
      :name "caddy"
      :command "caddy"
      :args '("run")
      :cwd "~/Projects/eventtemple"
      :tags '(dev))))

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

(setq sql-connection-alist
      '((farmers-truck-db
         (sql-product 'postgres)
         (sql-server "127.0.0.1")
         (sql-user "postgres")
         (sql-database "postgres")
         (sql-port 24464))
        (event-temple-db
         (sql-product 'postgres)
         (sql-server "127.0.0.1")
         (sql-user "postgres")
         (sql-database "postgres")
         (sql-port 5432))))

(setq! doom-themes-treemacs-theme "doom-colors")
(setq! treemacs-width 60)

(setq! default-abbrev-mode t)
(setq! abbrev-file-name "./abbrev.el")
(quietly-read-abbrev-file)

(setq! doom-font (font-spec :size 16))
(setq! doom-theme 'doom-snazzy)

(after! doom-themes
  (setq! doom-themes-enable-bold t)
  (setq! doom-themes-enable-italic t))

(custom-set-faces!
  '(line-number :foreground "#bbbbbb")
  '(line-number-current-line :foreground "#ffffff"))

(defvar-local my/branch-diff-stats-cache nil
  "Cached branch diff stats string for the current buffer.")

(defun my/git-default-branch ()
  "Determine the default branch (main or master) for the current repository."
  (cond
   ((= 0 (call-process "git" nil nil nil "rev-parse" "--verify" "refs/heads/main"))
    "main")
   ((= 0 (call-process "git" nil nil nil "rev-parse" "--verify" "refs/heads/master"))
    "master")
   (t nil)))

(defun my/git-branch-diff-stats ()
  "Compute total lines added/removed on the current branch vs the default branch.
Returns a formatted string like \"+42 -17\", or nil if not applicable."
  (when-let* ((default-directory (or (doom-project-root) default-directory))
              (default-branch (my/git-default-branch))
              (current-branch (magit-get-current-branch)))
    ;; Don't show stats when on the default branch itself
    (unless (string= current-branch default-branch)
      (let ((merge-base (string-trim
                         (shell-command-to-string
                          (format "git merge-base HEAD %s 2>/dev/null" default-branch)))))
        (when (and merge-base (not (string-empty-p merge-base)))
          (let ((numstat (shell-command-to-string
                          (format "git diff --numstat %s 2>/dev/null" merge-base)))
                (additions 0)
                (deletions 0))
            (dolist (line (split-string numstat "\n" t))
              (when (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
                (setq additions (+ additions (string-to-number (match-string 1 line))))
                (setq deletions (+ deletions (string-to-number (match-string 2 line))))))
            (when (or (> additions 0) (> deletions 0))
              (cons additions deletions))))))))

(defun my/branch-diff-update-cache ()
  "Update the branch diff stats cache for the current buffer."
  (setq my/branch-diff-stats-cache (my/git-branch-diff-stats)))

;; Refresh cache on save
(add-hook! 'after-save-hook #'my/branch-diff-update-cache)
;; Refresh cache on buffer switch
(add-hook! 'doom-switch-buffer-hook #'my/branch-diff-update-cache)
;; Refresh cache after magit operations
(add-hook! 'magit-post-refresh-hook #'my/branch-diff-update-cache)

(after! doom-modeline
  ;; Define the branch-diff segment
  (doom-modeline-def-segment branch-diff
    "Display total lines added/removed on the current branch vs default branch."
    (when-let ((stats my/branch-diff-stats-cache))
      (let ((additions (car stats))
            (deletions (cdr stats)))
        (concat
         " "
         (propertize (format "+%d" additions) 'face 'success)
         " "
         (propertize (format "-%d" deletions) 'face 'error)
         " "))))

  ;; Redefine the main modeline to include branch-diff after vcs
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs branch-diff check time)))

(setq! org-directory "~/my-org-roam/")

(use-package! org-protocol
  :after org)

(after! org
  (defun +org-src-company-setup ()
    "Enable company-mode in org-src blocks."
    (company-mode +1))
  (add-hook! 'org-src-mode-hook #'+org-src-company-setup)


  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-todo-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook! 'org-after-todo-statistics-hook #'org-summary-todo)
  )

(map! :map org-mode-map
      :localleader
      (:prefix ("l" . "insert link")
               "i" #'my/org-insert-info-link))

(map! :map org-mode-map
      :localleader
      (:prefix ("f" . "format")
               "i" #'org-indent-block))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq! org-modern-star 'replace
         org-modern-star-replace '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
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

(after! org
  (require 'ox-confluence))

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

(after! org
  (setq! org-agenda-todo-ignore-scheduled 'future)
  (setq! org-agenda-start-day "-1d")
  (setq! org-agenda-span 5)
  (setq! org-agenda-files '(
                            "~/my-org-roam/projects"
                            "~/my-org-roam/daily"
                            "~/my-org-roam/work-org-roam/daily"
                            "~/my-org-roam/work-org-roam/tickets"
                            "~/my-org-roam/sources"
                            "~/my-org-roam/mobile-notes"
                            "~/doom/config.org"
                            ))

  (defun org-get-title ()
    "Get the #+TITLE of the current buffer's file."
    (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
        (file-name-nondirectory (buffer-file-name))))

  (setq! org-agenda-prefix-format
         '((agenda . " %i %-12:c%?-12t% s")
           (todo . " %i %-12(org-get-title) ") 
           (tags . " %i %-12:c")
           (search . " %i %-12:c")))

  (setq! org-hide-emphasis-markers t)
  )

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

(after! org
  (setq! org-roam-directory "~/my-org-roam")
  (org-roam-db-autosync-mode))

(setq! org-roam-dailies-capture-templates
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

(setq! org-roam-capture-templates
      `(("g" "Generic" plain
         "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :%^{tag}:\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "- \n\n"))
                                     
         :unnarrowed t)
        ("t" "Ticket" plain
         "%?"
         :target (file+head "work-org-roam/tickets/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :ticket:event_temple\n"
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
                                     "* Checklist\n"))
         :unnarrowed t)
        ("p" "ET Project" plain
         "%?"
         :target (file+head "work-org-roam/projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :project:event_temple:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"
                                     "* Checklist\n"))
         :unnarrowed t)
        ("i" "Project" plain
         "%?"
         :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :project:\n\n"
                                     "* Description\n"
                                     "%^{Description}\n\n"))
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
                                     "- \n"))
         :unnarrowed t)
        ("P" "Person" plain
         "%?"
         :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}\n"
                                     "#+created: %U\n"
                                     "#+filetags: :%^{tag}: :person:\n\n"
                                     "* Description\n\n"
                                     "- \n"))
         :unnarrowed t)))

(setq! org-export-show-temporary-export-buffer nil)
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
(add-hook! 'after-save-hook #'my/org-to-md-on-save)

(defun invoice-ninga-get-api-token ()
  "Get the Invoice Ninja API token, fetching from auth-source if needed."
  (or invoice-ninga-api-token
      (setq invoice-ninga-api-token
            (auth-source-pick-first-password :host "invoice-ninga"))))

(let ((invoice-ninga-path "/home/devindavis/WebDev/Projects/invoice-ninga"))
  (when (file-exists-p invoice-ninga-path)
    (add-to-list 'load-path invoice-ninga-path)
    (require 'invoice-ninga)))

(add-hook! 'after-init-hook #'global-flycheck-mode)
;; Note: Biome handles linting through Apheleia integration
;; ESLint can still be used for projects that require it
(defun my/select-eslint-checker ()
  "Select javascript-eslint as the Flycheck checker."
  (flycheck-select-checker 'javascript-eslint))
(add-hook! 'typescript-mode-hook #'my/select-eslint-checker)

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

(use-package! winpulse
:config
(winpulse-mode +1)
  )

(use-package! web-mode
  :mode ("\\.ejs\\'" . web-mode)
  :config
  (setq! web-mode-content-types-alist
         '(("html" . "\\.ejs\\'")))
  (setq! web-mode-engines-alist
         '(("ejs" . "\\.ejs\\'"))))

(use-package! ripgrep
  :config
  (setq! ripgrep-arguments '("-C2")))

(after! projectile
  (setq! projectile-project-search-path '("~/WebDev/"))
  (setq projectile-mode-line "Projectile")
  (setq projectile-ignored-project-function #'file-remote-p)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-create-missing-test-or-implementation #'ignore)
  (setq projectile-dynamic-mode-line nil)

  (defun projectile-no-known-project (file)
    "Only prompt for local files."
    (unless (file-remote-p file)
      (projectile-ask-user-to-import-project file))))

(after! lsp-mode
  (setq lsp-warn-no-matched-clients nil)
  ;; Prevent LSP from starting on remote buffers
  (advice-add 'lsp :before-while
    (lambda (&rest _) (not (file-remote-p default-directory))))
  ;; Disconnect if LSP somehow starts on a remote buffer
  (add-hook 'lsp-after-open-hook
    (lambda ()
      (when (file-remote-p default-directory)
        (lsp-disconnect)))))

(after! lsp-mode
  ;; Disable rubocop-ls
  (setq! lsp-disabled-clients '(rubocop-ls))
  (setq! lsp-ruby-lsp-use-bundler t)
  (setq! lsp-ruby-lsp-formatter "auto")
  
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
  (setq! rspec-use-bundler-when-possible t)

  ;; Use rake for running specs (alternative to rspec command)
  (setq! rspec-use-rake-when-possible nil)

  ;; Compilation mode settings for better output
  (setq! compilation-scroll-output t))

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
  (setq! rake-completion-system 'default))

(map! :localleader
      :map (ruby-mode-map ruby-ts-mode-map)
      (:prefix ("k" . "rake")
       :desc "Run rake task"                    "k" #'rake
       :desc "Rerun last rake task"             "r" #'rake-rerun
       :desc "Find and run rake task"           "f" #'rake-find-task
       :desc "Regenerate task cache"            "c" #'rake-regenerate-cache))

;; Enable projectile-rails-mode in tree-sitter Ruby buffers
;; Add hook directly without after! to ensure it runs before buffers are opened
(add-hook! 'ruby-ts-mode-hook #'projectile-rails-mode)

(after! projectile-rails
  ;; Use compact Foo::Bar syntax instead of nested module declarations
  (defadvice! +projectile-rails-compact-class-a (main-definition name)
    "Use compact class syntax (Foo::Bar) instead of nested modules."
    :override #'projectile-rails--snippet-for-module
    (let ((class-name (s-join "::" (projectile-rails-classify name))))
      (format (concat main-definition "$2\nend") class-name))))

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
  (add-hook! 'typescript-mode-hook #'apheleia-mode)
  (add-hook! 'typescript-ts-mode-hook #'apheleia-mode)
  (add-hook! 'tsx-ts-mode-hook #'apheleia-mode)

  ;; Ruby formatting with RuboCop
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)
  (add-hook! 'ruby-mode-hook #'apheleia-mode)
  (add-hook! 'ruby-ts-mode-hook #'apheleia-mode)

  (setq! apheleia-formatters-respect-indent-level nil)
)

(after! lsp-mode
  (setq! lsp-enable-on-type-formatting nil)
  (setq! lsp-signature-auto-activate nil)
  (setq! lsp-modeline-code-actions-enable nil)
  (setq! lsp-modeline-diagnostics-enable nil)
  (setq! lsp-idle-delay 0.500)
  (setq! lsp-file-watch-ignored-directories
         '("[/\\\\]\\.git$"
           "[/\\\\]node_modules$"
           "[/\\\\]build$"
           "[/\\\\]dist$"))
  (setq! lsp-file-watch-threshold 1000)
  (setq! lsp-typescript-auto-import-completions nil)
  (setq! lsp-diagnostics-provider :flycheck)
  (setq! lsp-enable-suggest-server-download nil)
  (delete 'sql-mode lsp-language-id-configuration)

  )

(after! lsp-mode
  (add-hook 'lsp-before-open-hook
    (lambda ()
      (when (file-remote-p default-directory)
        (setq-local lsp-disabled-clients t)))))

(use-package! lsp-sqls
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(sql-mode . "sql"))
  (setq! lsp-sqls-server (expand-file-name "~/go/bin/sqls"))
  (setq! lsp-sqls-workspace-config-path "root"))  ; Use project .sqls/config.json

(add-hook! 'sql-mode-hook #'lsp!)

(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook! 'compilation-mode-hook #'my-compilation-mode-hook)

(after! company
  (setq! company-idle-delay 0.2)  ; Show completions after 0.2s
  (setq! company-minimum-prefix-length 2)  ; Trigger after 2 characters

  ;; Add file path completions for prog-mode and org-mode
  (set-company-backend! 'prog-mode 'company-capf 'company-files 'company-yasnippet)
  (set-company-backend! 'org-mode 'company-capf 'company-files 'company-yasnippet)

(add-hook 'prog-mode-hook (lambda () (add-to-list 'company-backends 'company-files)))
  )

(defun my-venice-api-key ()
  "Get Venice API key from 1Password."
  (auth-source-pick-first-password :host "venice.ai" :user "api_key"))

(setq! gptel-backend (gptel-make-openai "Venice"
                       :host "api.venice.ai"
                       :endpoint "/api/v1/chat/completions"
                       :stream t
                       :key #'my-venice-api-key
                       :models '(
                        llama-3.3-70b
                        mistral-7b-instruct
                        qwen-2.5-72b-instruct
                        deepseek-r1
                        claude-3-5-sonnet
                        gpt-4o-mini))
                    gptel-model 'llama-3.3-70b)

(setq! gptel-default-backend "Venice")

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
  (elfeed-score-enable)
  (map! :map elfeed-search-mode-map "=" elfeed-score-map)
  (add-hook! 'kill-emacs-hook #'elfeed-db-save)
  (run-at-time nil (* 8 60 60) #'elfeed-db-save)

  )
(setq! elfeed-search-print-entry-function #'elfeed-score-print-entry)
(setq! elfeed-score-serde-score-file "/home/devindavis/.doom.d/score.el")
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

(setq! logview-additional-submodes
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
  (setq! claude-code-terminal-backend 'vterm)
  (add-hook! 'vterm-mode-hook #'my/disable-line-numbers))


;; Configure window display for Claude Code buffers using Doom's popup system
;; Opens Claude sessions in a right-side window at 45% width
(set-popup-rule! "^\\*claude:.+:.+\\*$"
  :side 'right
  :size 0.45
  :select t
  :quit nil
  :ttl nil)

;; Add custom slash commands to the Claude Code transient menu
(after! claude-code
  (transient-append-suffix 'claude-code-slash-commands '(-1 -1)
    ["Custom Commands"
     ("F" "Full-context" (lambda () (interactive)
                           (let ((args (read-string "Full-context args: ")))
                             (claude-code--do-send-command
                              (if (string-empty-p args)
                                  "/full-context"
                                (concat "/full-context " args))))))]))

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
  (add-hook! 'claude-code-process-environment-functions
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
  (setq! prodigy-view-buffer-maximum-size 10000
         prodigy-view-truncate-by-default t)

(unless (eq system-type 'darwin)
  (prodigy-define-service
    :name "portfolio-website"
    :command "npm"
    :args '("run" "develop")
    :cwd "~/WebDev/Projects/PersonalSite"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(dev)))

(unless (eq system-type 'darwin)
    (prodigy-define-service
      :name "farmers-map"
      :command "npm"
      :args '("run" "dev")
      :cwd "~/WebDev/Projects/farmers-truck-map"
      :stop-signal 'sigkill
      :kill-process-buffer-on-stop t
      :tags '(dev)))
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

(add-hook! 'magit-pre-push-hook #'my/magit-prompt-tag-on-master-push)

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

;; Track PRs that are currently waiting for CI tests
(defvar my/forge-prs-awaiting-tests (make-hash-table :test 'equal)
  "Hash table tracking PR numbers that are awaiting CI test completion.")

(defun my/forge-get-base-branch ()
  "Determine the base branch (main or master) for the current repository."
  (or (magit-rev-verify "origin/main") "main"
      (magit-rev-verify "origin/master") "master"))

(defun my/forge-pr-awaiting-tests-p (pr-number)
  "Check if PR-NUMBER is currently awaiting CI tests."
  (gethash pr-number my/forge-prs-awaiting-tests))

(defun my/forge-mark-pr-awaiting-tests (pr-number)
  "Mark PR-NUMBER as awaiting CI tests."
  (puthash pr-number t my/forge-prs-awaiting-tests))

(defun my/forge-clear-pr-awaiting-tests (pr-number)
  "Clear the awaiting tests flag for PR-NUMBER."
  (remhash pr-number my/forge-prs-awaiting-tests))

(defun my/forge-pr-ci-status (owner name head-ref)
  "Check CI status for HEAD-REF in OWNER/NAME repository.
Returns `success' if all checks passed, `pending' if still running,
or `failure' if any check failed."
  (let* ((gh-command (format "gh api repos/%s/%s/commits/%s/check-runs --jq '.check_runs | if length == 0 then \"none\" elif all(.conclusion == \"success\") then \"success\" elif any(.status == \"in_progress\" or .status == \"queued\") then \"pending\" elif any(.conclusion == \"failure\" or .conclusion == \"cancelled\") then \"failure\" else \"pending\" end'"
                             owner name head-ref))
         (result (string-trim (shell-command-to-string gh-command))))
    (cond
     ((string= result "success") 'success)
     ((string= result "failure") 'failure)
     ((string= result "none") 'success)  ; No checks configured, safe to merge
     (t 'pending))))

(defun my/forge-get-pr-at-point ()
  "Get the pull request at point in forge buffers."
  (or (forge-pullreq-at-point)
      (forge-current-topic)))

(defun my/forge-pr-approved-p (owner name pr-number)
  "Check if PR PR-NUMBER in OWNER/NAME repository has been approved."
  (let* ((gh-command (format "gh api repos/%s/%s/pulls/%d/reviews --jq '[.[] | select(.state == \"APPROVED\")] | length'"
                             owner name pr-number))
         (result (string-trim (shell-command-to-string gh-command))))
    (and (string-match-p "^[0-9]+$" result)
         (> (string-to-number result) 0))))

(defun my/forge-smart-merge-by-data (owner name pr-number head-ref base-ref)
  "Core smart merge logic for a PR specified by its data.

OWNER and NAME identify the repository. PR-NUMBER is the pull request number.
HEAD-REF is the feature branch and BASE-REF is the target branch.

Returns a symbol indicating the result: `awaiting', `not-approved', `merged',
`conflict', or `error'."
  (cond
   ;; Check if PR is awaiting tests - query CI status and merge if ready
   ((my/forge-pr-awaiting-tests-p pr-number)
    (let ((ci-status (my/forge-pr-ci-status owner name head-ref)))
      (cond
       ((eq ci-status 'success)
        (my/forge-clear-pr-awaiting-tests pr-number)
        (message "PR #%d: CI checks passed. Squash merging..." pr-number)
        (let* ((pr-json (json-read-from-string
                         (string-trim (shell-command-to-string
                                       (format "gh pr view %d --json id,headRefOid" pr-number)))))
               (node-id (alist-get 'id pr-json))
               (head-oid (alist-get 'headRefOid pr-json))
               (merge-query (format "mutation { mergePullRequest(input: {pullRequestId: \"%s\", mergeMethod: SQUASH, expectedHeadOid: \"%s\"}) { pullRequest { number state } } }" node-id head-oid))
               (merge-result (string-trim (shell-command-to-string
                                           (format "gh api graphql -f query='%s' 2>&1" merge-query)))))
          (if (string-match-p "MERGED" merge-result)
              (progn
                (message "PR #%d: CI passed. Squash merged successfully." pr-number)
                'merged)
            (message "PR #%d: CI passed but merge failed: %s" pr-number merge-result)
            'error)))
       ((eq ci-status 'failure)
        (my/forge-clear-pr-awaiting-tests pr-number)
        (message "PR #%d: CI checks failed. Please fix the failures and try again." pr-number)
        'failure)
       (t
        (message "PR #%d: CI tests are still running. Run this command again once they complete." pr-number)
        'awaiting))))
   ;; Check if PR has been approved
   ((not (my/forge-pr-approved-p owner name pr-number))
    (message "PR #%d: This pull request has not been approved. Please obtain approval before merging." pr-number)
    'not-approved)
   ;; Proceed with update and merge workflow
   (t
    (let* ((gh-command (format "gh api repos/%s/%s/merges -f base=%s -f head=%s -f commit_message='Merge %s into %s to update PR' 2>&1"
                               owner name head-ref base-ref base-ref head-ref)))
      (message "PR #%d: Merging %s into %s..." pr-number base-ref head-ref)
      (let ((result (string-trim (shell-command-to-string gh-command))))
        (cond
         ;; Successful merge - response contains commit sha
         ((string-match-p "\"sha\"" result)
          (my/forge-mark-pr-awaiting-tests pr-number)
          (message "PR #%d: Base branch merged. CI tests are now running. Run this command again once tests pass to complete the merge." pr-number)
          'merged)
         ;; Empty response means branch is already up to date (204 No Content)
         ;; No new merge commit means no CI triggered, so safe to merge directly
         ;; Uses gh api graphql directly to get a fresh headRefOid and merge synchronously,
         ;; since forge-merge is async and its cached head OID may be stale.
         ((string-empty-p result)
          (let* ((pr-json (json-read-from-string
                           (string-trim (shell-command-to-string
                                         (format "gh pr view %d --json id,headRefOid" pr-number)))))
                 (node-id (alist-get 'id pr-json))
                 (head-oid (alist-get 'headRefOid pr-json))
                 (merge-query (format "mutation { mergePullRequest(input: {pullRequestId: \"%s\", mergeMethod: SQUASH, expectedHeadOid: \"%s\"}) { pullRequest { number state } } }" node-id head-oid))
                 (merge-result (string-trim (shell-command-to-string
                                             (format "gh api graphql -f query='%s' 2>&1" merge-query)))))
            (if (string-match-p "MERGED" merge-result)
                (progn
                  (message "PR #%d: Already up to date with %s. Squash merged." pr-number base-ref)
                  'merged)
              (message "PR #%d: Already up to date but merge failed: %s" pr-number merge-result)
              'error)))
         ;; Merge conflict
         ((string-match-p "merge conflict\\|cannot be merged\\|Merge conflict" result)
          (message "PR #%d: Merge conflict detected. Please resolve conflicts manually." pr-number)
          'conflict)
         ;; Any other error
         (t
          (message "PR #%d: Failed to merge base branch. Error: %s" pr-number result)
          'error)))))))

(defun my/forge-smart-merge ()
  "Smart merge the pull request at point.

Copies formatted PR details to the clipboard for Slack, then delegates
to `my/forge-smart-merge-by-data' using the PR at point in forge buffers."
  (interactive)
  (if-let* ((pr (my/forge-get-pr-at-point))
            (pr-number (oref pr number))
            (repo (forge-get-repository pr))
            (owner (oref repo owner))
            (name (oref repo name))
            (head-ref (oref pr head-ref))
            (base-ref (oref pr base-ref)))
      (progn
        ;; Copy formatted PR details to clipboard for Slack
        (let ((pr-alist `((title . ,(oref pr title))
                          (number . ,pr-number)
                          (headRefName . ,head-ref)
                          (url . ,(format "https://github.com/%s/%s/pull/%d" owner name pr-number))
                          (body . ,(oref pr body)))))
          (kill-new (my/forge-format-pr pr-alist))
          (message "Copied PR #%d to clipboard." pr-number))
        (my/forge-smart-merge-by-data owner name pr-number head-ref base-ref))
    (message "No pull request found at point.")))

(defun my/forge-complete-merge ()
  "Complete the merge after CI tests have passed.

Use this after my/forge-smart-merge has updated the PR and tests have completed."
  (interactive)
  (if-let* ((pr (my/forge-get-pr-at-point))
            (pr-number (oref pr number)))
      (progn
        (my/forge-clear-pr-awaiting-tests pr-number)
        (forge-merge pr 'squash)
        (message "PR #%d: Merge initiated." pr-number))
    (message "No pull request found at point.")))

(defun my/forge-reset-pr-status ()
  "Reset the awaiting tests status for a PR.

Use this if you need to retry the smart merge workflow."
  (interactive)
  (if-let* ((pr (my/forge-get-pr-at-point))
            (pr-number (oref pr number)))
      (progn
        (my/forge-clear-pr-awaiting-tests pr-number)
        (message "PR #%d: Status reset. You can now run smart-merge again." pr-number))
    (message "No pull request found at point.")))

;; Add smart merge keybindings to forge topic mode (PR view)
(map! :after forge
      :map forge-topic-mode-map
      :localleader
      (:prefix ("m" . "merge")
        :desc "Smart merge (update + wait for CI)" "m" #'my/forge-smart-merge
        :desc "Complete merge (after CI passes)"   "c" #'my/forge-complete-merge
        :desc "Reset PR merge status"              "r" #'my/forge-reset-pr-status))

;; Add to global forge prefix as well
(map! :leader
      (:prefix ("g" . "git")
        (:prefix ("f" . "forge")
          :desc "Smart merge PR"     "m" #'my/forge-smart-merge)))

(defun my/forge-extract-jira-url (body)
  "Extract the first Jira URL from PR BODY text, or nil if none found."
  (when (and body (string-match "https://[^ \n]*atlassian\\.net/browse/[A-Z]+-[0-9]+" body))
    (match-string 0 body)))

(defun my/forge-format-pr (pr)
  "Format a single PR alist into a Markdown deployment message.
PR should contain `title', `number', `headRefName', `url', and `body' keys."
  (let* ((title (alist-get 'title pr))
         (number (alist-get 'number pr))
         (branch (alist-get 'headRefName pr))
         (url (alist-get 'url pr))
         (jira-url (my/forge-extract-jira-url (alist-get 'body pr))))
    (concat (format "*%s* (#%d)\n- Branch: `%s`\n- URL: %s"
                    title number branch url)
            (when jira-url (format "\n- Jira: %s" jira-url)))))

(defun my/forge-copy-pr-urls ()
  "Copy formatted PR details of active (open) pull requests to the kill ring.

Lists open PRs by branch name and allows selecting one or all.
Copies a Markdown-formatted message with title, number, branch, and URL.
Requires the `gh` CLI to be installed and authenticated."
  (interactive)
  (let* ((json-output (string-trim
                       (shell-command-to-string
                        "gh pr list --state open --author @me --json url,headRefName,title,number,body")))
         (prs (condition-case nil
                  (json-read-from-string json-output)
                (error nil))))
    (if (or (null prs) (= (length prs) 0))
        (message "No open pull requests found.")
      (let* ((pr-alist (mapcar (lambda (pr)
                                 (cons (alist-get 'headRefName pr) pr))
                               prs))
             ;; Build candidates: "All" option + individual branch names
             (candidates (append '("All") (mapcar #'car pr-alist)))
             (selection (completing-read "Copy PR URL: " candidates nil t)))
        (if (string= selection "All")
            (let ((formatted (string-join
                              (mapcar (lambda (entry) (my/forge-format-pr (cdr entry)))
                                      pr-alist)
                              "\n\n")))
              (kill-new formatted)
              (message "Copied %d formatted PRs to kill ring." (length pr-alist)))
          (let ((pr (cdr (assoc selection pr-alist))))
            (kill-new (my/forge-format-pr pr))
            (message "Copied formatted PR: %s" (alist-get 'title pr))))))))

;; Add keybinding for copying PR URLs under the forge prefix
(map! :leader
      (:prefix ("g" . "git")
        (:prefix ("f" . "forge")
          :desc "Copy active PR URLs"  "u" #'my/forge-copy-pr-urls)))

(defconst my/forge-review-request-greetings
  '("Hey, could someone take a look at this when you get a chance?"
    "Got a PR up for review."
    "This one's ready for review whenever someone has time."
    "PR ready for review, let me know if you have questions."
    "Hey, this is up for review."
    "Could use a review on this one when you're free."
    "PR is ready to go, would appreciate a look."
    "Hey, got one ready for review."
    "This is good to go for review."
    "Ready for review, happy to walk through anything if needed.")
  "Pool of casual greetings for PR review request messages.")

(defconst my/forge-risk-thresholds
  '((low . 100) (medium . 500))
  "Line-count thresholds for auto-computing PR risk level.
PRs with total changes below `low' are Low risk, below `medium' are Medium,
otherwise High.")

(defconst my/forge-test-file-patterns
  '("test" "spec" "_test\\." "\\.test\\." "__tests__" "tests/")
  "Patterns that identify test files in PR file lists.")

(defun my/forge-extract-body-section (body heading)
  "Extract content under a ## HEADING section from PR BODY.
Returns trimmed text between the heading and the next ## heading or end of body.
Returns nil if the section is not found or empty."
  (when body
    (let ((start-re (concat "^## " (regexp-quote heading) "[^\n]*\n")))
      (when (string-match start-re body)
        (let* ((content-start (match-end 0))
               (content-end (if (string-match "^## " body content-start)
                                (match-beginning 0)
                              (length body)))
               (content (string-trim (substring body content-start content-end))))
          (unless (string-empty-p content) content))))))

(defun my/forge-compute-pr-size (additions deletions changed-files)
  "Format PR size as '+ADDITIONS/-DELETIONS lines across CHANGED-FILES files'."
  (format "+%d/-%d lines across %d file%s"
          additions deletions changed-files
          (if (= changed-files 1) "" "s")))

(defun my/forge-compute-risk-level (additions deletions)
  "Compute a risk level string based on total line changes.
Uses `my/forge-risk-thresholds' to determine Low/Medium/High."
  (let ((total (+ additions deletions)))
    (cond
     ((< total (alist-get 'low my/forge-risk-thresholds)) "Low")
     ((< total (alist-get 'medium my/forge-risk-thresholds)) "Medium")
     (t "High"))))

(defun my/forge-file-is-test-p (path)
  "Return non-nil if PATH matches any pattern in `my/forge-test-file-patterns'."
  (cl-some (lambda (pat) (string-match-p pat path))
           my/forge-test-file-patterns))

(defun my/forge-compute-focus-areas (files)
  "Return a formatted string of top 3 non-test files by change count.
FILES is a vector of alists with `path', `additions', and `deletions' keys.
Returns nil if no non-test files are found."
  (let* ((file-list (append files nil))
         (non-test (cl-remove-if
                    (lambda (f) (my/forge-file-is-test-p (alist-get 'path f)))
                    file-list))
         (sorted (sort non-test
                       (lambda (a b)
                         (> (+ (alist-get 'additions a) (alist-get 'deletions a))
                            (+ (alist-get 'additions b) (alist-get 'deletions b))))))
         (top (cl-subseq sorted 0 (min 3 (length sorted)))))
    (when top
      (mapconcat (lambda (f)
                   (format "`%s` (+%d/-%d)"
                           (file-name-nondirectory (alist-get 'path f))
                           (alist-get 'additions f)
                           (alist-get 'deletions f)))
                 top ", "))))

(defun my/forge-copy-pr-review-request ()
  "Copy an enriched review request message with PR details to the kill ring.

Selects a PR from the list of open PRs, fetches detailed metrics via `gh',
auto-computes Size, Risk, and Focus areas, parses What/Testing from the PR
body, and prompts for editable fields with auto-populated defaults.
Requires the `gh` CLI to be installed and authenticated."
  (interactive)
  (let* ((json-output (string-trim
                       (shell-command-to-string
                        "gh pr list --state open --author @me --json url,headRefName,title,number,body")))
         (prs (condition-case nil
                  (json-read-from-string json-output)
                (error nil))))
    (if (or (null prs) (= (length prs) 0))
        (message "No open pull requests found.")
      (let* ((pr-alist (mapcar (lambda (pr)
                                 (cons (alist-get 'headRefName pr) pr))
                               prs))
             (candidates (mapcar #'car pr-alist))
             (selection (completing-read "Review request for PR: " candidates nil t))
             (pr (cdr (assoc selection pr-alist)))
             (pr-number (alist-get 'number pr))
             ;; Fetch detailed metrics for the selected PR
             (detail-json (string-trim
                           (shell-command-to-string
                            (format "gh pr view %d --json additions,deletions,changedFiles,files,body"
                                    pr-number))))
             (details (condition-case nil
                          (json-read-from-string detail-json)
                        (error nil)))
             (additions (or (alist-get 'additions details) 0))
             (deletions (or (alist-get 'deletions details) 0))
             (changed-files (or (alist-get 'changedFiles details) 0))
             (files (alist-get 'files details))
             (detail-body (alist-get 'body details))
             ;; Auto-compute fields
             (size-str (my/forge-compute-pr-size additions deletions changed-files))
             (risk-default (my/forge-compute-risk-level additions deletions))
             (focus-str (my/forge-compute-focus-areas files))
             (what-default (or (my/forge-extract-body-section detail-body "Description")
                               (alist-get 'title pr)))
             (testing-default (or (my/forge-extract-body-section detail-body "How to test") ""))
             ;; Prompt user with auto-populated defaults
             (what (read-string "What: " what-default))
             (risk (read-string "Risk: " risk-default))
             (testing (read-string "Testing: " testing-default))
             (priority (completing-read "Priority: " '("Normal" "High" "Urgent") nil t))
             ;; Build the enriched message
             (greeting (nth (random (length my/forge-review-request-greetings))
                            my/forge-review-request-greetings))
             (base (my/forge-format-pr pr))
             (enriched (concat greeting "\n\n" base
                               (format "\n- What: %s" what)
                               (format "\n- Size: %s" size-str)
                               (format "\n- Risk: %s" risk)
                               (when (not (string-empty-p testing))
                                 (format "\n- Testing: %s" testing))
                               (when focus-str
                                 (format "\n- Focus areas: %s" focus-str))
                               (format "\n- Priority: %s" priority))))
        (kill-new enriched)
        (message "Copied enriched review request for: %s" (alist-get 'title pr))))))

;; Add keybinding for copying PR review request under the forge prefix
(map! :leader
      (:prefix ("g" . "git")
        (:prefix ("f" . "forge")
          :desc "Copy PR review request" "R" #'my/forge-copy-pr-review-request)))

(defun my/forge-select-and-merge-prs ()
  "Select active PRs from a list and smart-merge them.

Fetches open PRs via `gh', presents them by branch name, and runs the
smart merge workflow on the selected PR or all of them."
  (interactive)
  (let* ((repo-json (condition-case nil
                        (json-read-from-string
                         (string-trim (shell-command-to-string
                                       "gh repo view --json owner,name")))
                      (error nil)))
         (owner (when repo-json (alist-get 'login (alist-get 'owner repo-json))))
         (name (when repo-json (alist-get 'name repo-json))))
    (unless (and owner name)
      (user-error "Could not determine repository owner/name. Are you in a GitHub repo?"))
    (let* ((json-output (string-trim
                         (shell-command-to-string
                          "gh pr list --state open --author @me --json number,headRefName,baseRefName")))
           (prs (condition-case nil
                    (json-read-from-string json-output)
                  (error nil))))
      (if (or (null prs) (= (length prs) 0))
          (message "No open pull requests found.")
        (let* ((pr-alist (mapcar (lambda (pr)
                                   (cons (alist-get 'headRefName pr) pr))
                                 prs))
               (candidates (append '("All") (mapcar #'car pr-alist)))
               (selection (completing-read "Smart merge PR: " candidates nil t))
               (selected-prs (if (string= selection "All")
                                 (mapcar #'cdr pr-alist)
                               (list (cdr (assoc selection pr-alist))))))
          (dolist (pr selected-prs)
            (let ((pr-number (alist-get 'number pr))
                  (head-ref (alist-get 'headRefName pr))
                  (base-ref (alist-get 'baseRefName pr)))
              (my/forge-smart-merge-by-data owner name pr-number head-ref base-ref))))))))

;; Add keybinding for select-and-merge under the forge prefix
(map! :leader
      (:prefix ("g" . "git")
        (:prefix ("f" . "forge")
          :desc "Select & smart merge PRs" "M" #'my/forge-select-and-merge-prs)))

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

(map! "M-/" #'hippie-expand)

(setq! hippie-expand-try-functions-list
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

;; Load from local development path (Linux)
(let ((read-later-path "/home/devindavis/WebDev/Projects/read-later.el"))
  (when (file-exists-p read-later-path)
    (add-to-list 'load-path read-later-path)
    (require 'read-later)))

(setq read-later-api-auth-backend '1password)
(setq read-later-api-host "Instapaper")

;; Load from local development path (macOS)
(let ((read-later-path "/Users/devindavis/Projects/read-later.el"))
  (when (file-exists-p read-later-path)
    (add-to-list 'load-path read-later-path)
    (require 'read-later)))

(after! notmuch
  (setq! notmuch-hello-auto-refresh nil)
  
  (defun notmuch ()
    "Launch notmuch directly to unread inbox."
    (interactive)
    (let ((saved-searches '((unread . "tag:inbox and tag:unread -tag:deleted -tag:sentry -tag:github -tag:sent -tag:atlassian -tag:slack -tag:pganalyze"))))
      (notmuch-search (alist-get 'unread saved-searches))))
  
  (setq! notmuch-search-oldest-first nil
         message-send-mail-function 'message-send-mail-with-sendmail
         sendmail-program "msmtp"
         message-kill-buffer-on-exit t)
  
  (setq! +notmuch-sync-backend 'mbsync)
  (setq! +notmuch-mail-folder "~/Mail")
  
  (setq! notmuch-saved-searches
        '((:name "Inbox" :query "tag:inbox -tag:deleted -tag:sentry -tag:sent" :key "i")
          (:name "Unread" :query "tag:inbox and tag:unread -tag:deleted -tag:sentry -tag:github -tag:sent -tag:atlassian -tag:slack -tag:pganalyze" :key "u")
          (:name "All Mail" :query "*" :key "a")
          (:name "Finances" :query "tag:finance and -tag:deleted" :key "f")
          (:name "MyMail" :query "folder:mymail/** -tag:deleted" :key "m")
          (:name "Gmail" :query "folder:gmail/** -tag:deleted" :key "g")
          (:name "Deleted" :query "tag:deleted" :key "D")))

  (setq! user-full-name "Devin Davis"
         user-mail-address "devin@devdeveloper.ca"))

(map! :map notmuch-search-mode-map
      :n "a" #'notmuch-search-add-tag
      :n "r" #'notmuch-search-remove-tag
      :n "J" #'notmuch-jump-search
      :n "gr" #'notmuch-refresh-this-buffer)

(setq! jiralib-url "https://eventtemple.atlassian.net")
(after! org-jira
  (map! :map org-jira-mode-map
        :localleader
        (:prefix ("j" . "jira")
         ;; Projects
         (:prefix ("p" . "projects")
          :desc "Get projects"                    "g" #'org-jira-get-projects)
         ;; Issues
         (:prefix ("i" . "issues")
          :desc "Browse issue"                    "b" #'org-jira-browse-issue
          :desc "Get issues"                      "g" #'org-jira-get-issues
          :desc "Get issues from JQL"             "j" #'org-jira-get-issues-from-custom-jql
          :desc "Get issues (head only)"          "h" #'org-jira-get-issues-headonly
          :desc "Get issues by fix version"       "f" #'org-jira-get-issues-by-fixversion
          :desc "Update issue labels"             "l" #'org-jira-update-issue-labels
          :desc "Update issue"                    "u" #'org-jira-update-issue
          :desc "Progress issue"                  "w" #'org-jira-progress-issue
          :desc "Progress issue next"             "n" #'org-jira-progress-issue-next
          :desc "Assign issue"                    "a" #'org-jira-assign-issue
          :desc "Refresh issue"                   "r" #'org-jira-refresh-issue
          :desc "Refresh issues in buffer"        "R" #'org-jira-refresh-issues-in-buffer
          :desc "Create issue"                    "c" #'org-jira-create-issue
          :desc "Copy issue key"                  "k" #'org-jira-copy-current-issue-key)
         ;; Subtasks
         (:prefix ("s" . "subtasks")
          :desc "Create subtask"                  "c" #'org-jira-create-subtask
          :desc "Get subtasks"                    "g" #'org-jira-get-subtasks)
         ;; Comments
         (:prefix ("c" . "comments")
          :desc "Add comment"                     "c" #'org-jira-add-comment
          :desc "Update comment"                  "u" #'org-jira-update-comment)
         ;; Worklogs
         (:prefix ("w" . "worklogs")
          :desc "Update worklogs from clocks"     "u" #'org-jira-update-worklogs-from-org-clocks)
         ;; Todo sync
         :desc "Sync todo to Jira"                "t" #'org-jira-todo-to-jira)))

(use-package! auth-source-1password
  :after auth-source
  :config
  (setq auth-source-1password-vault "Private"
        auth-source-1password-field "password")  ; Add this line
  (auth-source-1password-enable))

(setq! elfeed-summary-settings
'((tag-groups (:repeat-feeds t))))


