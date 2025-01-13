;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file

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

;;BICEP MODE
;; (use-package! "bicep-mode"
  ;;:load-path "path/to/bicep-mode")

;;Shortcuts
(map! "C-c r"   #'eshell-isearch-backward)
(map! :leader
      :prefix ("o" . "open")
      "c" #'gptel)

;;PRETTIER CONFIG
(use-package! prettier
  :hook (
         (js-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (css-mode . prettier-mode)
         (html-mode . prettier-mode)
         (markdown-mode . prettier-mode)))

;;QC CLIENT CONFIG
(use-package! counsel-jq)

;;ORG MODE CONFIG
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)

(setq org-agenda-files '(
        "~/org/inbox.org"
        "~/org/warriertech.org"
        "~/org/personal.org"
))

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

(setq org-agenda-start-with-follow-mode t)
(setq display-line-numbers-type 'relative)

;;COMPILER CONFIGURATION
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook! 'compilation-mode-hook 'my-compilation-mode-hook)

;; LSP Config
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

;; Chat GPT 
(setq gpt-api-key (getenv "CHAT_GPT_API_KEY"))
(use-package! gptel
 :config
 (setq! gptel-api-key gpt-api-key))

;; Elfeed
(use-package! elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; Some functionality uses this to identify your e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
