;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; lein needs this when i do mx cider-jack-in in mac os x
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(better-defaults ;; https://github.com/technomancy/better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider ;; make sure to have a matching cider-nrepl version to go with cider
                      color-theme-solarized
                      multiple-cursors ;;https://github.com/magnars/multiple-cursors.el
                      ace-jump-mode ;;https://github.com/winterTTr/ace-jump-mode
                      expand-region
                      flycheck
                      auto-complete
                      helm-projectile ;; installs both helm and projectile at the same tiem
                      yasnippet
                      powerline-evil ;; installs both powerline and evil at the same time
                      key-chord ;;for remapping jk to escape like in EVIL / VIM from insert to normal mode
                      smartparens)) ;; https://github.com/Fuco1/smartparens/wiki

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (not package-archive-contents)
  (package-refresh-contents))

(load-theme 'solarized-dark t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(show-paren-mode t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo_for_Powerline" :foundry "outline" :slant normal :weight normal :height 160 :width normal)))))
;;(customize-variable (quote tab-stop-list))

;; quick access to init.el
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c j") 'helm-mini)

;; evil - but this does not play well with Multiple Cursors - no solution yet ;<
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

(require 'powerline-evil)
(powerline-center-evil-theme)

;; org mode for daily task mgt
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun kdby-create-dir-bef-save ()
  "function to create directory if not created and to be called before save"
  (interactive)
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (and (not (file-exists-p dir))
           (y-or-n-p (format "Directory %s does not exist, create it?" dir)))
      (make-directory dir t))))
(add-hook 'before-save-hook 'kdby-create-dir-bef-save)

;; TAB HYGIENE
;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to 
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)
;; (set-default tab-always-indent 1)

;; this sets tab to 4 in text mode for me
(defun kdby-tab-always-indent-for-textmode ()
  (progn
    (setq tab-always-indent t)
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq indent-line-function (quote insert-tab))))
(add-hook 'text-mode-hook 'kdby-tab-always-indent-for-textmode)

;; horizontal split window by default
(setq split-width-threshold 1)

;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; get more documentation for elisps
;; more options here: http://www.emacswiki.org/emacs/ElDoc
;; added context help function - neat!
(turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(defun rgr/toggle-context-help ()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it. A double toggle will make it reappear"
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (if (setq context-help (not context-help))
	(progn
	   (if (not (get-buffer-window (help-buffer)))
	       (display-buffer (help-buffer)))))
    (message "Context help %s" (if context-help "ON" "OFF"))))
;;
(defun rgr/context-help ()
  "Display function or variable at point in *Help* buffer if visible.
    Default behaviour can be turned off by setting the buffer local
    context-help to false"
  (interactive)
  (let ((rgr-symbol (symbol-at-point))) ; symbol-at-point http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
    (with-current-buffer (help-buffer)
      (unless (local-variable-p 'context-help)
        (set (make-local-variable 'context-help) t))
      (if (and context-help (get-buffer-window (help-buffer))
               rgr-symbol)
          (if (fboundp  rgr-symbol)
              (describe-function rgr-symbol) 
            (if (boundp  rgr-symbol) (describe-variable rgr-symbol)))))))
;;
(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag activate)
  (cond 
	((eq major-mode 'emacs-lisp-mode) (rgr/context-help) ad-do-it)
	((eq major-mode 'lisp-interaction-mode) (rgr/context-help) ad-do-it)
	((eq major-mode 'apropos-mode) (rgr/context-help) ad-do-it)
	(t ad-do-it)))

(global-set-key (kbd "C-c h") 'rgr/toggle-context-help)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; some problem with the brew version of emacs - graphical pop-up hangs emacs
;; suppress with the following ()
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
