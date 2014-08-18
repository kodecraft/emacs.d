;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)


;; maximise emacs in windows
;; (defun jbr-init ()
;;   "Called from term-setup-hook after the default
;; terminal setup is
;; done or directly from startup if term-setup-hook not
;; used.  The value
;; 0xF030 is the command for maximizing a window."
;;   (interactive)
;;   (w32-send-sys-command #xf030)
;;   (ecb-redraw-layout)
;;   (calendar)
;; )
;; (setq term-setup-hook 'jbr-init)
;; (setq window-setup-hook 'jbr-init)


(setq ring-bell-function 'ignore)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      color-theme-solarized))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'solarized-dark t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo_for_Powerline" :foundry "outline" :slant normal :weight normal :height 160 :width normal)))))
