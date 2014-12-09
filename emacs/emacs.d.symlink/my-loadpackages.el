; ~/.emacs.d/my-loadpackages.el
; loading package
(load "~/.emacs.d/my-packages.el")

(setq split-width-threshold 300)
(global-hl-line-mode +1)

;Tabs, 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;Font size
(set-face-attribute 'default nil :height 90)

;Enable ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Can use ~ to return to home dir in ido-find-file
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

(setq edbi:query-result-column-max-width 100)
(setq edbi:dbview-show-table-data-default-limit 10)
(autoload 'e2wm:dp-edbi "e2wm-edbi" nil t)
(require 'edbi)

(require 'windmove)

(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-B") 'windmove-left)

(require 'xclip)
(xclip-mode 1)
(setq x-select-enable-clipboard t)

(require 'paren)
;; ;; Show matching parentheses
(show-paren-mode 1)

(require 'yasnippet)
(yas/global-mode 1) ;; Move this line?

;; (require 'anything)
(require 'org-install)
(setq org-log-done t)

(load-library "postack")

;Predictive completion
(require 'pabbrev "pabbrev.el")
(global-pabbrev-mode)

(require 'zencoding-mode)
(zencoding-mode 1)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;Tramp: Apparently allows you to edit files remotely... let's see
(require 'tramp)
(setq tramp-default-method "scp")

;This should turn on linum by default
(require 'linum)
(global-linum-mode)

;This function allows you to toggle linum-mode
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)

;;PHP stuff
(require 'php-mode)

;;To use abbrev-mode, add lines like this:
  (add-hook 'php-mode-hook
    '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-jshint-load)
(setq jshint-configuration-path "/home/nathan/Dropbox/emacs/js-hint-config.json")

(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

;; uniquify changes conflicting buffer names from file<2> etc
(require 'uniquify)
;; uniquify changes conflicting buffer names from file<2> etc
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun current-dir-name ()
  (nth 1 (split-string (pwd) "Directory ")))

(require 'magit)
(global-set-key (kbd "C-x m")
  '(lambda () (interactive) (magit-status (current-dir-name))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
