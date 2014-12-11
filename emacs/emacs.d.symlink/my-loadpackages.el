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

(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-B") 'windmove-left)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(show-paren-mode 1)
(setq org-log-done t)

(load-library "postack")

(require 'pabbrev)
(global-pabbrev-mode)

(zencoding-mode 1)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(global-linum-mode)

;;To use abbrev-mode, add lines like this:
(add-hook 'php-mode-hook
  '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'php-mode-hook 'flymake-php-load)

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

(defun current-dir-name ()
  (nth 1 (split-string (pwd) "Directory ")))

(global-set-key (kbd "C-x m")
  '(lambda () (interactive) (magit-status (current-dir-name))))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))

(global-set-key "\M-\C-y" 'kill-ring-search)
