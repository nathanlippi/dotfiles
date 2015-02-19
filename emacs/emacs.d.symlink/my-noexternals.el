(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; ; Lets emacs uncompress .gz files before opening them
(auto-compression-mode 1)

;; ;; keep backup files neatly out of the way in .~/
(setq backup-directory-alist '(("." . ".~")))

;; ;Set prompts to one letter
(fset 'yes-or-no-p 'y-or-n-p)

;Change the order of file extensions
(setq ido-file-extensions-order '(".js" ".php" ".py" ".emacs"))

;; ;Start emacs without so much fanfare
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ;; You can delete over a selection
;; ;; Useful if you want to delete w/o keeping text in kill-ring
(delete-selection-mode 1)

(add-to-list 'same-window-buffer-names "*SQL*")

;; http://stackoverflow.com/questions/11623189/how-to-bind-keys-to-indent-unindent-region-in-emacs
(defun my-indent-region (spaces)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) spaces)
             (setq deactivate-mark nil))
      (self-insert-command 1)))

(global-set-key (kbd ">") '(lambda () (interactive) (my-indent-region 2)))
(global-set-key (kbd "<") '(lambda () (interactive) (my-indent-region -2)))

(global-set-key "\M-R" 'align-regexp)

; Make searches case insensitive (if lowercase)
(setq case-fold-search t)

;These functions remove a lot of the UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;just use y or n for confirmations
(defalias 'yes-or-no-p 'y-or-n-p)

(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; This does not seem to work reliably in messy code
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        ;; Nathan Lippi: What about something that matches quotes?
        (t nil)))

; Make sure emacs isn't accidentally killed
(defun paranoid-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))

;Duplicate a whole line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank))

;; Inspired by org mode -- My first REAL (with a tree of logic) elisp function
;; TODO: If there is a non-empty line below, insert a line
(defun duplicate-line-above ()
  (interactive)
  (setq lineContent (chomp (thing-at-point 'line)))
  (if (string= lineContent "")
      (previous-line))
  (setq lineToCopy (thing-at-point 'line))
  (move-end-of-line 1)
  (next-line)
  (insert lineToCopy)
  (move-beginning-of-line 1))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun delete-backward-sexp ()
  (interactive)
  (delete-sexp nil))

(defun delete-sexp (forward-p)
  (let ((beg (point))) (if forward-p (forward-sexp) (backward-sexp))
                                                    (kill-region beg (point))))

(defun kill-region-or-word ()
  "Call `kill-region’ or `backward-kill-word’ depending on whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
  (kill-region (point) (mark))
  (backward-kill-word 1)))

(defun line-text-up-with-char-above(charval)
  (interactive)
  (save-excursion
    (previous-line)
    (setq above-point (point))
    (forward-char)
    (search-forward charval (line-end-position) nil)
    (setq above-paren (point)))
  (insert (make-string (- above-paren above-point) ? )))

;; Line text up with parentheses
;; http://stackoverflow.com/questions/4074929/emacs-lisp-search-backward
;; This seems to work fine unless you call it two times in a row, with no other command in
;; between.  Then it will move forward the same amount as it did last time
(defun line-text-up-with-parentheses-above()
  (interactive)
  (line-text-up-with-char-above "("))

;; Bring text up to edge of page (now working -- find a command)
(defun bring-text-flush()
  (interactive)
  (back-to-indentation)
  (set-mark-command nil)
  (move-beginning-of-line nil)
  (kill-region (region-beginning) (region-end))
)

;; This does not do the desired behavior if is at beginning or end of word
(defun delete-word-from-middle ()
  (interactive)
  (delete-backward-sexp)
  (kill-sexp))

;; Should allow a numeric arg
(defun zap-to-char-backwards (arg char)
  (interactive)
  (let ((arg (* -1 arg)))
    (zap-to-char arg char)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;Binding backward kill word to new key, rebinding kill region
(global-set-key "\C-w" 'kill-region-or-word)

;; Other
(global-set-key (kbd "\C-c\C-d") 'duplicate-line)
(global-set-key (kbd "\<backtab>") 'indent-relative)
(global-set-key (kbd "\<C-S-iso-lefttab>") 'line-text-up-with-parentheses-above)

(global-set-key (kbd "C-<f5>") 'linum-mode)
(global-set-key (kbd "C-M-g") 'goto-line) ;; Does not work in terminal
(global-set-key (kbd "C-q") 'line-text-up-with-parentheses-above)

(global-set-key (kbd "C-'") 'postack-push)
(global-set-key (kbd "C-,") 'postack-pop)

;; Word wrapping
(global-set-key (kbd "C-<f2>") 'toggle-truncate-lines)

;; Simulate org-mode
(global-set-key (kbd "M-RET") 'duplicate-line-above) ;; Does not work in terminal
(global-set-key (kbd "C-j") 'duplicate-line-above) ;; Does work in terminal

;; Allows you to delete backwards a little more powerfully
(global-set-key (kbd "C-M-w") 'delete-backward-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

;; Zap-to-char, backwards
(global-set-key (kbd "C-M-z") 'zap-to-char-backwards)

;; Rebind comment-dwim to a more flexible fn
(global-set-key "\M-'" 'comment-or-uncomment-region-or-line)
