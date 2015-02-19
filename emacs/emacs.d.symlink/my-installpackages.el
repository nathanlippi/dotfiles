(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    anything
    ac-html
    ac-js2
    auto-complete
    color-theme-sanityinc-tomorrow
    edbi
    flymake-css
    flymake-cursor
    flymake-jshint
    flymake-php
    js2-mode
    key-chord
    kill-ring-search
    magit
    nodejs-repl
    pabbrev php-mode
    slime
    yasnippet
    web-beautify
    zencoding-mode
  )
  "a list of packages to ensure are installed at launch.")

(defun all-packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (all-packages-installed-p)
  (message "%s" "Emacs is refreshing its packages...")
  (package-refresh-contents)
  (message "%s" "Refreshing packages complete.")

  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(add-to-list 'load-path "~/.emacs.d/")
