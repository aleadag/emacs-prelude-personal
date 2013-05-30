;;; package --- Summary
;;; Commentary:
;;; Code:

;; Additional modules
(prelude-ensure-module-deps '(maxframe
                              w3m
                              bookmark+
                              instapaper
                              itail))

;; Configure for windows/cygwin
(defvar awang/cygwin-bin "C:/cygwin/bin")
(setenv "PATH" (concat awang/cygwin-bin ";"
                       (getenv "PATH")))
(add-to-list 'exec-path awang/cygwin-bin)
(setq explicit-shell-file-name (expand-file-name "bash.exe" awang/cygwin-bin))
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(setq-default ispell-program-name "aspell")

;;;
;;; Nice options to have On by default
;;;
(scroll-bar-mode -1)
(mouse-wheel-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set the font
(when (and (>= emacs-major-version 22))
  (let ((font-name "Consolas:pixelsize=18:foundry=unknown")
        (fontset nil)
        (zh-font (font-spec :family "Microsoft YaHei" :size 18)))
    (set-frame-font font-name)

    (setq fontset (frame-parameter nil 'font))
    ;; Set the Chinese font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font fontset charset zh-font))
    (add-to-list 'default-frame-alist `(font . ,fontset))))

;; w3m settings
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(setq browse-url-browser-function 'w3m-browse-url)

(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))

(eval-after-load 'w3m
  '(progn
     (setq w3m-use-cookies t)

     (defun awang/w3m-rename-buffer (url)
       "Suitable for adding to `w3m-display-hook'."
       (rename-buffer (format "*w3m %s*"
                              (or w3m-current-url ""))))

     (defadvice w3m-browse-url (around awang activate)
       "Always start a new session."
       (ad-set-arg 1 t)
       ad-do-it)

     (add-hook 'w3m-display-hook 'awang/w3m-rename-buffer)

     (require 'instapaper)
     (setq instapaper-api-base "http://www.instapaper.com/api/")
     (defadvice instapaper-add-from-w3m (around awang activate)
       (ad-set-arg 1 w3m-current-title)
       ad-do-it)
     (define-key w3m-mode-map "i" 'instapaper-add-from-w3m)

     (global-set-key "\C-xm" 'browse-url-at-point)))

;; Enable max frame
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Enable bookmark+
(require 'bookmark+)

(require 'itail)

(require 'tramp)
(setq tramp-default-method "scp")

(provide 'personal)

;;; personal ends here
