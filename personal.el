;;; package --- Summary
;;; Commentary:
;;; Code:

(setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
(add-to-list 'exec-path "C:\\cygwin\\bin")

;; Additional modules
(prelude-ensure-module-deps '(maxframe
                              w3m
                              bookmark+
                              instapaper
                              itail))

;; disable scroll ba
(scroll-bar-mode -1)

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

;; Configure for windows/cygwin
(setq shell-file-name "bash")
(setq-default ispell-program-name "aspell")

;; w3m setttings
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
       (rename-buffer (format "*w3m %s (%s)*"
                              (or w3m-current-title "")
                              (or w3m-current-url "")) t))

     (defadvice w3m-browse-url (around awang activate)
       "Always start a new session."
       (ad-set-arg 1 t)
       ad-do-it)

     (add-hook 'w3m-display-hook 'awang/w3m-rename-buffer)

     (global-set-key "\C-xm" 'browse-url-at-point)))

;; Enable max frame
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Enable bookmark+
(require 'bookmark+)

(require 'instapaper)
(define-key w3m-mode-map "i" 'instapaper-add-from-w3m)

(require 'itail)

(provide 'personal)
;;; personal ends here
