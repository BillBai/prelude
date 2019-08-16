;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preload configs
;; runs before prelude core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'env)

(if is-in-tencent
     (setq url-proxy-services
         '(("http" . "127.0.0.1:12639")
              ("https" . "127.0.0.1:12639"))))


(defvar prelude-theme)
(setq prelude-theme 'gruvbox-dark-hard)

(global-hl-line-mode 0)

;; (set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

;;Text Encoding
(set-language-environment "UTF-8")

;; use emacs-china melpa source
;; (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set frame width and height
(if (display-graphic-p)
    (progn
        (setq initial-frame-alist
            '(
                 (tool-bar-lines . 0)
                 (width . 233) ; chars
                 (height . 60) ; lines
                 (left . 0)
                 (top . 0)))
        (setq default-frame-alist
            '(
                 (tool-bar-lines . 0)
                 (width . 233)
                 (height . 60)
                 (left . 0)
                 (top . 0))))
    (progn
        (setq initial-frame-alist '( (tool-bar-lines . 0)))
        (setq default-frame-alist '( (tool-bar-lines . 0)))))


;; disable scroll bar for GUI
(when (display-graphic-p)
    (scroll-bar-mode -1))

;; NO tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; no menu bar for terminal
(unless (display-graphic-p)
    (menu-bar-mode -1))

;; enable global linum mode
(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.p
(setq x-stretch-cursor t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quick access to recent files
(recentf-mode 1) ; keep a list of recently opened files

(provide 'preload)
;; preload.el ends here
