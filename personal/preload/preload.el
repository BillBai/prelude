;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preload configs
;; some basic tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set frame width and height
(if (display-graphic-p)
    (progn
        (setq initial-frame-alist
            '(
                 (tool-bar-lines . 0)
                 (width . 106) ; chars
                 (height . 60) ; lines
                 (left . 23)
                 (top . 42)))
        (setq default-frame-alist
            '(
                 (tool-bar-lines . 0)
                 (width . 106)
                 (height . 60)
                 (left . 23)
                 (top . 42))))
    (progn
        (setq initial-frame-alist '( (tool-bar-lines . 0)))
        (setq default-frame-alist '( (tool-bar-lines . 0)))))


;; disable scroll bar for GUI
(when (display-graphic-p)
    (scroll-bar-mode -1))

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

;;Text Encoding
(set-language-environment "UTF-8")

(provide 'preload)
;; preload.el ends here
