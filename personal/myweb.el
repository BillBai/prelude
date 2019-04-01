
;;;
;; Web Frontend
;;;
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(prelude-require-package 'emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(prelude-require-packages '(js-doc
                            js2-mode
                            js2-refactor
                            xref-js2
                            json-mode
                            json-snatcher
                            tern
                            web-beautify
                            skewer-mode
                            livid-mode))

(require 'tern)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; ;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; ;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap [(meta ?.)] nil)
(define-key tern-mode-keymap [(meta ?,)] nil)

(use-package web-beautify
  :defer t
  :init
  (progn
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
    (eval-after-load 'js
      '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))))


(require 'js-doc)
