;;; personal.el

;;;
;; File Comment Header
;;;

(defun make-file-header ()
    "Insert header comment for file"
  (interactive)
  (insert "/**" "\n")
  (insert "* File: " (file-name-nondirectory buffer-file-name) "\n")
  (insert "* Project: " "melo" "\n")
  (insert "* Created Date: " (format-time-string "%A, %B %d %Y, %l:%M:%S %p") "\n")
  (insert "* Author: billbai billbai@tencent.com" "\n")
  (insert "* -----" "\n")
  (insert "* Copyright (c) " (format-time-string "%Y") " Tencent" "\n")
  (insert "*/" "\n"))

;;
(setq projectile-keymap-prefix (kbd "C-c p"))

(setq whitespace-line-column 120)

;; disable scroll bar for GUI
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; enable global linum mode
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; use ibuffer instead of helm-buffer-list
;; restore from the helm everywhere key binding
(global-set-key (kbd "C-x C-b") 'ibuffer)


(recentf-mode 1) ; keep a list of recently opened files
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

(global-set-key (kbd "<f5>") 'execute-extended-command)


;; set font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Sarasa Mono T SC" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Sarasa Mono T SC-16"))
    (add-to-list 'default-frame-alist '(font . "Sarasa Mono T SC-16"))))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Sarasa Mono T SC" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Sarasa Mono T SC-16"))
    (add-to-list 'default-frame-alist '(font . "Sarasa Mono T SC-16"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Sarasa Mono T SC" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Sarasa Mono T SC-16"))
    (add-to-list 'default-frame-alist '(font . "Sarasa Mono T SC-16")))))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

(prelude-require-package 'cnfonts)

(cnfonts-enable)

(add-to-list 'default-frame-alist '(height . 67))
(add-to-list 'default-frame-alist '(width . 142))

(prelude-require-package 'solarized-theme)
(prelude-require-package 'spacemacs-theme)
(prelude-require-package 'material-theme)

(setq prelude-theme 'spacemacs-dark)
(load-theme 'spacemacs-dark t)

(prelude-require-package 'use-package)

(prelude-require-package 'quelpa)

;; (quelpa '(pyim-greatdict :fetcher github :repo "tumashu/pyim-greatdict"))


;;;
;;Text Encoding
;;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;;;
;;   Chinese Input
;;;;

(prelude-require-package 'pyim)
(prelude-require-package 'pyim-basedict)
(prelude-require-package 'posframe)

(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  (use-package pyim-greatdict
    :ensure nil
    :config (pyim-greatdict-enable))

  ;; 五笔用户使用 wbdict 词库
  ;; (use-package pyim-wbdict
  ;;   :ensure nil
  ;;   :config (pyim-wbdict-gbk-enable))

  (setq default-input-method "pyim")


  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; (setq-default pyim-english-input-switch-functions
                ;; '(pyim-probe-dynamic-english
                  ;; pyim-probe-isearch-mode
                  ;; pyim-probe-program-mode
                  ;; pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  ;; (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示7个候选词
  (setq pyim-page-length 7)


  :bind
  (
   ;; ("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))


;;;
;; Org
;;;

(eval-after-load "org"
  '(require 'ox-md nil t))

(progn
  ;; org-mode setup

  ;; when opening a org file, don't collapse headings
  (setq org-startup-folded nil)

  ;; wrap long lines. don't let it disappear to the right
  (setq org-startup-truncated nil)

  ;; when in a url link, enter key should open it
  (setq org-return-follows-link t)

  ;; make org-mode” syntax color embedded source code
  (setq org-src-fontify-natively t))

(setq org-agenda-files (file-expand-wildcards "~/Dropbox/GTD/*.org"))

;;;
;; C / C++
;;;
(prelude-require-package 'irony)
(prelude-require-package 'company-irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(prelude-require-package 'yasnippet)
(prelude-require-package 'auto-complete-c-headers)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(prelude-require-package 'flycheck-rtags)

(prelude-require-package 'rtags)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))


;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(yas-global-mode 1)


;;;
;; Project Management
;;;

(prelude-require-package 'neotree)

(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

(setq-default neo-show-hidden-files t)


;;;
;; Web Frontend
;;;
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

;;;
;; Python
;;;

(prelude-require-package 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "~/.virtualenvs/")



;;;
;; Misc
;;;

;; nyan nyan nyan~~~
(prelude-require-package 'nyan-mode)
(nyan-mode)

(prelude-require-package 'whitespace-cleanup-mode)


;;;
;; LaTeX
;;;

(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list
                              'TeX-command-list
                              '("XeLaTeX" "%`xelatex%(mode)%' %t"
                                TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil)
                             (setq TeX-show-compilation t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal.el ends here
