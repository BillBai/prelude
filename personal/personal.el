;;; Theme
;; (disable-theme 'zenburn)

;; (prelude-require-package 'solarized-theme)
;; (setq prelude-theme 'solarized-light)

(add-to-list 'default-frame-alist '(height . 42))
(add-to-list 'default-frame-alist '(width . 120))

(prelude-require-package 'spacemacs-theme)

(prelude-require-package 'use-package)

(prelude-require-package 'quelpa)

(quelpa '(pyim-greatdict :fetcher github :repo "tumashu/pyim-greatdict"))


;;;
;; Coding
;;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;;;
;;   Chinese Input
;;;;

(prelude-require-package 'pyim)
(prelude-require-package 'pyim-basedict)

(pyim-basedict-enable)

(pyim-greatdict-enable)


(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

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
  ;;               '(pyim-probe-dynamic-english
  ;;                 pyim-probe-isearch-mode
  ;;                 pyim-probe-program-mode
  ;;                 pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 7)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (
   ;; ("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

;;;
;; Org
;;;

(eval-after-load "org"
  '(require 'ox-md nil t))


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

(prelude-require-package 'web-beautify)

(prelude-require-package 'js2-mode)
(prelude-require-package 'js2-refactor)
(prelude-require-package 'xref-js2)
(prelude-require-package 'json-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(prelude-require-package 'web-beautify)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))


(prelude-require-package 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (ternmode)
                           (company-mode)))


(prelude-require-package 'skewer-mode)

(skewer-setup)

(prelude-require-package 'indium)



;;;
;; Python
;;;

(prelude-require-package 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "~/.virtualenvs/")



;;;
;; Misc
;;;

(prelude-require-package 'elfeed)

(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"))


(global-set-key (kbd "C-x w") 'elfeed)

(prelude-require-package 'nyan-mode)

(nyan-mode)

(prelude-require-package 'whitespace-cleanup-mode)



;;;
;; LaTeX
;;;

(prelude-require-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(mapc (lambda (mode)
(add-hook 'LaTeX-mode-hook mode))
      (list 'auto-complete-mode
   'auto-fill-mode
   'LaTeX-math-mode
   'turn-on-reftex
   'linum-mode))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xelatex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))
; set pdf view tool
;; (setq TeX-view-program-list '(("Evince" "evince %o")))
;; (cond
;;  ((eq system-type 'windows-nt)
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
;;                                                  (output-dvi "Yap"))))))

;;  ((eq system-type 'gnu/linux)
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (setq TeX-view-program-selection '((output-pdf "Evince")
;;                                                  (output-dvi "Evince")))))))
;; ; XeLaTeX
(add-hook 'LaTeX-mode-hook (lambda()
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-command-default "XeLaTeX")
    (setq TeX-save-query  nil )
    (setq TeX-show-compilation f)
    ))

;;; personal.el ends here
