;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Chinese Input Method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'use-package)
(prelude-require-package 'pyim)
(prelude-require-package 'pyim-basedict)
(prelude-require-package 'posframe)

;; use quelpa to get pyim-greatdict
;; (prelude-require-package 'quelpa)
;; (quelpa '(pyim-greatdict :fetcher github :repo "tumashu/pyim-greatdict"))

(use-package pyim
    :ensure nil
    :demand t
    :config
    ;; 激活 basedict 拼音词库
    (use-package pyim-basedict
        :ensure nil
        :config (pyim-basedict-enable))

    ;; greatdict is too slow....
    ;; (use-package pyim-greatdict
    ;;   :ensure nil
    ;;   :config (pyim-greatdict-enable))

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
    (setq-default pyim-english-input-switch-functions
        '(pyim-probe-dynamic-english
             pyim-probe-isearch-mode
             pyim-probe-program-mode
             pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
        '(pyim-probe-punctuation-line-beginning
             pyim-probe-punctuation-after-punctuation))

    ;; 开启拼音搜索功能
    (pyim-isearch-mode 1)

    ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
    ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
    ;; 手动安装 posframe 包。
    (setq pyim-page-tooltip 'posframe)

    ;; 选词框显示7个候选词
    (setq pyim-page-length 7)

    :bind
    ;; 与 pyim-probe-dynamic-english 配合
    (("M-j" . pyim-convert-string-at-point)
        ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'pyim)
;; pyim.el ends here
