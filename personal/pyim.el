;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Chinese Input Method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(pyim
                            quelpa
                            pyim-basedict
                            posframe))

(use-package pyim
  ;; :after liberime-config
  :config
  (setq default-input-method "pyim")
  
  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示 5 个候选词
  ;; (setq pyim-page-length 5)

  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  
  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; (setq pyim-default-scheme 'rime)
  
  ;; use western punctuation
  (setq pyim-punctuation-dict nil)
  )

(provide 'pyim)
;; pyim.el ends here
