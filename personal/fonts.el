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

(defun my-set-symbol-fonts (fontsizes-list)
  (let* ((fontname "Symbola")
         (fontsize (nth 0 fontsizes-list))
         (fontspec (font-spec :name fontname
                              :size fontsize
                              :weight 'normal
                              :slant 'normal)))
    (if (cnfonts--fontspec-valid-p fontspec)
        (set-fontset-font "fontset-default" 'symbol fontspec nil 'append)
      (message "字体 %S 不存在！" fontname))))

(add-hook 'cnfonts-set-font-finish-hook 'my-set-symbol-fonts)
