(prelude-require-package 'cnfonts)

;; set fonts
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

(use-package cnfonts
    :ensure t
    :init
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
    (add-hook 'cnfonts-set-font-finish-hook 'my-set-symbol-fonts))

(provide 'fonts)
;; fonts.el ends here
