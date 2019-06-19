(require 'header2)

(if is-in-tencent
    (progn (setq my/header-author-info "billbai <billbai@tencent.com>")
        (setq header-copyright-notice (format "Copyright %s Tencent. All rights reserved.\n" (format-time-string "%Y"))))
    (progn (setq my/header-author-info "billbai <baiyinfeng94@gmail.com>")
        (setq header-copyright-notice (format "Copyright %s billbai <baiyinfeng94@gmail.com>. All rights reserved.\n" (format-time-string "%Y")))))

(defsubst my/header-author ()
    "Insert current user's name (`user-full-name') as this file's author."
    (insert header-prefix-string "Author: " my/header-author-info  "\n"))

(setq make-header-hook '(header-file-name                            
                            header-copyright
                            header-end-line))

(provide 'auto-header)
