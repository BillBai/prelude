(require 'header2)

(if is-in-tencent
    (progn (setq my/header-author-info "billbai <billbai@tencent.com>")
        (setq header-copyright-notice (format "Copyright (c) %s Tencent\n" (format-time-string "%Y"))))
    (progn (setq my/header-author-info "billbai <baiyinfeng94@gmail.com>")
        (setq header-copyright-notice (format "Copyright (c) %s billbai\n" (format-time-string "%Y")))))

(defsubst my/header-author ()
    "Insert current user's name (`user-full-name') as this file's author."
    (insert header-prefix-string "Author: " my/header-author-info  "\n"))

(setq make-header-hook '(header-file-name
                            my/header-author
                            header-copyright
                            header-creation-date
                            header-description
                            header-end-line))

()

(provide 'auto-header)
