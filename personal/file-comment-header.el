;;;
;; File Comment Header
;;;

(defun git-file-path (path)
  (let* ((root (file-truename (vc-git-root path)))
         (filename (file-name-nondirectory path))
         (filename-length (length filename)))
    (let ((chunk (file-relative-name path root)))
      chunk)))


(defun make-company-file-header ()
    "Insert header comment for file"
  (interactive)
  (insert "/**" "\n")
  (insert " * File: " (git-file-path buffer-file-name) "\n")
  (insert " * Project: " (projectile-project-name) "\n")
  (insert " * Created Date: " (format-time-string "%a, %b %d %Y, %l:%M:%S %p") "\n")
  (insert " * Author: billbai <billbai@tencent.com>" "\n")
  (insert " * -----" "\n")
  (insert " * Copyright (c) " (format-time-string "%Y") " Tencent" "\n")
  (insert " */" "\n"))


(defun make-self-file-header ()
  "Insert self project header comment for file"
  (interactive)
  (insert "/**" "\n")
  (insert " * File: " (git-file-path buffer-file-name) "\n")
  (insert " * Project: " (projectile-project-name) "\n")
  (insert " * Created Date: " (format-time-string "%a, %b %d %Y, %l:%M:%S %p") "\n")
  (insert " * Author: billbai <billbai42@gmail.com>" "\n")
  (insert " * -----" "\n")
  (insert " * Copyright (c) " (format-time-string "%Y") " billbai" "\n")
  (insert " */" "\n"))


