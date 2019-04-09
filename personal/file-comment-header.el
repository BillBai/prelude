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
  (insert comment-start "\n")
  (insert comment-start " * File: " (git-file-path buffer-file-name) "\n")
  (insert comment-start " * Project: " (projectile-project-name) "\n")
  (insert comment-start " * Created Date: " (format-time-string "%a, %b %d %Y, %l:%M:%S %p") "\n")
  (insert comment-start " * Author: billbai <billbai@tencent.com>" "\n")
  (insert comment-start " * -----" "\n")
  (insert comment-start " * Copyright (c) " (format-time-string "%Y") " Tencent" "\n")
  (insert comment-start "\n"))



(defun make-self-file-header ()
  "Insert self project header comment for file"
  (interactive)
  (insert comment-start "\n")
  (insert comment-start " * File: " (git-file-path buffer-file-name) "\n")
  (insert comment-start " * Project: " (projectile-project-name) "\n")
  (insert comment-start " * Created Date: " (format-time-string "%a, %b %d %Y, %l:%M:%S %p") "\n")
  (insert comment-start " * Author: billbai <billbai42@gmail.com>" "\n")
  (insert comment-start " * -----" "\n")
  (insert comment-start " * Copyright (c) " (format-time-string "%Y") " billbai" "\n")
  (insert comment-start  "\n"))
