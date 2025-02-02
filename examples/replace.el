;; org-web-tools-read-url-as-org
;; Occur "- .* :: .*$" -> ""
;; ^[ ]*[0-9]*: - * →

(defun my/replace-link ()
  (interactive)
  (while (re-search-forward (concat "^" org-bracket-link-regexp) nil t)
    (let ((string
           (concat "<<<"
                   (match-string-no-properties 2)
                   ">>>")))
      (replace-match string))))


(defun my/replace-link-second ()
  (interactive)
  (while (re-search-forward org-bracket-link-regexp nil t)
    (let ((string
           (concat (match-string-no-properties 2))))
      (replace-match string))))
