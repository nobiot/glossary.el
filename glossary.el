;;; glossary.el --- Make glossary. Fontify your terms and phrases as you type. Jump to their definition and back.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 nobiot

;; Author: nobiot <me@nobiot.com>
;; Created: 30 March 2024
;; Last modified: 02 February 2025
;; Version: 20250202.2009
;; Keywords: wp, hypermedia, xref, etags

;;; Commentary:

;; OVERVIEW

;; glossary.el lets you make a file (or files) that serve as a dictionary or
;; glossary, where you write down definitions and descriptions of terms and
;; phrases. Once you've done that, glossary.el will automatically highlight
;; (fontify) these terms and phrases in your plaint-text notes buffers as you
;; write. No special markup or syntax is required, and you are free to use any
;; file extensions and major modes including `markdown-` `org-`, and
;; `text-mode`.

;; When a term is fontified, you can jump to its definition. Simply place your
;; cursor on a term and call the `xref-find-definitions` command (by default
;; bound to "M-."). The command `xref-go-back` ("M-,") will take you back where
;; you jumped from. Fontification and jumping features also apply to other modes
;; such as EWW and EPUB (via `nov.el`) as long as the texts are displayed in
;; plain-text (and thus, PDF files are not supported).

;; SETTING UP GLOSSARY.EL

;; 1. Download `glossary.el' in the repository, for example in your `~/src/`
;;    directory (I use it in the example below).

;; 2. Optionally, You can also download some sample glossary files under
;;    `examples` subdirectory. These are collection of terms I crafted from
;;    Wikipedia's glossary pages and the classic epic, Moby-Dick (available
;;    through Project Gutenberg). Use them to test drive glossary.el.

;; 3. Put the following configuration in your init file.

;; README contains the same example configuration with more annotations to
;; explain user options. In this source header, I am omitting the annotations
;; for brevity.

;; (use-package glossary
;;   :load-path ("~/src/glossary/")
;;   :hook ((text-mode eww-mode nov-mode) . glossary-font-lock-mode)
;;   :custom
;;   (glossary-files-and-directories
;;    '("~/src/glossary/examples/Glossary-video-game-terms.txt"
;;      "~/src/glossary/examples/Glossary-biology.txt"))
;;   (glossary-file-extensions '("org" "md" "txt")) ;; default
;;   (glossary-glossary-exclude-regexps '("/\\.")) ;; default
;;   (glossary-tags-file-default "~/src/glossary/test/glossary-TAGS"))

;; HOW TO USE GLOSSARY.EL

;; 1. Write definitions of terms and phrases.

;;    To write definitions, use the <<this is a term>> syntax. You can have only
;;    one term per line. If two or more terms exist in a single line, only the
;;    first one is recognized as a term. For aliases and variations -- e.g.
;;    singular and plural of the same term -- place them in different lines. See
;;    example glossary files available in this repository under the `test/`
;;    subdirectory. You can use any extensions for glossary files. Simply list
;;    them with user option `glossary-file-extensions'. You can have
;;    definitions in your journals, bibliographic notes, etc. You are free to
;;    organize your glossary files in your own way and create as many of them as
;;    you wish -- a single big file that contain all definitions across genres
;;    or split definitions per theme or genre, etc. Simply list the glossary
;;    file(s) or containing directories in user option
;;    `glossary-files-and-directories'.

;; 2. Call the command `glossary-tags-create'.

;;    This gets glossary.el to collect definitions of terms and phrases from
;;    `glossary-files-and-directories` and to create / update the special index
;;    file `glossary-tags-file-default` (conventionally called "TAGS" file )
;;    that contains the location of all the definitions across the glossary
;;    files.

;; 3. Enable `glossary-font-lock-mode' in a buffer such as text-mode or org-mode.

;;    The terms in your glossary will be automatically fontified.

;; 4. To jump to the definition, call `xref-find-definitions' ("M-." by
;;    default).

;; 5. When you add a new term to the dictionary or change the location of the
;;    definitions, call `glossary-update-all' command.

;;    This calls the underlying etags program (shell command) to update the TAGS
;;    file by re-creating it. It will then refresh font-lock in all buffers that
;;    have `glossary-font-lock-mode' enabled.

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'etags)
;; To silence compiler warnings
(declare-function org-fold-show-context "org-fold")
(defvar glossary-font-lock-mode)

;;;; User Options

(defgroup glossary nil
  "Search definitions for plain-text documents."
  :group 'glossary
  :prefix "glossary")

;;;;; Definitions
(defcustom glossary-tags-file-default (file-name-concat
                                  user-emacs-directory "glossary-TAGS")
  "File name of TAGS file for `glossary'.
Default is nil indicating that TAGS are not for searching for the
definition in the TAG. The user must ensure that the file name is
absolute."
  :type '(file :tag "File name for TAGS file"))

(defcustom glossary-file-extensions (list "org" "md" "txt")
  "File extensions to be searched for definitions."
  :type '(repeat :tag "File exglossarysion without a dot \".\"" string))

(defcustom glossary-files-and-directories (list "~/org/")
  "Files and directories to be searched for definitions.
The elements must be an absolute file name for a file or directory."
  :type '(repeat :tag "List of files and directories" file))

(defcustom glossary-glossary-exclude-regexps (list "/\\.")
  "List of regexps for file names to be excluded for glossary.
When glossary.el scan files for terms to create to creates a
dictionary file (TAGS file), it will not descend into the matched
directories to look for files or scan the matched files.

Default is any dotfiles (files and directories whose name start
with a dot \".\").

The regexps are matched by the absolute file name. Because a
regexp needs to match only a part of the file name, it is not
recommended to use a file name of a high-level directory such as
\"~/\" or \"/home/\" in this user option."
  :type '(repeat file directory))

(defface glossary-id-face
  '((t :inherit font-lock-keyword-face))
  "Face for the definition terms glossary.el's TAGS file.")

;;;; Variables

(defvar glossary-regexps nil
  "List of regexps for font-locking the terms `glossary' recognizes.
Set by function `glossary-regexps'. See its document string for
more detail.")

(defvar glossary-regexp-length-limit 1500
  "Max number of terms in each regexp in variable `glossary-regexps' list.")

(defvar glossary-original-find-tag-default-function nil
  "User's original value of `find-tag-default-function'.")

;; The original `org-ctags' uses double angle brackets. It is adapted to work
;; with etags program. This regexp works with triple brackets for radio targets
;; as well as the original double. \"/\\1/i\" is needed for etags program.
;; Ensure that only one term appears in a line.
(defconst glossary-tags-regexp "/[ \t]*<<\\([^<>]+\\)>>/\\1/i")

;;;; Commands

;;;###autoload
(define-minor-mode glossary-mode
  "Enable `glossary'.
Disabling it also disables `glossary-font-lock-mode' from all buffers
that have enabled it."
  :global t
  (if glossary-mode
      ;; Activate
      (progn (glossary-regexps-initialize)
             (advice-add 'xref-find-definitions :before
                         'glossary-xref-after-jump-hook-prepare-advice))
    ;; Deactivate
    (advice-remove 'xref-find-definitions
                   'glossary-xref-after-jump-hook-prepare-advice)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when glossary-font-lock-mode (glossary-font-lock-mode -1))))))

;;;###autoload
(define-minor-mode glossary-font-lock-mode
  "Enable font-lock by `glossary' locally.
If the global minor mode `glossary-mode' is not active yet, it will be
also activated."
  :global nil
  (if glossary-font-lock-mode
      ;; Activate
      (progn
        (unless glossary-mode (glossary-mode +1))
        (glossary-tags-font-lock-add-keywords)
        (setq-local glossary-original-find-tag-default-function
                    find-tag-default-function)
        (setq-local find-tag-default-function 'glossary-id-at-point))
    ;; Deactivate
    (glossary-tags-font-lock-remove-keywords)
    (setq-local find-tag-default-function
                glossary-original-find-tag-default-function))
  (font-lock-flush))

;;;###autoload
(defun glossary-tags-create (&optional select)
  "(Re)create TAGS file.
The TAGS file will contain tag definitions for all the files with
one of the extensions from this list: .org, .md, and .txt. in the
directory and its subdirectories.

Passing SELECT to this command with a single `universal-argument'
brings completion in minibuffer to let you create a new TAGS file
or select from `tags-table-list'."
  (interactive "P")
  (let ((tags-file (if (or select (length> tags-table-list 1))
                       (glossary-completing-read-tags-file)
                     (or tags-file-name (glossary-tags-file-default))))
        (files (seq-filter #'file-exists-p (glossary-files)))
        (exitcode nil))
    (cond
     ((not files) (message "(glossary) No files to scan for definitions"))
     ((or (string-empty-p tags-file) (not tags-file))
      (message "(glossary) No TAGS file name selected"))
     (t (save-excursion
          (setq exitcode
                (call-process-shell-command
                 (format "%s --language=none --regex=%s --output=%s %s"
                         "etags"
                         (shell-quote-argument glossary-tags-regexp)
                         (shell-quote-argument tags-file) ;output FILE
                         (mapconcat #'shell-quote-argument files " "))))
          (if (eql 0 exitcode)
              (progn (visit-tags-table tags-file)
                     (glossary-regexps-initialize)
                     (message "(glossary) TAGS file created/updated"))
            nil))))))

;;;###autoload
(defun glossary-update-all ()
  "(Re)create TAGS file & refresh font-lock in all relevant buffers."
  (interactive)
  (glossary-tags-create)
  (glossary-font-lock-refresh-all-buffers))

;;;###autoload
(defun glossary-tags-file-select ()
  "Select a TAGS file from `tags-table-list`.
This function also initialize and refresh all
`glossary-font-lock-mode' buffers with the newly selected TAGS file.

The new TAGS file will be passed to the function
`visit-tags-table' to set relevant variables."
  (interactive)
  (let ((tags-file (glossary-completing-read-tags-file)))
    (when tags-file (visit-tags-table tags-file))
    (glossary-regexps-initialize-and-refresh-all)))

(defun glossary-font-lock-refresh ()
  "Refresh `font-lock-keywords' in current buffer.
This command checks if `glossary-font-lock-mode' is enabled for the
current buffer."
  (interactive)
  (when (and glossary-font-lock-mode tags-file-name)
    ;; FIXME 2024-11-06 I am using the difference between global and local
    ;; values of `glossary-regexps' to manage the update with removing the old
    ;; regexps (current local) and add new ones (global to local). This is
    ;; especially important if you have deleted some terms from the current
    ;; active glossary (which we can have only one at a time because of the
    ;; current design). This means, I believe, I cannot support different sets
    ;; of regexps in different buffers -- this may be a reasonable requirement,
    ;; for example to have different genres of glossaries applied for font-lock
    ;; in different buffers. Currently, I don't know how to support this
    ;; requirement.
    (glossary-tags-font-lock-remove-keywords)
    (glossary-tags-font-lock-add-keywords)
    (font-lock-flush)))

(defun glossary-font-lock-refresh-all-buffers ()
  "Refresh `font-lock-keywords' in all `glossary-font-lock-mode' buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer (glossary-font-lock-refresh))))

;;;; Functions

;;;;; Functions for Definitions and Tags

(defun glossary-id-at-point (&optional point)
  "Return glossary-id at POINT.
glossary-id is a text-property added by font-lock keywords. See
`glossary-tags-font-lock-add-keywords'."
  (let ((point (or point (point))))
    (or (get-text-property point 'glossary-id) (find-tag-default))))

(defun glossary-regexps ()
  "Set and return the global variable `glossary-regexps'.
The list is used to update the local `font-lock-keywords' used to
fontify the terms you define in the dictionary files via the TAGS
file (`tags-file-name').

Updating variable `glossary-regexps' can happen even if the current
buffer is not glossary-relevant, for example, the *scratch* buffer.
You can and should be able to initiate the dictionary update via
`glossary-tags-create' anywhere.

Each item of the variable `glossary-regexps' is a regexp. When
you have more terms than `glossary-regexp-length-limit',
glossary.el will create another regexp adding to the list. This
is because Emacs has a length limit of regexp.

There is another reason for glossary.el to create a separate
regexp. That's when the term uses CJK characters; more precisely,
the first character of the term determines this. This is because
CJK languages do not use space to separate words, so the regexp
needs to work on continuous string. For other languages, regexp
is constructed to match only the word constructs, normally
separated by spaces. There should be more characters that
separate words. Refer to documentation for regexp for more
detail."
  (let* ((terms-all (glossary-tags-all))
         (CJK-terms (seq-filter
                     ;; Only the first character of the term is checked.
                     (lambda (str) (string-prefix-p "CJK"
                                                    (get-char-code-property
                                                     (string-to-char str)
                                                     'name)))
                     terms-all))
         (terms (if CJK-terms
                    (seq-difference terms-all CJK-terms)
                  terms-all))
         (CJK-regexps (mapcar
                       (lambda (tags) (regexp-opt tags nil))
                       (seq-partition CJK-terms glossary-regexp-length-limit)))
         (regexps (mapcar (lambda (tags) (regexp-opt tags 'words))
                          (seq-partition terms glossary-regexp-length-limit))))
    ;; Change the global value to cascade to buffer local value. This is meant
    ;; to allow each buffer to delete the font-lock-keywords from local value of
    ;; `glossary-regexps' before updating the local value to the new default value.
    (setq-default glossary-regexps (if CJK-regexps
                                  (append CJK-regexps regexps)
                                regexps))))

(defun glossary-regexps-initialize ()
  "Initialize variable `glossary-regexps' globally.
This function sets global value of variable `glossary-regexps' and return it,
and sets `tags-file-name' globally with the value of user option
`glossary-tags-file-default' if `tags-file-name' is nil.

For more detail about variable `glossary-regexps', see document string of
function `glossary-regexps'."
  (unless tags-file-name (setq tags-file-name (glossary-tags-file-default)))
  (glossary-regexps))

(defun glossary-regexps-initialize-and-refresh-all ()
  "Initialize variable `glossary-regexps' and refresh all buffers.
This function is meant to be used in the advice function for
`visit-tags-table'."
  (glossary-regexps-initialize)
  (glossary-font-lock-refresh-all-buffers))

(defun glossary-files ()
  "Return file names to be parsed for definitions.
The file names returned are absolute and fully expanded.

This function goes through `glossary-files-and-directories' except for those
excluded by `glossary-glossary-exclude-regexps'. If an element is a directory
and not excluded, it is recursively searched for the files. These files are the
input for the \"etags\" program to create the TAGS file."

  (glossary-files-1 :files-and-directories glossary-files-and-directories
                    :file-extensions glossary-file-extensions
                    :exclude-regexps glossary-glossary-exclude-regexps))

(cl-defun glossary-files-1 (&key (files-and-directories glossary-files-and-directories)
                                 (file-extensions glossary-file-extensions)
                                 (exclude-regexps glossary-glossary-exclude-regexps))
  "Return file names in FILES-AND-DIRECTORIES.
The file names returned are absolute and fully expanded; thus, \"~/\" on Linux
is expanded to \"/home/<user>/\".

This function will return files with FILE-EXTENSIONS and exclude file names that
match EXCLUDE-REGEXPS.

This function iterates over FILES-AND-DIRECTORIES and skips if the element is a
relative file name. If an element is a directory, it is recursively searched for
the files."
  (let* ((regexp (concat ".\\("
                         (mapconcat #'identity
                                    file-extensions
                                    "\\|")
                         "\\)$"))
         (include-p (lambda (file-name)
                      (glossary-file-or-dir-include-p file-name exclude-regexps)))
         (files (glossary-files-in-directories files-and-directories regexp include-p)))
    ;; Need absolute names fully expanded
    (when files
      (thread-last files
                   (seq-filter #'file-exists-p)
                   (seq-filter #'file-readable-p)
                   (delete-dups)
                   (mapcar #'expand-file-name)))))

(defun glossary-file-or-dir-include-p (file-or-dir exclude-regexps)
  "Return non-nil if FILE-OR-DIR to be included for EXCLUDE-REGEXPS."
  (not (glossary-file-or-dir-exclude-p file-or-dir exclude-regexps)))

(defun glossary-file-or-dir-exclude-p (file-or-dir exclude-regexps)
  "Return non-nil if FILE-OR-DIR is to be excluded by EXCLUDE-REGEXPS.

EXCLUDE-REGEXPS is a list of regexp. FILE-OR-DIR must be an
absolute file name. This function expands the absolute
FILE-OR-DIR to its canonical name; thus \"~/\" becomes
\"/home/<user>/\" in Linux. The `string-match' is used to do the
regexp matching on this canonical name."
  (let ((regexps exclude-regexps)
        (result nil)
        (file-name (when (file-name-absolute-p file-or-dir)
                     (expand-file-name file-or-dir))))
    (when file-name
      (while (and regexps (null result))
        (let ((regexp (pop regexps)))
          ;; Do not expand regexp. On Windows, `expand-file-name' expands "/\\."
          ;; to "/", causing any file name to be excluded. Avoid expanding the
          ;; regexp on Windows.
          (setq result (string-match-p regexp file-name)))))
    result))

(defun glossary-files-in-directories (files-and-directories regexp include-p)
  "Return files from FILES-AND-DIRECTORIES.
FILES-AND-DIRECTORIES is a list of absolute file names for files
and directories. REGEXP and INCLUDE-P are passed to the arguments
of `directory-files-recursively'."
  (let ((files))
    (when files-and-directories
      (dolist (file-or-dir files-and-directories files)
        (when (file-name-absolute-p file-or-dir)
          ;; Directory
          (if (file-directory-p file-or-dir)
              (setq files
                    (seq-filter
                     include-p
                     (append files (directory-files-recursively
                                    file-or-dir
                                    regexp
                                    nil
                                    include-p))))
            ;; File
            (when (and (string-match-p regexp file-or-dir)
                       (funcall include-p file-or-dir))
              (setq files (append files (list file-or-dir))))))))))

(defun glossary-tags-file-default ()
  "Return tags file name fully expanded.
This function returns nil if `glossary-file-name-default' is nil
or relative file name."
  (when (and glossary-tags-file-default
             (file-name-absolute-p glossary-tags-file-default))
    (expand-file-name glossary-tags-file-default)))

(defun glossary-tags-all ()
  "Return list of all tags defined in the active TAGS file.
Returns a list of strings."
  (let (taglist)
    (unless tags-file-name (setq tags-file-name (glossary-tags-file-default)))
    (with-temp-buffer
      (insert-file-contents tags-file-name)
      (goto-char (point-min))
      ;; The regexp taken from `'org-ctags-get-filename-for-tag'
      (while (re-search-forward "^.*\^?\\(.*\\)\^A\\([0-9]+\\),\\([0-9]+\\)$"
                                nil t)
        (push (substring-no-properties (match-string 1)) taglist)))
    taglist))

;;;;; Functions for Font-Lock for TAGS

(defun glossary-tags-font-lock-add-keywords ()
  "Add tags as font-lock keywords.
It is meant to for `glossary-font-lock-mode'.

This function adds keywords in this form as described in
`font-lock-keywords'. (MATCHER HIGHLIGHT ...), where HIGHLIGHT is
MATCH-HIGHLIGHT. Thus, when all these applied, the form is
expanded into like this:

 (MATCHER (SUBEXP FACENAME [OVERRIDE)))

For our purpose, the MATCHER is a function returned by
`apply-partially'. SUBEXP is fixed to 0, FACENAME is face
`glossary-id-face', and we pass the optional OVERRIDE argument with
value \\='prepend\\='."
  (when (listp glossary-regexps)
    (setq-local glossary-regexps (default-value 'glossary-regexps))
    (dolist (regexp glossary-regexps)
      (font-lock-add-keywords
       nil
       `((,(apply-partially #'glossary-tags-add-text-properties regexp)
          (0 'glossary-id-face prepend)))
       :append))
    (setq font-lock-extra-managed-props '(glossary-id))))

(defun glossary-tags-font-lock-remove-keywords ()
  "Remove tags from font-lock keywords.
This function is meant for `glossary-font-lock-mode'."
  (dolist (regexp glossary-regexps)
    (font-lock-remove-keywords
     nil
     `((,(apply-partially #'glossary-tags-add-text-properties regexp)
        (0 'glossary-id-face prepend))))))

(defun glossary-tags-add-text-properties (regexp limit)
  "Add text properties for terms matched by REGEXP.

This function is meant to be used for font-lock with function
`font-lock-add-keywords'.

This function adds a text-property glossary-id for the match for each
regexp in the list variable `glossary-regexps'. The value of the glossary-id
text-property is the matched string itself. LIMIT is the bound
argument for `re-search-forward'.

This function is adapted from `org-activate-target-links'. It is
a MATCHER in this form described in `font-lock-keywords':

 (MATCHER HIGHLIGHT...)

where MATCHER is, in this case, a function name to call to make
the search, called with one argument, the limit of the search.
The reason why this function has two arguments is because in our
case `apply-partially' is used to create multiple functions with
REGEXP is supplied. See `glossary-tags-font-lock-add-keywords'.

Note that this function must return non-nil, move point, and set
‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would. MATCHER regexps is and should be
generated via the  function ‘regexp-opt’."
  (let ((case-fold-search t))
    (when (re-search-forward regexp limit t)
      ;; FIXME Do I need `org-remove-flyspell-overlays-in' at all?
      ;; (org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'glossary-id
                                 (buffer-substring-no-properties
                                  (match-beginning 0)
                                  (match-end 0))))
      ;; Need to return non-nil for the font-lock to stay in the buffer.
      t)))

;;;; Completing-read for tags-file

(defun glossary-completing-read-tags-file ()
  "Complete-reading a TAGS file from `tags-table-list'.
This also lets you select a new TAGS file that is not in the list
with using `read-file-name' completing user interface. You can
select a non-existing file name, but this function does not
create it. It is expected that the calling function will take
care of the non-existing file name. For example, simply set it in
a variable."
  (let* ((tags-files (add-to-list 'tags-table-list (glossary-tags-file-default)))
         (new "... (Create or select a new TAGS file)")
         (candiates (append tags-files (list new)))
         (choice
          (completing-read
           "Select a TAGS file: "
           (glossary-completing-read-collection-function candiates)
           nil t)))
    (if (equal choice new)
        (expand-file-name (read-file-name "New TAGS file: "))
      choice)))

(defun glossary-completing-read-collection-function (candidates)
  "Return lambda function for `completing-read' with CANDIDATES."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata
          (category . glossary-tags-file)
          (affixation-function . glossary-completing-read-affixation-function))
      (complete-with-action action candidates string predicate))))

(defun glossary-completing-read-affixation-function (candidates)
  "Return affixation for `completing-read' with CANDIDATES.
This function is meant to be used in the lambda function created
and returned by `glossary-completing-read-collection-function'."
  (let ((prefix-current "current global >> ")
        (prefix-others  "                  "))
    (mapcar
     (lambda (candidate)
       (if (string-equal (default-value 'tags-file-name) candidate)
           (list candidate
                 (propertize prefix-current 'face 'completions-annotations)
                 "")
         (list candidate prefix-others "")))
     candidates)))

;;;; Other utility functions

(defun glossary-xref-after-jump-hook-prepare-advice (&rest _)
  "Add hook to `xref-after-jump-hook'.
This function is meant to advise `xref-find-definitions' as
`glossary-mode' is activated. The intent is to localize the effect of
`xref-after-jump-hook' to only when jumped from buffers with
`glossary-font-lock-mode' activated."
  (when glossary-font-lock-mode
    (add-hook 'xref-after-jump-hook
              'glossary-xref-after-jump-reveal-context)))

;;;###autoload
(defun glossary-xref-after-jump-reveal-context ()
  "Make point and context visible if folded."
  (pcase-let
      ((`(,fn . ,ov) (get-char-property-and-overlay
                      (point) 'isearch-open-invisible)))
    ;; FIXME 2024-11-06 Using text-prop / ov prop 'isearch-open-invisible to
    ;; check if the region at point is folded. The presence. It should hold a
    ;; function name which is called to open the folded region for `isearch'.
    ;; There may be a simpler and more expressive way to do this, but I have yet
    ;; to see one.
    (when fn (cond ((and (functionp fn) ov) (funcall fn ov))
                   ((and (featurep 'org-fold) (derived-mode-p 'org-mode))
                    (org-fold-show-context 'link-search)))))
  (remove-hook 'xref-after-jump-hook
               'glossary-xref-after-jump-reveal-context))

(provide 'glossary)
;;; glossary.el ends here
