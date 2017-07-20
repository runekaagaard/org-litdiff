(setq ol-litdiff-dir "/Users/r/ws/org-litdiff")
(setq ol-ctags-dir "/usr/local/Cellar/universal-ctags/HEAD-b5c9b76/bin")
(setq ol-tag-file "/tmp/org-litdiff.tags")
(setq ol-active-buffer nil)

(defun ol-chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun ol-save-buffer-visiting (file-path)
  (let (
      (other-buf (find-buffer-visiting file-path))
    )
    (save-excursion
      (if other-buf (progn
        (set-buffer other-buf)
        (save-buffer)
      ))
    )
  )
)

(defun ol-read-field (line name)
  (let ((s (concat name ":\\([^\t]+\\)")))
    (if (string-match s line)
        (match-string 1 line)
    )
  )
)

(defun ol-lines-from-file (file-path begin end)
  (shell-command-to-string (format "sed -n %s,%sp %s" line end file-path))
)

(defun ol-generate-tags (file-path)
  (ol-save-buffer-visiting file-path)
  (call-process-shell-command (format "
    %s/ctags --fields=* -o - %s | grep -e '!_TAG' -e 'end:' > %s"
    ol-ctags-dir
    file-path
    ol-tag-file
  ))
)

(defun ol-find-tag-by-lineno ()
  (let*
    (
      (output (car (last (split-string (ol-chomp (shell-command-to-string (format
        "%s/readtags -e -n -t %s -Q '(and (<= $line %d) (>= $end %d))' -l"
        ol-ctags-dir ol-tag-file (line-number-at-pos) (line-number-at-pos)
        ))) "\n"))))
      (scope (ol-read-field output "scope:[a-zA-Z0-9-_]+"))
      (name (car (split-string output "\t")))
    )
    (cons name (if scope scope "root"))
  )
)

(defun ol-code-block-at-point ()
  (interactive)
  (org-between-regexps-p "^[ \t]*#\\+NAME:.*" "^[ \t]*#\\+end_.*")
)

(defun ol-parse-code-block-at-point ()
  (interactive)
  (let (
      (msg "No or invalid code block found at point.")
      (block (ol-code-block-at-point))
    )
    (unless block (user-error msg))
    (let* (
        (block (ol-code-block-at-point))
        (code (buffer-substring-no-properties (car block) (cdr block)))
        (s "#\\+NAME: ol::\\([/a-zA-Z0-1-_.]+\\)::\\([a-zA-Z0-9-_.]+\\)::\\([A-za-z0-9-_]+\\)")
      )
      (unless (string-match s code) (user-error msg))
      (let* (
          (file-path (match-string 1 code))
          (scope (match-string 2 code))
          (name (match-string 3 code))
        )
        (list file-path scope name)
      )
    )
  )
)

(defun ol-find-lines-by-tag-name (name scope)
  (let*
    (
      (s
        (if (equal scope "root")
          "%s/readtags -e -n -t %s -Q '(and (eq? $name \"%s\"))' -l"
          "%s/readtags -e -n -t %s -Q '(and (eq? $name \"%s\") (eq? $scope-name \"%s\"))' -l"
        )
      )
      (output (car (split-string (ol-chomp (shell-command-to-string (format
        s
        ol-ctags-dir ol-tag-file name scope
        ))) "\n"))
      )
      (line (ol-read-field output "line"))
      (end (ol-read-field output "end"))
    )
    
    (cons line end)
  )
)

(defun ol-delete-block ()
  "Narrow buffer to the current block."
  (interactive)
  (let* ((case-fold-search t)
	 (blockp (ol-code-block-at-point)))
    (if blockp
	(delete-region (car blockp) (cdr blockp))
      (user-error "Not in a block"))))

(defun ol-insert-code-block (code type file-path name scope)
  (insert (concat
    "#+NAME: ol::" file-path "::" scope "::" name "\n"
    "#+BEGIN_SRC " type "\n"
    code
    "#+END_SRC\n"
  ))
)
  
;;;;;;;;;;;;;;;;;;;

(defun ol-on ()
  (interactive)
  (setq ol-active-buffer (current-buffer))
)

(defun ol-off ()
  (interactive)
  (setq ol-active-buffer nil)
)

; TODO: Check if not already included. Handle errors.
(defun ol-include-tag ()
  (interactive)
  (ol-generate-tags (file-truename buffer-file-name))
  (let* (
      (tag-info (ol-find-tag-by-lineno))
      (name (car tag-info))
      (scope (cdr tag-info))
      (lines (ol-find-lines-by-tag-name name scope))
      (line (car lines))
      (end (cdr lines))
      (language (replace-regexp-in-string "-mode" "" (message "%s" major-mode)))
      (file-path (file-truename buffer-file-name))
      (code (ol-lines-from-file file-path line end))
    )
    (set-buffer ol-active-buffer)
    (ol-insert-code-block code language file-path name scope)
  )
)

(defun ol-refresh-code-block ()
  (interactive)
  (let* (
      (block (ol-parse-code-block-at-point))   
      (file-path (pop block))
      (scope (pop block))
      (name (pop block))
      (noop (ol-generate-tags file-path))
      (lines (ol-find-lines-by-tag-name name scope))
      (line (car lines))
      (end (cdr lines))
      (code (ol-lines-from-file file-path line end))
    )
    (ol-delete-block)
    (ol-insert-code-block code "python" file-path name scope)
  )
)

(defun ol-jump ()
  (interactive)
  (if (equal (current-buffer) ol-active-buffer)
    (progn
      (let* (
          (block (ol-parse-code-block-at-point))   
          (file-path (pop block))
          (scope (pop block))
          (name (pop block))
          (noop (ol-generate-tags file-path))
          (lines (ol-find-lines-by-tag-name name scope))
          (line (car lines))
        )
        (find-file-other-window file-path)
        (goto-line (string-to-int line))
        (reposition-window)
      )
    )
    (pop-to-buffer ol-active-buffer)
  )
)
