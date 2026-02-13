;; -*- lexical-binding: t; -*-
(require 'subr-x)

(defvar bufo-mode-syntax-table
  (let ((table (make-syntax-table)))
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?@ ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun bufo-types ()
  '("i8" "i16" "i32" "i64" "usize" "u4" "u8" "u16" "u32" "u64"
    "char" "Any"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"))

(defun bufo-keywords ()
  '("func" "libpath" "static" "linker" "os" "default" "config"
    "else" "enum" "union" "import" "return" "if" "for"
    "return"  "sizeof" "static" "struct" "switch" "while"
    "union"  "dynamic" "while" "alignas" "alignof" "and"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun bufo-font-lock-keywords ()
  (list
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (bufo-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (bufo-types) 'symbols) . font-lock-type-face)))

(defun bufo--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun bufo--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun bufo--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (bufo--previous-non-empty-line)))
         (indent-len 4)
         (prev-indent (bufo--indentation-of-previous-non-empty-line)))
    (cond
     ;; case: if (...) <statement>;   → do not indent next line
     ((string-match-p "^\\s-*if\\s-*([^)]*)\\s-*[^;]+;\\s-*$" prev-line)
      prev-indent)

     ;; existing rules …
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     ((string-match-p "^\\s-*if\\s-*(" prev-line)
      (+ prev-indent indent-len))
     ((string-match-p "^\\s-*else" prev-line)
      prev-indent)
     (t prev-indent))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun bufo-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (bufo--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(define-derived-mode bufo-mode prog-mode "BUFO- Mode"
  :syntax-table bufo-mode-syntax-table
  (setq-local font-lock-defaults '(bufo-font-lock-keywords))
  (setq-local indent-line-function 'bufo-indent-line)
  (setq-local comment-start "// "))

;; Highlight only special-case whitespace in bufo-mode
(defun bufo--show-whitespace ()
  "Highlight tabs and trailing spaces in `bufo-mode`."
  (font-lock-add-keywords
   nil
   '(("\t"     0 '(:background "gray20" :foreground "orange") t) ; tabs
     ("[ ]+$"  0 '(:background ".") t))))                       ; trailing spaces

(add-hook 'bufo-mode-hook #'bufo--show-whitespace)


(provide 'bufo-mode)
