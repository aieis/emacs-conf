(require 'json)
(require 'subr-x)


(defun aieis/snbt-skip-ws ()
  "Skip whitespace characters."
  (skip-chars-forward " \t\n\r"))

(defun aieis/snbt-parse-object ()
  "Parse an SNBT object into an Elisp alist."
  (forward-char 1) ; skip '{'
  (let ((obj nil))
    (aieis/snbt-skip-ws)
    (while (not (eq (char-after) ?}))
      (aieis/snbt-skip-ws)
      (let (key)
        ;; Read Key
        (if (eq (char-after) ?\")
            ;; Handle quoted keys just in case
            (progn
              (setq key (read (current-buffer)))
              (aieis/snbt-skip-ws)
              (unless (eq (char-after) ?:) (error "Expected ':' after key"))
              (forward-char 1))
          ;; Handle unquoted keys: read until ':'
          (let ((start (point)))
            (skip-chars-forward "^:")
            (setq key (string-trim (buffer-substring start (point))))
            (forward-char 1))) ; skip ':'

        ;; Parse Value
        (let ((val (aieis/snbt-parse-value)))
          (push (cons key val) obj))

        (aieis/snbt-skip-ws)
        (when (eq (char-after) ?,)
          (forward-char 1)
          (aieis/snbt-skip-ws))))
    (forward-char 1) ; skip '}'
    (nreverse obj)))

(defun aieis/snbt-parse-array ()
  "Parse an SNBT array into an Elisp vector."
  (forward-char 1) ; skip '['
  (let ((arr nil))
    (aieis/snbt-skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (aieis/snbt-parse-value) arr)
      (aieis/snbt-skip-ws)
      (when (eq (char-after) ?,)
        (forward-char 1)
        (aieis/snbt-skip-ws)))
    (forward-char 1) ; skip ']'
    (vconcat (nreverse arr))))

(defun aieis/snbt-parse-value ()
  "Dispatch parser based on the current character."
  (aieis/snbt-skip-ws)
  (let ((c (char-after)))
    (cond
     ((eq c ?{) (aieis/snbt-parse-object))
     ((eq c ?\[) (aieis/snbt-parse-array))
     ((eq c ?\") (read (current-buffer))) ; Emacs 'read' natively parses strings
     ;; Match booleans and null
     ((looking-at "\\(?:true\\|false\\|null\\)\\b")
      (let ((match (match-string 0)))
        (goto-char (match-end 0))
        (cond ((string= match "true") t)
              ((string= match "false") :json-false)
              ((string= match "null") nil))))
     ;; Match numbers with optional SNBT suffixes (b, s, L, f, d)
     ((looking-at "-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\([bBsSlLfFdD]\\)?\\b")
      (let ((num-str (match-string 0))
            (suffix (match-string 1)))
        (goto-char (match-end 0))
        (if suffix
            ;; Strip the 'b' (or other suffix) before converting to number
            (string-to-number (substring num-str 0 -1))
          (string-to-number num-str))))
     (t (error "Unexpected character at point %d: %c" (point) c)))))

(defun aieis/snbt-string-to-json ()
  "Parse an SNBT string and return a strict JSON string."

  (interactive)
  (let* ((snbt-string (buffer-string))
		 (elisp-data (with-temp-buffer
                       (insert snbt-string)
                       (goto-char (point-min))
                       (aieis/snbt-parse-value)))

         (json-encoding-pretty-print t))
	(erase-buffer)
	(insert (json-encode elisp-data))))

(defun aieis/elisp-to-snbt-string (data)
  "Recursively convert Elisp data back to an SNBT string."
  (cond
   ((listp data) ; Object (alist)
    (concat "{"
            (mapconcat (lambda (pair)
                         ;; Print unquoted key, followed by recursively parsed value
                         (format "%s:%s" (car pair) (elisp-to-snbt-string (cdr pair))))
                       data ",")
            "}"))
   ((vectorp data) ; Array
    (concat "[" (mapconcat #'elisp-to-snbt-string data ",") "]"))
   ((stringp data) (format "\"%s\"" data)) ; Strings get quoted
   ((numberp data) (number-to-string data)) ; Numbers (Note: loses 'b' suffix)
   ((eq data t) "true")
   ((eq data :json-false) "false")
   (t "null")))

(defun aieis/json-string-to-snbt ()
  "Convert a strict JSON string back to a loose SNBT string."
  (interactive)

  (let* ((json-string (buffer-string))
		 (json-object-type 'alist)
         (json-array-type 'vector)
         (elisp-data (json-read-from-string json-string)))
	(erase-buffer)
	(insert (elisp-to-snbt-string elisp-data))))
