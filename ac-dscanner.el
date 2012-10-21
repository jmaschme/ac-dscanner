;;; auto-complete-dscanner.el --- Auto Completion source for dscanner for GNU Emacs

;; Based on auto-complete-clang.el by Brian Jiang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Auto Completion source for dscanner. Most of codes are taken from
;; company-dscanner.el and modified and enhanced for Auto Completion.

;;; Code:


(provide 'ac-dscanner)
(require 'auto-complete)

(defcustom ac-dscanner-executable
  (executable-find "dscanner")
  "*Location of dscanner executable"
  :group 'auto-complete
  :type 'file)

;;; Extra compilation flags to pass to dscanner.
(defcustom ac-dscanner-flags nil
  "Extra flags to pass to the Dscanner executable.
This variable will typically contain include paths, e.g., ( \"-I~/MyProject\", \"-I.\" )."
  :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))


(defconst ac-dscanner-completion-pattern
  "^\\(%s[^\s\n:]*\\) [cisvmkfgPM]\\(?: : \\)*\\(.*$\\)")
;;  "^\\(%s[^\s\n:]*\\) [cisvmkfgPM]")

(defconst ac-dscanner-error-buffer-name "*dscanner error*")

(defun ac-dscanner-parse-output (prefix)
  (goto-char (point-min))
  (let ((pattern (format ac-dscanner-completion-pattern
                         (regexp-quote prefix)))
        lines match detailed_info
        (prev-match ""))
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (string= "Pattern" match)
        (setq detailed_info (match-string-no-properties 2))
      
        (if (string= match prev-match)
            (progn
              (when detailed_info
                (setq match (propertize match
                                        'ac-dscanner-help
                                        (concat
                                         (get-text-property 0 'ac-dscanner-help (car lines))
                                         "\n"
                                         detailed_info)))
                (setf (car lines) match)
                ))
          (setq prev-match match)
          (when detailed_info
            (setq match (propertize match 'ac-dscanner-help detailed_info)))
          (push match lines))))    
    lines))


(defun ac-dscanner-handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create ac-dscanner-error-buffer-name))
         (cmd (concat ac-dscanner-executable " " (mapconcat 'identity args " ")))
         (pattern (format ac-dscanner-completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more agressively if no match was found.
                (message "dscanner failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\ndscanner failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun ac-dscanner-call-process (prefix &rest args)
  (let ((buf (get-buffer-create "*dscanner-output*"))
        res)
    (with-current-buffer buf (erase-buffer))
    (setq res (apply 'call-process-region (point-min) (point-max)
                       ac-dscanner-executable nil buf nil args))
    (with-current-buffer buf
      (unless (eq 0 res)
        (ac-dscanner-handle-error res args))
      ;; Still try to get any useful input.
      (ac-dscanner-parse-output prefix))))


(defsubst ac-dscanner-build-complete-args (pos)
  (append '()
	  ac-dscanner-flags
	  '("-e" "--dotComplete")
	  (list (format "%s" pos))))


(defsubst ac-dscanner-clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)

(defun ac-dscanner-document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-dscanner-help item))
        (ac-dscanner-clean-document s)))
  ;; (popup-item-property item 'ac-dscanner-help)
  )


(defface ac-dscanner-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for dscanner candidate"
  :group 'auto-complete)

(defface ac-dscanner-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the dscanner selected candidate."
  :group 'auto-complete)

(defsubst ac-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defun ac-dscanner-candidate ()
  (unless (ac-in-string/comment)
    (save-restriction
      (widen)
      (apply 'ac-dscanner-call-process
             ac-prefix
             (ac-dscanner-build-complete-args (point))))))


(defvar ac-template-start-point nil)
(defvar ac-template-candidates (list "ok" "no" "yes:)"))

(defun ac-dscanner-action ()
  (interactive)
  ;; (ac-last-quick-help)
  (let ((help (ac-dscanner-clean-document (get-text-property 0 'ac-dscanner-help (cdr ac-last-completion))))
        (raw-help (get-text-property 0 'ac-dscanner-help (cdr ac-last-completion)))
        (candidates (list)) ss fn args (ret-t "") ret-f)
    (setq ss (split-string raw-help "\n"))
    (dolist (s ss)
      (when (string-match "\\[#\\(.*\\)#\\]" s)
        (setq ret-t (match-string 1 s)))
      (setq s (replace-regexp-in-string "\\[#.*?#\\]" "" s))
      (cond ((string-match "^\\([^(]*\\)\\((.*)\\)" s)
             (setq fn (match-string 1 s)
                   args (match-string 2 s))
             (push (propertize (ac-dscanner-clean-document args) 'ac-dscanner-help ret-t
                               'raw-args args) candidates)
             (when (string-match "\{#" args)
               (setq args (replace-regexp-in-string "\{#.*#\}" "" args))
               (push (propertize (ac-dscanner-clean-document args) 'ac-dscanner-help ret-t
                                 'raw-args args) candidates))
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-dscanner-clean-document args) 'ac-dscanner-help ret-t
                                 'raw-args args) candidates)))
            ((string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t) ;; check whether it is a function ptr
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args 'ac-dscanner-help ret-f 'raw-args "") candidates)
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args 'ac-dscanner-help ret-f 'raw-args "") candidates)))))
    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-template-candidates candidates)
           (setq ac-template-start-point (point))
           (ac-complete-template)
           
           (unless (cdr candidates) ;; unless length > 1
             (message (replace-regexp-in-string "\n" "   ;    " help))))
          (t
           (message (replace-regexp-in-string "\n" "   ;    " help))))))

(defun ac-dscanner-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c)
                  ;; ->
                  (and (eq ?> c)
                       (eq ?- (char-before (1- (point)))))
                  ;; ::
                  (and (eq ?: c)
                       (eq ?: (char-before (1- (point))))))
          (point)))))

(ac-define-source dscanner
  '((candidates . ac-dscanner-candidate)
    (candidate-face . ac-dscanner-candidate-face)
    (selection-face . ac-dscanner-selection-face)
    (prefix . ac-dscanner-prefix)
    (requires . 0)
    (document . ac-dscanner-document)
    (action . ac-dscanner-action)
    (cache)
    (symbol . "c")))

(defun ac-dscanner-same-count-in-string (c1 c2 s)
  (let ((count 0) (cur 0) (end (length s)) c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))

(defun ac-dscanner-split-args (s)
  (let ((sl (split-string s ", *")))
    (cond ((string-match "<\\|(" s)
           (let ((res (list)) (pre "") subs)
             (while sl
               (setq subs (pop sl))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((and (ac-dscanner-same-count-in-string ?\< ?\> subs)
                           (ac-dscanner-same-count-in-string ?\( ?\) subs))
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           sl))))


(defun ac-template-candidate ()
  ac-template-candidates)

(defun ac-template-action ()
  (interactive)
  (unless (null ac-template-start-point)
    (let ((pos (point)) sl (snp "")
          (s (get-text-property 0 'raw-args (cdr ac-last-completion))))
      (cond ((string= s "")
             ;; function ptr call
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-dscanner-split-args s))
             (cond ((featurep 'yasnippet)
                    (dolist (arg sl)
                      (setq snp (concat snp ", ${" arg "}")))
                    (condition-case nil
                        (yas/expand-snippet (concat "("  (substring snp 2) ")")
                                            ac-template-start-point pos) ;; 0.6.1c
                      (error
                       ;; try this one:
                       (ignore-errors (yas/expand-snippet
                                       ac-template-start-point pos
                                       (concat "("  (substring snp 2) ")"))) ;; work in 0.5.7
                       )))
                   ((featurep 'snippet)
                    (delete-region ac-template-start-point pos)
                    (dolist (arg sl)
                      (setq snp (concat snp ", $${" arg "}")))
                    (snippet-insert (concat "("  (substring snp 2) ")")))
                   (t
                    (message "Dude! You are too out! Please install a yasnippet or a snippet script:)"))))
             (t
             (unless (string= s "()")
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               (cond ((featurep 'yasnippet)
                      (setq s (replace-regexp-in-string "<#" "${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
                      (condition-case nil
                          (yas/expand-snippet s ac-template-start-point pos) ;; 0.6.1c
                        (error
                         ;; try this one:
                         (ignore-errors (yas/expand-snippet ac-template-start-point pos s)) ;; work in 0.5.7
                         )))
                     ((featurep 'snippet)
                      (delete-region ac-template-start-point pos)
                      (setq s (replace-regexp-in-string "<#" "$${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, $${..." s))
                      (snippet-insert s))
                     (t
                      (message "Dude! You are too out! Please install a yasnippet or a snippet script:)")))))))))


(defun ac-template-prefix ()
  ac-template-start-point)


;; this source shall only be used internally.
(ac-define-source template
  '((candidates . ac-template-candidate)
    (prefix . ac-template-prefix)
    (requires . 0)
    (action . ac-template-action)
    (document . ac-dscanner-document)
    (cache)
    (symbol . "t")))

;;; auto-complete-dscanner.el ends here
