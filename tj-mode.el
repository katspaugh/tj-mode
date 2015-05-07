;;; tj-mode.el --- Highlight JavaScript with Tern -*- lexical-binding: t -*-

;; Author: katspaugh@gmail.com
;; Keywords: languages, javascript
;; URL: https://github.com/katspaugh/tj-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (tern "0.0.1") (js2-mode "20150202"))

;;; Commentary:
;;; `tj-mode' highlights JavaScript based on Tern's AST.

;;; Code:

(require 'tern)
(require 'js2-mode) ; Only for indentation

(defvar tj/faces
  '(
    ("Identifier" . font-lock-variable-name-face)
    ("ArgumentIdentifier" . font-lock-variable-name-face)
    ("FunctionIdentifier" . font-lock-function-name-face)
    ("SpecialIdentifier" . font-lock-builtin-face)
    ("Property" . font-lock-variable-name-face)
    ("Keyword" . font-lock-keyword-face)
    ("Literal" . font-lock-constant-face)
    ("StringLiteral" . font-lock-string-face)
    ("NumberLiteral" . font-lock-constant-face)
    ("Comment" . font-lock-comment-face)
    ("Warning" . font-lock-warning-face)
    ("Error" . error))
  "Map of token types to font faces.")

(defun tj/echo-message (new-point &rest args)
  "Called by point-motion hooks to echo property at NEW-POINT.

ARGS are ignored."
  (let ((msg (get-text-property new-point 'help-echo)))
    (when (and (stringp msg)
               (not (current-message))
               (not (active-minibuffer-window)))
      (message msg))))

(defun tj/set-face (beg end face &optional msg)
  "Fontify a region from BEG to END with FACE (optional MSG)."
  (with-silent-modifications
    (put-text-property beg end 'font-lock-face face)
    (when msg
      (put-text-property beg end 'help-echo msg)
      (put-text-property beg end 'point-entered #'tj/echo-message))))

(defun tj/mark-block (beg end)
  "Mark a region from BEG to END as a block (for moving through code)."
  (with-silent-modifications
    (put-text-property beg end 'tj/block-start beg)
    (put-text-property beg end 'tj/block-end end)))

(defun tj/clear-faces ()
  "Clear font faces."
  (with-silent-modifications
    (remove-text-properties
     (point-min) (point-max)
     '(font-lock-face nil
                      tj/block-start
                      tj/block-end
                      help-echo nil
                      point-entered nil))))

(defun tj/fontify (messages)
  "Fontify with MESSAGES."
  (tj/clear-faces)
  (mapc (lambda (node)
          (let ((beg (1+ (cdr (assoc 'start node))))
                (end (1+ (cdr (assoc 'end node))))
                (type (cdr (assoc 'type node)))
                (msg (cdr (assoc 'message node))))
            (if (string= type "BlockStatement")
                (tj/mark-block beg (1- end)))
              (when (string= type "Error")
                (setq end (point-max)))
              (let ((face (cdr (assoc type tj/faces))))
                (when face
                  (tj/set-face beg end face msg)))))
        messages))

(defun tj/goto-block-pos (text-prop)
  "Go to the position stored in TEXT-PROP."
  (let ((pos (get-text-property (point) text-prop)))
    (when (numberp pos)
      (goto-char pos))))

(defun tj/beginning-of-block ()
  "Go to the beginning of a block statement."
  (tj/goto-block-pos 'tj/block-start))

(defun tj/end-of-block ()
  "Go to the end of a block statement."
  (tj/goto-block-pos 'tj/block-end))

(defvar-local tj/timer nil "Idle timer.")

(defun tj/cancel-timer ()
  "Cancel the idle timer."
  (when (timerp tj/timer)
    (cancel-timer tj/timer)))

(defun tj/query-callback (buffer data)
  "Tern query callback.

BUFFER is the analyzed buffer, DATA is a map of messages per file."
  (when (eq buffer (current-buffer))
    (let ((messages
           (cdr (assoc-string
                 (tern-project-relative-file)
                 data))))
      (when messages
        (tj/fontify messages)))))

(defun tj/query (buffer)
  "Run Tern query on BUFFER."
  (when (eq (current-buffer) buffer)
    (tern-run-query
     (apply-partially #'tj/query-callback buffer)
     "highlight" (point) :full-file)))

(defvar-local tj/highlight-refs-timer nil "Idle timer to highlight reference at point.")

(defun tj/overlay-region (start end)
  "Put overlay on region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'name 'tj/overlay)
    (overlay-put overlay 'face 'highlight)))

(defun tj/clear-overlays ()
  "Remove all overlays."
  (remove-overlays (point-min) (point-max) 'name 'tj/overlay))

(defun tj/highlight-refs-callback (data)
  "Highlight references of variable at point.

DATA contains a list of references and their positions."
  (tj/clear-overlays)
  (mapc
   (lambda (ref)
     (let ((file (cdr (assq 'file ref))))
       (when (string= file (tern-project-relative-file))
         (let ((start (1+ (cdr (assq 'start ref))))
               (end (1+ (cdr (assq 'end ref)))))
           (tj/overlay-region start end)))))
   (cdr (assq 'refs data))))

(defun tj/looking-at (type)
  "Whether point is at a token of TYPE."
  (let ((face (get-text-property (point) 'font-lock-face)))
    (eq face (cdr (assoc type tj/faces)))))

(defun tj/refs-query ()
  "Run a `refs' query."
  (unless (or
           (tj/looking-at "SpecialIdentifier")
           (tj/looking-at "Keyword")
           (tj/looking-at "Literal")
           (tj/looking-at "StringLiteral")
           (tj/looking-at "NumberLiteral")
           (tj/looking-at "Comment")
           (tj/looking-at "Warning")
           (tj/looking-at "Error")))
    (tern-run-query #'tj/highlight-refs-callback "refs" (point) :silent))

(defun tj/highlight-refs (&rest args)
  "Highlight reference at point in it's lexical scope.

Ignore ARGS"
  (when (timerp tj/highlight-refs-timer)
    (cancel-timer tj/highlight-refs-timer))
  (setq tj/highlight-refs-timer (run-with-idle-timer
                                 0.25 nil #'tj/refs-query)))

(defun tj/start (&rest args)
  "Start highlighting.

ARGS are ignored."
  (tj/cancel-timer)
  (setq tj/timer (run-with-idle-timer
                  0.25 nil #'tj/query (current-buffer))))

(defun tj-mode-enter ()
  "Enter tj-mode."
  (tern-mode t)
  (add-hook 'kill-buffer-hook #'tj-mode-exit nil t)
  (add-hook 'change-major-mode-hook #'tj-mode-exit nil t)
  (add-hook 'after-change-functions #'tj/start nil t)
  (add-hook 'post-command-hook #'tj/highlight-refs nil t)
  ;; Syntactic beginning/end of function definition
  (set (make-local-variable 'beginning-of-defun-function) #'tj/beginning-of-block)
  (set (make-local-variable 'end-of-defun-function) #'tj/end-of-block)
  ;; We are using js2-mode's indentation functions because they are brilliant
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  ;; A hack to get Tern running
  (tern-get-type)
  (run-with-timer 0.25 nil #'tj/query (current-buffer)))

(defun tj-mode-exit ()
  "Exit tj-mode."
  (interactive)
  (tj/cancel-timer)
  (remove-hook 'kill-buffer-hook #'tj-mode-exit t)
  (remove-hook 'change-major-mode-hook #'tj-mode-exit t)
  (remove-hook 'after-change-functions #'tj/start t)
  (remove-hook 'post-command-hook #'tj/highlight-refs t)
  (tern-mode -1)
  (tj/clear-faces))

(defvar tj-mode-hook nil)

(defvar tj-mode-map
  (make-sparse-keymap)
  "Key-map for tj-mode.")

;;;###autoload
(define-derived-mode tj-mode prog-mode "TJ"
  "Major mode to highlight JavaScript using an external parser–Tern."
  (tj-mode-enter))

(provide 'tern-js)

;;; tj-mode.el ends here
