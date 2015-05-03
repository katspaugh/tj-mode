;;; tj-mode.el --- Highlight JavaScript with Tern -*- lexical-binding: t -*-

;; Author: katspaugh@gmail.com
;; Keywords: tools
;; URL: https://github.com/katspaugh/tj-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (tern "*"))

;;; Commentary:
;;; `tj-mode' highlights JavaScript based on Tern's AST.

;;; Code:

(require 'tern)

(defvar tj/faces
  '(
    ("ArgumentName" . font-lock-variable-name-face)
    ("FunctionName" . font-lock-function-name-face)
    ("Identifier" . font-lock-variable-name-face)
    ("Property" . font-lock-variable-name-face)
    ("Keyword" . font-lock-keyword-face)
    ("Literal" . font-lock-string-face)
    ;("NumberLiteral" . font-lock-string-face)
    ("ThisExpression" . font-lock-constant-face)
    ("Error" . font-lock-warning-face)
    ("Comment" . font-lock-comment-face))
  "Map of token types to font faces.")


(defun tj/query (buffer)
  "Run Tern query on BUFFER."
  (tj/cancel-timer)
  (when (eq (current-buffer) buffer)
    (tern-run-query
     (apply-partially #'tj/query-callback buffer)
     "tokens" (point) :silent)))

(defun tj/query-callback (buffer data)
  "Tern query callback.

BUFFER is the analyzed buffer, DATA is a list of tokens."
  (tj/cancel-timer)
  (when (eq (current-buffer) buffer)
    (tj/clear-faces)
    (let ((messages
           (cdr (assoc-string
                 (file-name-nondirectory (buffer-file-name))
                 data))))
      (when messages
        (mapc (lambda (node)
                (let ((beg (cdr (assoc 'start node)))
                      (end (cdr (assoc 'end node)))
                      (type (cdr (assoc 'type node)))
                      (msg (cdr (assoc 'message node))))
                  (let ((face (cdr (assoc type tj/faces))))
                    (when face
                      (tj/set-face (1+ beg) (1+ end) face msg)))))
              messages)))))

(defun tj/echo-message (new-point _ignore)
  "Called by point-motion hooks to echo property at NEW-POINT."
  (let ((msg (get-text-property new-point 'help-echo)))
    (when (and (stringp msg)
               (not (active-minibuffer-window)))
      (message msg))))

(defun tj/set-face (beg end face &optional msg)
  "Fontify a region from BEG to END with FACE."
  (put-text-property beg end 'font-lock-face face)
  (when msg
    (put-text-property beg end 'help-echo msg)
    (put-text-property beg end 'point-entered #'tj/echo-message)))

(defun tj/clear-faces ()
  "Clear font faces."
  (with-silent-modifications
    (remove-text-properties
     (point-min) (point-max)
     '(font-lock-face nil
                      help-echo nil
                      point-entered nil))))

(defvar-local tj/timer nil "Idle timer.")

(defun tj/cancel-timer ()
  "Cancels idle timer."
  (when (timerp tj/timer)
    (cancel-timer tj/timer)))

(defun tj/start (&rest _ignore)
  "Start highlighting."
  (tj/cancel-timer)
  (setq tj/timer (run-with-idle-timer
                  0.5 nil #'tj/query (current-buffer))))

(defun tj-mode-enter ()
  "Enter tj-mode."
  (tern-mode t)
  (add-hook 'kill-buffer-hook #'tj-mode-exit nil t)
  (add-hook 'change-major-mode-hook #'tj-mode-exit nil t)
  (add-hook 'after-change-functions #'tj/start nil t)
  (tj/start))

(defun tj-mode-exit ()
  "Exit tj-mode."
  (tj/cancel-timer)
  (remove-hook 'kill-buffer-hook #'tj-mode-exit)
  (remove-hook 'change-major-mode-hook #'tj-mode-exit)
  (remove-hook 'after-change-functions #'tj/start)
  (tern-mode -1)
  (tj/clear-faces))

;;;###autoload
(define-minor-mode tj-mode
  "Major mode for editing JavaScript code."
  nil
  "TJ"
  nil
  (if tj-mode
      (tj-mode-enter)
    (tj-mode-exit)))

(provide 'tern-js)

;;; tj-mode.el ends here
