;;; aide.el --- An Emacs front end for GPT APIs like OpenAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Junji Zhi and contributors

;; Author: Junji Zhi
;; Keywords: gpt-4 openai chatgpt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple wrapper to call GPT APIs
;;
;; For details, please see http://github.com/junjizhi/aide.el

;;; Code:

(require 'request) ;; M-x package-install RET request RET

(defgroup aide nil
  "aide.el custom settings"
  :group 'external
  :prefix "aide-")

(defcustom aide-chat-model "gpt-3.5-turbo"
  "The model paramater that aide.el sends to OpenAI Chat API."
  :type 'string
  :group 'aide)

(defcustom aide-max-input-tokens 3800
  "The maximum number of tokens that aide.el sends to OpenAI API.
Only affects the send COMPLETE buffer function."
  :type 'integer
  :group 'aide)

;; Not currently utilized.
;; (defcustom aide-max-output-tokens 100
;;   "The max-tokens parameter that aide.el sends to OpenAI API."
;;   :type 'integer
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-temperature 1
;;   "The temperature paramater that aide.el sends to OpenAI API. 1 is default."
;;   :type 'float
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-top-p 0.1
;;   "The top-p parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-frequency-penalty 0
;;   "The frequency_penalty parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-presence-penalty 0
;;   "The presence_penalty parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

(defcustom aide-openai-api-key-getter (lambda () openai-api-key)
  "Function that retrieves the valid OpenAI API key"
  :type 'function
  :group 'aide)

(defcustom aide-save-chat-file "~/aide-log.txt"
  "The location of the chat log; everything sent to and from OpenAI. Nil to disable."
  :type 'function
  :group 'aide)

;; Functions for users to call
(defun aide-openai-chat-region-insert (start end)
  "Send the region to OpenAI Chat API and insert the result to the end of buffer.

The function is smart to check if current buffer in org mode, and present result accordingly.

START and END are selected region boundaries.
"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (is-in-org-mode (string-equal major-mode "org-mode"))
         (extra-conditions "\"\n\nIn your response, limit the characters to 80 characters
per line for text explanations and add line breaks if needed. Do not apply the character limit to code blocks.")
         (final-prompt (concat "Please help me with the following question:\n\n \"" region extra-conditions))
         original-point
         tmp-text-end-point)
    (goto-char (point-max))
    (setq original-point (point))
    (insert "\n\n>>> GPT: Generating response... (This is placeholder text. It will disppear. DO NOT edit.)")
    (setq tmp-text-end-point (point))

    (let ((x (make-overlay original-point tmp-text-end-point)))
      (overlay-put x 'face '(:foreground "lime green"))
      (deactivate-mark))

    (aide--openai-chat-string final-prompt (lambda (result)
                                             (delete-region original-point tmp-text-end-point)
                                       (if result
                                           (progn
                                             (if is-in-org-mode
                                                 (insert "\n\n>>> GPT:\n#+BEGIN_SRC markdown\n" result "\n#+END_SRC")
                                               (insert "\n\n>>> GPT: " result))
                                             (if is-in-org-mode
                                                 nil
                                                 (let ((x (make-overlay original-point (point-max))))
                                                   (overlay-put x 'face '(:foreground "orange red"))
                                               (deactivate-mark)))
                                             result)
                                         (message "Empty result"))))))

(defun aide-openai-chat-paragraph-insert ()
  "Send the current paragraph to OpenAI Chat API and append the result to the end of the buffer
"
  (interactive)
  (let (region-start
        region-end)
    (save-excursion
      (backward-paragraph)
      (setq region-start (point))
      (forward-paragraph)
      (setq region-end (point))
      )
    (aide-openai-chat-region-insert region-start region-end)))

(defun aide-openai-chat-buffer-insert (&optional result)
  "Send the ENTIRE buffer, up to max tokens, to OpenAI and insert the result to
the end of buffer.
Assumes the user is providing a prompt to ChatGPT somewhere in the enclosed text."
  (interactive)
  (if result
      (progn
        (let* ((original-point (point)))
          (goto-char (point-max))
          (insert "\n" result)
          (fill-paragraph)
          (let ((x (make-overlay original-point (point-max))))
            (overlay-put x 'face '(:foreground "orange red")))))
   (aide--openai-chat-string
    (buffer-substring-no-properties (get-min-point) (point-max))
    'aide-openai-chat-buffer-insert)))

;; TODO rewrite to use chat with appropriate prompt. Complete endpoint gone in 1/2024
;; (defun aide-openai-tldr-region (start end)
;;   "Send the region to OpenAI autocomplete engine and get the TLDR result.

;; START and END are selected region boundaries."
;;   (interactive "r")
;;   (let* ((region (buffer-substring-no-properties start end))
;;          (result (aide--openai-complete-string (concat region "\n\n tl;dr:"))))
;;     (message "%s" result)))


;; TODO rewrite to use chat with appropriate prompt. Complete endpoint gone in 1/2024
;; (defun aide-openai-edits-region-insert (start end)
;;    "Send the region to OpenAI edits and insert the result to the end of region.
;; START and END are selected region boundaries."
;;   (interactive "r")
;;   (let* ((region (buffer-substring-no-properties start end))
;;          (result (aide-openai-edits (funcall aide-openai-api-key-getter) "Rephrase the text" region)))
;;     (goto-char end)
;;     (if result
;;         (progn
;;           (insert "\n" result)
;;           (fill-paragraph)
;;           (let ((x (make-overlay end (point))))
;;             (overlay-put x 'face '(:foreground "orange red")))
;;           result)
;;       (message "Empty result"))))

;; TODO rewrite to use chat with appropriate prompt. Edit endpoint gone in 1/2024
;; (defun aide-openai-edits-region-replace (start end)
;;   "Send the region to OpenAI edits and replace the region.

;; START and END are selected region boundaries.

;; The original content will be stored in the kill ring."
;;   (interactive "r")
;;   (let* ((region (buffer-substring-no-properties start end))
;;          (result (aide-openai-edits (funcall aide-openai-api-key-getter) "Rephrase the text" region)))
;;     (goto-char end)
;;     (if result
;;         (progn
;;           (kill-region start end)
;;           (insert "\n" result)
;;           (fill-paragraph)
;;           (let ((x (make-overlay end (point))))
;;             (overlay-put x 'face '(:foreground "orange red")))
;;           result)
;;       (message "Empty result"))))

;; private; should not be called by users

(defun aide-openai-chat (api-key prompt callback)
  "Return the prompt answer from OpenAI API.
API-KEY is the OpenAI API key.

PROMPT is the prompt string we send to the API."
  (let* ((result nil)
        (auth-value (format "Bearer %s" api-key)) 
        (payload (json-encode `(("model"  . ,aide-chat-model)
;                                ("max_tokens" . ,aide-max-output-tokens)
;                                ("temperature" . ,aide-temperature)
;                               ("frequency_penalty" . ,aide-frequency-penalty)
;                                ("presence_penalty" . ,aide-presence-penalty)
;                               ("top_p" . ,aide-top-p)))
                                ("messages" . [(("role" . "user") ("content" . ,prompt))])))))
    (message "Waiting for OpenAI...")
    (request
      "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :data payload
      :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
      :sync nil
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progn
                    (setq result (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0))))
                    (log-call-response prompt result)
                    (funcall callback result)
                    (message "Done."))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "Got error: %S, payload: %S" error-thrown payload))))
      result))

(defun aide--openai-chat-string (string callback)
  (aide-openai-chat (funcall aide-openai-api-key-getter) string callback))

(defun get-min-point ()
  "OpenAI API limits requests of > ~4000 tokens (model-specific; davinci
maxes out at request of 4000 tokens; ~15200 char"
  (if (> (buffer-size) (* 4 (or aide-max-input-tokens 3800))) ;; 1 tokens = ~4 char
      (- (point-max) (* 4 (or aide-max-input-tokens 3800)))
    (point-min)))

(defun log-call-response (prompt response)
  (if aide-save-chat-file
      (write-region
      ; Starting with * allows users to view log w/ org mode for easy folding
       (concat "*" (current-time-string) "\n" prompt "\n" response)
       nil aide-save-chat-file 'append)))

(provide 'aide)
;;; aide.el ends here
