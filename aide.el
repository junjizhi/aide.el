;;; aide.el --- An Emacs front end for GPT APIs like OpenAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junji Zhi

;; Author: Junji Zhi
;; Keywords: gpt-3 openai

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

(require 'request)

(defgroup aide nil
  "aide.el custom settings"
  :group 'external
  :prefix "aide-")

(defcustom aide-max-tokens 100
  "The max-tokens paramater that aide.el would send to OpenAI API."
  :type 'integer
  :group 'aide)

(defun aide-openai-complete (api-key prompt)
  "Return the prompt answer from OpenAI API.
API-KEY is the OpenAI API key.

PROMPT is the prompt string we send to the API."
  (let ((result nil)
        (auth-value (format "Bearer %s" api-key)))
    (request
      "https://api.openai.com/v1/engines/davinci/completions"
      :type "POST"
      :data (json-encode `(("prompt" . ,prompt)
                           ("max_tokens" . ,aide-max-tokens)
                           ("temperature" . 0)
                           ("top_p" . 0.1)))
      :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
      :sync t
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (alist-get 'text (elt (alist-get 'choices data) 0))))))
    result))

(defun aide-openai-complete-region (start end)
  "Send the region to OpenAI autocomplete engine and get the result.

START and END are selected region boundaries."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (result (aide--openai-complete-string region)))
    (message "%s" result)))

(defun aide-openai-complete-region-insert (start end)
  "Send the region to OpenAI and insert the result to the end of buffer.

START and END are selected region boundaries."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (result (aide--openai-complete-string region)))
    (goto-char (point-max))
    (insert (format "\n%s" result))))

(defun aide-openai-complete-buffer-insert ()
  "Send the ENTIRE buffer to OpenAI and insert the result to the end of buffer."
  (interactive)
  (let* ((region nil) (result nil))
    (setq region (buffer-substring-no-properties (point-min) (point-max)))
    (setq result (aide--openai-complete-string region))
    (goto-char (point-max))
    (insert "\n" result)))

;; private

(defun aide--openai-complete-string (string)
  (aide-openai-complete openai-api-key string))

(provide 'aide)
;;; aide.el ends here
