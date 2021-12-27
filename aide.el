;;; aide.el --- An Emacs front end for GPT APIs like OpenAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junji Zhi

;; Author: Junji Zhi <junjizhi.to@gmail.com>
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

(defun aide-openai-complete (api-key prompt &optional max-tokens)
  "Returns the prompt answer from OpenAI API"
  (or max-tokens (setq max-tokens 100))
  (let ((result nil)
        (auth-value (format "Bearer %s" api-key)))
    (request
      "https://api.openai.com/v1/engines/davinci/completions"
      :type "POST"
      :data (json-encode `(("prompt" . ,prompt)
                           ("max_tokens" . ,max-tokens)
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
  "Send the region to OpenAI autocomplete engine and get the result"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (result (jz/openai-complete openai-api-key region 50)))
    (message "%s" result)))

(provide 'aide)
;;; aide.el ends here
