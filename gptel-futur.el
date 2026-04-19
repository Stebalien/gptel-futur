;;; gptel-futur.el --- Futur support for gptel      -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steven Allen

;; Author: Steven Allen <steven@stebalien.com>
;; Homepage: https://github.com/Stebalien/gptel-futur
;; Package-Requires: ((emacs "28.1") (gptel "0.9.9.4"))

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

;; Futur support for gptel.

;;; Code:

(require 'futur)
(require 'gptel-request)

(cl-defmethod future-blocker-abort ((fsm gptel-fsm) _ _)
  "Futur blocker implementation for gptel's FSM."
  (gptel-abort fsm))

(cl-defmethod future-blocker-wait ((fsm gptel-fsm))
  "Futur blocker implementation for gptel's FSM."
  (let* ((state (gptel-fsm-state fsm)))
    (pcase state
      ('DONE t)
      ('WAIT (when-let* ((proc (car (cl-rassoc fsm gptel--request-alist :key #'car))))
               (futur-blocker-wait proc))))))

;;;###autoload
(defun gptel-futur-make-tool (&rest args)
  "Define a new tool for `gptel'.

All ARGS are the same as in `gptel-make-tool', which see, except
that:

1. There's no need to specify ASYNC, this tool is always async.
2. FUNCTION is expected to return a futur instead of taking a
   callback."
  (when-let* ((fn (plist-get args :function)))
    (cl-callf copy-sequence args)
    (cl-callf plist-put args
      :function (lambda (cb &rest args)
                  (futur-bind (apply fn args) cb)))
    (cl-callf plist-put args :async t))
  (apply #'gptel-make-tool args))

;;;###autoload
(cl-defun gptel-futur-request ( &optional prompt
                                &rest args
                                &key callback schema
                                &allow-other-keys )
  "Call `gptel-request' with the given PROMPT, delivering the response via a futur.

On success, the LLMs response will be delivered as the futur's value.
If a SCHEMA is provided, it will be deserialized and provided as an
object.

On failure, the error will be delivered as a failure and can be handled
via futur's :error-fun

All ARGS are passed to `gptel-request', which see. CALLBACKs will
still be called as usual before delivering results to the futur.

\(fn ARGS...)"
  (declare (indent 1))
  (futur-new
   (lambda (f)
     (apply #'gptel-request prompt
            :callback
            (lambda (res info)
              (unwind-protect
                  (when callback (funcall callback res info))
                (cond
                 ((eq res nil)
                  (let ((msg (or (plist-get info :status)
                                 "gptel request failed with no status message")))
                    (futur-deliver-failure f (list 'error msg))))
                 ((eq res 'abort)
                  (futur-deliver-failure f (list 'gptel-abort "gptel request aborted")))
                 ((stringp res)
                  (if schema
                      (condition-case val
                          (gptel--json-read-string res)
                        (:success (futur-deliver-value f val))
                        (t (futur-deliver-failure f val)))
                    (futur-deliver-value f res))))))
            args))))

(provide 'gptel-futur)
;;; gptel-futur.el ends here
