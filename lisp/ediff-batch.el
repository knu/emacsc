;;; ediff-batch.el --- helper for ediff(1) and ediff-merge(1)
;;
;; Copyright (c) 2012, 2013 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.
;;
;; See https://github.com/knu/emacsc for the latest information.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/emacsc
;; Created: 23 Jul 2012
;; Version: 1.1.20130925
;; Keywords: tools

;;; Commentary:

;;; Code:

(require 'ediff)

;; save and restore window configuration
(defvar ediff-batch-saved-window-configuration nil "Saved window configuration for ediff")
(defun ediff-batch-save-window-configuration ()
  (setq ediff-batch-saved-window-configuration (current-window-configuration)))
(add-hook 'ediff-before-setup-hook 'ediff-batch-save-window-configuration)
(defun ediff-batch-restore-window-configuration ()
  (set-window-configuration ediff-batch-saved-window-configuration))
(add-hook 'ediff-suspend-hook 'ediff-batch-restore-window-configuration t)
(add-hook 'ediff-quit-hook 'ediff-batch-restore-window-configuration t)

;; batch mode (for use from git mergetool etc.)
(ediff-defvar-local ediff-batch-in-batch-mode-p nil "True if in batch mode")
(ediff-defvar-local ediff-batch-close-on-quit nil "True if the buffer should be closed on quit.")

(defun ediff-batch-batch-mode (&optional mode)
  (let (ret)
    (ediff-with-current-buffer ediff-buffer-A
      (case mode
        (set
         (setq ret (setq ediff-batch-in-batch-mode-p t)))
        (unset
         (setq ret ediff-batch-in-batch-mode-p)
         (setq ediff-batch-in-batch-mode-p nil))
        (t
         (setq ret ediff-batch-in-batch-mode-p))))
    ret))

(defadvice ediff-find-file (around
                            mark-newly-opened-buffers
                            (file-var buffer-name &optional last-dir hooks-var)
                            activate)
  (let* ((file (symbol-value file-var))
         (existing-p (and find-file-existing-other-name
                          (find-buffer-visiting file))))
    ad-do-it
    (or existing-p
        (ediff-with-current-buffer (symbol-value buffer-name)
          (setq ediff-batch-close-on-quit t)))))

(defun ediff-batch-save-merge ()
  (if (ediff-batch-batch-mode)
      (let ((file ediff-merge-store-file))
        (if file
            (ediff-with-current-buffer ediff-buffer-C
              (set-visited-file-name file t)
              (save-buffer))))
    (ediff-maybe-save-and-delete-merge)))

(remove-hook 'ediff-quit-merge-hook 'ediff-maybe-save-and-delete-merge)
(add-hook 'ediff-quit-merge-hook 'ediff-batch-save-merge)

(defadvice ediff-cleanup-mess (around
                               support-batch-mode
                               ()
                               activate)
  (let ((batch-p (ediff-batch-batch-mode 'unset))
        (buffers (list ediff-buffer-A ediff-buffer-B ediff-ancestor-buffer))
        (buffer-C ediff-buffer-C))
    ad-do-it
    (dolist (buffer buffers)
      (ediff-with-current-buffer buffer
        (and ediff-batch-close-on-quit (kill-buffer))))
    (when batch-p
      (ediff-kill-buffer-carefully buffer-C)
      (delete-frame))))

;;;###autoload
(defun ediff-files-in-batch-mode
    (file-A file-B &optional startup-hooks)
  (ediff-files
   file-A file-B
   (cons (function (lambda () (ediff-batch-batch-mode 'set))) startup-hooks)))

;;;###autoload
(defun ediff-merge-files-in-batch-mode
    (file-A file-B &optional startup-hooks merge-buffer-file)
  (ediff-merge-files
   file-A file-B
   (cons (function (lambda () (ediff-batch-batch-mode 'set))) startup-hooks)
   merge-buffer-file))

;;;###autoload
(defun ediff-merge-files-with-ancestor-in-batch-mode
    (file-A file-B file-ancestor &optional startup-hooks merge-buffer-file)
  (ediff-merge-files-with-ancestor
   file-A file-B file-ancestor
   (cons (function (lambda () (ediff-batch-batch-mode 'set))) startup-hooks)
   merge-buffer-file))

(provide 'ediff-batch)

;;; ediff-batch.el ends here
