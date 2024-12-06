;;; emacsc.el --- helper for emacsc(1)
;;
;; Copyright (c) 2012-2024 Akinori MUSHA
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

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/emacsc
;; Created: 11 Apr 2012
;; Version: 1.6.20241206
;; Keywords: tools

;;; Commentary:

;; emacsc(1) is a wrapper of emacsclient(1) for use within a terminal,
;; and emacsc.el is a tiny little tweak to the server module to help
;; emacsc(1) interact with the running Emacs.

;; This package contains some scripts in the `bin' directory, so
;; install them manually into a directory in your path.  For example,
;; dired(1) opens a directory with dired, magit(1) opens a directory
;; with magit-status, ediff(1) is a diff(1) like tool to invoke
;; ediff-files, and so on.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dired)
  (require 'server))

;;;###autoload
(with-eval-after-load 'server
  (defun server-eval-and-print-Ad-emacsc-suppress-output (args)
    (let ((expr (car args)))
      (if (string-prefix-p "@" expr)
          (list (substring expr 1) nil)
        (cons expr (cdr args)))))
  (advice-add #'server-eval-and-print :filter-args #'server-eval-and-print-Ad-emacsc-suppress-output))

;;;###autoload
(defun dired-start (&optional file-or-files switches)
  "Start a `dired' buffer with FILE-OR-FILES selected.  It can be a single file, directory or a list of files and directories.

Optional second argument SWITCHES is passed through to `dired', which see."
  (interactive)
  (let ((files (if (stringp file-or-files)
                   (list file-or-files)
                 (or file-or-files (list default-directory)))))
    (if (= (length files) 1)
        (let* ((file (car files))
               (file
                (if file
                    (expand-file-name file)
                  default-directory))
               (dirname
                (if (file-directory-p file)
                    file
                  (file-name-directory file)))
               (buffer (dired dirname switches)))
          (with-current-buffer buffer
            (revert-buffer)
            (dired-goto-file
             (if (file-directory-p file)
                 (concat (file-name-as-directory file) "..")
               file)))
          (switch-to-buffer buffer))
      (let* ((files (mapcar #'expand-file-name files))
             (root (file-name-directory (cl-reduce #'fill-common-string-prefix files)))
             (buffer (dired root switches)))
        (with-current-buffer buffer
          (revert-buffer)
          (dolist (file files)
            (let* ((rel (file-relative-name file root))
                   (components (file-name-split rel))
                   (dir root))
              (dolist (component (butlast components))
                (setq dir (expand-file-name component dir))
                (and (file-directory-p dir)
                     (dired-maybe-insert-subdir dir)))
              (and (dired-goto-file file)
                   (save-excursion (dired-mark 1))))))
        (switch-to-buffer buffer)))
    ;; force redisplay of hl-line-mode
    (and (bound-and-true-p hl-line-mode)
         (fboundp 'hl-line-highlight)
         (hl-line-highlight))))

(provide 'emacsc)

;;; emacsc.el ends here
