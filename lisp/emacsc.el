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
;; Version: 1.5.20240628
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

;;;###autoload
(with-eval-after-load 'server
  (cl-defun server-eval-and-print-Ad-emacsc-suppress-output ((expr proc))
     (if (string-prefix-p "@" expr)
         (list (substring expr 1) nil)
       (list expr proc)))
  (advice-add #'server-eval-and-print :filter-args #'server-eval-and-print-Ad-emacsc-suppress-output))

(provide 'emacsc)

;;; emacsc.el ends here
