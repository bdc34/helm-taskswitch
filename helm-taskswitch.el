;;; helm-taskswitch.el --- Use helm to switch windows and buffers

;; Author: Brian Caruso <briancaruso@gmail.com>
;; Maintainer: Brian Caruso <briancaruso@gmail.com>
;; Created: 2016-05-27
;; URL: https://github.com/bdc34/helm-taskswitch
;; Package-Version: 20160728.001
;; Package-Requires: ((helm "20160420.259") (ewmctrl "20150630.138"))
;; Keywords: desktop, windows

;;; Commentary:

;; `helm-taskswitch' provides an window and buffer switching with helm.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; TODO Install wmctrl, ewmctrl.el and helm.

;; ## Usage

;; To activate use `M-x helm-task-switcher'.

;; ## Issues / bugs

;; If you discover an issue or bug in `helm-taskswitch' not already noted:

;; * as a TODO item, or

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `helm-task-switcher', wmctrl, helm and ewmctrl.el.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


;; Customisable variables.

(defgroup helm-taskswitch nil
  "Use helm to switch between tasks (X11 windows or buffers)"
  :group 'external)

(defgroup helm-taskswitch-faces nil
  "Customize the appearance of helm-taskswitch."
  :prefix "helm-"
  :group 'helm-taskswitch
  :group 'helm-faces)

(defface helm-taskswitch-browser-face
    '((t (:foreground "green")))
  "Face used for web browsers."
  :group 'helm-taskswitch-faces)

(defface helm-taskswitch-term-face
    '((((background dark)) :foreground "RosyBrown")
      (((background light)) :foreground "SlateGray"))
  "Face used for terminals."
  :group 'helm-taskswitch-faces)

(defface helm-taskswitch-emacs-face
    '((t (:foreground "Sienna3")))
  "Face used for emacs."
  :group 'helm-taskswitch-faces)


;; Things that would be nice to have customizations for:
;; boring client windows
;; faces
;; truncate long lines

;; Internal Code

(defun helm-taskswitch-format-candidate (al)
  "Formats a candidate client window for task switcher"
  (cons (helm-taskswitch-format-candidate-title al) (list al)))

(defun helm-taskswitch-format-candidate-title (al)
  "Format the title of the candidate window"
  (let ((wmclass (helm-taskswitch-wmclass al )) ;(cdr (assoc 'wmclass al))))
        (title   (helm-taskswitch-title al ))) ;(cdr (assoc 'title   al)))))
    (format "%-18s  %s" wmclass title )))


(defun helm-taskswitch-title (al)
  (let* ( (title   (cdr (assoc 'title   al)))
          (wmclass (cdr (assoc 'wmclass al))) )
    (cond ((string-equal wmclass "google-chrome.google-chrome" )
           ;; chrome sometimes ends with "- Google Chrome"
           ;; and some pages repeat the URL in the title
           (s-chop-suffix "- Google Chrome" title))
          (t title)) ))

(defun helm-taskswitch-wmclass (al)
  (let ((title   (cdr (assoc 'title   al)))
         (wmclass (cdr (assoc 'wmclass al))))
    (cond ((string-equal wmclass "google-chrome.google-chrome" )
           (propertize "Google-chrome" 'face 'helm-taskswitch-browser-face ))
          ((string-equal wmclass "terminator.Terminator")
           (propertize "Terminator" 'face 'helm-taskswitch-term-face))
          ((string-equal wmclass "emacs.Emacs")
           (propertize "Emacs" 'face 'helm-taskswitch-emacs-face))
          (t wmclass) )))


(defun helm-taskswitch-client-candidates ()
  "Return a list windows with title and wmclass"
  (mapcar 'helm-taskswitch-format-candidate (ewmctrl--list-windows)) )

(defun helm-taskswitch-close-candidates (c)
  "Closes a candidate window from ewmctrl--list-windows"
  (mapc (lambda (c)
          (let* ((id (cdr (assoc 'window-id (car c))))
                 (killcmd (concat ewmctrl-wmctrl-path " -i -c '" id "'")))
            (message killcmd)
            (call-process-shell-command killcmd )))
        (helm-marked-candidates)))

(defun helm-taskswitch-open-preview-jpg (id)
  "Make a preview jpg of a x window in /tmp/id.jpg"

  ;; This is too slow and buggy.
  
  ;; Encountering some bug in xwd a lot, it looks like this
  ;;xwd: Getting target window information.
  ;;
  ;;X Error of failed request:  BadMatch (invalid parameter attributes)
  ;;  Major opcode of failed request:  73 (X_GetImage)
  ;;  Serial number of failed request:  255
  ;;  Current serial number in output stream:  255

 (let* ((tmp-win-file (concat "/tmp/" id ".jpg"))
        (cmd (concat "/usr/bin/xwd" " -silent -id '" id "' | /usr/bin/convert - " tmp-win-file )))
      (progn
       (message cmd)
       (call-process-shell-command cmd)
       (find-file tmp-win-file))))

(defun helm-taskswitch-make-preview-windows (wmctrl-list)
   "Make a directory of preview jpegs"
   (mapcar 'make-preview-jpg wmctrl-list))

;; The class for sources is defined in helm-source.el
(setq helm-source-wmctrl-windows
      '((name . "client windows")
        (candidates . helm-taskswitch-client-candidates)
        (action . (("Forground" . (lambda (c) (ewmctrl--focus-window-by-id (cdr (assoc 'window-id (car c))))))
                   ("close window" . helm-taskswitch-close-candidates )
                   ("Split top bottom" . (lambda (candidate) (message "Not yet implemented: top-bottom %s" candidate)))
                   ("Split right left" . (lambda (candidate) (message "Not yet implemented: right-left %s" candidate)))
                   ("preview" . (lambda (c) ( helm-taskswitch-open-preview-jpg (cdr (assoc 'window-id (car c))))))
                   ("dump client window s-exp" . prin1 )
                   ))
        ))

; '(helm-source-buffers-list
;   helm-source-recentf
;   helm-source-buffer-not-found)

;;;###autoload
(defun helm-taskswitch ()
  "Use helm to switch between tasks (X11 windows, buffers or recentf)"
  (interactive)
  (require 'ewmctrl)
  (select-frame-set-input-focus (window-frame (selected-window)))
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-wmctrl-windows
                   helm-source-buffers-list
                   helm-source-recentf 
                   helm-source-buffer-not-found)
        :buffer "*helm-taskswitch*"
        ))

(provide 'helm-taskswitch)

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-taskswitch.el ends here
