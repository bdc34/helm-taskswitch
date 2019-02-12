;;; helm-taskswitch.el --- Use helm to switch windows and buffers

;; Author: Brian Caruso <briancaruso@gmail.com>
;; Maintainer: Brian Caruso <briancaruso@gmail.com>
;; Created: 2016-05-27
;; URL: https://github.com/bdc34/helm-taskswitch
;; Package-Version: 20160728.001
;; Package-Requires: ((helm "3.0"))
;; Keywords: desktop, windows

;;; Commentary:

;; `helm-taskswitch' provides an X window and buffer switching with helm.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)
;; - [TODOs](#TODOs)

;; ## Installation

;; Install wmctrl and helm.

;; ## Usage

;; To activate use `M-x helm-task-switcher'.

;; ## Issues / bugs

;; If you discover an issue or bug in `helm-taskswitch' not already noted:

;; * as a TODO item, or

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `helm-task-switcher', wmctrl and helm.

;; ## TODOs

;; * Track or get with focus history and use it to order candidates. Current
;;   order is arbitrary.

;; Intersting blog post about alt-tab, suggests markov model
;; http://www.azarask.in/blog/post/solving-the-alt-tab-problem/
;; Ideas from that: other hot key for "go back to most recent window"

;; * Keep emacs filter emacs out of focus history when it is used for switching


;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version;; Code from https://github.com/flexibeast/ewmctrl is used under GNU 3.

;;; Code:


;; Customisable variables.

(defgroup helm-taskswitch nil
  "Use helm to switch between tasks (X11 windows or buffers)"
  :group 'external)

(defcustom helm-taskswitch-wmctrl-path "/usr/bin/wmctrl"
  "Absolute path to wmctrl executable."
  :type '(file :must-match t)
  :group 'helm-taskswitch
) 

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


(defcustom helm-taskswitch-pre-switch-hook '()
  "Called before a switch to a client, not called in other helm sources, buffers, etc.

Each Function is called with two arguments, 
First the x window id.
Second, the candidate window associative list."
  :type '(repete function)
  :group 'helm-taskswitch)

;; Internal Code

(defun helm-taskswitch-format-candidate (al)
  "Formats a candidate client window for task switcher.

  AL is the associative list for a window."
  (cons
   (format "%-18s  %s" (helm-taskswitch-wmclass al) (helm-taskswitch-title al))
   (list al)))

(defun helm-taskswitch-title (al)
  "Formats the title of a window.

  AL is an associative list for a window."
  (let* ( (title   (cdr (assoc 'title   al)))
          (wmclass (cdr (assoc 'wmclass al))))
     ;; redundent
    (replace-regexp-in-string "- Google Chrome" ""
                              ;; bad title for cornell's Jira
                              (replace-regexp-in-string "- Cornell University Library https://culibrary.atlassian.net.*" "Jira Issues" title))))


(defun helm-taskswitch-wmclass (al)
  (let ((title   (cdr (assoc 'title   al)))
        (wmclass (cdr (assoc 'wmclass al))))
    (cond ((string-equal wmclass "google-chrome.Google-chrome" )
           (propertize "Google-chrome" 'face 'helm-taskswitch-browser-face ))
          ((string-equal wmclass "terminator.Terminator")
           (propertize "Terminator" 'face 'helm-taskswitch-term-face))
          ((string-equal wmclass "emacs.Emacs")
           (propertize "Emacs" 'face 'helm-taskswitch-emacs-face))
          (t wmclass) )))

(setq helm-taskswitch--wmctrl-field-count 10)

(defun helm-taskswitch--list-windows ()
  "Internal function to get a list of desktop windows via `wmctrl'."
  (let ((bfr (generate-new-buffer " *helm-taskswitch-wmctrl-output*"))
        (fields-re (concat "^"
                           (mapconcat 'identity (make-list (1- helm-taskswitch--wmctrl-field-count) "\\(\\S-+\\)\\s-+") "")
                           "\\(.+\\)"))
        (windows-list '()))
    (call-process-shell-command (concat helm-taskswitch-wmctrl-path " -lGxp") nil bfr)
    (with-current-buffer bfr
      (goto-char (point-min))
      (while (re-search-forward fields-re nil t)
        (let ((window-id (match-string 1))
              (desktop-number (match-string 2))
              (pid (match-string 3))
              (x-offset (match-string 4))
              (y-offset (match-string 5))
              (width (match-string 6))
              (height (match-string 7))
              (wmclass (match-string 8))
              (client-host (match-string 9))
              (title (match-string 10)))
          (setq windows-list
                (append windows-list
                        (list
                         `((window-id . ,window-id)
                           (desktop-number . ,desktop-number)
                           (pid . ,pid)
                           (x-offset . ,x-offset)
                           (y-offset . ,y-offset)
                           (width . ,width)
                           (height . ,height)
                           (wmclass . ,wmclass)
                           (client-host . ,client-host)
                           (title . ,title))))))))
    (kill-buffer bfr)
    windows-list))



(defun helm-taskswitch-focus-window-by-cancidate (c)
  "Internal function to focus the desktop window specified by ID."
  (let ((id (cdr (assoc 'window-id (car c)))))
    (dolist (f helm-taskswitch-pre-switch-hook) (funcall f c))
    (call-process-shell-command (concat helm-taskswitch-wmctrl-path " -i -a '" id "'"))))
  
(defun helm-taskswitch-close-candidates (c)
  "Closes a candidate window from helm-taskswitch--list-windows"
  (mapc (lambda (c)
          (let* ((id (cdr (assoc 'window-id (car c))))
                 (killcmd (concat helm-taskswitch-wmctrl-path " -i -c '" id "'")))
            (message killcmd)
            (call-process-shell-command killcmd )))
        (helm-marked-candidates)))


(defun helm-taskswitch-client-candidates ()
  "Return a list windows with title and wmclass."
  (mapcar 'helm-taskswitch-format-candidate (helm-taskswitch--list-windows)) )

(setq  helm-source-wmctrl-windows
       (helm-build-sync-source
           "client windows sync"
         :fuzzy-match t
         :candidates 'helm-taskswitch-client-candidates
         :action '(("Forground" . helm-taskswitch-focus-window-by-cancidate )
                   ("close window" . helm-taskswitch-close-candidates )
                   ("Split top bottom" . (lambda (candidate) (message "Not yet implemented: top-bottom %s" candidate)))
                   ("Split right left" . (lambda (candidate) (message "Not yet implemented: right-left %s" candidate)))
                   ;;("preview" . (lambda (c) ( helm-taskswitch-open-preview-jpg (cdr (assoc 'window-id (car c))))))
                   ("dump client window s-exp" . prin1 ))))

;;;###autoload
(defun helm-taskswitch ()
  "Use helm to switch between tasks (X11 windows, buffers or recentf)"
  (interactive)
  (run-hooks 'helm-taskswitch-open-hooks )
  (select-frame-set-input-focus (window-frame (selected-window)))


  
  (unless helm-source-buffers-list
   (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))

;; consider :resume t
;           :preselect 
  (helm :sources '(helm-source-wmctrl-windows
                   helm-source-buffers-list
                   helm-source-recentf 
                   helm-source-buffer-not-found)
        :buffer "*helm-taskswitch*"
        ))

(provide 'helm-taskswitch)

;;-------------------------------------------------------------------------
;; In progress:  of ordering of candidates 

;; TODO Need focus history before emacs is focused for taskswitch
;; get this with a process that runs xprop -root -spy
;; and watches for _NET_ACTIVE_WINDOW

;; TODO sort candidates based history
;; Get most recent window before task switch, look at out freqs, order by that,
;; then, for all remain windows, order by global in freq
;; (let* ((all (get-all-windows))
;;        (current (get-current-window))
;;        (out-list (get-out-list current))
;;        (never-out ( list-subtract all out-list))
;;        (candidate (append out-list (order-like never-out global-in-freq))))  
;;   candidate)

;; ( in-freqs * per-win-out-freqs * current_win_id )
;;
;; freq-req = ( winid , (times...))
;;
;; in-freqs = tree< freq-req >
;;
;; per-win-out-freqs = tree< (winid, tree< freq-rec >)>

;; for xproc -spy
;;   on _NET_ACTIVE_WINDOW: update-per-win-out-freq( current_win_id, new_win_id)
;;                          current_win_id = new_win_id
;;   
;;   
;; TODO avoid recording emacs in history when used as task switcher
;; Cannot just remove the emacs switch sicne emacs might have been focused
;; before the call to helm-taskswitch. Maybe check before the call to
;; select-frame-set-input-focus?
;; something like: if not emacs-focused-p: bring-forward-but-remove-from-history


;  First stab at how to do this: 1 in emacs use a
;; process to run xproc -root -spy to listen for focus change events 2
;;
;; (defun helm-taskswitch--track-history (proc string)
;;   ... keep track of focus history ... )
;; 
;; (make-process :command '('xproc' '-root' '-spy')
;;    :connection-type 'pipe'
;;    :filter helm-taskswitch--track-history) 


(require 's)

(defun to-wids  (string)
  "For a string, find all events that look like a focus change and return
   a list of the window ids in order"

  ;; interesting events:
  ;; NET_ACTIVE_WINDOW - newly active/focused window
  ;; NET_CLIENT_LIST - all client windows
  (let ((foc-regex  "_NET_ACTIVE_WINDOW(WINDOW): window id # 0x\\([0-9a-f][0-9a-f]+\\)"))
    (mapcar 'cadr (s-match-strings-all foc-regex string))))

;; 

(defun ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (lines (to-wids string )))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (mapc (lambda (s) (insert (format "%s\n" s))) lines)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

 (make-process
  :name "xproc-focus-spy"
  :buffer "*xproc-focus-spy*"
  :command '("xprop" "-root" "-spy")
  :noquery t
  :filter 'ordinary-insertion-filter
  )


;; (defun helm-taskswitch-open-preview-jpg (id)
;;   "Make a preview jpg of a x window in /tmp/id.jpg"

;;   ;; This is too slow and buggy.
  
;;   ;; Encountering some bug in xwd a lot, it looks like this
;;   ;;xwd: Getting target window information.
;;   ;;
;;   ;;X Error of failed request:  BadMatch (invalid parameter attributes)
;;   ;;  Major opcode of failed request:  73 (X_GetImage)
;;   ;;  Serial number of failed request:  255
;;   ;;  Current serial number in output stream:  255

;;  (let* ((tmp-win-file (concat "/tmp/" id ".jpg"))
;;         (cmd (concat "/usr/bin/xwd" " -silent -id '" id "' | /usr/bin/convert - " tmp-win-file )))
;;       (progn
;;        (message cmd)
;;        (call-process-shell-command cmd)
;;        (find-file tmp-win-file))))

;; (defun helm-taskswitch-make-preview-windows (wmctrl-list)
;;    "Make a directory of preview jpegs"
;;    (mapcar 'make-preview-jpg wmctrl-list))


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
