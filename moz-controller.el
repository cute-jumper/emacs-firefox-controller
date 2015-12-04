;;; moz-controller.el --- Control Firefox from Emacs

;; Copyright (C) 2014 任文山 (Ren Wenshan)

;; Author: 任文山 (Ren Wenshan) <renws1990 at gmail.com>
;; URL: https://github.com/RenWenshan/emacs-moz-controller
;; Version: 0.0.1
;; Package-Requires: ((moz "0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; With this program and Firefox plugin `MozRepl', you can control your Firefox
;; to scroll up, scroll down, zoom in, zoom out, switch tabs, close current
;; tabs and etc.

;;; Installation: see the Installation section in the README.org

;;; Usage: see the Usage section in the README.org

;;; Code:
(require 'moz)

(defgroup moz-controller nil
  "Control Firefox from Emacs"
  :group 'moz-controller)

(defcustom moz-controller-zoom-step 0.1
  "Zoom step, default to 0.1, it is supposed to be a positive number."
  :group 'moz-controller
  :type 'number)

(defvar moz-controller-mode-map nil
  "Keymap for controlling Firefox from Emacs.")

(defvar moz-controller-mode-hook nil
  "Hook to run upon entry into moz-controller-mode.")

(defvar moz-controller-repl-output ""
  "Output from *MozRepl*.")

(defvar moz-controller-command-type nil
  "The type of command that we send to *MozRepl*.")

(defun moz-controller-repl-filter (output)
  "Filter function of *MozRepl*.

It gets the useful output of *MozRepl*, store it in `moz-controller-repl-output` and `kill-ring`"
  (unless (string= output "repl> ")   ; ignore empty output (page up, page down, etc)
    (setq moz-controller-repl-output
          (replace-regexp-in-string "\"\\(\\(.*\n?\\)*\\)\"\nrepl> " "\\1" output))
    (cond ((eq moz-controller-command-type 'moz-controller-get-current-url-type)
           (message moz-controller-repl-output)
           ;; append to kill-ring
           (kill-new moz-controller-repl-output))
          ((eq moz-controller-command-type 'moz-controller-switch-tab-type)
           (let* ((tab-titles (split-string moz-controller-repl-output "\n"))
                  (selected-title
                   (completing-read "Select tab: " tab-titles)))
             (moz-controller-send
              (format
               "gBrowser.selectTabAtIndex(%s);"
               (position selected-title tab-titles :test 'equal)))))
          ((eq moz-controller-command-type 'moz-controller-switch-tab-by-id-show-id)
           (moz-controller-send
            (format
             "t.map(function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});gBrowser.selectTabAtIndex(%s);"
             (read-string "Tab id: ")))))))

(defun moz-controller-send (command &optional command-type)
  "Set command type and send COMMAND to `inferior-moz-process'."
  (setq moz-controller-command-type command-type)
  (comint-simple-send (inferior-moz-process) command))

(defmacro moz-controller-defun (name doc command &optional command-type)
  "Macro for defining moz commands.

NAME: function name.
DOC: docstring for the function.
COMMAND: the desired JavaScript expression, as a string.
COMMAND-TYPE: the type of the command that is used for output filtering."
  (declare (indent 1)
           (doc-string 2))
  `(defun ,name ()
     ,doc
     (interactive)
     (moz-controller-send ,command ,command-type)))

(moz-controller-defun moz-controller-page-refresh
  "Refresh current page"
  "setTimeout(function(){content.document.location.reload(true);}, '500');")

(moz-controller-defun moz-controller-page-down
  "Scroll down the current window by one page."
  "content.window.scrollByPages(1);")

(moz-controller-defun moz-controller-page-up
  "Scroll up the current window by one page."
  "content.window.scrollByPages(-1);")

(moz-controller-defun moz-controller-page-top
  "Move to the top of the page."
  "goDoCommand(\"cmd_moveTop\");")

(moz-controller-defun moz-controller-page-bottom
  "Move to the bottom of the page."
  "goDoCommand(\"cmd_moveBottom\");")

(moz-controller-defun moz-controller-tab-close
  "Close current tab."
  "content.window.close();")

(moz-controller-defun moz-controller-zoom-in
  "Zoom in."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom += "
          (number-to-string moz-controller-zoom-step) ";"))

(moz-controller-defun moz-controller-zoom-out
  "Zoom out."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom -= "
          (number-to-string moz-controller-zoom-step) ";"))

(moz-controller-defun moz-controller-zoom-reset
  "Zoom reset."
  "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom = 1")

(moz-controller-defun moz-controller-tab-previous
  "Switch to the previous tab."
  "gBrowser.tabContainer.advanceSelectedTab(-1, true);")

(moz-controller-defun moz-controller-tab-next
  "Switch to the next tab."
  "gBrowser.tabContainer.advanceSelectedTab(1, true);")

(moz-controller-defun moz-controller-view-page-source
  "View current page source code."
  "BrowserViewSourceOfDocument(gBrowser.contentDocument);")

(moz-controller-defun moz-controller-get-current-url
  "Get the current tab's URL and add to `kill-ring'."
  "gBrowser.contentWindow.location.href;"
  'moz-controller-get-current-url-type)

(moz-controller-defun moz-controller-select-all
  "Select all the content in the current page."
  "goDoCommand('cmd_selectAll')")

(moz-controller-defun moz-controller-copy
  "Copy."
  "goDoCommand('cmd_copy');")

(moz-controller-defun moz-controller-paste
  "Paste."
  "goDoCommand('cmd_paste');")

(moz-controller-defun moz-controller-cut
  "Cut."
  "goDoCommand('cmd_cut');")

(moz-controller-defun moz-controller-switch-tab
  "Switch the tab."
  "Array.prototype.map.call(gBrowser.tabs, function(tab) {return tab.label;}).join(\"\\n\");"
  'moz-controller-switch-tab-type)

(moz-controller-defun moz-controller-switch-tab-by-id
  "Switch the tab by id."
  "i=0;t=Array.prototype.slice.call(gBrowser.tabs);t.map(function(tab){tab.label=\"[\" + (i++) + \"]\" + tab.label;})"
  'moz-controller-switch-tab-by-id-show-id)

(moz-controller-defun moz-controller-new-tab
  "Add new tab."
  "gBrowser.addTab();")

(moz-controller-defun moz-controller-new-tab-and-switch
  "Add new tab and switch to it."
  "gBrowser.selectedTab = gBrowser.addTab();")

(moz-controller-defun moz-controller-startpage
  "Goto start page."
  "gBrowser.loadURI('about:home')")

(moz-controller-defun moz-controller-goto-url
  "Goto URL."
  (format "gBrowser.loadURI(\"http://%s\");" (read-string "Goto: http://")))

;; (defun moz-controller-edit ()
;;   (interactive)
;;   (moz-controller-send "a=Array.prototype.concat.call(Array.prototype.slice.call(content.document.getElementsByTagName('input')).filter(function(i){return (i.type == \"text\" || i.type == \"password\");}), Array.prototype.slice.call(content.document.getElementsByTagName('textarea')));i=-1;")
;;   (moz-controller-send "if (i != -1) a[i].style.backgroundColor=b;\
;; i=(i+1)%a.length;\
;; b=a[i].style.backgroundColor;\
;; a[i].style.backgroundColor='yellow';\
;; a[i].focus();")
;;   (moz-controller-send (format "a[i].value='%s';" (read-string "Input: "))))

(unless moz-controller-mode-map
  (setq moz-controller-mode-map
        (let ((moz-controller-map (make-sparse-keymap)))
          (define-key moz-controller-map (kbd "C-c m R") 'moz-controller-page-refresh)
          (define-key moz-controller-map (kbd "C-c m n") 'moz-controller-page-down)
          (define-key moz-controller-map (kbd "C-c m p") 'moz-controller-page-up)
          (define-key moz-controller-map (kbd "C-c m <") 'moz-controller-page-top)
          (define-key moz-controller-map (kbd "C-c m >") 'moz-controller-page-bottom)
          (define-key moz-controller-map (kbd "C-c m k") 'moz-controller-tab-close)
          (define-key moz-controller-map (kbd "C-c m b") 'moz-controller-tab-previous)
          (define-key moz-controller-map (kbd "C-c m f") 'moz-controller-tab-next)
          (define-key moz-controller-map (kbd "C-c m +") 'moz-controller-zoom-in)
          (define-key moz-controller-map (kbd "C-c m -") 'moz-controller-zoom-out)
          (define-key moz-controller-map (kbd "C-c m 0") 'moz-controller-zoom-reset)
          (define-key moz-controller-map (kbd "C-c m u") 'moz-controller-view-page-source)
          (define-key moz-controller-map (kbd "C-c m l") 'moz-controller-get-current-url)
          (define-key moz-controller-map (kbd "C-c m t") 'moz-controller-new-tab-and-switch)
          (define-key moz-controller-map (kbd "C-c m B") 'moz-controller-switch-tab)
          moz-controller-map)))

;;;###autoload
(define-minor-mode moz-controller-mode
  "Toggle moz-controller mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{moz-controller-mode-map}

Entry to this mode calls the value of `moz-controller-mode-hook'."

  :init-value nil
  :lighter " MozCtrl"
  :group 'moz-controller
  :keymap moz-controller-mode-map

  (let ((mode moz-controller-mode))
    (with-current-buffer (process-buffer (inferior-moz-process))
      (if mode
          (add-hook 'comint-output-filter-functions #'moz-controller-repl-filter nil t)
        (remove-hook 'comint-output-filter-functions #'moz-controller-repl-filter t))))
  (if moz-controller-mode
      (run-mode-hooks 'moz-controller-mode-hook)))

;;;###autoload
(define-globalized-minor-mode moz-controller-global-mode
  moz-controller-mode
  moz-controller-on)

(defun moz-controller-on ()
  "Enable moz-controller minor mode."
  (moz-controller-mode t))

(defun moz-controller-off ()
  "Disable moz-controller minor mode."
  (moz-controller-mode nil))

(defun moz-controller-global-on ()
  "Enable moz-controller global minor mode."
  (moz-controller-global-mode t))

(defun moz-controller-global-off ()
  "Disable moz-controller global minor mode."
  (moz-controller-global-mode nil))

(provide 'moz-controller)
;;; moz-controller.el ends here
