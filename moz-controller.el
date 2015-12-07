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
          ((eq moz-controller-command-type 'moz-controller-switch-tab-by-id-type)
           (moz-controller-send
            (format
             "Array.prototype.map.call(gBrowser.tabs,\
function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});\
gBrowser.selectTabAtIndex(%s);"
             (read-string "Tab id: "))))
          ((eq moz-controller-command-type 'moz-controller-search-start-type)
           (moz-controller-search-edit)))))

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

(moz-controller-defun moz-controller-page-line-down
  "Scroll down the current window by one line."
  "goDoCommand('cmd_scrollLineDown');")

(moz-controller-defun moz-controller-page-line-up
  "Scroll up the current window by one line."
  "goDoCommand('cmd_scrollLineUp');")

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
  "(function(){\
var i=0;\
Array.prototype.slice.call(gBrowser.tabs).map(function(tab){tab.label=\"[\" + (i++) + \"]\" + tab.label;});\
})();"
  'moz-controller-switch-tab-by-id-type)

(moz-controller-defun moz-controller-new-tab
  "Add new tab."
  "gBrowser.addTab();")

(moz-controller-defun moz-controller-new-tab-and-switch
  "Add new tab and switch to it."
  "gBrowser.selectedTab = gBrowser.addTab();")

(moz-controller-defun moz-controller-startpage
  "Goto start page."
  "gBrowser.loadURI('about:home');")

(moz-controller-defun moz-controller-goto-url
  "Goto URL."
  (format "gBrowser.loadURI(\"http://%s\");" (read-string "Goto: http://")))

(moz-controller-defun moz-controller-go-forward
  "Foward."
  "gBrowser.goForward();")

(moz-controller-defun moz-controller-go-back
  "Back."
  "gBrowser.goBack();")

(moz-controller-defun moz-controller-maximize-window
  "Maximize window."
  "maximize();")

(moz-controller-defun moz-controller-minimize-window
  "Minimize window."
  "minimize();")

(moz-controller-defun moz-controller-restore-window
  "Restore window."
  "restore();")

(moz-controller-defun moz-controller-search-start
  "Start search"
  "gFindBar.open();"
  'moz-controller-search-start-type)

(defsubst moz-controller-search-help ()
  (message "[n]: search forward; [p]: search backward; [e]: edit search string; any other keys quit search."))

(moz-controller-defun moz-controller-search-edit
  "Edit search string."
  ;; FIXME
  (let ((map (make-sparse-keymap))
        (search-string
         (progn (setq overriding-local-map)
                (read-string "Search: "))))
    (define-key map "n" #'moz-controller-search-next)
    (define-key map "p" #'moz-controller-search-previous)
    (define-key map "e" #'moz-controller-search-edit)
    (define-key map [t] #'moz-controller-search-quit)
    (add-hook 'mouse-leave-buffer-hook #'moz-controller-search-quit)
    (add-hook 'kbd-macro-termination-hook #'moz-controller-search-quit)
    (setq overriding-local-map map)
    (moz-controller-search-help)
    (format "gFindBar._findField.value='%s';" search-string)))

(moz-controller-defun moz-controller-search-next
  "Goto next search."
  (progn (moz-controller-search-help)
         "gFindBar.onFindAgainCommand(false);"))

(moz-controller-defun moz-controller-search-previous
  "Goto previous search."
  (progn (moz-controller-search-help)
         "gFindBar.onFindAgainCommand(true);"))

(moz-controller-defun moz-controller-search-quit
  "Quit search."
  (progn
    (setq overriding-local-map)
    (remove-hook 'mouse-leave-buffer-hook #'moz-controller-search-quit)
    (remove-hook 'kbd-macro-termination-hook #'moz-controller-search-quit)
    (message "Quit moz search.")
    "gFindBar.close();"))

(defvar moz-controller-remote-mode-map
  (let ((map (make-sparse-keymap)))
    ;; page
    (define-key map "r" #'moz-controller-page-refresh)
    (define-key map "j" #'moz-controller-page-line-down)
    (define-key map "k" #'moz-controller-page-line-up)
    (define-key map "n" #'moz-controller-page-down)
    (define-key map "p" #'moz-controller-page-up)
    (define-key map "<" #'moz-controller-page-top)
    (define-key map ">" #'moz-controller-page-bottom)
    ;; zoom
    (define-key map "+" #'moz-controller-zoom-in)
    (define-key map "-" #'moz-controller-zoom-out)
    (define-key map "0" #'moz-controller-zoom-reset)
    ;; tab
    (define-key map "x" #'moz-controller-tab-close)
    (define-key map "h" #'moz-controller-tab-previous)
    (define-key map "l" #'moz-controller-tab-next)
    (define-key map "t" #'moz-controller-new-tab-and-switch)
    (define-key map "T" #'moz-controller-new-tab)
    (define-key map "b" #'moz-controller-switch-tab)
    (define-key map "B" #'moz-controller-switch-tab-by-id)
    ;; navigation
    (define-key map "L" #'moz-controller-get-current-url)
    (define-key map "H" #'moz-controller-startpage)
    (define-key map "g" #'moz-controller-goto-url)
    (define-key map "f" #'moz-controller-go-forward)
    (define-key map "b" #'moz-controller-go-back)
    ;; select, cut, copy & paste
    (define-key map "a" #'moz-controller-select-all)
    (define-key map "W" #'moz-controller-cut)
    (define-key map "w" #'moz-controller-copy)
    (define-key map "y" #'moz-controller-paste)
    ;; window management
    (define-key map "^" #'moz-controller-maximize-window)
    (define-key map "&" #'moz-controller-restore-window)
    (define-key map "*" #'moz-controller-minimize-window)
    ;; search
    (define-key map "s" #'moz-controller-search-start)
    ;; switch to direct mode
    (define-key map (kbd "C-z") #'moz-controller-switch-to-direct-mode)
    ;; exit
    (define-key map "q" #'moz-controller-remote-mode-quit)
    map)
  "Keymap of `moz-controller-remote-mode'.")

(defun moz-controller-remote-mode-quit ()
  (interactive)
  ;;TODO
  )

(defun moz-controller-switch-to-direct-mode ()
  (interactive)
  ;;TODO
  )

(defvar moz-controller-remote-mode-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'moz-controller-search-next)
    (define-key map "p" #'moz-controller-search-previous)
    (define-key map "e" #'moz-controller-search-edit)
    (define-key map [t] #'moz-controller-search-quit)
    map)
  "Keymap of search in `moz-controller-remote-mode'.")

(defvar moz-controller--generate-key-function-string
  "function mozControllerGenerateKey(target,isCtrl,isAlt,isShift,keycode,charcode){\
if (target==gURLBar.inputField && keycode == KeyEvent.DOM_VK_RETURN) {gBrowser.loadURI(target.value); content.window.focus(); return;}\
else if (target == BrowserSearch.searchBar.textbox.inputField && keycode == KeyEvent.DOM_VK_RETURN) { BrowserSearch.searchBar.doSearch(target.value,'tab'); return;}\
var evt=document.createEvent('KeyboardEvent');\
evt.initKeyEvent('keypress',true,true,null,isCtrl,isAlt,isShift,false,keycode,charcode);\
target.dispatchEvent(evt);\
}")

(moz-send-string moz-controller--generate-key-function-string)

(defun moz-controller-send-key (charcode &optional ctrlp altp shiftp keycode target)
  (moz-controller-send (format "mozControllerGenerateKey(%s,%s,%s,%s,%s,%s);"
                               (or target "document.commandDispatcher.focusedElement || document")
                               (moz-controller-e2j ctrlp)
                               (moz-controller-e2j altp)
                               (moz-controller-e2j shiftp)
                               (or keycode "0")
                               charcode)))

(defconst moz-controller-special-key-table
  '((backspace . "BACK_SPACE")
    (prior . "PAGE_UP")
    (next . "PAGE_DOWN")
    (print . "PRINTSCREEN")))

(defun moz-controller-e2j (e)
  (pcase e
    ((pred booleanp) (if e "true" "false"))
    ((pred symbolp) (format "KeyEvent.DOM_VK_%s"
                            (or (assoc-default e moz-controller-special-key-table)
                                (upcase (symbol-name e)))))
    (_ 0)))

(moz-controller-send-key ?e)
(moz-controller-send-key 0 nil nil nil "KeyEvent.DOM_VK_BACK_SPACE")
(moz-controller-send-key ?v nil t)
(moz-controller-send-key 0 nil nil nil "KeyEvent.DOM_VK_RETURN")

(defvar moz-controller-direct-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'moz-controller-direct-mode-quit)
    (define-key map (kbd "C-G") #'moz-controller-highlight-focus)
    (define-key map (kbd "C-z") #'moz-controller-switch-to-remote-mode)
    (define-key map [t] #'moz-controller-direct-mode-send-key)
    map)
  "Keymap of `moz-controller-direct-mode'.")

(defun moz-controller-direct-mode ()
  (setq overriding-local-map map))

(defun moz-controller-switch-to-remote-mode ()
  (interactive)
  ;;TODO
  )

(moz-controller-defun moz-controller-highlight-focus
  "Highlight the focused element."
  "(function(){if (document.commandDispatcher.focusedElement) {\
var originalColor=document.commandDispatcher.focusedElement.style.backgroundColor;\
document.commandDispatcher.focusedElement.style.backgroundColor='yellow';\
setTimeout(function(){document.commandDispatcher.focusedElement.style.backgroundColor=originalColor;},1000);\
}})();")

(defun moz-controller-direct-mode-quit ()
  (interactive)
  (if (eq last-command 'moz-controller-direct-mode-quit)
      (progn
        (setq overriding-local-map)
        (message "Exit moz-controller-direct-mode."))
    (moz-send-string "content.window.focus();")
    (message "Move focus to firefox content window.
Press C-g again to exit moz-controller-direct-mode.")))

(defun moz-controller-direct-mode-send-key ()
  (interactive)
  (let* ((evt last-input-event)
         (mods (event-modifiers evt))
         (c (event-basic-type evt)))
    (message (concat "Key sent to firefox: "
                     (if mods (format "%s " mods) "")
                     (format (if (characterp c) "%c" "%s") c)))
    (moz-controller-send-key (if (characterp c) c 0)
                             (and (member 'control mods) t)
                             (and (member 'meta mods) t)
                             (and (member 'shift mods) t)
                             (moz-controller-e2j c))))

(defun debug ()
  (message "%s: %s" (event-modifiers last-input-event)
           (event-basic-type last-input-event)))
(add-hook 'post-command-hook 'debug)
(remove-hook 'post-command-hook 'debug)

;; (defun moz-controller-edit ()
;;   (interactive)
;;   (moz-controller-send "a=Array.prototype.concat.call(Array.prototype.slice.call(content.document.getElementsByTagName('input')).filter(function(i){return (i.type == \"text\" || i.type == \"password\");}), Array.prototype.slice.call(content.document.getElementsByTagName('textarea')));i=-1;")
;;   (moz-controller-send "if (i != -1) a[i].style.backgroundColor=b;\
;; i=(i+1)%a.length;\
;; b=a[i].style.backgroundColor;\
;; a[i].style.backgroundColor='yellow';\
;; a[i].focus();")
;;   (moz-controller-send (format "a[i].value='%s';" (read-string "Input: "))))

;;TODO REMOVE THIS PLEASE
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

(provide 'moz-controller)
;;; moz-controller.el ends here
