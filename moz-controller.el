;;; moz-controller.el --- Control Firefox from Emacs

;; Copyright (C) 2015 Junpeng Qiu
;; Copyright (C) 2014 任文山 (Ren Wenshan)

;; Author: Junpeng Qiu <qjpchmail@gmail.com>, 任文山 (Ren Wenshan)
;; URL: https://github.com/cute-jumper/emacs-moz-controller+
;; Version: 0.1
;; Package-Requires: ((moz "0") (popwin "1.0.0"))
;; Keywords: extensions

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
(require 'popwin)
(require 'font-lock)

(defgroup moz-controller nil
  "Control Firefox from Emacs"
  :group 'moz-controller)

(defcustom moz-controller-zoom-step 0.1
  "Zoom step, default to 0.1, it is supposed to be a positive number."
  :group 'moz-controller
  :type 'number)

;; --------------------- ;;
;; global vars and utils ;;
;; --------------------- ;;
(defvar moz-controller--overriding-keymap nil
  "Original `overriding-local-map'.")

(defun moz-controller--safe-read-string (prompt &optional callback)
  (let (overriding-local-map)
    (condition-case err
        (replace-regexp-in-string "\'" "\\\\'" (read-string prompt))
      (quit (and callback (funcall callback))
            (signal 'quit nil)))))

(defun moz-controller--send (command &optional command-type)
  "Set command type and send COMMAND to `inferior-moz-process'."
  (setq moz-controller--remote-command-type command-type)
  (comint-simple-send (inferior-moz-process) command))

(defun moz-controller--make-keymap (keymap-alist)
  (let ((map (make-sparse-keymap))
        key)
    (dolist (module keymap-alist)
      (dolist (lst (cdr module))
        (setq key (cadr lst))
        (define-key map (if (integer-or-marker-p key)
                            (kbd key)
                          key)
          (car lst))))
    map))

;; -------------------------- ;;
;; utils to build help window ;;
;; -------------------------- ;;
(defvar moz-controller--help-window nil)

(defun moz-controller--popwin (size)
  (interactive)
  (when (not (window-live-p moz-controller--help-window))
    (with-current-buffer (window-buffer (setq moz-controller--help-window
                                              (cadr
                                               (popwin:create-popup-window size))))
      (setq mode-line-format nil)))
  moz-controller--help-window)

(defun moz-controller--show-help-from-keymap-alist (keymap-alist column-count)
  (let ((help "")
        (index 0)
        (line-count 0)
        (separator (propertize "→" 'face font-lock-builtin-face))
        first-column-max-widths
        second-column-max-widths)
    (dolist (module keymap-alist)
      (setq help (concat help
                         (propertize (car module) 'face font-lock-constant-face) "\n")
            line-count (1+ line-count)
            first-column-max-widths (make-list column-count 0)
            second-column-max-widths (make-list column-count 0))
      (dolist (lst (cdr module))
        (let* ((idx (mod index column-count))
               (curr-val-1 (nth idx first-column-max-widths))
               (curr-val-2 (nth idx second-column-max-widths)))
          (setf (nth idx first-column-max-widths)
                (max curr-val-1 (length (if (vectorp (nth 1 lst))
                                            "any other key"
                                          (nth 1 lst)))))
          (setf (nth idx second-column-max-widths)
                (max curr-val-2 (length (nth 2 lst))))
          (setq index (1+ index))))
      (setq index 0)
      (dolist (lst (cdr module))
        (let ((idx (mod index column-count))
              (key (cadr lst)))
          (and (> index 0)
               (= idx 0)
               (setq help (concat help "\n"))
               (setq line-count (1+ line-count)))
          (setq help (concat help " "
                             (format
                              (format "%%%ds %%s %%-%ds"
                                      (nth idx first-column-max-widths)
                                      (nth idx second-column-max-widths))
                              (propertize (if (vectorp key) "any other key" key)
                                          'face font-lock-keyword-face)
                              separator
                              (propertize (nth 2 lst) 'face font-lock-function-name-face))))
          (setq index (1+ index))))
      (setq help (concat help "\n"))
      (setq line-count (1+ line-count))
      (setq index 0))
    (with-current-buffer (window-buffer (moz-controller--popwin line-count))
      (erase-buffer)
      (insert help))))

(defun moz-controller--hide-current-help ()
  (when (window-live-p moz-controller--help-window)
    (delete-window moz-controller--help-window)))

;; -------------------------------------------------------------------------- ;;
;; #####  ###### #    #  ####  ##### ######       #    #  ####  #####  ###### ;;
;; #    # #      ##  ## #    #   #   #            ##  ## #    # #    # #      ;;
;; #    # #####  # ## # #    #   #   #####  ##### # ## # #    # #    # #####  ;;
;; #####  #      #    # #    #   #   #            #    # #    # #    # #      ;;
;; #   #  #      #    # #    #   #   #            #    # #    # #    # #      ;;
;; #    # ###### #    #  ####    #   ######       #    #  ####  #####  ###### ;;
;; -------------------------------------------------------------------------- ;;
(defvar moz-controller--repl-output ""
  "Output from *MozRepl*.")

(defvar moz-controller--remote-search-string nil)

(defvar moz-controller--remote-command-type nil
  "The type of command that we send to *MozRepl*.")

;; ----------------------- ;;
;; remote-mode help system ;;
;; ----------------------- ;;
(defun moz-controller--remote-mode-show-command (func-sym)
  (let (doc)
    (catch 'break
      (dolist (module moz-controller--remote-mode-keymap-alist)
        (dolist (lst (cdr module))
          (when (eq (car lst) func-sym)
            (setq doc (nth 2 lst))
            (throw 'break nil)))))
    (message "Send command: %s" doc)))

(defun moz-controller--remote-mode-search-show-string ()
  (message "Search: %s" moz-controller--remote-search-string))

(defun moz-controller--remote-mode-search-show-help ()
  (moz-controller--show-help-from-keymap-alist
   moz-controller--remote-mode-search-keymap-alist
   4))

(defun moz-controller--remote-mode-show-help ()
  (moz-controller--show-help-from-keymap-alist
   moz-controller--remote-mode-keymap-alist
   3))

;; -------------------------------------------------------------------- ;;
;; convenient macro to define commands for `moz-controller-remote-mode' ;;
;; -------------------------------------------------------------------- ;;
(defmacro moz-controller-remote-defun (name doc command &optional not-helpful-p &rest filter-body)
  "Macro for defining moz-controller commands."
  (declare (indent 1)
           (doc-string 2))
  (let ((filter-name (intern (format "%S-repl-filter" name)))
        (no-filter-p (not filter-body))
        (command-type (and filter-body (intern (format "%S-type" name)))))
    (if no-filter-p
        `(defun ,name ()
           ,doc
           (interactive)
           (moz-controller--send ,command)
           (or ,not-helpful-p (moz-controller--remote-mode-show-command ',name)))
      `(progn
         (defun ,name ()
           ,doc
           (interactive)
           (with-current-buffer (process-buffer (inferior-moz-process))
             (add-hook 'comint-output-filter-functions #',filter-name nil t))
           (moz-controller--send ,command ',command-type)
           (or ,not-helpful-p (moz-controller--remote-mode-show-command ',name)))
         (defun ,filter-name (output)
           (setq moz-controller--repl-output
                 (replace-regexp-in-string "\"\\(\\(.*\n?\\)*\\)\"\nrepl> " "\\1" output))
           (when (eq moz-controller--remote-command-type ',command-type)
             (unwind-protect
                 (progn
                   ,@filter-body)
               (with-current-buffer (process-buffer (inferior-moz-process))
                 (remove-hook 'comint-output-filter-functions #',filter-name))
               (setq moz-controller--remote-command-type))))))))

;; ---------------------------- ;;
;; Various remote-mode commands ;;
;; ---------------------------- ;;
(moz-controller-remote-defun moz-controller-page-refresh
  "Refresh current page"
  "setTimeout(function(){content.document.location.reload(true);}, '500');")

(moz-controller-remote-defun moz-controller-page-line-down
  "Scroll down the current window by one line."
  "goDoCommand('cmd_scrollLineDown');")

(moz-controller-remote-defun moz-controller-page-line-up
  "Scroll up the current window by one line."
  "goDoCommand('cmd_scrollLineUp');")

(moz-controller-remote-defun moz-controller-page-down
  "Scroll down the current window by one page."
  "content.window.scrollByPages(1);")

(moz-controller-remote-defun moz-controller-page-up
  "Scroll up the current window by one page."
  "content.window.scrollByPages(-1);")

(moz-controller-remote-defun moz-controller-page-top
  "Move to the top of the page."
  "goDoCommand('cmd_moveTop');")

(moz-controller-remote-defun moz-controller-page-bottom
  "Move to the bottom of the page."
  "goDoCommand('cmd_moveBottom');")

(moz-controller-remote-defun moz-controller-tab-close
  "Close current tab."
  "content.window.close();")

(moz-controller-remote-defun moz-controller-zoom-in
  "Zoom in."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom += "
          (number-to-string moz-controller-zoom-step) ";"))

(moz-controller-remote-defun moz-controller-zoom-out
  "Zoom out."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom -= "
          (number-to-string moz-controller-zoom-step) ";"))

(moz-controller-remote-defun moz-controller-zoom-reset
  "Zoom reset."
  "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom = 1")

(moz-controller-remote-defun moz-controller-tab-previous
  "Switch to the previous tab."
  "gBrowser.tabContainer.advanceSelectedTab(-1, true);")

(moz-controller-remote-defun moz-controller-tab-next
  "Switch to the next tab."
  "gBrowser.tabContainer.advanceSelectedTab(1, true);")

(moz-controller-remote-defun moz-controller-view-page-source
  "View current page source code."
  "BrowserViewSourceOfDocument(gBrowser.contentDocument);")

(moz-controller-remote-defun moz-controller-get-current-url
  "Get the current tab's URL and add to `kill-ring'."
  "gBrowser.contentWindow.location.href;"
  nil
  (message "URL copied: %s" moz-controller--repl-output)
  (kill-new moz-controller--repl-output))

(moz-controller-remote-defun moz-controller-select-all
  "Select all the content in the current page."
  "goDoCommand('cmd_selectAll');")

(moz-controller-remote-defun moz-controller-unselect
  "Unselect."
  "goDoCommand('cmd_selectNone');")

(moz-controller-remote-defun moz-controller-copy
  "Copy."
  "goDoCommand('cmd_copy');")

(moz-controller-remote-defun moz-controller-paste
  "Paste."
  "goDoCommand('cmd_paste');")

(moz-controller-remote-defun moz-controller-cut
  "Cut."
  "goDoCommand('cmd_cut');")

(moz-controller-remote-defun moz-controller-switch-tab
  "Switch the tab."
  "Array.prototype.map.call(gBrowser.tabs, function(tab) {return tab.label;}).join(\"\\n\");"
  t
  (let* (overriding-local-map
         (tab-titles (split-string moz-controller--repl-output "\n"))
         (selected-title
          (completing-read "Select tab: " tab-titles)))
    (moz-controller--send
     (format
      "gBrowser.selectTabAtIndex(%s);"
      (position selected-title tab-titles :test 'equal)))))

(moz-controller-remote-defun moz-controller-switch-tab-by-id
  "Switch the tab by id."
  "(function(){\
var i=0;\
Array.prototype.slice.call(gBrowser.tabs).map(function(tab){tab.label=\"[\" + (i++) + \"]\" + tab.label;});\
})();"
  t
  (moz-controller--send
   (format
    "Array.prototype.map.call(gBrowser.tabs,\
function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});\
gBrowser.selectTabAtIndex(%d);"
    (string-to-int
     (moz-controller--safe-read-string "Tab id: "
                                       (lambda ()
                                         (moz-controller--send
                                          "Array.prototype.map.call(gBrowser.tabs,\
 function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});")))))))

(moz-controller-remote-defun moz-controller-new-tab
  "Add new tab."
  "gBrowser.addTab();")

(moz-controller-remote-defun moz-controller-new-tab-and-switch
  "Add new tab and switch to it."
  "gBrowser.selectedTab = gBrowser.addTab();")

(moz-controller-remote-defun moz-controller-startpage
  "Goto start page."
  "gBrowser.loadURI('about:home');")

(moz-controller-remote-defun moz-controller-goto-url
  "Goto URL."
  (format "gBrowser.loadURI('http://%s');"
          (moz-controller--safe-read-string "Goto: http://")))

(moz-controller-remote-defun moz-controller-go-forward
  "Foward."
  "gBrowser.goForward();")

(moz-controller-remote-defun moz-controller-go-back
  "Back."
  "gBrowser.goBack();")

(moz-controller-remote-defun moz-controller-maximize-window
  "Maximize window."
  "maximize();")

(moz-controller-remote-defun moz-controller-minimize-window
  "Minimize window."
  "minimize();")

(moz-controller-remote-defun moz-controller-restore-window
  "Restore window."
  "restore();")

;; --------------------------- ;;
;; remote-mode search commands ;;
;; --------------------------- ;;
(moz-controller-remote-defun moz-controller-search-start
  "Start search."
  "gFindBar.open();"
  t
  (moz-controller-search-edit))

(moz-controller-remote-defun moz-controller-search-edit
  "Edit search string."
  (progn (moz-controller--hide-current-help)
         (setq moz-controller--remote-search-string
               (moz-controller--safe-read-string "Search: "
                                                 #'moz-controller-search-quit))
         (add-hook 'mouse-leave-buffer-hook #'moz-controller-search-quit)
         (add-hook 'kbd-macro-termination-hook #'moz-controller-search-quit)
         (setq overriding-local-map moz-controller-remote-mode-search-map)
         (moz-controller--remote-mode-search-show-help)
         (moz-controller--remote-mode-search-show-string)
         (format "gFindBar._findField.value='%s';" moz-controller--remote-search-string))
  t)

(moz-controller-remote-defun moz-controller-search-next
  "Goto next search."
  (progn (moz-controller--remote-mode-search-show-string)
         "gFindBar.onFindAgainCommand(false);")
  t)

(moz-controller-remote-defun moz-controller-search-previous
  "Goto previous search."
  (progn (moz-controller--remote-mode-search-show-string)
         "gFindBar.onFindAgainCommand(true);")
  t)

(moz-controller-remote-defun moz-controller-search-quit
  "Quit search."
  (progn
    (setq overriding-local-map moz-controller-remote-mode-map)
    (remove-hook 'mouse-leave-buffer-hook #'moz-controller-search-quit)
    (remove-hook 'kbd-macro-termination-hook #'moz-controller-search-quit)
    (moz-controller--hide-current-help)
    (moz-controller--remote-mode-show-help)
    "gFindBar.close();")
  t)

;; ------------- ;;
;; search keymap ;;
;; ------------- ;;
(defvar moz-controller--remote-mode-search-keymap-alist
  '(("moz-remote-mode-search" .
     ((moz-controller-search-next "n" "search forward")
      (moz-controller-search-previous "p" "search backward")
      (moz-controller-search-edit "e" "edit search string")
      (moz-controller-search-quit [t] "quit search")))))

(defvar moz-contnroller-remote-mode-search-map
  (moz-controller--make-keymap moz-controller--remote-mode-search-keymap-alist)
  "Keymap of search in `moz-controller-remote-mode'.")

;; ------------------ ;;
;; remote-mode keymap ;;
;; ------------------ ;;
(defvar moz-controller--remote-mode-keymap-alist
  `(("page" .
     ((moz-controller-page-refresh "r" "refresh")
      (moz-controller-page-line-down "j" "line down")
      (moz-controller-page-line-up "k" "line up")
      (moz-controller-page-down "n" "page down")
      (moz-controller-page-up "p" "page up")
      (moz-controller-page-top "<" "page top")
      (moz-controller-page-bottom ">" "page bottom")))
    ("zoom" .
     ((moz-controller-zoom-in "+" "zoom in")
      (moz-controller-zoom-out "-" "zoom out")
      (moz-controller-zoom-reset "0" "zoom reset")))
    ("tab" .
     ((moz-controller-tab-close "x" "close tab")
      (moz-controller-tab-previous "h" "previous tab")
      (moz-controller-tab-next "l" "next tab")
      (moz-controller-new-tab-and-switch "t" "new tab and switch")
      (moz-controller-new-tab "T" "new tab in background")
      (moz-controller-switch-tab "C-b" "switch tab by name")
      (moz-controller-switch-tab-by-id "M-b" "switch tab by id")))
    ("navigation" .
     ((moz-controller-get-current-url "L" "copy current url")
      (moz-controller-startpage "H" "homepage")
      (moz-controller-goto-url "g" "goto url")
      (moz-controller-go-forward "f" "forward")
      (moz-controller-go-back "b" "backward")))
    ("edit" .
     ((moz-controller-select-all "a" "select all")
      (moz-controller-unselect "u" "unselect")
      (moz-controller-cut "W" "cut")
      (moz-controller-copy "w" "copy")
      (moz-controller-paste "y" "paste")))
    ("window" .
     ((moz-controller-maximize-window "^" "maximize")
      (moz-controller-restore-window "&" "restore")
      (moz-controller-minimize-window "*" "minimize")))
    ("misc" .
     ((moz-controller-search-start "s" "search-mode")
      (moz-controller-switch-to-direct-mode "C-z" "switch to moz-controller-direct-mode")
      (moz-controller-remote-mode-quit "q" "quit")))))

(defvar moz-controller-remote-mode-map
  (let ((map (moz-controller--make-keymap
              moz-controller--remote-mode-keymap-alist)))
    (define-key map [t] (lambda () (interactive) (message "Undefined.")))
    map)
  "Keymap of `moz-controller-remote-mode'.")

;; -------------------------- ;;
;; Other remote-mode commands ;;
;; -------------------------- ;;
(defun moz-controller-remote-mode-quit ()
  (interactive)
  (remove-hook 'mouse-leave-buffer-hook #'moz-controller-remote-mode-quit)
  (remove-hook 'kbd-macro-termination-hook #'moz-controller-remote-mode-quit)
  (setq overriding-local-map moz-controller--overriding-keymap)
  (setq moz-controller--overriding-keymap)
  (moz-controller--hide-current-help)
  (message "Exit moz-controller-remote-mode."))

(defun moz-controller-switch-to-direct-mode ()
  (interactive)
  (moz-controller-remote-mode-quit)
  (moz-controller-direct-mode))

;;;###autoload
(defun moz-controller-remote-mode ()
  (interactive)
  (setq moz-controller--overriding-keymap overriding-local-map)
  (setq overriding-local-map moz-controller-remote-mode-map)
  (add-hook 'mouse-leave-buffer-hook #'moz-controller-remote-mode-quit)
  (add-hook 'kbd-macro-termination-hook #'moz-controller-remote-mode-quit)
  (message "Enter moz-controller-remote-mode.")
  (moz-controller--remote-mode-show-help))

;; --------------------------------------------------------------------- ;;
;; #####  # #####  ######  ####  #####       #    #  ####  #####  ###### ;;
;; #    # # #    # #      #    #   #         ##  ## #    # #    # #      ;;
;; #    # # #    # #####  #        #   ##### # ## # #    # #    # #####  ;;
;; #    # # #####  #      #        #         #    # #    # #    # #      ;;
;; #    # # #   #  #      #    #   #         #    # #    # #    # #      ;;
;; #####  # #    # ######  ####    #         #    #  ####  #####  ###### ;;
;; --------------------------------------------------------------------- ;;
(defconst moz-controller--special-key-table
  '((backspace . "BACK_SPACE")
    (prior . "PAGE_UP")
    (next . "PAGE_DOWN")
    (print . "PRINTSCREEN")))

(defvar moz-controller--generate-key-function-string
  "if (typeof(mozControllerGenerateKey) == 'undefined'){\
function mozControllerGenerateKey(target,isCtrl,isAlt,isShift,keycode,charcode){\
if (target==gURLBar.inputField && keycode == KeyEvent.DOM_VK_RETURN) {gBrowser.loadURI(target.value); content.window.focus(); return;}\
else if (target == BrowserSearch.searchBar.textbox.inputField && keycode == KeyEvent.DOM_VK_RETURN) { BrowserSearch.searchBar.doSearch(target.value,'tab'); return;}\
var evt=document.createEvent('KeyboardEvent');\
evt.initKeyEvent('keypress',true,true,null,isCtrl,isAlt,isShift,false,keycode,charcode);\
target.dispatchEvent(evt);\
}\
}")

(defun moz-controller--e2j (e)
  (pcase e
    ((pred booleanp) (if e "true" "false"))
    ((pred symbolp) (format "KeyEvent.DOM_VK_%s"
                            (or (assoc-default e moz-controller--special-key-table)
                                (upcase (symbol-name e)))))
    (_ 0)))

(defun moz-controller--direct-send-key (charcode &optional ctrlp altp shiftp keycode target)
  (moz-controller--send moz-controller--generate-key-function-string)
  (moz-controller--send (format "mozControllerGenerateKey(%s,%s,%s,%s,%s,%s);"
                                (or target "document.commandDispatcher.focusedElement || document")
                                (moz-controller--e2j ctrlp)
                                (moz-controller--e2j altp)
                                (moz-controller--e2j shiftp)
                                (or keycode "0")
                                charcode)))

;; ---------------- ;;
;; direct-mode help ;;
;; ---------------- ;;
(defun moz-controller--direct-mode-show-help ()
  (moz-controller--show-help-from-keymap-alist
   moz-controller-direct-mode-keymap-alist
   3))

;; ------ ;;
;; Keymap ;;
;; ------ ;;
(defvar moz-controller-direct-mode-keymap-alist
  '(("special key bindings" .
     ((moz-controller-direct-mode-focus-or-quit "C-g" "focus(once), quit(twice)")
      (moz-controller-highlight-focus "M-g" "highlight focus")
      (moz-controller-switch-to-remote-mode "C-z" "switch to moz-controller-remote-mode")))))

(defvar moz-controller-direct-mode-map
  (let ((map (moz-controller--make-keymap moz-controller-direct-mode-keymap-alist)))
    (define-key map [t] #'moz-controller-direct-mode-send-key)
    map)
  "Keymap of `moz-controller-direct-mode'.")

;; ---------------------------------------- ;;
;; Commands in `moz-controller-direct-mode' ;;
;; ---------------------------------------- ;;
(defun moz-controller-direct-mode-send-key ()
  (interactive)
  (let* ((evt last-input-event)
         (mods (event-modifiers evt))
         (c (event-basic-type evt)))
    (message (concat "Key sent: "
                     (if mods (format "%s " mods) "")
                     (format (if (characterp c) "%c" "%s") c)))
    (moz-controller--direct-send-key (if (characterp c) c 0)
                                     (and (member 'control mods) t)
                                     (and (member 'meta mods) t)
                                     (and (member 'shift mods) t)
                                     (moz-controller--e2j c))))

(defun moz-controller-highlight-focus ()
  "Highlight the focused element."
  (interactive)
  (moz-controller--send
   "(function(){if (document.commandDispatcher.focusedElement) {\
var originalColor=document.commandDispatcher.focusedElement.style.backgroundColor;\
document.commandDispatcher.focusedElement.style.backgroundColor='yellow';\
setTimeout(function(){document.commandDispatcher.focusedElement.style.backgroundColor=originalColor;},1000);\
}})();"))

(defun moz-controller-direct-mode-focus-or-quit (&optional quitp)
  (interactive "P")
  (if (or quitp (eq last-command 'moz-controller-direct-mode-focus-or-quit))
      (progn
        (remove-hook 'mouse-leave-buffer-hook #'moz-controller-direct-mode-focus-or-quit)
        (remove-hook 'kbd-macro-termination-hook #'moz-controller-direct-mode-focus-or-quit)
        (setq overriding-local-map moz-controller--overriding-keymap)
        (setq moz-controller--overriding-keymap)
        (message "Exit moz-controller-direct-mode.")
        (moz-controller--hide-current-help))
    (moz-send-string "content.window.focus();")
    (message "Move focus to content window.
Press C-g again to exit moz-controller-direct-mode.")))

(defun moz-controller-switch-to-remote-mode ()
  (interactive)
  (moz-controller-direct-mode-focus-or-quit t)
  (moz-controller-remote-mode))

;;;###autoload
(defun moz-controller-direct-mode ()
  (interactive)
  (add-hook 'mouse-leave-buffer-hook #'moz-controller-direct-mode-focus-or-quit)
  (add-hook 'kbd-macro-termination-hook #'moz-controller-direct-mode-focus-or-quit)
  (setq moz-controller--overriding-keymap overriding-local-map)
  (setq overriding-local-map moz-controller-direct-mode-map)
  (message "Enter moz-controller-direct-mode.")
  (moz-controller--direct-mode-show-help))

;; Unused but maybe useful stuffs.
;; (defun moz-controller-edit ()
;;   (interactive)
;;   (moz-controller--send "a=Array.prototype.concat.call(Array.prototype.slice.call(content.document.getElementsByTagName('input')).filter(function(i){return (i.type == \"text\" || i.type == \"password\");}), Array.prototype.slice.call(content.document.getElementsByTagName('textarea')));i=-1;")
;;   (moz-controller--send "if (i != -1) a[i].style.backgroundColor=b;\
;; i=(i+1)%a.length;\
;; b=a[i].style.backgroundColor;\
;; a[i].style.backgroundColor='yellow';\
;; a[i].focus();")
;;   (moz-controller--send (format "a[i].value='%s';" (read-string "Input: "))))

(provide 'moz-controller)
;;; moz-controller.el ends here
