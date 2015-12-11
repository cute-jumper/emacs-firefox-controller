;;; firefox-controller.el --- An improved Firefox controller for Emacs

;; Copyright (C) 2015 Junpeng Qiu
;; Copyright (C) 2014 任文山 (Ren Wenshan)

;; Author: Junpeng Qiu <qjpchmail@gmail.com>, 任文山 (Ren Wenshan)
;; URL: https://github.com/cute-jumper/emacs-firefox-controller
;; Version: 0.1
;; Package-Requires: ((moz "0") (popwin "1.0.0") (cl-lib "0.5"))
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

;;            __________________________________________________

;;                 FIREFOX-CONTROLLER: AN IMPROVED FIREFOX
;;                          CONTROLLER FOR EMACS!

;;                               Junpeng Qiu
;;            __________________________________________________


;; Table of Contents
;; _________________

;; 1 Installation
;; .. 1.1 Firefox Extension
;; .. 1.2 Emacs Extensions
;; 2 Intro
;; 3 `firefox-controller-remote-mode'
;; 4 `firefox-controller-direct-mode'
;; 5 Customization
;; 6 Acknowledgment





;; 1 Installation
;; ==============

;; 1.1 Firefox Extension
;; ~~~~~~~~~~~~~~~~~~~~~

;;   Install [MozRepl] in Firefox.


;;   [MozRepl] https://addons.mozilla.org/en-US/firefox/addon/mozrepl/


;; 1.2 Emacs Extensions
;; ~~~~~~~~~~~~~~~~~~~~

;;   1. Install [moz.el] and [popwin-el].
;;   2. Install this package and add it to your load path
;;      ,----
;;      | (add-to-list 'load-path "/path/to/emacs-firefox-controller")
;;      | (require 'firefox-controller)
;;      `----


;;   [moz.el] https://github.com/bard/mozrepl/wiki/Emacs-integration

;;   [popwin-el] https://github.com/m2ym/popwin-el


;; 2 Intro
;; =======

;;   This project started as a fork of [Wenshan]'s [moz-controller], but I
;;   ended up rewriting most of the code and chose a quite different way to
;;   use it.

;;   In `firefox-controller', we have two different modes:
;;   1. `firefox-controller-remote-mode': This is based on the original
;;      `moz-controller', but the number of available commands increases
;;      from 11 to more than 30, and simpler key bindings and a better UI
;;      are also provided.
;;   2. `firefox-controller-direct-mode': In this mode, you can send the
;;      keys directly to firefox.


;;   [Wenshan] https://github.com/RenWenshan

;;   [moz-controller] https://github.com/RenWenshan/emacs-moz-controller


;; 3 `firefox-controller-remote-mode'
;; ==================================

;;   Use `M-x firefox-controller-remote-mode' to enter
;;   `firefox-controller-remote-mode'. It is called `remote-mode' because
;;   the keys that you pressed are handled by Emacs and Emacs will send
;;   control commands to firefox.

;;   Here is the screeshot when using `firefox-controller-remote-mode':
;;   [https://github.com/cute-jumper/ace-pinyin/blob/master/screenshots/remote-mode.png]

;;   As we can see in the screenshot, we have defined most of the commonly used
;;   commands in firefox. For example, n to scroll down by one page, and t to
;;   open a new tab and switch to it. You can exit the
;;   `firefox-controller-remote-mode' by q, and you can search the web page by
;;   pressing s, and here is the screenshot for the search mode in
;;   `firefox-controller-remote-mode' (we are searching "bibtex" in the current
;;   web page):
;;   [https://github.com/cute-jumper/ace-pinyin/blob/master/screenshots/search-mode.png]


;; 4 `firefox-controller-direct-mode'
;; ==================================

;;   The limitation of `firefox-controller-remote-mode' is that under that
;;   mode, the web page is /non-interactive/. We can only view, scroll,
;;   switch tab, search and open another tab for a new URL. However, if you
;;   want to jump to a link or enter some text in the input box, these
;;   functions are not implemented. Here comes
;;   `firefox-controller-direct-mode', which can be combined with some
;;   powerful firefox extensions such as [KeySnail] to build emacs-like
;;   mouseless browsing experience for firefox. We don't have to recreate
;;   some firefox extensions in `firefox-controller-remote-mode'.

;;   The use of `firefox-controller-direct-mode' is quite straightforward.
;;   `M-x firefox-controller-direct-mode', then you can use all the key
;;   bindings as if you are in firefox instead of Emacs, except for three
;;   special key bindings:
;;   1. C-g: Whenl you press C-g once, the focus of the firefox will move
;;      to the content window, and if you press C-g twice in a row, it will
;;      quit `firefox-controller-direct-mode'.
;;   2. M-g: This is bound to `firefox-controller-highlight-focus', which
;;      can show a temporary background color in the current focused
;;      element. This command is useful since the foreground application is
;;      Emacs, firefox won't show the current focused element(at least, we
;;      can't see it in Plasma 5 in Linux, which is my test environment).
;;      You can use M-g to give you a visual hint about the location of the
;;      cursor.
;;   3. C-z: This command switches from the current mode to
;;      `firefox-controller-remote-mode'.

;;   Here is the screenshot to use `firefox-controller-direct-mode':
;;   [https://github.com/cute-jumper/ace-pinyin/blob/master/screenshots/direct-mode.gif]

;;   Explanation: After I invoke `firefox-controller-direct-mode', I type
;;   C-l to go to the address bar, and use M-g to highlight my current
;;   location(which is the address bar of course). Then go to google.com,
;;   and use M-g again to confirm the current focused element in firefox is
;;   the seach box. After I type and search "emacs", I use KeySnail's
;;   plugin [hok] to jump to a link and open it.


;;   [KeySnail] https://github.com/mooz/keysnail

;;   [hok] https://github.com/mooz/keysnail/raw/master/plugins/hok.ks.js


;; 5 Customization
;; ===============

;;   - `firefox-controller-zoom-step': Zoom step. Default value is 0.1.
;;   - `firefox-controller-highlight-focus-background': The background
;;      color used by `firefox-controller-highlight-focus' command. Default
;;      value is "yellow".


;; 6 Acknowledgment
;; ================

;;   - [RenWenshan] for the original [moz-controller].


;;   [RenWenshan] https://github.com/RenWenshan/

;;   [moz-controller] https://github.com/RenWenshan/emacs-moz-controller



;;; Code:

(require 'moz)
(require 'popwin)
(require 'cl-lib)
(require 'font-lock)

(defgroup firefox-controller nil
  "Control Firefox from Emacs"
  :group 'firefox-controller)

(defcustom firefox-controller-zoom-step 0.1
  "Zoom step, default to 0.1, it is supposed to be a positive number."
  :group 'firefox-controller
  :type 'number)

(defcustom firefox-controller-highlight-focus-background "yellow"
  "Background color to highlight focus."
  :group 'firefox-controller
  :type 'string)

;; --------------------- ;;
;; global vars and utils ;;
;; --------------------- ;;
(defvar firefox-controller--overriding-keymap nil
  "Original `overriding-local-map'.")

(defvar firefox-controller--repl-output ""
  "Output from *MozRepl*.")

(defvar firefox-controller--remote-search-string nil)

(defvar firefox-controller--remote-command-type nil
  "The type of command that we send to *MozRepl*.")

(defvar firefox-controller--help-window nil)

;; ------------- ;;
;; search keymap ;;
;; ------------- ;;
(defvar firefox-controller--remote-mode-search-keymap-alist
  '(("firefox-controller-remote-mode-search" .
     ((firefox-controller-search-next "n" "search forward")
      (firefox-controller-search-previous "p" "search backward")
      (firefox-controller-search-edit "e" "edit search string")
      (firefox-controller-search-quit [t] "quit search")))))

(defun firefox-controller--make-keymap (keymap-alist)
  (let ((map (make-sparse-keymap))
        key)
    (dolist (module keymap-alist)
      (dolist (lst (cdr module))
        (setq key (cadr lst))
        (define-key map (if (vectorp key) key (kbd key)) (car lst))))
    map))

(defvar firefox-controller-remote-mode-search-map
  (firefox-controller--make-keymap firefox-controller--remote-mode-search-keymap-alist)
  "Keymap of search in `firefox-controller-remote-mode'.")

;; ------------------ ;;
;; remote-mode keymap ;;
;; ------------------ ;;
(defvar firefox-controller--remote-mode-keymap-alist
  `(("page" .
     ((firefox-controller-page-refresh "r" "refresh")
      (firefox-controller-page-line-down "j" "line down")
      (firefox-controller-page-line-up "k" "line up")
      (firefox-controller-page-left "[" "scroll left")
      (firefox-controller-page-right "]" "scroll right")
      (firefox-controller-page-down "n" "page down")
      (firefox-controller-page-up "p" "page up")
      (firefox-controller-page-top "<" "page top")
      (firefox-controller-page-bottom ">" "page bottom")
      (firefox-controller-focus-content "C-g" "focus content")))
    ("zoom" .
     ((firefox-controller-zoom-in "+" "zoom in")
      (firefox-controller-zoom-out "-" "zoom out")
      (firefox-controller-zoom-reset "0" "zoom reset")))
    ("tab" .
     ((firefox-controller-tab-close "x" "close tab")
      (firefox-controller-tab-previous "h" "previous tab")
      (firefox-controller-tab-next "l" "next tab")
      (firefox-controller-new-tab-and-switch "t" "new tab and switch")
      (firefox-controller-new-tab "T" "new tab in background")
      (firefox-controller-switch-tab "C-b" "switch tab by name")
      (firefox-controller-switch-tab-by-id "M-b" "switch tab by id")))
    ("navigation" .
     ((firefox-controller-get-current-url "L" "copy current url")
      (firefox-controller-startpage "H" "homepage")
      (firefox-controller-goto-url "g" "goto url")
      (firefox-controller-go-forward "f" "forward")
      (firefox-controller-go-back "b" "backward")))
    ("edit" .
     ((firefox-controller-select-all "a" "select all")
      (firefox-controller-unselect "u" "unselect")
      (firefox-controller-cut "W" "cut")
      (firefox-controller-copy "w" "copy")
      (firefox-controller-paste "y" "paste")))
    ("window" .
     ((firefox-controller-maximize-window "^" "maximize")
      (firefox-controller-restore-window "&" "restore")
      (firefox-controller-minimize-window "*" "minimize")))
    ("misc" .
     ((firefox-controller-search-start "s" "search-mode")
      (firefox-controller-switch-to-direct-mode "C-z" "switch to firefox-controller-direct-mode")
      (firefox-controller-remote-mode-quit "q" "quit")))))

(defvar firefox-controller-remote-mode-map
  (let ((map (firefox-controller--make-keymap
              firefox-controller--remote-mode-keymap-alist)))
    (define-key map [t] (lambda () (interactive) (message "Undefined.")))
    map)
  "Keymap of `firefox-controller-remote-mode'.")

;; ------------------ ;;
;; direct-mode keymap ;;
;; ------------------ ;;
(defvar firefox-controller-direct-mode-keymap-alist
  '(("special key bindings" .
     ((firefox-controller-direct-mode-focus-or-quit "C-g" "focus(once), quit(twice)")
      (firefox-controller-highlight-focus "M-g" "highlight focus")
      (firefox-controller-switch-to-remote-mode "C-z" "switch to firefox-controller-remote-mode")))))

(defvar firefox-controller-direct-mode-map
  (let ((map (firefox-controller--make-keymap firefox-controller-direct-mode-keymap-alist)))
    (define-key map [t] #'firefox-controller-direct-mode-send-key)
    map)
  "Keymap of `firefox-controller-direct-mode'.")

(defun firefox-controller--safe-read-string (prompt &optional callback)
  (let (overriding-local-map)
    (condition-case err
        (replace-regexp-in-string "\'" "\\\\'" (read-string prompt))
      (quit (and callback (funcall callback))
            (signal 'quit nil)))))

(defun firefox-controller--send (command &optional command-type)
  "Set command type and send COMMAND to `inferior-moz-process'."
  (setq firefox-controller--remote-command-type command-type)
  (comint-simple-send (inferior-moz-process) command))

;; -------------------------- ;;
;; utils to build help window ;;
;; -------------------------- ;;
(defun firefox-controller--popwin (size)
  (interactive)
  (when (not (window-live-p firefox-controller--help-window))
    (with-current-buffer (window-buffer (setq firefox-controller--help-window
                                              (cadr
                                               (popwin:create-popup-window size))))
      (setq mode-line-format nil)))
  firefox-controller--help-window)

(defun firefox-controller--show-help-from-keymap-alist (keymap-alist column-count)
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
    (with-current-buffer (window-buffer (firefox-controller--popwin line-count))
      (erase-buffer)
      (insert help))))

(defun firefox-controller--hide-current-help ()
  (when (window-live-p firefox-controller--help-window)
    (delete-window firefox-controller--help-window)))

;; -------------------------------------------------------------------------- ;;
;; #####  ###### #    #  ####  ##### ######       #    #  ####  #####  ###### ;;
;; #    # #      ##  ## #    #   #   #            ##  ## #    # #    # #      ;;
;; #    # #####  # ## # #    #   #   #####  ##### # ## # #    # #    # #####  ;;
;; #####  #      #    # #    #   #   #            #    # #    # #    # #      ;;
;; #   #  #      #    # #    #   #   #            #    # #    # #    # #      ;;
;; #    # ###### #    #  ####    #   ######       #    #  ####  #####  ###### ;;
;; -------------------------------------------------------------------------- ;;
;; ----------------------- ;;
;; remote-mode help system ;;
;; ----------------------- ;;
(defun firefox-controller--remote-mode-show-command (func-sym)
  (let (doc)
    (catch 'break
      (dolist (module firefox-controller--remote-mode-keymap-alist)
        (dolist (lst (cdr module))
          (when (eq (car lst) func-sym)
            (setq doc (nth 2 lst))
            (throw 'break nil)))))
    (message "Send command: %s" doc)))

(defun firefox-controller--remote-mode-search-show-string ()
  (message "Search: %s" firefox-controller--remote-search-string))

(defun firefox-controller--remote-mode-search-show-help ()
  (firefox-controller--show-help-from-keymap-alist
   firefox-controller--remote-mode-search-keymap-alist
   4))

(defun firefox-controller--remote-mode-show-help ()
  (firefox-controller--show-help-from-keymap-alist
   firefox-controller--remote-mode-keymap-alist
   3))

;; -------------------------------------------------------------------- ;;
;; convenient macro to define commands for `firefox-controller-remote-mode' ;;
;; -------------------------------------------------------------------- ;;
(defmacro firefox-controller-remote-defun (name doc command &optional not-helpful-p &rest filter-body)
  "Macro for defining firefox-controller commands."
  (declare (indent 1)
           (doc-string 2))
  (let ((filter-name (intern (format "%S-repl-filter" name)))
        (no-filter-p (not filter-body))
        (command-type (and filter-body (intern (format "%S-type" name)))))
    (if no-filter-p
        `(defun ,name ()
           ,doc
           (interactive)
           (firefox-controller--send ,command)
           (or ,not-helpful-p (firefox-controller--remote-mode-show-command ',name)))
      `(progn
         (defun ,name ()
           ,doc
           (interactive)
           (with-current-buffer (process-buffer (inferior-moz-process))
             (add-hook 'comint-output-filter-functions #',filter-name nil t))
           (firefox-controller--send ,command ',command-type)
           (or ,not-helpful-p (firefox-controller--remote-mode-show-command ',name)))
         (defun ,filter-name (output)
           (setq firefox-controller--repl-output
                 (replace-regexp-in-string "\"\\(\\(.*\n?\\)*\\)\"\nrepl> " "\\1" output))
           (when (eq firefox-controller--remote-command-type ',command-type)
             (unwind-protect
                 (progn
                   ,@filter-body)
               (with-current-buffer (process-buffer (inferior-moz-process))
                 (remove-hook 'comint-output-filter-functions #',filter-name))
               (setq firefox-controller--remote-command-type))))))))

;; ---------------------------- ;;
;; Various remote-mode commands ;;
;; ---------------------------- ;;
(firefox-controller-remote-defun firefox-controller-page-refresh
  "Refresh current page"
  "setTimeout(function(){content.document.location.reload(true);}, '500');")

(firefox-controller-remote-defun firefox-controller-page-line-down
  "Scroll down the current window by one line."
  "goDoCommand('cmd_scrollLineDown');")

(firefox-controller-remote-defun firefox-controller-page-line-up
  "Scroll up the current window by one line."
  "goDoCommand('cmd_scrollLineUp');")

(firefox-controller-remote-defun firefox-controller-page-down
  "Scroll down the current window by one page."
  "content.window.scrollByPages(1);")

(firefox-controller-remote-defun firefox-controller-page-up
  "Scroll up the current window by one page."
  "content.window.scrollByPages(-1);")

(firefox-controller-remote-defun firefox-controller-page-left
  "Scroll to the left."
  "goDoCommand('cmd_scrollLeft');")

(firefox-controller-remote-defun firefox-controller-page-right
  "Scroll to the right."
  "goDoCommand('cmd_scrollRight');")

(firefox-controller-remote-defun firefox-controller-page-top
  "Move to the top of the page."
  "goDoCommand('cmd_moveTop');")

(firefox-controller-remote-defun firefox-controller-page-bottom
  "Move to the bottom of the page."
  "goDoCommand('cmd_moveBottom');")

(firefox-controller-remote-defun firefox-controller-focus-content
  "Move focus to content."
  "content.window.focus();")

(firefox-controller-remote-defun firefox-controller-tab-close
  "Close current tab."
  "content.window.close();")

(firefox-controller-remote-defun firefox-controller-zoom-in
  "Zoom in."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom += "
          (number-to-string firefox-controller-zoom-step) ";"))

(firefox-controller-remote-defun firefox-controller-zoom-out
  "Zoom out."
  (concat "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom -= "
          (number-to-string firefox-controller-zoom-step) ";"))

(firefox-controller-remote-defun firefox-controller-zoom-reset
  "Zoom reset."
  "gBrowser.selectedBrowser.markupDocumentViewer.fullZoom = 1")

(firefox-controller-remote-defun firefox-controller-tab-previous
  "Switch to the previous tab."
  "gBrowser.tabContainer.advanceSelectedTab(-1, true);")

(firefox-controller-remote-defun firefox-controller-tab-next
  "Switch to the next tab."
  "gBrowser.tabContainer.advanceSelectedTab(1, true);")

(firefox-controller-remote-defun firefox-controller-view-page-source
  "View current page source code."
  "BrowserViewSourceOfDocument(gBrowser.contentDocument);")

(firefox-controller-remote-defun firefox-controller-get-current-url
  "Get the current tab's URL and add to `kill-ring'."
  "gBrowser.contentWindow.location.href;"
  nil
  (message "URL copied: %s" firefox-controller--repl-output)
  (kill-new firefox-controller--repl-output))

(firefox-controller-remote-defun firefox-controller-select-all
  "Select all the content in the current page."
  "goDoCommand('cmd_selectAll');")

(firefox-controller-remote-defun firefox-controller-unselect
  "Unselect."
  "goDoCommand('cmd_selectNone');")

(firefox-controller-remote-defun firefox-controller-copy
  "Copy."
  "goDoCommand('cmd_copy');")

(firefox-controller-remote-defun firefox-controller-paste
  "Paste."
  "goDoCommand('cmd_paste');")

(firefox-controller-remote-defun firefox-controller-cut
  "Cut."
  "goDoCommand('cmd_cut');")

(firefox-controller-remote-defun firefox-controller-switch-tab
  "Switch the tab."
  "Array.prototype.map.call(gBrowser.tabs, function(tab) {return tab.label;}).join(\"\\n\");"
  t
  (let* (overriding-local-map
         (tab-titles (split-string firefox-controller--repl-output "\n"))
         (selected-title
          (completing-read "Select tab: " tab-titles)))
    (firefox-controller--send
     (format
      "gBrowser.selectTabAtIndex(%s);"
      (cl-position selected-title tab-titles :test 'equal)))))

(firefox-controller-remote-defun firefox-controller-switch-tab-by-id
  "Switch the tab by id."
  "(function(){\
var i=0;\
Array.prototype.slice.call(gBrowser.tabs).map(function(tab){tab.label=\"[\" + (i++) + \"]\" + tab.label;});\
})();"
  t
  (firefox-controller--send
   (format
    "Array.prototype.map.call(gBrowser.tabs,\
function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});\
gBrowser.selectTabAtIndex(%d);"
    (string-to-number
     (firefox-controller--safe-read-string "Tab id: "
                                           (lambda ()
                                             (firefox-controller--send
                                              "Array.prototype.map.call(gBrowser.tabs,\
 function(tab){tab.label=tab.label.replace(/\[[0-9]+\]/, '');});")))))))

(firefox-controller-remote-defun firefox-controller-new-tab
  "Add new tab."
  "gBrowser.addTab();")

(firefox-controller-remote-defun firefox-controller-new-tab-and-switch
  "Add new tab and switch to it."
  "gBrowser.selectedTab = gBrowser.addTab();")

(firefox-controller-remote-defun firefox-controller-startpage
  "Goto start page."
  "gBrowser.loadURI('about:home');")

(firefox-controller-remote-defun firefox-controller-goto-url
  "Goto URL."
  (format "gBrowser.loadURI('http://%s');"
          (firefox-controller--safe-read-string "Goto: http://")))

(firefox-controller-remote-defun firefox-controller-go-forward
  "Foward."
  "gBrowser.goForward();")

(firefox-controller-remote-defun firefox-controller-go-back
  "Back."
  "gBrowser.goBack();")

(firefox-controller-remote-defun firefox-controller-maximize-window
  "Maximize window."
  "maximize();")

(firefox-controller-remote-defun firefox-controller-minimize-window
  "Minimize window."
  "minimize();")

(firefox-controller-remote-defun firefox-controller-restore-window
  "Restore window."
  "restore();")

;; --------------------------- ;;
;; remote-mode search commands ;;
;; --------------------------- ;;
(firefox-controller-remote-defun firefox-controller-search-start
  "Start search."
  "gFindBar.open();"
  t
  (firefox-controller-search-edit))

(firefox-controller-remote-defun firefox-controller-search-edit
  "Edit search string."
  (progn (firefox-controller--hide-current-help)
         (setq firefox-controller--remote-search-string
               (firefox-controller--safe-read-string "Search: "
                                                     #'firefox-controller-search-quit))
         (add-hook 'mouse-leave-buffer-hook #'firefox-controller-search-quit)
         (add-hook 'kbd-macro-termination-hook #'firefox-controller-search-quit)
         (setq overriding-local-map firefox-controller-remote-mode-search-map)
         (firefox-controller--remote-mode-search-show-help)
         (firefox-controller--remote-mode-search-show-string)
         (format "gFindBar._findField.value='%s';" firefox-controller--remote-search-string))
  t)

(firefox-controller-remote-defun firefox-controller-search-next
  "Goto next search."
  (progn (firefox-controller--remote-mode-search-show-string)
         "gFindBar.onFindAgainCommand(false);")
  t)

(firefox-controller-remote-defun firefox-controller-search-previous
  "Goto previous search."
  (progn (firefox-controller--remote-mode-search-show-string)
         "gFindBar.onFindAgainCommand(true);")
  t)

(firefox-controller-remote-defun firefox-controller-search-quit
  "Quit search."
  (progn
    (setq overriding-local-map firefox-controller-remote-mode-map)
    (remove-hook 'mouse-leave-buffer-hook #'firefox-controller-search-quit)
    (remove-hook 'kbd-macro-termination-hook #'firefox-controller-search-quit)
    (firefox-controller--hide-current-help)
    (firefox-controller--remote-mode-show-help)
    "gFindBar.close();")
  t)

;; -------------------------- ;;
;; Other remote-mode commands ;;
;; -------------------------- ;;
(defun firefox-controller-remote-mode-quit ()
  (interactive)
  (remove-hook 'mouse-leave-buffer-hook #'firefox-controller-remote-mode-quit)
  (remove-hook 'kbd-macro-termination-hook #'firefox-controller-remote-mode-quit)
  (setq overriding-local-map firefox-controller--overriding-keymap)
  (setq firefox-controller--overriding-keymap)
  (firefox-controller--hide-current-help)
  (message "Exit firefox-controller-remote-mode."))

(defun firefox-controller-switch-to-direct-mode ()
  (interactive)
  (firefox-controller-remote-mode-quit)
  (firefox-controller-direct-mode))

;;;###autoload
(defun firefox-controller-remote-mode ()
  "Enter `firefox-controller-remote-mode'."
  (interactive)
  (setq firefox-controller--overriding-keymap overriding-local-map)
  (setq overriding-local-map firefox-controller-remote-mode-map)
  (add-hook 'mouse-leave-buffer-hook #'firefox-controller-remote-mode-quit)
  (add-hook 'kbd-macro-termination-hook #'firefox-controller-remote-mode-quit)
  (message "Enter firefox-controller-remote-mode.")
  (firefox-controller--remote-mode-show-help))

;; --------------------------------------------------------------------- ;;
;; #####  # #####  ######  ####  #####       #    #  ####  #####  ###### ;;
;; #    # # #    # #      #    #   #         ##  ## #    # #    # #      ;;
;; #    # # #    # #####  #        #   ##### # ## # #    # #    # #####  ;;
;; #    # # #####  #      #        #         #    # #    # #    # #      ;;
;; #    # # #   #  #      #    #   #         #    # #    # #    # #      ;;
;; #####  # #    # ######  ####    #         #    #  ####  #####  ###### ;;
;; --------------------------------------------------------------------- ;;
(defconst firefox-controller--special-key-table
  '((backspace . "BACK_SPACE")
    (prior . "PAGE_UP")
    (next . "PAGE_DOWN")
    (print . "PRINTSCREEN")))

(defvar firefox-controller--generate-key-function-string
  "if (typeof(firefoxControllerGenerateKey) == 'undefined'){\
function firefoxControllerGenerateKey(target,isCtrl,isAlt,isShift,keycode,charcode){\
if (target==gURLBar.inputField && keycode == KeyEvent.DOM_VK_RETURN) {gBrowser.loadURI(target.value); content.window.focus(); return;}\
else if (target == BrowserSearch.searchBar.textbox.inputField && keycode == KeyEvent.DOM_VK_RETURN) { BrowserSearch.searchBar.doSearch(target.value,'tab'); return;}\
var evt=document.createEvent('KeyboardEvent');\
evt.initKeyEvent('keypress',true,true,null,isCtrl,isAlt,isShift,false,keycode,charcode);\
target.dispatchEvent(evt);\
}\
}")

(defun firefox-controller--e2j (e)
  (pcase e
    ((pred booleanp) (if e "true" "false"))
    ((pred symbolp) (format "KeyEvent.DOM_VK_%s"
                            (or (assoc-default e firefox-controller--special-key-table)
                                (upcase (symbol-name e)))))
    (_ 0)))

(defun firefox-controller--direct-send-key (charcode &optional ctrlp altp shiftp keycode target)
  (firefox-controller--send firefox-controller--generate-key-function-string)
  (firefox-controller--send (format "firefoxControllerGenerateKey(%s,%s,%s,%s,%s,%s);"
                                    (or target "document.commandDispatcher.focusedElement || document")
                                    (firefox-controller--e2j ctrlp)
                                    (firefox-controller--e2j altp)
                                    (firefox-controller--e2j shiftp)
                                    (or keycode "0")
                                    charcode)))

;; ---------------- ;;
;; direct-mode help ;;
;; ---------------- ;;
(defun firefox-controller--direct-mode-show-help ()
  (firefox-controller--show-help-from-keymap-alist
   firefox-controller-direct-mode-keymap-alist
   3))

;; -------------------------------------------- ;;
;; Commands in `firefox-controller-direct-mode' ;;
;; -------------------------------------------- ;;
(defun firefox-controller-direct-mode-send-key ()
  (interactive)
  (let* ((evt last-input-event)
         (mods (event-modifiers evt))
         (c (event-basic-type evt)))
    (message (concat "Key sent: "
                     (if mods (format "%s " mods) "")
                     (format (if (characterp c) "%c" "%s") c)))
    (firefox-controller--direct-send-key (if (characterp c) c 0)
                                         (and (member 'control mods) t)
                                         (and (member 'meta mods) t)
                                         (and (member 'shift mods) t)
                                         (firefox-controller--e2j c))))

(defun firefox-controller-highlight-focus ()
  "Highlight the focused element."
  (interactive)
  (firefox-controller--send
   (format
    "(function(){if (document.commandDispatcher.focusedElement) {\
var originalColor=document.commandDispatcher.focusedElement.style.backgroundColor;\
document.commandDispatcher.focusedElement.style.backgroundColor='%s';\
setTimeout(function(){document.commandDispatcher.focusedElement.style.backgroundColor=originalColor;},1000);\
}})();" firefox-controller-highlight-focus-background)))

(defun firefox-controller-direct-mode-focus-or-quit (&optional quitp)
  (interactive "P")
  (if (or quitp (eq last-command 'firefox-controller-direct-mode-focus-or-quit))
      (progn
        (remove-hook 'mouse-leave-buffer-hook #'firefox-controller-direct-mode-focus-or-quit)
        (remove-hook 'kbd-macro-termination-hook #'firefox-controller-direct-mode-focus-or-quit)
        (setq overriding-local-map firefox-controller--overriding-keymap)
        (setq firefox-controller--overriding-keymap)
        (message "Exit firefox-controller-direct-mode.")
        (firefox-controller--hide-current-help))
    (firefox-controller--send "content.window.focus();")
    (message "Move focus to content window.  \
Press C-g again to exit firefox-controller-direct-mode.")))

(defun firefox-controller-switch-to-remote-mode ()
  (interactive)
  (firefox-controller-direct-mode-focus-or-quit t)
  (firefox-controller-remote-mode))

;;;###autoload
(defun firefox-controller-direct-mode ()
  "Enter `firefox-controller-direct-mode'."
  (interactive)
  (add-hook 'mouse-leave-buffer-hook #'firefox-controller-direct-mode-focus-or-quit)
  (add-hook 'kbd-macro-termination-hook #'firefox-controller-direct-mode-focus-or-quit)
  (setq firefox-controller--overriding-keymap overriding-local-map)
  (setq overriding-local-map firefox-controller-direct-mode-map)
  (message "Enter firefox-controller-direct-mode.")
  (firefox-controller--direct-mode-show-help))

;; Unused but maybe useful stuffs.
;; (defun firefox-controller-edit ()
;;   (interactive)
;;   (firefox-controller--send "a=Array.prototype.concat.call(Array.prototype.slice.call(content.document.getElementsByTagName('input')).filter(function(i){return (i.type == \"text\" || i.type == \"password\");}), Array.prototype.slice.call(content.document.getElementsByTagName('textarea')));i=-1;")
;;   (firefox-controller--send "if (i != -1) a[i].style.backgroundColor=b;\
;; i=(i+1)%a.length;\
;; b=a[i].style.backgroundColor;\
;; a[i].style.backgroundColor='yellow';\
;; a[i].focus();")
;;   (firefox-controller--send (format "a[i].value='%s';" (read-string "Input: "))))

(provide 'firefox-controller)
;;; firefox-controller.el ends here
