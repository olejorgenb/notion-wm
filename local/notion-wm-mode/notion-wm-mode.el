;;; notion.el --- Tight integration of emacs with the notion window manager

;; Copyright (C) 2005-2006 by Stefan Reichör

;; Filename: notion.el
;; Author: Stefan Reichör, <stefan@xsteve.at>

;; notion.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; notion.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; notion.el is an emacs interface for the notion window manager

;; You need mod_notionflux-3 (at least from 2005-04-21)
;; mod_notionflux-3 can be found here: http://modeemi.fi/~tuomov/repos/

;; Put the following in your .emacs to make the notion-wm-mode function available:
;; (autoload 'notion-wm-mode "notion" "Major mode to edit notion config files" t)

;; The latest version of notion.el can be found at http://www.xsteve.at/prg/emacs/notion.el

;; Comments / suggestions welcome!

;;; Todo
;;  * Better error handling - at the moment they are only shown on the
;;    terminal, where notion was started

;;; History:
;;

;;; Code:

;; --------------------------------------------------------------------------------
;; notion interaction via notionflux
;; --------------------------------------------------------------------------------



(defun notion-wm-run-notionflux (cmd)
  (shell-command-to-string (concat "notionflux -e " (shell-quote-argument cmd))))

(defun notion-wm-send-string (str)
  "Send STR to notion, using the notionflux program."
  (notion-wm-run-notionflux str))

(defun notion-wm-send-region (start end)
  "Send send the region to notion, using the notionflux program."
  (interactive "r")
  (notion-wm-run-notionflux (buffer-substring start end)))

(defun notion-wm-send-current-line ()
  "Send send the actual line to notion, using the notionflux program."
  (interactive)
  (notion-wm-run-notionflux (buffer-substring (line-beginning-position) (line-end-position))))

(defun notion-wm-send-proc ()
  "Send proc around point to notion."
  (interactive)
  (let (start end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq start (point))
      (lua-end-of-proc)
      (setq end (point)))
    (notion-wm-send-region start end)))

(defun notion-wm-send-buffer ()
  "Send send the buffer content to notion, using the notionflux program."
  (interactive)
  (notion-wm-send-region (point-min) (point-max)))


(defun notion-wm-cmd (cmd)
  "Send a command to notion.
The command is prefixed by a return statement."
  (interactive "sNotion cmd: ")
  (let ((result (notion-wm-run-notionflux (concat "return " cmd))))
    (when (interactive-p)
      (message result))
    result))


;; --------------------------------------------------------------------------------
;; Utility functions that need notion-wm-emacs.lua
;; --------------------------------------------------------------------------------

(defun notion-wm-client-list ()
  "Return the list of the notion clients."
  (let* ((s (notion-wm-cmd "emacs.list_clients()"))
         (s0 (substring s 1 (- (length s) 2)))
         (client-list (split-string s0 "\\\\\n")))
    client-list))


;; (ido-completing-read "notion window: " (notion-wm-client-list) t t nil nil (car (notion-wm-client-list)))

(defun notion-wm-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (notion-wm-client-list))))
  (notion-wm-send-string (concat "WRegion.goto(ioncore.lookup_clientwin(\"" name "\"))")))


;; --------------------------------------------------------------------------------
;; The notion edit mode, based on lua mode
;; --------------------------------------------------------------------------------

(defvar notion-wm-mode-map () "Keymap used in `notion-wm-mode' buffers.")

(when (not notion-wm-mode-map)
  (setq notion-wm-mode-map (make-sparse-keymap))
  (define-key notion-wm-mode-map [(control ?c) (control ?p)] 'notion-wm-send-proc)
  (define-key notion-wm-mode-map [(control ?c) (control ?r)] 'notion-wm-send-region)
  (define-key notion-wm-mode-map [(control ?c) (control ?b)] 'notion-wm-send-buffer)
  (define-key notion-wm-mode-map [(control ?c) (control ?l)] 'notion-wm-send-line)
  )

(easy-menu-define notion-wm-mode-menu notion-wm-mode-map
"'notion-wm-mode' menu"
                  '("Notion"
                    ("Interaction"
                    ["Send Procedure" notion-wm-send-proc t]
                    ["Send Region" notion-wm-send-region t]
                    ["Send Buffer" notion-wm-send-buffer t]
                    ["Send String" notion-wm-send-string t]
                    ["Send Line" notion-wm-send-line t]
                    )
                    ["Goto client" notion-wm-goto-client t]
                    ))

(define-derived-mode notion-wm-mode lua-mode "notion"
  "notion-wm-mode provides a tight integration of emacs and notion.
"
  (use-local-map notion-wm-mode-map))

;; --------------------------------------------------------------------------------
;; various stuff for testing purposes
;; --------------------------------------------------------------------------------


;; (notion-wm-send-string "ioncore.goto_next_screen()")
;; (notion-wm-cmd "ioncore.goto_next_screen()")

;;(defun notion-wm-show-message-for-cmd (cmd)
;;  (interactive "snotion command: ")
;;  (notion-wm-run-notionflux (concat "mod_query.message(ioncore.find_screen_id(0)," cmd ")")))


;; (notion-wm-client-list)


;; (notion-wm-show-message-for-cmd "ioncore.version()")
;; (notion-wm-send-string "return ioncore.version()")
;; (notion-wm-send-string "return 4+5")

;; (notion-wm-cmd "ioncore.version()")
;; (notion-wm-cmd "4+5")

 ;; (setenv "NOTIONFLUX_SOCKET" "/tmp/fileM5J57y")

;; things to support
;;table ioncore.clientwin_list()

;; WClientWin ioncore.lookup_clientwin(string name)

;; bool WRegion.goto(WRegion reg)

(provide 'notion-wm-mode)

;;; notion.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


