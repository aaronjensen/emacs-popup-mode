;;; popup-mode.el --- Popup-mode extracted from doom -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2020 Henrik Lissner.
;; Modifications Copyright (c) 2020 Aaron Jensen.

;; Version: 0.0.1
;; Package-Requires: (hide-mode-line)

;;; Code:
(require 'popup-mode-core)
(require 'popup-mode-settings)

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")

(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")

(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")

(defvar +popup--inhibit-transient nil)
(defvar +popup--inhibit-select nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--remember-last t)
(defvar +popup--last nil)
(defvar-local +popup--timer nil)


;;
;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; AJ: Disable until we have an equivalent
    ;; (when (featurep! :editor evil)
    ;;   ;; For maximum escape coverage in emacs state buffers; this only works in
    ;;   ;; GUI Emacs, in tty Emacs use C-g instead
    ;;   (define-key map [escape] #'doom/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Doom's popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'doom-escape-hook #'+popup-close-on-escape-h 'append)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-escape-hook #'+popup-close-on-escape-h)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (+popup-cleanup-rules-h)
         (dolist (prop +popup-window-parameters)
           (delq (assq prop window-persistent-parameters)
                 window-persistent-parameters)))))

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
    (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
              nil 'local)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)


;;
;; Macros

(defmacro with-popup-rules! (rules &rest body)
  "Evaluate BODY with popup RULES. RULES is a list of popup rules. Each rule
should match the arguments of `+popup-define' or the :popup setting."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (set-popup-rules! ,rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     ,@body))

(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          +popup--last)
     (dolist (p popups)
       (+popup/close p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup/restore)
           (unless in-popup-p
             (select-window origin)))))))


;;
;; Default popup rules & bootstrap

(defcustom popup-mode-all-rule nil
  "When non-nil, treats all buffers that begin with `*' or ` *' as popups.")
(defcustom popup-mode-defaults-rule t
  "When non-nil, sets up rules for many common popups.")

(set-popup-rules!
  (when popup-mode-all-rule
    '(("^\\*"  :slot 1 :vslot -1 :select t)
      ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))
  (when popup-mode-defaults-rule
    '(("^\\*Completions" :ignore t)
      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
       :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
      ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ("^\\*doom:"  ; editing buffers (interaction required)
       :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*\\(?:Wo\\)?Man "
       :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc"
       :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize"
       :slot 2 :side right :select t :quit t)
      ("^ \\*undo-tree\\*"
       :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*[Hh]elp"
       :slot 2 :vslot -8 :size 0.35 :select t)
      ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
       :vslot -11 :size 0.35 :select t)
      ("^\\*info\\*$"  ; `Info-mode'
       :slot 2 :vslot 2 :size 0.45 :select t)))
  '(("^\\*Warnings" :vslot 99 :size 0.25)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
    ("^\\*\\(?:Proced\\|timer-list\\|Process List\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :ignore t)))

(add-hook 'doom-init-ui-hook #'+popup-mode 'append)

(dolist (hook '(+popup-adjust-fringes-h
                +popup-adjust-margins-h
                +popup-set-modeline-on-enable-h
                +popup-unset-modeline-on-disable-h))
  (add-hook '+popup-buffer-mode-hook hook))

;;
;; Hacks

;; (load! "+hacks")

(provide 'popup-mode)

;;; popup-mode.el ends here
