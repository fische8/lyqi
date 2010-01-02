;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lp-base)
(require 'lyqi-syntax)
(require 'lyqi-fontify)
(require 'lyqi-indent)
(require 'lyqi-editing-commands)

;;;
;;; Customization
;;;
(defgroup lyqi nil
  "LilyPond quick insert mode."
  :prefix "lyqi:"
  :group 'applications)

(defcustom lyqi:prefered-languages '(italiano nederlands)
  "Prefered languages for note names.  The first choice is used
in new files, or when the language of an existing file cannot be
guessed."
  :group 'lyqi
  :type '(set (const :tag "Italian/French" 'italiano)
              (const :tag "Dutch" 'nederlands)
              (const :tag "German" 'deutsch)
              (const :tag "English" 'english)))

(defcustom lyqi:prefered-octave-mode 'absolute
  "Prefered octave mode, used in new files."
  :group 'lyqi
  :type '(choice (const :tag "Absolute octaves" 'absolute)
                 (const :tag "Relative octaves" 'relative)))

(defcustom lyqi:keyboard-mapping 'azerty
  "Keyboard mapping, used to associate keys to commands in quick
insert mode map."
  :group 'lyqi
  :type '(choice (const :tag "AZERTY" 'azerty)
                 (const :tag "QWERTY" 'qwerty)))

(defcustom lyqi:custom-key-map nil
  "Key/command alist, for customizing the quick insertion mode map."
  :group 'lyqi
  :type '(alist :key-type string :value-type function))

;;; TODO: function for detecting note language
;;; TODO: function for detecting use of \relative

(defun lyqi:select-next-language (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax (lp:current-syntax)))
         (current-language (slot-value (lyqi:language syntax) 'name))
         (next-language (loop for langs on lyqi:prefered-languages
                              for lang = (first langs)
                              if (eql lang current-language)
                              return (or (cadr langs) (first lyqi:prefered-languages)))))
    (set-slot-value syntax 'language (lyqi:select-language next-language))
    (force-mode-line-update)
    (lp:parse-and-highlight-buffer)))

(defun lyqi:header-line-select-next-language (event)
  "Like `lyqi:select-next-language', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:select-next-language)))

(defun lyqi:toggle-relative-mode (&optional syntax)
  (interactive)
  (let ((syntax (or syntax (lp:current-syntax))))
    (set-slot-value syntax 'relative-mode
                    (not (slot-value syntax 'relative-mode))))
  (force-mode-line-update))

(defun lyqi:header-line-toggle-relative-mode (event)
  "Like `lyqi:toggle-relative-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:toggle-relative-mode)))

(defun lyqi:toggle-quick-edit-mode (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax (lp:current-syntax)))
         (quick-edit-mode (not (slot-value syntax 'quick-edit-mode))))
    (set-slot-value syntax 'quick-edit-mode quick-edit-mode)
    (if quick-edit-mode
        (lyqi:quick-insert-mode-map)
        (lyqi:normal-mode-map)))
  (use-local-map lyqi-mode-map)
  (force-mode-line-update))

(defun lyqi:header-line-toggle-quick-edit-mode (event)
  "Like `lyqi:toggle-quick-edit-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi:toggle-quick-edit-mode)))

(defun lyqi:header-line-lyqi-mode (event)
  "Like `lyqi-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-mode)))

(defun lyqi:set-header-line-format ()
  (setq header-line-format
        '(" "
          (:eval (propertize (symbol-name (slot-value (lyqi:language (lp:current-syntax)) 'name))
                             'help-echo "select next language"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-select-next-language)))))
          " | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'relative-mode)
                                 "relative"
                                 "absolute")
                             'help-echo "toggle octave mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-toggle-relative-mode)))))
          " mode | "
          (:eval (propertize (if (slot-value (lp:current-syntax) 'quick-edit-mode)
                                 "quick insert"
                                 "normal")
                             'help-echo "toggle edit mode"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (header-line
                                           keymap (mouse-1 . lyqi:header-line-toggle-quick-edit-mode)))))
          " edition"
          (:eval (if after-change-functions "" " | "))
          (:eval (if after-change-functions
                     ""
                     (propertize "¡BUG!"
                                 'help-echo "re-run lyqi-mode"
                                 'mouse-face 'mode-line-highlight
                                 'local-map '(keymap (header-line
                                                      keymap (mouse-1 . lyqi:header-line-lyqi-mode)))))))))

(defvar lyqi-mode-map nil
  "Keymap used in `lyqi-mode'.")

(defun lyqi-mode ()
  "Major mode for editing LilyPond music files, with quick insertion.

\\{lyqi-mode-map}

Moreover, in quick insertion mode:
TODO
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lyqi-mode)
  (setq mode-name "Lyqi")
  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lyqi:indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lyqi:indent-region)
  ;; before and after change function (for parse update)
  (make-local-variable 'before-change-functions)
  (make-local-variable 'after-change-functions)
  (pushnew 'lp:before-parse-update before-change-functions)
  (setq after-change-functions '(lp:parse-update))
  ;; buffer syntax
  (make-local-variable 'lp:*current-syntax*)
  (unless lp:*current-syntax*
    (setq lp:*current-syntax*
          (make-instance 'lyqi:lilypond-syntax
                         :language (lyqi:select-language (first lyqi:prefered-languages))
                         :relative-mode (eql lyqi:prefered-octave-mode 'relative))))
  (lp:parse-and-highlight-buffer)
  ;; header line shows info on lyqi mode
  ;; TODO: custom variable to turn off header line
  (lyqi:set-header-line-format)
  (use-local-map LilyPond-mode-map))

(defconst lyqi:+azerty-mode-map+
  '(;; Rest, skip, etc
    ("v" lyqi:insert-rest)
    ("b" lyqi:insert-mm-rest)
    ("q" lyqi:insert-chord-repetition)
    ;; also available: lyqi:insert-spacer lyqi:insert-skip
    ;; Pitches
    ("e" lyqi:insert-note-c)
    ("r" lyqi:insert-note-d)
    ("t" lyqi:insert-note-e)
    ("d" lyqi:insert-note-f)
    ("f" lyqi:insert-note-g)
    ("g" lyqi:insert-note-a)
    ("c" lyqi:insert-note-b)
    ;; Alterations
    ("z" lyqi:change-alteration-down)
    ("y" lyqi:change-alteration-up)
    ("a" lyqi:change-alteration-neutral)
    ;; Octaves
    ("s" lyqi:change-octave-down)
    ("h" lyqi:change-octave-up)
    ;; also available: lyqi:change-octave-zero
    ;; Durations
    ("i" lyqi:change-duration-1)
    ("o" lyqi:change-duration-2)
    ("j" lyqi:change-duration-4)
    ("k" lyqi:change-duration-8)
    ("l" lyqi:change-duration-16)
    ("m" lyqi:change-duration-32)
    ;; also available: lyqi:change-duration-64 lyqi:change-duration-128
    ("p" lyqi:change-duration-dots)))

(defconst lyqi:+qwerty-mode-map+
  '(;; Rest, skip, etc
    ("v" lyqi:insert-rest)
    ("b" lyqi:insert-mm-rest)
    ("q" lyqi:insert-chord-repetition)
    ;; also available: lyqi:insert-spacer lyqi:insert-skip
    ;; Pitches
    ("e" lyqi:insert-note-c)
    ("r" lyqi:insert-note-d)
    ("t" lyqi:insert-note-e)
    ("d" lyqi:insert-note-f)
    ("f" lyqi:insert-note-g)
    ("g" lyqi:insert-note-a)
    ("c" lyqi:insert-note-b)
    ;; Alterations
    ("w" lyqi:change-alteration-down)
    ("y" lyqi:change-alteration-up)
    ;; also available: lyqi:change-alteration-neutral
    ;; Octaves
    ("s" lyqi:change-octave-down)
    ("h" lyqi:change-octave-up)
    ("a" lyqi:change-octave-zero)
    ;; Durations
    ("i" lyqi:change-duration-1)
    ("o" lyqi:change-duration-2)
    ("j" lyqi:change-duration-4)
    ("k" lyqi:change-duration-8)
    ("l" lyqi:change-duration-16)
    (";" lyqi:change-duration-32)
    ;; also available: lyqi:change-duration-64 lyqi:change-duration-128
    ("p" lyqi:change-duration-dots)))

(eval-when (load)
  (setq lyqi-mode-map (make-sparse-keymap))
  (define-key lyqi-mode-map "\C-cq" 'lyqi:toggle-quick-edit-mode))

(defun lyqi:quick-insert-mode-map ()
  (loop for (key command) in (if (eql lyqi:keyboard-mapping 'azerty)
                                 lyqi:+azerty-mode-map+
                                 lyqi:+qwerty-mode-map+)
        do (define-key lyqi-mode-map key command))
  (loop for (key command) in lyqi:custom-key-map
        do (define-key lyqi-mode-map key command)))

(defun lyqi:normal-mode-map ()
  (loop for (key command) in (if (eql lyqi:keyboard-mapping 'azerty)
                                 lyqi:+azerty-mode-map+
                                 lyqi:+qwerty-mode-map+)
        do (define-key lyqi-mode-map key 'self-insert-command))
  (loop for (key command) in lyqi:custom-key-map
        do (define-key lyqi-mode-map key 'self-insert-command)))

(provide 'lyqi-mode)
