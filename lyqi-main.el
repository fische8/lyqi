;; Copyright (c) 2009 Nicolas Sceaux, 2016 Héctor Lahoz
;;
;; lyqi-main.el - main mode setup, keymaps and modeline
;;
;; This file is part of lyqi.
;;
;; lyqi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; lyqi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with lyqi.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; lyqi-main.el
;;;

(require 'newcomment)

(defun lyqi-version ()
  "Display the current version of lyqi."
  (interactive)
  (message lyqi-version))

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lp-base)
(require 'lyqi-syntax)
(require 'lyqi-indent)
(require 'lyqi-midi)
(require 'lyqi-editing-commands)
(require 'lyqi-compile-commands)
(require 'lyqi-completion "completion")
(require 'lyqi-help)
(require 'lyqi-vars)

;;;
;;; Language selection
;;;
;;; 1) if \include "language.ly" is found is the buffer, use that language
;;; 2) if the buffer filename is in one of the directories listed in
;;; `lyqi-projects-language', then use the specified language
;;; 3) try to detect note names in the buffer
;;; 4) otherwise, use the prefered language.
;;;

(defvar lyqi-sorted-projects-language nil)

(eval-when (load)
  (setq lyqi-sorted-projects-language
        (sort (loop for (directory language) in lyqi-projects-language
                    collect (cons (file-name-as-directory (file-truename directory)) language))
              (lambda (elt1 elt2)
                (let ((str1 (first elt1))
                      (str2 (first elt2)))
                  (>= (length str1) (length str2)))))))

(defun lyqi-file-in-defined-projects-p (filename)
  "If `filename' is in a sub-directory of one of the directories
specified in `lyqi-projects-lanugages', return the associated langugage.
Otherwise, return NIL."
  (loop with file-dir = (file-name-directory (file-truename filename))
        with file-dir-length = (length file-dir)
        for (dir . lang) in lyqi-sorted-projects-language
        for len = (length dir)
        if (and (<= len file-dir-length)
                (string= dir (substring file-dir 0 (length dir))))
        return lang))

(defun lyqi-detect-buffer-language ()
  "Detect language used in current buffer.

- if \include \"<language>.ly\" is found is the buffer, use that language
- if the buffer filename is in one of the directories listed in
`lyqi-projects-language', then use the specified language
- try to detect note names in the buffer
- otherwise, use the prefered language."
  (save-excursion
    (goto-char (point-min))
    (or (and (re-search-forward "\\\\include \"\\(italiano\\|english\\|deutsch\\)\\.ly\"" nil t)
             (intern (match-string-no-properties 1)))
        (lyqi-file-in-defined-projects-p (buffer-file-name))
        (and (re-search-forward "\\(^\\|[ \t]\\)\\(do\\|re\\|mi\\|fa\\|sol\\|la\\|si\\)[',]*[1248]" nil t)
             'italiano) ;; TODO: choose first do-re-mi language from `lyqi-preferred-languages'
        (and (re-search-forward "\\(^\\|[ \t]\\)[a-h][',]*[1248]" nil t)
             'nederlands) ;; TODO: choose first a-b-c language from `lyqi-preferred-languages'
        (first lyqi-preferred-languages))))

(defun lyqi-select-next-language (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax lp--current-syntax))
         (current-language (slot-value (lyqi--language syntax) 'name))
         (next-language (loop with possible-languages = (slot-value syntax 'possible-languages)
                              for langs on possible-languages
                              for lang = (first langs)
                              if (eql lang current-language)
                              return (or (cadr langs) (first possible-languages)))))
    (set-slot-value syntax 'language (lyqi-select-language next-language))
    (force-mode-line-update)
    (lyqi--parse-and-highlight-buffer)))

;;;
;;; Mode maps
;;;
(defvar lyqi-normal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cq" 'lyqi-toggle-quick-edit-mode)
    (define-key map "\C-c\C-t" 'lyqi-transpose-region)
    (define-key map "\C-c\C-l" 'lyqi-compile-ly)
    (define-key map "\C-c\C-s" 'lyqi-open-pdf)
    (define-key map [(control c) return] 'lyqi-open-midi)
    (define-key map "\"" 'lyqi-insert-delimiter)
    (define-key map "}" 'lyqi-insert-closing-delimiter)
    (define-key map ">" 'lyqi-insert-closing-delimiter)
    (define-key map [tab] 'lyqi-indent-line)
    (define-key map [(control tab)] 'lyqi-complete-word)
    map)
  "Keymap used in `lyqi-mode', in normal editing.")

(defvar lyqi-quick-insert-mode-map
  (let ((map (copy-keymap lyqi-normal-mode-map)))
    (define-key map "\"" 'lyqi-enter-quick-insert-string)
    (define-key map "\\" 'lyqi-enter-quick-insert-command)
    map)
  "Keymap used in `lyqi-mode', in quick insertion editing.")

(defvar lyqi-quick-insert-string-mode-map
  (let ((map (copy-keymap lyqi-normal-mode-map)))
    (define-key map "\"" 'lyqi-quit-quick-insert-string)
    map)
  "Keymap used in `lyqi-mode', when inserting a string in quick insertion editing.")

(defvar lyqi-quick-insert-command-mode-map
  (let ((map (copy-keymap lyqi-normal-mode-map)))
    (define-key map " " 'lyqi-quit-quick-insert-command)
    map)
  "Keymap used in `lyqi-mode', when inserting a \\command in quick insertion editing.")

(defun lyqi-toggle-quick-edit-mode (&optional syntax)
  (interactive)
  (let* ((syntax (or syntax lp--current-syntax))
         (quick-edit-mode (not (slot-value syntax 'quick-edit-mode))))
    (set-slot-value syntax 'quick-edit-mode quick-edit-mode)
    (if quick-edit-mode
        (use-local-map lyqi-quick-insert-mode-map)
      (use-local-map lyqi-normal-mode-map)))
  (force-mode-line-update))

(defun lyqi-enter-quick-insert-string ()
  (interactive)
  (lyqi-insert-delimiter)
  (use-local-map lyqi-quick-insert-string-mode-map))

(defun lyqi-quit-quick-insert-string ()
  (interactive)
  (lyqi-insert-delimiter)
  (use-local-map lyqi-quick-insert-mode-map))

(defun lyqi-enter-quick-insert-command ()
  (interactive)
  (insert-char last-command-event 1)
  (use-local-map lyqi-quick-insert-command-mode-map))

(defun lyqi-quit-quick-insert-command ()
  (interactive)
  (insert-char last-command-event 1)
  (use-local-map lyqi-quick-insert-mode-map))

(defconst lyqi-+azerty-mode-map+
  '(;; Rest, skip, etc
    ("v" lyqi-insert-rest)
    ("b" lyqi-insert-spacer)
    ("q" lyqi-insert-chord-repetition)
    ;; also available: lyqi-insert-spacer lyqi-insert-skip
    ;; Pitches
    ("e" lyqi-insert-note-c)
    ("r" lyqi-insert-note-d)
    ("t" lyqi-insert-note-e)
    ("d" lyqi-insert-note-f)
    ("f" lyqi-insert-note-g)
    ("g" lyqi-insert-note-a)
    ("c" lyqi-insert-note-b)
    ;; Alterations
    ("z" lyqi-change-alteration-down)
    ("y" lyqi-change-alteration-up)
    ("a" lyqi-change-alteration-neutral)
    ;; Octaves
    ("s" lyqi-change-octave-down)
    ("h" lyqi-change-octave-up)
    ;; also available: lyqi-change-octave-zero
    ;; Durations
    ("i" lyqi-change-duration-1)
    ("o" lyqi-change-duration-2)
    ("j" lyqi-change-duration-4)
    ("k" lyqi-change-duration-8)
    ("l" lyqi-change-duration-16)
    ("m" lyqi-change-duration-32)
    ("p" lyqi-change-duration-dots)
    ;; also available: lyqi-change-duration-64 lyqi-change-duration-128
    ;; Undo
    ("u" undo)
    ("<" lyqi-insert-opening-delimiter)))

(defconst lyqi-+qwerty-lr-mode-map+
  '(;; Rest, skip, etc
    ("v" lyqi-insert-rest)
    ("b" lyqi-insert-spacer)
    ("q" lyqi-insert-chord-repetition)
    ;; also available: lyqi-insert-spacer lyqi-insert-skip
    ;; Pitches
    ("e" lyqi-insert-note-c)
    ("r" lyqi-insert-note-d)
    ("t" lyqi-insert-note-e)
    ("d" lyqi-insert-note-f)
    ("f" lyqi-insert-note-g)
    ("g" lyqi-insert-note-a)
    ("c" lyqi-insert-note-b)
    ;; Alterations
    ("w" lyqi-change-alteration-down)
    ("y" lyqi-change-alteration-up)
    ;; also available: lyqi-change-alteration-neutral
    ;; Octaves
    ("s" lyqi-change-octave-down)
    ("h" lyqi-change-octave-up)
    ("a" lyqi-change-octave-zero)
    ;; Durations
    ("i" lyqi-change-duration-1)
    ("o" lyqi-change-duration-2)
    ("j" lyqi-change-duration-4)
    ("k" lyqi-change-duration-8)
    ("l" lyqi-change-duration-16)
    (";" lyqi-change-duration-32)
    ("p" lyqi-change-duration-dots)
    ;; also available: lyqi-change-duration-64 lyqi-change-duration-128
    ;; Undo
    ("u" undo)))

;; same as lyqi-+querty-lr-mpde-map+ but replacing alteration and octave
;; with more logic keys w/s for alteration and q/a for octave
(defconst lyqi-+qwerty-ud-mode-map+
  '(;; Rest, skip, etc
    ("v" lyqi-insert-rest)
    ("b" lyqi-insert-spacer)
    ;; also available: lyqi-insert-spacer lyqi-insert-skip
    ;; Pitches
    ("e" lyqi-insert-note-c)
    ("r" lyqi-insert-note-d)
    ("t" lyqi-insert-note-e)
    ("d" lyqi-insert-note-f)
    ("f" lyqi-insert-note-g)
    ("g" lyqi-insert-note-a)
    ("c" lyqi-insert-note-b)
    ;; Alterations
    ("w" lyqi-change-alteration-up)
    ("s" lyqi-change-alteration-down)
    ;; also available: lyqi-change-alteration-neutral
    ;; Octaves
    ("q" lyqi-change-octave-up)
    ("a" lyqi-change-octave-down)
    ;; Durations
    ("i" lyqi-change-duration-1)
    ("o" lyqi-change-duration-2)
    ("j" lyqi-change-duration-4)
    ("k" lyqi-change-duration-8)
    ("l" lyqi-change-duration-16)
    (";" lyqi-change-duration-32)
    ("p" lyqi-change-duration-dots)
    ;; also available: lyqi-change-duration-64 lyqi-change-duration-128
    ;; Undo
    ("u" undo)))

(defmacro lyqi-define-string-insert-command (string &optional with-space-around)
  (let ((fn-name (intern (format "insert-string-%s" string))))
    `(progn
       (defun ,fn-name ()
         ,(format "Insert string \"%s\"" string)
         (interactive)
         ,(if with-space-around
              `(lyqi-with-space-around
                (insert ,string))
              `(insert ,string)))
       (byte-compile ',fn-name)
       ',fn-name)))

;;;
;;; Header line
;;;

(defun lyqi-mode-line-select-next-language (event)
  "Like `lyqi-select-next-language', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-select-next-language)))

(defun lyqi-mode-line-toggle-quick-edit-mode (event)
  "Like `lyqi-toggle-quick-edit-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-toggle-quick-edit-mode)))

(defun lyqi-mode-line-set-global-master-file (event filename)
  "Like `lyqi-set-global-master-file', but temporarily select EVENT's window."
  (interactive "e\nfGlobal master file: ")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-set-global-master-file filename)))

(defun lyqi-mode-line-set-buffer-master-file (event filename)
  "Like `lyqi-set-buffer-master-file', but temporarily select EVENT's window."
  (interactive "e\nfBuffer master file:")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-set-buffer-master-file filename)))

(defun lyqi-mode-line-unset-master-file (event)
  "Like `lyqi-unset-master-file', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-unset-master-file)))

(defun lyqi-mode-line-lyqi-mode (event)
  "Like `lyqi-mode', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (lyqi-mode)))

(defun lyqi-set-mode-line-modes ()
  (setq mode-line-modes
        '("%[("
          (:eval (propertize (if (slot-value lp--current-syntax 'quick-edit-mode)
                                 (format "%s:quick insert" mode-name)
			       mode-name)
                             'help-echo "mouse-1: toggle edit mode, mouse-2: major mode help, mouse-3: toggle minor modes"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (mode-line
                                           keymap (down-mouse-3 . mode-line-mode-menu-1)
                                           (mouse-2 . describe-mode)
                                           (down-mouse-1 . lyqi-mode-line-toggle-quick-edit-mode)))))
          ", "
          (:eval (propertize (format "lang: %s"
                                     (substring (symbol-name (slot-value (lyqi--language lp--current-syntax) 'name))
                                                0 2))
                             'help-echo "select next language"
                             'mouse-face 'mode-line-highlight
                             'local-map '(keymap
                                          (mode-line
                                           keymap (mouse-1 . lyqi-mode-line-select-next-language)))))
          (:eval (if (lyqi-defined-master-file) ", " ""))
          (:eval (if (lyqi-defined-master-file)
                     (propertize (format "Master file: %s" (file-name-nondirectory (lyqi-master-file)))
                                 'help-echo "mouse-1: set global master file, mouse-2: unset master file, mouse-3 set buffer-local master file"
                                 'mouse-face 'mode-line-highlight
                                 'local-map '(keymap
                                              (mode-line
                                               keymap (mouse-1 . lyqi-mode-line-set-global-master-file)
                                               (mouse-3 . lyqi-mode-line-set-buffer-master-file)
                                               (mouse-2 . lyqi-mode-line-unset-master-file))))
                     ""))
          (:eval (if after-change-functions "" ", "))
          (:eval (if after-change-functions
                     ""
                     (propertize "¡BUG!"
                                 'help-echo "re-run lyqi-mode"
                                 'mouse-face 'mode-line-highlight
                                 'local-map '(keymap (mode-line
                                                      keymap (mouse-1 . lyqi-mode-line-lyqi-mode))))))
          ")%]  ")))

(defvar lyqi-mode-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\' "w" st)
    (modify-syntax-entry ?\, "w" st)
    (modify-syntax-entry ?\. "w" st)
    (modify-syntax-entry ?\* "w" st)
    (modify-syntax-entry ?\/ "w" st)
    st)
  "Syntax table used in `lyqi-mode' buffers.")

;;;###autoload
(defun lyqi-mode ()
  "Major mode for editing LilyPond music files, with quick insertion.

\\{lyqi-normal-mode-map}

In quick insertion mode:
\\{lyqi-quick-insert-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lyqi-mode)
  (setq mode-name "Lyqi")
  (set-syntax-table lyqi-mode-syntax-table)
  ;; insert template when visiting new file
  (when (= (buffer-size) 0)
    (make-local-variable 'tempo-interactive)
    (setq tempo-interactive t)
    (tempo-template-SATB-skeleton))
  ;; comment syntax variables
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%{? *")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'block-comment-start)
  (setq block-comment-start "%{")
  (make-local-variable 'block-comment-end)  
  (setq block-comment-end   "%}")
  (setq comment-region-function 'lyqi-comment-region)
  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lyqi-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lyqi-indent-region)
  ;; before and after change function (for parse update)
  (make-local-variable 'before-change-functions)
  (make-local-variable 'after-change-functions)
  (pushnew 'lp:before-parse-update before-change-functions)
  (setq after-change-functions '(lyqi--parse-update))
  ;; avoid point adjustment
  (make-local-variable 'global-disable-point-adjustment)
  (setq global-disable-point-adjustment t)
  ;; buffer syntax
  (make-local-variable 'lp--current-syntax)
  (unless lp--current-syntax
    (let ((lang (lyqi-detect-buffer-language)))
      (setq lp--current-syntax
            (make-instance 'lyqi-lilypond-syntax
                           :language (lyqi-select-language lang)))
      (pushnew lang (slot-value lp--current-syntax 'possible-languages))))
  (lyqi--parse-and-highlight-buffer)
  ;; buffer master file
  (make-local-variable 'lyqi-buffer-master-file)
  ;; mode line modes
  (make-local-variable 'mode-line-modes)
  (lyqi-set-mode-line-modes)
  ;; midi backend
  (lyqi-start-midi-backend)
  ;; default mode-map
  (use-local-map lyqi-normal-mode-map)
  ;; read customized keymap and set quick-insert-mode-map accordingly
  (loop for (key command) in (case lyqi-keyboard-mapping
				   ((azerty) lyqi-+azerty-mode-map+)
				   ((qwerty-lr) lyqi-+qwerty-lr-mode-map+)
				   (t lyqi-+qwerty-ud-mode-map+))
        do (define-key lyqi-quick-insert-mode-map key command))
  (loop for (key command) in lyqi-custom-key-map
        if (stringp command)
        do (define-key lyqi-quick-insert-mode-map
             key (eval `(lyqi-define-string-insert-command ,command))) ;; urgh
        else if (and (listp command) (eql (car command) 'space-around))
        do (define-key lyqi-quick-insert-mode-map
             key (eval `(lyqi-define-string-insert-command ,(cdr command) t)))
        else do (define-key lyqi-quick-insert-mode-map key command)))

(provide 'lyqi-main)
