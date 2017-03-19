;; Copyright 2009 Nicolas Sceaux, 2016 Héctor Lahoz
;;
;; lyqi-midi.el - MIDI interface
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
;;; lyqi-midi.el - MIDI interface
;;;

(require 'eieio)
(require 'lyqi-syntax)

(defvar lyqi-midi-player-object nil)

;;;
;;; Backends
;;;

(defclass lyqi-midi-player ()
  ((last-note :initform 0
	      :documentation "last note we played. A number as in MIDI")
   (process :initform nil)))

(defgeneric lyqi-player-terminate (player)
  "Tell `player' to terminate.")

(defmethod lyqi-player-terminate ((this lyqi-midi-player))
  t)

;; (defmethod play-note ((player lyqi-midi-player) note &optional ref-note)
;;   "Tell `player' to play `note'.  If `ref-note' is non-NIL, then
;; consider that `note' is relative to `ref-note'."
;;   (let ((midi-note (if ref-note
;;                        (lyqi-midi-relative-pitch note ref-note)
;;                        (lyqi-midi-absolute-pitch note))))))

;;;
;;; PC Speaker
;;;
(defclass lyqi-pcspkr-midi-player (lyqi-midi-player)
  ()
  "Play a note in the PC speaker")

(defmethod lyqi-player-terminate ((this lyqi-pcspkr-midi-player))
  (when (oref this process)
	(delete-process (oref this process))))

(defmacro have-beepd ()
  "Check beepd availability"
  (with-temp-buffer
	(= 0 (call-process-shell-command "which" nil t nil "beepd"))))

(defmacro lyqi-set-pcspkr-playnote ()
  `(defmethod play-note ((this lyqi-pcspkr-midi-player) note)
     (let ((freq (* 440 (expt 2 (/ (- note 69) 12.0))))
	   (duration 600))
       ,(if (have-beepd)
	    '(when (oref this process)
	       (process-send-string (oref this process) (format "%d %d\n" freq duration)))
	  '(call-process "beep" nil nil nil "-l" (number-to-string duration) "-f" (number-to-string freq))))))

(defmacro lyqi-set-pcspkr-start ()
  `(defmethod start-player ((this lyqi-pcspkr-midi-player))
     "Arranca el proceso hijo (beepd)"
     (oset this process
	   ,(if (have-beepd)
		'(let ((process-connection-type nil))
		   (start-process "beepd" nil "beepd"))
	      'nil))
     (set-process-query-on-exit-flag (oref this process) nil)))

;;;
;;; Mac OS X: MidiScript
;;;

(defclass lyqi-osx-midi-player (lyqi-midi-player) ())

(defmethod play-note ((player lyqi-osx-midi-player) midi-note)
  ;(do-applescript (format "ignoring application responses tell application \"MidiScript\" to playnote %d" midi-note))
  (start-process-shell-command
   "midiscript" nil
   (format "osascript -e 'tell application \"MidiScript\" to playnote %d'"
           midi-note)))

;;;
;;; Alsa (Linux): lyqikbd
;;;
;;; TODO

(defun lyqi-start-midi-backend ()
  (when (and lyqi-midi-backend (not lyqi-midi-player-object))
    (setq lyqi-midi-player-object
	  (case lyqi-midi-backend
		;; TODO: alsa backend
		(pcspkr (make-instance 'lyqi-pcspkr-midi-player))
		(osx  (make-instance 'lyqi-osx-midi-player))))
    ;; TODO: this should be done by the constructor above. Don't know how yet
    (when (lyqi-pcspkr-midi-player-p lyqi-midi-player-object)
      (lyqi-set-pcspkr-start)
      (lyqi-set-pcspkr-playnote)
      (start-player lyqi-midi-player-object))))

(defun lyqi-stop-midi-backend ()
  (when lyqi-midi-player-object
    (lyqi-player-terminate lyqi-midi-player-object)
    (setq lyqi-midi-player-object nil)))

(defun lyqi-play-note (note &optional ref-note)
  (when lyqi-midi-player-object
    (let ((midi-note (if ref-note
			 (lyqi-midi-relative-pitch note ref-note)
		       (with-slots (pitch alteration octave-modifier) note
				   (+ (aref [0 2 4 5 7 9 11] pitch)
				      (/ alteration 2)
				      (* octave-modifier 12)
				      48)))))
      (play-note lyqi-midi-player-object midi-note))))

(provide 'lyqi-midi)
