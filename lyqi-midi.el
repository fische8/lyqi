;; Copyright 2009 Nicolas Sceaux, 2016 Héctor Lahoz
;;
;; lyqi-midi.el - Midi interface
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
;;; lyqi-midi.el - midi interfaces
;;;

(require 'eieio)
(require 'lyqi-syntax)

(defvar lyqi:midi-backend-object nil)

(defun lyqi:start-midi-backend ()
  (when (and lyqi:midi-backend (not lyqi:midi-backend-object))
    (setq lyqi:midi-backend-object
          (case lyqi:midi-backend
            ;; TODO: alsa backend
            ((osx)  (make-instance 'lyqi:osx-midi-player))))))

(defun lyqi:stop-midi-backend ()
  (when lyqi:midi-backend-object
    (lyqi:player-terminate)
    (setq lyqi:midi-backend-object nil)))
    
(defun lyqi:play-note (note)
  (when lyqi:midi-backend-object
    (lyqi:player-play-note lyqi:midi-backend-object note)))

;;;
;;; Backends
;;;

(defclass lyqi:midi-player ()
  ((last-note :initform nil)))

(defgeneric lyqi:player-play-midi-note (player midi-note)
  "Tell `player' to play `midi-note', a number.")
(defgeneric lyqi:player-terminate (player)
  "Tell `player' to terminate.")

(defmethod lyqi:player-terminate ((this lyqi:midi-player))
  t)

(defmethod lyqi:player-play-note ((player lyqi:midi-player) note &optional ref-note)
  "Tell `player' to play `note'.  If `ref-note' is non-NIL, then
consider that `note' is relative to `ref-note'."
  (let ((midi-note (if ref-note
                       (lyqi:midi-relative-pitch note ref-note)
                       (lyqi:midi-absolute-pitch note))))
    (lyqi:player-play-midi-note player midi-note)))

(defun lyqi:midi-absolute-pitch (note)
  (with-slots (pitch alteration octave-modifier) note
    (+ (aref [0 2 4 5 7 9 11] pitch)
       (/ alteration 2)
       (* octave-modifier 12)
       48)))

(defun lyqi:midi-relative-pitch (note ref-note)
  ;; TODO
  (lyqi:midi-absolute-pitch note))

;;;
;;; Mac OS X: MidiScript
;;;

(defclass lyqi:osx-midi-player (lyqi:midi-player) ())

(defmethod lyqi:player-play-midi-note ((player lyqi:osx-midi-player) midi-note)
  ;(do-applescript (format "ignoring application responses tell application \"MidiScript\" to playnote %d" midi-note))
  (start-process-shell-command
   "midiscript" nil
   (format "osascript -e 'tell application \"MidiScript\" to playnote %d'"
           midi-note)))

;;;
;;; Alsa (Linux): lyqikbd
;;;

;;; TODO

(provide 'lyqi-midi)
