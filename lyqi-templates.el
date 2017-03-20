;; 2016 Héctor Lahoz
;;
;; lyqi-templates.el - define some common teplates for lilypond files
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

(require 'tempo)

(tempo-define-template "ly-header" '(
"\\version \"2.12\"
\\header {
	title=\""
(p "title: " )
"\"
	composer=\""
(p "composer: ")
"\"
}
"
)
"\header"
"Skeleton for a new lilypond buffer for SATB scores"
)

(tempo-define-template "SATB-skeleton" '(
"%Time-stamp: <>" 'n
(tempo-template-ly-header) 'n
"musicSoprano = {
"
;; tempo 'p is to set mark. You can navigate through points of interest
;; with tempo-forward-mark and tempo-backward-mark
'p 'n
"}
"
'n
"musicAlto = {
"
'p 'n
"}
"
'n
"musicTenor = {
"
'p 'n
"}
"
'n
"musicBass = {
"
'p 'n
"}
"
'n
"globalSettings = {
	\\key "
(p "initial key: ") 'n
"	\\time "
(p "initial time: ") 'n
"}

\\bookpart {
	\\score {
		\\new ChoirStaff <<
			\\new Staff \\with {instrumentName = #\"Soprano\" shortInstrumentName = #\"S\"} {
				\\clef treble
				\\globalSettings
				\\musicSoprano
			}
			\\addlyrics { }
			\\new Staff \\with {instrumentName = #\"Alto\" shortInstrumentName = #\"A\"} {
				\\clef treble
				\\globalSettings
				\\musicAlto
			}
			\\addlyrics { }
			\\new Staff \\with {instrumentName = #\"Tenor\" shortInstrumentName = #\"T\"} {
				\\clef \"treble_8\"
				\\globalSettings
			}
			\\addlyrics { }
			\\new Staff \\with {instrumentName = #\"Bass\" shortInstrumentName = #\"B\"} {
				\\clef bass
				\\globalSettings
			}
			\\addlyrics { }
		>>
		\\layout {
			\\context {
				\\RemoveEmptyStaffContext
			}
			\\context {
				\\ChoirStaff
				\\override	 VerticalAxisGroup #'remove-first = ##t
			}
		}
	}
}

\\score {
	{
		\\tempo 4 = "
(p "quarter notes per minute for MIDI output: ") 'n
"		<<

		>>
	}
	\\midi {
		\\context {
			\\Voice
			\\remove \"Dynamic_performer\"
		}
	}
}"
))

(tempo-define-template "voice-with-piano-skeleton" '(
"%Time-stamp: <>" 'n
(tempo-template-ly-header) 'n
"melody = \\relative c'' {
  \\clef treble
  \\key c \\major
  \\time 4/4

  a b c d
}

text = \\lyricmode {
  Aaa Bee Cee Dee
}

upper = \\relative c'' {
  \\clef treble
  \\key c \\major
  \\time 4/4

  a4 b c d
}

lower = \\relative c {
  \\clef bass
  \\key c \\major
  \\time 4/4

  a2 c
}

\\score {
  <<
    \\new Voice = \"mel\" { \\autoBeamOff \\melody }
    \\new Lyrics \\lyricsto mel \\text
    \\new PianoStaff <<
      \\new Staff = \"upper\" \\upper
      \\new Staff = \"lower\" \\lower
    >>
  >>
  \\layout {
    \\context { \\Staff \\RemoveEmptyStaves }
  }
  \\midi { }
}"
))
