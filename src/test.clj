

; use frequencies instead of pitches
; use multiples of BPM / beat
; split into motifs
; define measure of consonance (using harmonic series and root of chords)
; make this more declarative

; TENSION AS A RESOURCE??

; stuff that affects transition vector (somewhat ordered):
 ; tension already used (should be more likely to resolve with less tension left)
 ; distance between notes, this is probably always the same
 ; syncopation
 ; pitch relative to current chord
 ; pitch relative to tonic

; motifs are more likely to resolve at less tense pitches and on the down beat
; after resolution, we can start another motif or reuse a previous one

; separate randomness from other code since it isn't referentially transparent

; create a bunch of functions:
 ; previous note => tension vector
 ; current chord => tension vector

; if the next note is above prev-note, multiply by a factor



