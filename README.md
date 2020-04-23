# scherz
opinionated generative music

## Intro

scherz can generate chord progressions using color, dissonance, and gravity
-  increasing color raises the chances of chords venturing further in the circle of fifths
-  increasing dissonance results in more "unnatural" sounding chords
-  increasing gravity prioritizes half step resolutions and tighter voice leading

## Build instructions (requires npm)
  ```
  npm init
  npm install scherz
  ```
  
in the repl, we can generate a progression with the required parameters.  for example:
  
```
const { generateProgression } = require('scherz')
const forces = [
    { color: 0, dissonance: 0.2, gravity: 0.5 },
    { color: 0.4, dissonance: 0.4, gravity: 0.25 },
    { color: 0, dissonance: 0.8, gravity: 0 },
    { color: 0.2, dissonance: 0.2, gravity: 0.25 },
]
const scales = ['lydian']
const options = { type: "M7", tonic: "D", dissonance: 0.15 }
generateProgression(scales, forces, options)
=>  [ { scale: 'lydian',
        inversion: 0,
        name: 'AM',
        tonic: 'D',
        pitches: [ 'A', 'E', 'C#', 'A' ],
        dissonance: 10,
        notes: [ 45, 52, 61, 69 ] },
      { scale: 'lydian',
        inversion: 0,
        name: 'F♯m/B',
        tonic: 'D',
        pitches: [ 'B', 'F#', 'C#', 'A' ],
        dissonance: 12,
        notes: [ 47, 54, 61, 69 ] },
      { scale: 'lydian',
        inversion: 3,
        name: 'G♯m7add4',
        tonic: 'E',
        pitches: [ 'F#', 'B', 'C#', 'G#', 'D#' ],
        dissonance: 15,
        notes: [ 54, 59, 61, 68, 75 ] },
      { scale: 'lydian',
        inversion: 1,
        name: 'F♯7sus4/D♯',
        tonic: 'E',
        pitches: [ 'D#', 'B', 'E', 'C#', 'F#' ],
        dissonance: 24,
        notes: [ 51, 59, 64, 73, 78 ] },
      { scale: 'lydian',
        inversion: 1,
        name: 'G♯m7add9',
        tonic: 'B',
        pitches: [ 'B', 'G#', 'D#', 'F#', 'A#' ],
        dissonance: 11,
        notes: [ 47, 56, 63, 66, 70 ] } ]
```
  
`generateProgression` takes in three parameters:
  - a list of objects (referred to as forces), 
    - `color` `consonance` and `gravity` are required
    - each value corresponding to these keys must be between 0 and 1
  - a list of scales to choose chords from, `["lydian", "diminished"]` for example
    -  `util.scales` contains a list of all possible scales
  - an optional "options" object, which takes contains three keys:
    - `tonic` the root of the initial chord (defaults to `"C"`)
    - `seed` an integer that determines which chord is chosen in case of a tie
    - `dissonance` a number between 0 and 1 that affects the dissonance of the first chord

we can generate chords one by one using `generateChords` which also takes in three parameters and outputs a list of chords matching the corresponding color, consonance, and gravity
- a list of scales to choose chords from
- the previous chord
- an object with the keys `color` `consonance` and `gravity`

```
const { initialChords, generateChords } = require('scherz')
const scales = ['melodic-minor', 'augmented']

// initialChord optionally takes in a third parameter that corresponds to dissonance
const initialChord = initialChords(scales, "Db", 0.25)[0]

const force = { color: 0.25, dissonance: 0.5, gravity: 0.5 }
generateChords(scales, initialChord, force)
=>  [ { scale: 'melodic-minor',
        inversion: 0,
        name: 'D♭7sus2add13',
        tonic: 'Ab',
        pitches: [ 'Db', 'Cb', 'Ab', 'Bb', 'Eb' ],
        dissonance: 18,
        notes: [ 49, 59, 68, 70, 75 ] } ]
```

forces can have three optional keys: `arc` `temper` and `incline`, which must have either `"asc"` or `"desc"` as their corresponding values
  - `arc: "asc"` ensures that generated chords modulate only to brighter keys
  - `temper: "asc"` attempts to generate "happy" sounding chords
  - `incline: "asc"` generates chords that are spatially higher than the previous chord
