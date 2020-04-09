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
const { generate } = require('scherz')
const forces = [
    { color: 0, dissonance: 0.2, gravity: 0.5 },
    { color: 0.4, dissonance: 0.4, gravity: 0.25 },
    { color: 0, dissonance: 0.8, gravity: 0 },
    { color: 0.2, dissonance: 0.2, gravity: 0.25 },
]
const scales = ['lydian']
const options = { type: "M7", tonic: "D" }
generate.generateProgression(scales, forces, options)
=>  [ { scale: 'lydian',
        inversion: 0,
        name: 'DM7',
        tonic: 'D',
        pitches: [ 'D', 'A', 'C#', 'F#' ],
        dissonance: 9,
        notes: [ 50, 57, 61, 66 ] },
      { scale: 'lydian',
        inversion: 2,
        name: 'Bm7',
        tonic: 'D',
        pitches: [ 'F#', 'B', 'D', 'A' ],
        dissonance: 11,
        notes: [ 54, 59, 62, 69 ] },
      { scale: 'lydian',
        inversion: 2,
        name: 'EM/F#',
        tonic: 'E',
        pitches: [ 'F#', 'B', 'E', 'G#' ],
        dissonance: 15,
        notes: [ 54, 59, 64, 68 ] },
      { scale: 'lydian',
        inversion: 1,
        name: 'C#m/D#',
        tonic: 'E',
        pitches: [ 'D#', 'E', 'G#', 'C#' ],
        dissonance: 23,
        notes: [ 63, 64, 68, 73 ] },
      { scale: 'lydian',
        inversion: 2,
        name: 'BMadd7',
        tonic: 'B',
        pitches: [ 'F#', 'D#', 'A#', 'B' ],
        dissonance: 11,
        notes: [ 54, 63, 70, 71 ] } ]
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

if specified, the initial chord's type must be present in at least one of the scales given.  we can call `util.possibleChordTypes` to verify:
```
util.possibleChordTypes(['lydian', 'diminished'])
=> ['M', 'm', '°', 'M7', '7', 'm7', 'ø7', 'M7sus2', '7sus2', '7sus4', 'M7sus4', '°7', '°M7']
```

we can generate chords one by one using `generateChords` which also takes in three parameters and outputs a list of chords matching the corresponding color, consonance, and gravity
- a list of scales to choose chords from
- the previous chord
- an object with the keys `color` `consonance` and `gravity`

```
const scales = ['melodic-minor', 'augmented']
const initialChord = generate.initialChords(scales, "Db")[0]
const force = { color: 0.25, dissonance: 0.5, gravity: 0.5 }
generate.generateChords(scales, initialChord, force)
=>  [ { scale: 'melodic-minor',
        inversion: 0,
        name: 'Bbm7/Eb',
        tonic: 'Ab',
        pitches: [ 'Eb', 'Bb', 'Db', 'F', 'Ab' ],
        dissonance: 16,
        notes: [ 63, 70, 73, 77, 80 ] },
      { scale: 'melodic-minor',
        inversion: 0,
        name: 'CbM/Db',
        tonic: 'Gb',
        pitches: [ 'Db', 'Cb', 'Eb', 'Gb' ],
        dissonance: 16,
        notes: [ 61, 71, 75, 78 ] },
      { scale: 'melodic-minor',
        inversion: 1,
        name: 'Abm7/Db',
        tonic: 'Gb',
        pitches: [ 'Db', 'Cb', 'Eb', 'Gb', 'Ab' ],
        dissonance: 16,
        notes: [ 61, 71, 75, 78, 80 ] } ]
```

forces can have three optional keys: `arc` `temper` and `incline`, which must have either `"asc"` or `"desc"` as their corresponding values
  - `arc: "asc"` ensures that generated chords modulate only to brighter keys
  - `temper: "asc"` attempts to generate "happy" sounding chords
  - `incline: "asc"` generates chords that are spatially higher than the previous chord
