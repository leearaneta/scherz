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
const tensions = [
    {color: 0.4, dissonance: 0.6, gravity: 0.2},
    {color: 0.2, dissonance: 0.8, gravity: 0}
]
const scales = ['lydian']
const options = {type: "M7", root: "C"}
generate.generateProgression(scales, tensions, options)
=>  [ { scale: 'lydian',
        tonic: 'C',
        inversion: 0,
        type: 'M7',
        notes: [ 60, 64, 67, 71 ],
        pitches: [ 'C', 'E', 'G', 'B' ],
        name: 'CM7' },
      { scale: 'lydian',
        tonic: 'D',
        inversion: 1,
        type: 'M7sus2',
        notes: [ 64, 69, 73, 74 ],
        pitches: [ 'E', 'A', 'C#', 'D' ],
        name: 'DM7sus2' },
      { scale: 'lydian',
        tonic: 'A',
        inversion: 3,
        type: 'M7sus2',
        notes: [ 68, 69, 71, 76 ],
        pitches: [ 'G#', 'A', 'B', 'E' ],
        name: 'AM7sus2' } ]
```
  
`generateProgression` takes in three parameters:
  - a list of objects, each object with keys `color` `consonance` and `gravity`
    - each value corresponding to these keys must be between 0 and 1
  - a list of scales to choose chords from, `["lydian", "diminished"]` for example
    -  `util.scales` contains a list of all possible scales
  - an optional "options" object, which takes contains three keys:
    - `root` the root of the initial chord (defaults to `"C"`)
    - `type` the type of the initial chord, eg. `"M7"`
    - `seed` an integer that determines which chord is chosen in case of a tie

if specified, the initial chord's type must be present in at least one of the scales given.  we can call `util.possibleChordTypes` to verify:
```
util.possibleChordTypes(['lydian', 'diminished'])
=> ['M', 'm', '°', 'M7', 'D7', 'm7', 'ø7', 'M7sus2', '7sus2', '7sus4', 'M7sus4', 'Madd2', 'madd2', '°7', '°M7']
```
  
we can generate chords one by one using `generateChords` which also takes in three parameters and outputs a list of chords matching the corresponding color, consonance, and gravity
- a list of scales to choose chords from
- the previous chord
- an object with the keys `color` `consonance` and `gravity`

```
const scales = ['melodic-minor', 'augmented']
const initialChord = generate.initialChord(scales, "m7", "Db")
const tension = {color: 0.6, dissonance: 0.2, gravity: 0}
generate.generateChords(scales, initialChord, tension)
=>  [ { scale: 'melodic-minor',
        tonic: 'F',
        inversion: 2,
        type: '7sus4',
        notes: [ 67, 70, 72, 77 ],
        pitches: [ 'G', 'Bb', 'C', 'F' ],
        name: 'C7sus4' },
      { scale: 'melodic-minor',
        tonic: 'F',
        inversion: 0,
        type: 'madd2',
        notes: [ 65, 67, 68, 72 ],
        pitches: [ 'F', 'G', 'Ab', 'C' ],
        name: 'Fmadd2' },
      { scale: 'melodic-minor',
        tonic: 'F',
        inversion: 3,
        type: 'madd2',
        notes: [ 60, 65, 67, 68 ],
        pitches: [ 'C', 'F', 'G', 'Ab' ],
        name: 'Fmadd2' } ]
```
