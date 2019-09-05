# scherz
opinionated generative music

## Intro

scherz can generate chord progressions using color, dissonance, and gravity
-  increasing color raises the chances of chords venturing further in the circle of fifths
-  increasing dissonance results in more "unnatural" sounding chords
-  increasing gravity prioritizes half step resolutions and tighter voice leading

`generateProgression` takes in four parameters:
  - a list of objects, each object with keys `color` `consonance` and `gravity`
    - each value corresponding to these keys must be between 0 and 1
  - a list of scales to choose chords from, `["lydian", "diminished"]` for example
  - an optional start tonic, defaults to "C"
  - the type of the initial chord, defaults to :i of the first scale given
  
## Build instructions (requires npm)
  ```
  npm init
  npm install scherz
  ```
  
in the repl, we can generate a progression with the required parameters.  for example:
  
  ```
  const { generate } = require('scherz')
  const tensions = [{color: 0.4, dissonance: 0.6, gravity: 0.2}]
  const scales = ['lydian', 'diminished']
  const result = generate.generateProgression(scales, tensions, "C", "M7")
  result
  [ { scale: 'lydian',
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
    name: 'DM7sus2' } ]
  ```

`util.scales` is a list of all possible scales

if specified, the initial chord's type must be present in at least one of the scales given.  we can call `generate.possibleTypes` to verify:
  ```
  generate.possibleTypes(['lydian', 'diminished'])
  => ['M', 'm', '°', 'M7', 'D7', 'm7', 'ø7', 'M7sus2', '7sus2', '7sus4', 'M7sus4', 'Madd2', 'madd2', '°7', '°M7']
  ```
