# scherz
opinionated generative music

## Intro

scherz can generate chord progressions using color, dissonance, and gravity
-  increasing color raises the chances of chords venturing further in the circle of fifths
-  increasing dissonance results in more "unnatural" sounding chords
-  increasing gravity prioritizes half step resolutions and tighter voice leading

`generate` takes in three parameters:
  - a list of objects, each object with keys `color` `consonance` and `gravity`
    - each value corresponding to these keys must be between 0 and 1
  - a list of scales to choose chords from, `["lydian" "diminished"]` for example
  - an optional start tonic, defaults to "C"
  
## Build instructions (requires npm)
  `npm init`
  `npm install scherz`
  
in the repl, we can generate a progression with the required parameters.  for example:
  
  ```
  const scherz = require('scherz')
  const tensions = [{color: 0.4, dissonance: 0.6, gravity: 0.2}]
  const scales = ['lydian', 'diminished']
  const result = scherz.generate(tensions, scales)
  result.progression
  => [{scale: 'lydian',
        tonic: 'C',
        inversion: 0,
        notes: [60, 64, 67, 72],
        pitches: ['C', 'E', 'G', 'C'],
        type: 'CM',
        circle: ['Db', 'Ab', 'Eb', 'Bb', 'F', 'C', 'G', 'D', 'A', 'E', 'B', 'F#'],
        sorted: ['C', 'G', 'E']},
       {scale: 'lydian',
        tonic: 'Bb',
        inversion: 0,
        notes: [64, 67, 70, 74],
        pitches: ['E', 'G', 'Bb', 'D'],
        type: 'Em7-5',
        circle: ['Cb', 'Gb', 'Db', 'Ab', 'Eb', 'Bb', 'F', 'C', 'G', 'D', 'A', 'E'],
        sorted: ['Bb', 'G', 'D', 'E']}]
  ```

the first chord in the progression is always the :i chord in the first specified scale
possible scales are located in `scherz.scales`

