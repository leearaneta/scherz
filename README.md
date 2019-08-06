# scherz
opinionated generative music

## Intro

scherz can generate chord progressions using color, dissonance, and gravity
-  increasing color raises the chances of chords venturing farther in the circle of fifths
-  increasing dissonance results in more "unnatural" sounding chords
-  increasing gravity prioritizes half step resolutions and tighter voice leading

`main` takes in three parameters:
  - a vector of maps, each map with keys `color` `consonance` and `gravity`
    - each value corresponding to these keys must be between 0 and 1
  - a vector of scales to choose chords from, `[:lydian :diminished]` for example
  - an optional start tonic, defaults to :C
  
  ## Build instructions (requires leiningen)
  `lein repl`
  
  in the repl, we can run the main function with the required parameters.  for example:
  
  `(def color [0 0.4 0 0])`
  
  `(def dissonance [0 0.5 0.8 0])`
  
  `(def gravity [0 0 0 0])`
  
  `(def tensions (map (fn [c d g] {:color c :dissonance d :gravity g}) color dissonance gravity))`
  
  `(def scales [:major :diminished])`
  
  `(main tensions scales)`
  
  if you want to listen to the generated progression:
  
  `(require '[scherz.play :refer [play-progression]])`
  
  `(play-progression (main tensions scales)`
