extensions [ csv palette ]

globals [
  region-boundaries ; a list of regions definitions, where each region is a list of its min pxcor and max pxcor
  plant-number

]

breed [ pollinators pollinator ]
;breed [ plants plant ]

;patches are plants
;patch variables
patches-own[
  plant.id
  number-of-visits
  region
  cluster
  flower-density       ;; More density more attractive to pollinatiors a proxy of energy available
  ;plant.id
  ;no.of.flowers
  ;no.of.flowers_max
]

;variables characterising the pollinators
pollinators-own [
  species
  flight_speed
  foraging_range
  foraging_max
  degree
  degree_max
  energy
  body_mass
  stdev-angle      ; Correlated random walk
  euse
]

;to setup
 ; clear-all
  ;file-close-all ; Close any files open from last run
  ;setup-landscape_1 ;divided into 16 or 25 areas
  ;setup-landscape_2 ;divided into more patchy areas
  ;read-pollinators-from-csv
  ;setup-pollinators
  ;reset-ticks
;end


;; Generating the landscapes
;;; LANDSCAPE 1
to setup-landscape_1
 clear-all

  ;make-landscape_1_scenarios
 ;ask patches [set pcolor white]
  ;resize-world 0 100 0 100

  set region-boundaries calculate-region-boundaries number-of-regions
  ;print (word "Region boundaries " region-boundaries)
  let region-numbers (range 1 (number-of-regions + 1))
  (foreach region-boundaries region-numbers [ [boundariesx region-numberx] ->
    (foreach region-boundaries region-numbers [ [boundariesy region-numbery] ->
      ask patches with [ pxcor >= first boundariesx and pxcor <= last boundariesx and pycor >= first boundariesy and pycor <= last boundariesy] [

        ;print (word "region number " region-number " boundaries " boundaries)
        set region region-numberx +  ( region-numbery - 1 ) * number-of-regions

        set pcolor palette:scale-gradient [[255 255 0] [0 0 255]] region 1 (number-of-regions * number-of-regions)
        ;set pcolor 5 + ( (region-numberx * region-numbery) - 1 )* 10

        ; (ifelse
        ; landscape_1_scenarios = "fragmented-segregated" [ set pcolor
            ;set plant-number 160 / 16  ; same amount of plants in every local area
            ;Here I want to randomly assign the 16 colors to each local area, maybe the same a just assigning randomly to the entire landscape
            ;set pcolor according to csv file!!!!


      ]
    ])
  ])

end


to-report calculate-region-boundaries [ num-regions ]
  ; The region definitions are built from the region divisions:
  let divisions region-divisions num-regions
  ; Each region definition lists the min-pxcor and max-pxcor of the region.
  ; To get those, we use `map` on two "shifted" copies of the division list,
  ; which allow us to scan through all pairs of dividers
  ; and built our list of definitions from those pairs:
  report (map [ [d1 d2] -> list (d1 ) (d2 ) ] (but-last divisions) (but-first divisions))
end

to-report region-divisions [ num-regions ]
  ; This procedure reports a list of pxcor that should be outside every region.
  ; Patches with these pxcor will act as "dividers" between regions.
  report n-values (num-regions + 1) [ n ->
    [ pxcor ] of patch (min-pxcor + (n  * ((max-pxcor - min-pxcor) / num-regions))) 0
  ]

  reset-ticks
end


;to make-landscape_1_scenarios
          ;if landscape_1_scenarios = "fragmented-segregated" [
            ;set plant-number 160 / 16  ; same amount of plants in every local area
            ;set pcolor according to plant colors in the csv file!!!!
            ;Here I wanted to assign one color per local area (saqure or region) --> segregated landscape
            ; ]

          ;if landscape = "habitat-structured"[
            ;set ....
            ;here only plants can be inside certain habitat, i.e. habitat-nested using the habitat info from the plant-parameter file
            ;]

          ;if landscape "heterogenously-mixed"[
             ; Here I wanted to assign all 16 colors=plants randomly to every local area --> mixed landscape
             ;]
;end



;;;;;;;

;;; LANDSCAPE 2
to setup-landscape_2
  clear-all
  make-landscape_2_scenarios

  let dim 100
  let ext dim ^ 2
  resize-world 0 (dim - 1) 0 (dim - 1)
  set-patch-size 4
  ask patches [
     set cluster nobody
  ]

;; Define the number of seed points from the slider
  let seedcount dim * dim * seed-percent
  let seedlist []

;; Generate a list for the land cover seeds
;; Notice: The sum would converge to the seedcount by sampling
  while [length seedlist != land-cover-classes][
    set seedlist lput (random (seedcount / land-cover-classes * 2) + 1) seedlist
    ]

;; Criteria and scaling on very high seed counts
  if sum seedlist > ext [
    let c ext / sum seedlist
    set seedlist map floor map [ ?1 -> c * ?1 ] seedlist
    let change (item 0 seedlist + ext - sum seedlist)
    set seedlist replace-item 0 seedlist change
    ]


;; Scatter seed
  let i 0
  foreach seedlist [ ?1 ->
    set i i + 1
    repeat ?1 [
      ask one-of (patches with [cluster = nobody])[
        set cluster i
        set pcolor 3 + 10 * i
      ]
    ]
  ]

;; Fill the voids by propagation
;; Credit: Uri Wilensky, Patch Cluster Example
  while [any? patches with [cluster = nobody]] [
    ask patches with [cluster = nobody][
      let c [cluster] of one-of neighbors4
      if c != nobody [
        set cluster c
        set pcolor 3 + 10 * c
      ]
    ]
  ]


  reset-ticks
end


;;; make landscape_2_scenarios
;;; !!!NB we need to add flowers to the patches
to make-landscape_2_scenarios

  if landscape_2_scenarios = "fragmented" [
    set land-cover-classes 4
    set seed-percent 0.02 ]

  if landscape_2_scenarios = "intermediate-complexity" [
    set land-cover-classes 4
    set seed-percent 0.003 ]

  if landscape_2_scenarios = "homogenous" [
    set land-cover-classes 3
    set seed-percent 0.001 ]

end

;;; to setup real world landscape, here we could also have three examples from Nepali landscapes
; I will ask Tom Timberlake to make three gis renderings for us with color-coded habitats where we can add the plants and flowers
to setup-landscape_3
 clear-all
 import-drawing "Chickwell Farm.png"
 import-pcolors "Chickwell Farm.png"
 reset-ticks
end

;;;;;;;;;;;;;;;;

; SETUP TURTLES AND TURLE PROCEDURES
; procedure to read some turtle properties from a file
to setup-pollinators
  reset-ticks
  ;clear-plot
  clear-turtles
  set-default-shape pollinators "bug" ; tried "bee 2" shape but does not work, why?
  file-close-all ; close all open files
  if not file-exists? "pollinator_parameters.csv" [
    user-message "No file 'pollinator_parameters.csv' exists! Try pressing WRITE-TURTLES-TO-CSV."
    stop
  ]
  file-open "pollinator_parameters.csv" ; open the file with the turtle data
  ;; To skip the header row in the while loop,
  ;  read the header row here to move the cursor down to the next line.
  let headings csv:from-row file-read-line
  ; We'll read all the data in a single loop
  while [ not file-at-end? ] [
    ; here the CSV extension grabs a single line and puts the read data in a list
    let pollinator_data csv:from-row file-read-line
    ;set pollinator_data word pollinator_data ";"  ; add semicolon for loop termination
    print pollinator_data
    ; now we can use that list to create a turtle with the saved properties
    create-pollinators number-of-pollinators [
      setxy random-pxcor random-pycor
      set species        item 0 pollinator_data
      set flight_speed   item 1 pollinator_data
      set foraging_range item 2 pollinator_data
      set foraging_max   item 3 pollinator_data
      set degree         item 4 pollinator_data
      set degree_max     item 5 pollinator_data
      set energy         item 6 pollinator_data
      set body_mass      item 7 pollinator_data
      set size           item 8 pollinator_data
      set color          item 9 pollinator_data
      set euse 0
    ]
  ]

    file-close ; make sure to close the file
end

;;; !!! NB LEO, I wanted to import plant info but realised that plants are not a breed but the patches
; how to do this? should we make plant a breed? or can you import directly
;to make-plants
;  reset-ticks
 ; file-close-all ; close all open files
 ; if not file-exists? "plant_parameters.csv" [
  ;  user-message "No file 'plant_parameters.csv' exists! Try pressing WRITE-TURTLES-TO-CSV."
   ; stop
  ;]
  ;file-open "plant_parameters.csv" ; open the file with the turtle data
  ;; To skip the header row in the while loop,
  ;  read the header row here to move the cursor down to the next line.
 ; let headings csv:from-row file-read-line
  ; We'll read all the data in a single loop
 ; while [ not file-at-end? ] [
    ; here the CSV extension grabs a single line and puts the read data in a list
  ;  let plant_data csv:from-row file-read-line
    ;set pollinator_data word pollinator_data ";"  ; add semicolon for loop termination
   ; print plant_data
    ; now we can use that list to create a turtle with the saved properties
    ;create-plants plant-number [
     ; set plant.id item 0 plant_data
     ; set no.of.flowers item 1 plant_data
      ;set no.of.flowers_max item 2 plant_data
    ;]
  ;]
;end

to go

  if not any? pollinators or ticks = 500 [
    file-close
    stop
  ]

  ask pollinators [

    move-pollinators
    eat
    death
  ]

  ;ask patches with [pcolor != white] [

    ;patch-flowering

  ;]

end

;pollinator behaviour
to move-pollinators

  ;!!! NB. comment to Leo: this could probably be written more elegantly, we could also consider to let all pollinators turn in the same way [ turn-pillinators ]
  ; Set turning angle
  ;
  (ifelse
    species = "poll_1"  [ turn-pollinators   ]
    species = "poll_2"  [ turn-pollinators   ]
    species = "poll_3"  [ turn-pollinators   ]
    species = "poll_4"  [ turn-pollinators   ]
    species = "poll_5"  [ turn-pollinators   ]
    species = "poll_6"  [ turn-pollinators   ]
    species = "poll_7"  [ turn-pollinators   ]
    species = "poll_8"  [ turn-pollinators   ]
    species = "poll_9"  [ turn-pollinators   ]
    species = "poll_10" [ turn-pollinators   ]
    species = "poll_11" [ turn-pollinators   ]
    species = "poll_12" [ turn-pollinators   ]
    species = "poll_13" [ turn-diptera   ]
    species = "poll_14" [ turn-diptera   ]
    species = "poll_15" [ turn-diptera   ]
    species = "poll_16" [ turn-diptera   ]
  )
  ; Exponential is equivalent to normal centered in 0
  ;
  let step-length random-exponential flight_speed ; rate ;

  ; Distance travelled
  ;
  fd step-length

  ;
  ; energy lost by movement
  ;
  set euse step-length * 0.1
  set energy (energy - euse)

end

;; Pollinators procedure
to turn-pollinators
  rt random-normal 0 stdev-angle ; this option would activate the slider for turn angle, and apply correlated direction
                                 ;this applies correlated direction instead of fully random degree angle, based on empirical data for turn angles
end


to turn-diptera
  set heading random-float 360
end

  ; pollinator's procedure
;
to eat
  ;if is-flowering = TRUE [

    if  flower-density >= 10  [
      count-visits
      set energy energy + 1                    ; adds the energy gain from flower nectar/pollen consumption
      set flower-density flower-density - 10   ; less energy in the plant
      ;show (word "flower-density: " flower-density " Is Flowering color " pcolor )
    ]
  ;]
end


; pollinators die after running out of fuel=energy
;
to death
    if ( energy < 0 ) [ die ]
end

to count-visits
    set number-of-visits number-of-visits + 1
    ;;
    ;; File recording visits
    ;;
    ;file-print (word species ";" pcolor )
    ;;print (word species ";" pcolor )
end
@#$#@#$#@
GRAPHICS-WINDOW
226
21
634
430
-1
-1
4.0
1
10
1
1
1
0
1
1
1
0
99
0
99
0
0
1
ticks
30.0

BUTTON
668
22
837
55
setup-landscape_2
setup-landscape_2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
9
60
191
93
number-of-regions
number-of-regions
1
20
4.0
1
1
NIL
HORIZONTAL

BUTTON
8
19
191
52
NIL
setup-landscape_1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
667
99
839
132
seed-percent
seed-percent
0.001
0.02
0.003
0.001
1
NIL
HORIZONTAL

SLIDER
667
60
839
93
land-cover-classes
land-cover-classes
1
12
4.0
1
1
NIL
HORIZONTAL

BUTTON
229
443
353
476
NIL
setup-pollinators
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
365
443
537
476
number-of-pollinators
number-of-pollinators
1
30
5.0
1
1
NIL
HORIZONTAL

CHOOSER
8
105
189
150
landscape_1_scenarios
landscape_1_scenarios
"fragmented-segregated" "habitat-structured" "heterogenously-mixed"
2

CHOOSER
667
144
841
189
landscape_2_scenarios
landscape_2_scenarios
"fragmented" "intermediate-complexity" "homogenous"
1

BUTTON
229
488
354
521
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
894
20
1030
53
NIL
setup-landscape_3
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
