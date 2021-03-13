;plant-pollinator interaction simulation by Susanne Kortsch
;last edited 09.03.2021

globals [ plant-number ]

breed [ pollinators pollinator ]
;breed [flowers flower] ;consider making flowers a breed

;variables characterising the pollinators
pollinators-own [
  species
  energy
  mean-step-length ;
  euse
  real_x
  real_y
]

;patches are plants
;patch variables
patches-own [
  plant-density
  patch.visited? ; true or false statement to record visits to colored patches i.e. plants
  number-of-visits
  is-flowering
]

to setup
  clear-all
  setup-patches
  setup-pollinators
  reset-ticks

  if (file-exists? "output.txt") ;delete previous file
  [
    carefully
    [file-delete "output.txt"]
    [print error-message]
  ]

  file-open "output.txt"
  file-type "species.id;"
  file-print "flower.color;"

end

; set up the landscape
; heterogenous lamndscape --> the four plants are mixed and randomly distributed
; homogenous landscape --> the four plants are seggregtaed but randomly dispersed within their area
to setup-patches

    if landscape = "heterogenous" [

    set plant-number 100 ; ( 61 * 61 ) / 9 ; to make a proportion of the patches a plant

  ask patches [set pcolor white]

    ask n-of plant-number patches [
      ;set plant-density random-float 1
      ;set pcolor scale-color one-of [ green magenta yellow orange ] plant-density 0 1
      set pcolor one-of [ green magenta yellow orange ]
  ]
  ]

  if landscape = "homogenous" [

    ask patches [set pcolor white]

    set plant-number 100 / 4 ;(( 60 * 60 ) / 9) / 4 ; to make a proportion of the patches a plant

    ask n-of plant-number patches with [pxcor < 60 and pxcor > 0 and pycor < 0 and pycor > -60]
    [ ;set plant-density random-float 1
      ;set pcolor scale-color green plant-density 0 1]
      set pcolor green ]

ask n-of plant-number patches with [pxcor < 0 and pxcor  > -60 and pycor < 60 and pycor > 0]
    [ ;set plant-density random-float 1
      ;set pcolor scale-color magenta plant-density 0 1]
      set pcolor magenta ]

ask n-of plant-number patches with [pxcor < 0 and pxcor  > -60 and pycor < 0 and pycor > -60]
    [  ;set plant-density random-float 1
       ;set pcolor scale-color yellow plant-density 0 1]
      set pcolor yellow ]

 ask n-of plant-number patches with [pxcor < 60 and pxcor  > 0 and pycor < 60 and pycor > 0]
    [  ;set plant-density random-float 1
       ;set pcolor scale-color orange plant-density 0 1]
      set pcolor orange ]
  ]

end

; sets up three pollinator species: bumblebee, bee, hoverfly
to setup-pollinators
 set-default-shape pollinators "bug" ; tried "bee 2" shape but does not work, why?

  create-pollinators 10 [
    set species "bumblebee"
    set size 4
    set color violet
    set energy 300
    set mean-step-length 2
    set euse 0
    setxy random-pxcor random-pycor
  ]

  create-pollinators 10 [
    set species "bee"
    set size 3
    set color red
    set energy 200
    set mean-step-length 2
    set euse 0
    setxy random-pxcor random-pycor
  ]

  create-pollinators 10 [
    set species "hoverfly"
     set size 3
   set color black
  set energy 150
  set mean-step-length 2
    set euse 0
    setxy random-pxcor random-pycor
  ]
end

 to go
  move-pollinators
  death
  patch-flowering

  tick
end

to patch-flowering
  ask patches [
     (ifelse
      pcolor = magenta [
        seasonal-probabilistc-flowering
      ]
      pcolor = yellow [
        seasonal-probabilistc-flowering
        ]
     )
  ]

end


to seasonal-probabilistc-flowering

  let dayofyear ticks mod 365
  ;let alpha     mean-day * mean-day / variance-pol
  ;let lambda    1 / (variance / mean)

  ; Amplitud ( 1 + cos ( tiempo 2 180 / periodo ) )
  ;report (0.5 * (1 + cos ( ticks * 2 * 180 / periodo-ambiente ) ))

  ;; gamma-distribution

end


; empirically derived turning angles of pollinators (for bumblebee and bees)
; hoverflies move randomly
to move-pollinators
  ;file-open "output.txt"

  ask pollinators with [species = "bumblebee"]  [
    move-bumblebee
    eat
    count-visits
  ]

  ask pollinators with [species = "bee"]  [

   let turn 0
   let randBar random 214 + 1 ; turning angles randomly chosen from empirical turning angle histogram with a total of 214 turning angles (derived from Becher et al 2016))
   if randBar >= 1 and randBar < 24 [ set turn 0 + random-float 10 ] ; random-float prints a number at least 0 but less than 19
   if randBar >= 24 and randBar < 36 [ set turn 10 + random-float 10 ]
   if randBar >= 36 and randBar < 45 [ set turn 20 + random-float 10 ]
   if randBar >= 45 and randBar < 58 [ set turn 30 + random-float 10 ]
   if randBar >= 58 and randBar < 73 [ set turn 40 + random-float 10  ]
   if randBar >= 73 and randBar < 81 [ set turn 50 + random-float 10 ]
   if randBar >= 81 and randBar < 86 [ set turn 60 + random-float 10 ]
   if randBar >= 86 and randBar < 97 [ set turn 70 + random-float 10 ]
   if randBar >= 97 and randBar < 107 [ set turn 80 + random-float 10 ]
   if randBar >= 107 and randBar < 112 [ set turn 90 + random-float 10 ]
   if randBar >= 112 and randBar < 124 [ set turn 100 + random-float 10 ]
   if randBar >= 124 and randBar < 133 [ set turn 110 + random-float 10 ]
   if randBar >= 133 and randBar < 143 [ set turn 120 + random-float 10 ]
   if randBar >= 143 and randBar < 154 [ set turn 130 + random-float 10 ]
   if randBar >= 154 and randBar < 166 [ set turn 140 + random-float 10 ]
   if randBar >= 166 and randBar < 178 [ set turn 150 + random-float 10 ]
   if randBar >= 178 and randBar < 192 [ set turn 160 + random-float 10 ]
   if randBar >= 192 and randBar <= 214 [ set turn 170 + random-float 10 ]

 if random-float 1 > 0.5 [ set turn turn * -1 ]
 rt turn ;this applies correlated direction instead of fully random degree angle, based on empirical data for turn angles
 ;rt random-normal 0 stdev-angle ; this option would activate the slider for turn angle, and apply correlated direction

      let step-length abs random-normal 0 mean-step-length
      set real_x real_x + (dx * step-length)
      set real_y real_y + (dy * step-length)
      set euse step-length
      fd step-length
      set energy (energy - euse)

    eat

      if pcolor != white [ set patch.visited? TRUE ]

  file-type (word species ";")
  file-print (word pcolor ";")
  ]

;hoverlfies move randomly in "exponentially distributed step length"
     ask pollinators with [species = "hoverfly"]  [
    set heading random-float 360
      let step-length random-exponential mean-step-length ; rate ;
      set real_x real_x + (dx * step-length)
      set real_y real_y + (dy * step-length)
     set euse step-length
      fd step-length
    set energy (energy - euse)

    eat

    if pcolor != white [ set patch.visited? TRUE ]

 File-type (word species ";")
 file-print (word pcolor ";")
    ]

 ;file-close
end

to count-visits
    if pcolor != white [
      set patch.visited? TRUE
      set number-of-visits number-of-visits + 1
      ;;
      ;; File recording visits
      ;;
      file-type (word species ";")
      file-print (word pcolor ";")
  ]
end


;; Pollinators procedure
to move-bumblebee
    let turn 0
    let randBar random 628 + 1 ; randomly chosen bar from the empirical turning angle histogram with a total of 628 turning angles (returns random number between [1..628]
   ; 18 bars, each 10 degrees, assuming symmetry of turning right and left  (derived from harmonic radar dataset Emma Wright (PhD thesis))

    if randBar >= 1 and randBar < 92 [ set turn 0 + random-float 10 ]
    if randBar >= 92 and randBar < 144 [ set turn 10 + random-float 10 ]
    if randBar >= 144 and randBar < 206 [ set turn 20 + random-float 10 ]
    if randBar >= 206 and randBar < 262 [ set turn 30 + random-float 10 ]
    if randBar >= 262 and randBar < 302 [ set turn 40 + random-float 10 ]
    if randBar >= 302 and randBar < 343 [ set turn 50 + random-float 10 ]
    if randBar >= 343 and randBar < 363 [ set turn 60 + random-float 10 ]
    if randBar >= 363 and randBar < 396 [ set turn 70 + random-float 10 ]
    if randBar >= 396 and randBar < 419 [ set turn 80 + random-float 10 ]
    if randBar >= 419 and randBar < 440 [ set turn 90 + random-float 10 ]
    if randBar >= 440 and randBar < 456 [ set turn 100 + random-float 10 ]
    if randBar >= 456 and randBar < 483 [ set turn 110 + random-float 10 ]
    if randBar >= 483 and randBar < 506 [ set turn 120 + random-float 10 ]
    if randBar >= 506 and randBar < 526 [ set turn 130 + random-float 10 ]
    if randBar >= 526 and randBar < 542 [ set turn 140 + random-float 10 ]
    if randBar >= 542 and randBar < 570 [ set turn 150 + random-float 10 ]
    if randBar >= 570 and randBar < 599 [ set turn 160 + random-float 10 ]
    if randBar >= 599 and randBar <= 628 [ set turn 170 + random-float 10 ]

    if random-float 1 > 0.5 [ set turn turn * -1 ]
    rt turn ;this applies correlated direction instead of fully random degree angle, based on empirical data for turn angles
            ;rt random-normal 0 stdev-angle ; this option would activate the slider for turn angle, and apply correlated direction

    let step-length abs random-normal 0 mean-step-length
    set real_x real_x + (dx * step-length)
    set real_y real_y + (dy * step-length)
    set euse step-length
    fd step-length
    set energy (energy - euse)

end

;; sheep procedure, sheep eat grass
to eat
  ;; check to make sure there is grass here
    set energy energy + 1 ; adds the energy gain from flower nectar/pollen consumption
end


; pollinators die after running out of fuel=energy
to death     ; the bees die if they run out of energy
  ask turtles [
    if ( energy < 0 ) [ die ]
      ]
end

;; update the plots in the interface tab
to my-update-plots
  plot count pollinators
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
823
624
-1
-1
5.0
1
10
1
1
1
0
1
1
1
-60
60
-60
60
1
1
1
ticks
30.0

BUTTON
93
14
156
47
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
20
14
83
47
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
19
62
157
107
landscape
landscape
"heterogenous" "homogenous"
0

MONITOR
25
143
143
188
Total flower visited
count patches with [patch.visited? = TRUE]
17
1
11

PLOT
899
43
1099
193
Pollinator count
Time
Number
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

SLIDER
18
261
193
294
periodo-ambient
periodo-ambient
0
180
180.0
1
1
NIL
HORIZONTAL

PLOT
892
262
1092
412
plot 1
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (0.5 * (1 + cos ( ticks *  180 / periodo-ambient ) ))"

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
NetLogo 6.1.1
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
