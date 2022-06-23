extensions [ csv palette vid profiler ]

globals [
  region-boundaries   ; a list of regions definitions, where each region is a list of its min pxcor and max pxcor
  habitat-proportions ; a list of habitat proportions produced by the landscape generators
  day                 ; day of the simulation
  pollinators_number_list     ; list with number of pollinators to setup

]

breed [ pollinators pollinator ]
breed [observers observer]    ; virtual observer

;; patches have 1 species of plants
;; patch variables have underscores
;;
patches-own[
  number_of_visits
  habitat
  plant_species
  flower_density       ;; More density more attractive to pollinatiors a proxy of energy available
  flower_max           ;; Parameters to replenish the flower_density each day with a random number between flower_min and flower_max
  flower_min           ;;
  pollinators_weigth   ;; auxiliar variable to determine pollinators preferences: flower_density * niche_preferences,
]

;;
;; pollinators variables have underscores

pollinators-own [
  species
  eusocial               ; is a eusocial species?
  nest_habitat           ;
  nest                   ; the nest patch
  flight_speed
  stdev_angle           ; Correlated random walk
  niche_list            ; plants that the pollinators pollinates
  niche_preferences     ; Preferences, list of probabilities must sum 1
  max_distance          ; Max distance eusocial pollinators forage before returning to nest
  perception_angle
  perception_range

  body_mass
  foraging_distance
  adaptative_step       ; distance travelled
  found_plant           ; found a plant to pollinate
  on_nest               ; pollinator is on nest
]


to setup
  clear-all
  set-default-shape pollinators "bee"

  (ifelse

    landscape_type = "Regular"        [ setup-landscape_1 ];divided into 16 or 25 areas ]
    landscape_type = "Random natural" [ setup-landscape_2 ]
    landscape_type = "Image"          [ setup-landscape_3 ]
  )
  setup-plants
  setup-pollinators
  set habitat-proportions calculate-habitat-area

  if generate-output-file [

    let hstime remove-item 2 ( substring date-and-time 0 5 )

    ;let file-name (word "Simulations/Visits_" substring date-and-time 16 27 "_" hstime "_run_" behaviorspace-run-number ".csv")
    ;
    ; To use with nlrx
    ;
    let file-name (word "Simulations/Visits_" substring date-and-time 16 27 "_" hstime "_run_" nlrx-experiment ".csv")

    file-open "Simulations/Run_habitat_parameters.csv"

    ;file-print (word   "filename; land-cover-classes; seed-percent; habitat_proportions; Mean-free-habitat-path")
    file-print (word   file-name ";" landscape_type ";"  land-cover-classes ";" seed-percent ";" habitat-proportions ";" calculate-plants-by-habitat ";" calculate-mean-free-path ";" calculate-distance-plants)
    file-close


    file-open file-name
    print (word file-name " - habitat proportions: " habitat-proportions )
    file-print (word   "run; day; pollinator_agent; pollinator_species; plant_patch; plant_species; flower_density; habitat; foraging_distance")
  ]
  if video [
     vid:reset-recorder
     vid:start-recorder
  ;        ; vid:record-interface
     vid:record-view
  ]
  reset-ticks
end

;;
;; Generating a regular landscape of squares
;;
to setup-landscape_1

  ;resize-world 0 99 0 99
  ;set-patch-size 4

  let number-of-regions int sqrt land-cover-classes

  set region-boundaries calculate-region-boundaries number-of-regions
  ;print (word "Region boundaries " region-boundaries)

  let region-numbers (range 1 (number-of-regions + 1))
  (foreach region-boundaries region-numbers [ [boundariesx region-numberx] ->
    (foreach region-boundaries region-numbers [ [boundariesy region-numbery] ->
      ask patches with [ pxcor >= first boundariesx and pxcor <= last boundariesx and pycor >= first boundariesy and pycor <= last boundariesy] [

        ;print (word "region number " region-number " boundaries " boundaries)
        set habitat region-numberx +  ( region-numbery - 1 ) * number-of-regions

        ;set pcolor palette:scale-gradient [[255 255 0] [0 0 255]] habitat 1 (number-of-regions * number-of-regions)
        set pcolor 3 + 10 * habitat

      ]
    ])
  ])

end


;;
;; Auxiliar routine for setup_landscape_1
;;
to-report calculate-region-boundaries [ num-regions ]
  ; The region definitions are built from the region divisions:
  let divisions region-divisions num-regions
  ; Each region definition lists the min-pxcor and max-pxcor of the region.
  ; To get those, we use `map` on two "shifted" copies of the division list,
  ; which allow us to scan through all pairs of dividers
  ; and built our list of definitions from those pairs:
  report (map [ [d1 d2] -> list (d1 ) (d2 ) ] (but-last divisions) (but-first divisions))
end

;;
;; Auxiliar routine for setup_landscape_1
;;
to-report region-divisions [ num-regions ]
  ; This procedure reports a list of pxcor that should be outside every region.
  ; Patches with these pxcor will act as "dividers" between regions.
  report n-values (num-regions + 1) [ n ->
    [ pxcor ] of patch (min-pxcor + (n  * ((max-pxcor - min-pxcor) / num-regions))) 0
  ]

end

;;
;; Random natural
;;
to setup-landscape_2

  let dim world-width     ; assumes sizes mutilples of 10
  if dim mod 10 != 0 [
    print "ERROR: Assumes world-width multiples of 10"
    stop
  ]

  let ext dim ^ 2
  ;resize-world 0 (dim - 1) 0 (dim - 1)
  ;set-patch-size 4
  ask patches [
     set habitat nobody
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
      ask one-of (patches with [habitat = nobody])[
        set habitat i
        set pcolor 3 + 10 * i
      ]
    ]
  ]

;; Fill the voids by propagation
;; Credit: Uri Wilensky, Patch habitat Example
  while [any? patches with [habitat = nobody]] [
    ask patches with [habitat = nobody][
      let c [habitat] of one-of neighbors4
      if c != nobody [
        set habitat c
        set pcolor 3 + 10 * c
      ]
    ]
  ]

end




;;; to setup real world landscape, here we could also have three examples from Nepali landscapes
; I will ask Tom Timberlake to make three gis renderings for us with color-coded habitats where we can add the plants and flowers
to setup-landscape_3
 ;import-drawing "Chickwell Farm.png"
 import-pcolors "Chickwell Farm.png"
end

;
; procedure to read pollinators parameters from a file
;
to setup-pollinators
  file-close-all ; close all open files
  if not file-exists? pol-parameters-file-name [
    user-message (word "No file " pol-parameters-file-name " exists!")
    stop
  ]
  file-open pol-parameters-file-name; open the file with the turtle data

  ;; To skip the header row in the while loop,
  ;  read the header row here to move the cursor down to the next line.
  let headings csv:from-row file-read-line

  set pollinators_number_list [] ; Initialize list of pollinators' numbers

  ; We'll read all the data in a single loop
  while [ not file-at-end? ] [

    ; here the CSV extension grabs a single line and puts the read data in a list
    let pollinator_data csv:from-row file-read-line

    ;print pollinator_data

    ; we add number-of-pollinators for each pollinator in the file
    ;
    create-pollinators 1 [
      setxy random-pxcor random-pycor
      set species        item 0 pollinator_data
      set pollinators_number_list lput item 1 pollinator_data pollinators_number_list
      set eusocial       item 2 pollinator_data         ; 0= None, 1=solitary, 2=full
      set flight_speed   item 3 pollinator_data         ; if body_mass > 0 then it is calcualted from Liam's model
      set stdev_angle    item 4 pollinator_data

      let niche_str      item 5 pollinator_data         ; Niche = which plants the pollinator can pollinate - represent traits like open/closed flowers
      set niche_list read-from-string niche_str
      let niche_str1      item 6 pollinator_data
      set niche_preferences read-from-string niche_str1
      if length niche_list != length niche_preferences [
        print (word "ERROR: Pollinator sp " species " doesn't have the same number of niche items and preferences")
        show (word "niche_list: " niche_list " niche_preferences: " niche_preferences)
        stop
      ]
      ;show (word "niche_list: " niche_list " niche_preferences: " niche_preferences)

      set nest_habitat     item 7 pollinator_data     ; The habitat where the nest is
      set max_distance     item 8 pollinator_data           ; max distance a pollinator flies before return to nest, if body_mass >0 set from Liam's model
      ;set energy energy_by_distance * 100                   ; initial amount of energy

      set perception_range item 9 pollinator_data
      set perception_angle item 10 pollinator_data
                                                            ; Should add memory_extinction 1/minutes-per-day = 1 Day
                                                            ; last_found_patch to signal the last plant they found and to communicate
                                                            ; to other pollinators in nest.

      set body_mass      item 11 pollinator_data             ; Not needed unless we parametrize from body_mass
      set size           item 12 pollinator_data
      set color          item 13 pollinator_data
      set adaptative_step 0
      set found_plant     false

      body-mass-dependent-distance                       ; setup parameters when body_mass > 0
    ]
  ]

  file-close ; make sure to close the file

  ask pollinators [

    hatch (item (species - 1) pollinators_number_list) - 1
  ]

  eusociality-setup

  print word "pollinators_number_list: " pollinators_number_list

end

to body-mass-dependent-distance
  if body_mass > 0 [                                  ; Parametrize flight_speed and max_distance using Liam's model
    (ifelse eusocial = 3                                ; highly eusocial
      [
        set flight_speed ( exp (5.34 + body_mass * 0.3) * 10 ) / minutes-per-day
        set max_distance   exp (5.34 + body_mass * 0.3)
      ]
      eusocial = 2                                      ; primitively eusocial
      [
        set flight_speed ( exp (5.34 - 1.12 + body_mass *  0.3 ) * 10 ) / minutes-per-day
        set max_distance ( exp (5.34 - 1.12 + body_mass *  0.3 ) )
      ]
      eusocial = 1                                      ; solitary with nest
      [
        set flight_speed ( exp (5.34 - 1.13 + body_mass * 0.3 ) * 10 ) / minutes-per-day
        set max_distance ( exp (5.34 - 1.13 + body_mass * 0.3 ) )
      ]
      eusocial = 0                                      ; solitary no nest
      [
        set flight_speed ( exp (5.34 - 1.13 + body_mass * 0.3 ) * 10 ) / minutes-per-day
        set max_distance ( exp (5.34 - 1.13 + body_mass * 0.3 ) )
      ]
    )
    set flight_speed precision flight_speed 1
    set max_distance precision max_distance 1
    ;show (word "species: " species " eusocial: " eusocial " body_mass: " body_mass " fligth_speed: "  flight_speed " max_distance: " max_distance)
  ]
end
;;
;; Set nest sites for eusocial pollinators
;;
to eusociality-setup
  let max-species max [species] of pollinators
  let sp-list (range 1 ( max-species + 1))
  foreach sp-list [ sp ->
    let sp-pollinator one-of pollinators with [sp = species and eusocial > 0 ]
    if sp-pollinator != nobody  [
      let eusocial-sp [eusocial] of sp-pollinator
      ifelse eusocial-sp = 2 or eusocial-sp = 3 [
        let ne-habitat [nest_habitat] of sp-pollinator
        let nest-patch one-of patches with [ habitat = ne-habitat ]
        if nest-patch = nobody  [
          print "Habitat of an eusociality pollinator must be valid !!!!!!!!!!!!!"
          stop
        ]

        ;print (word "Pollinator: " sp-pollinator " Habitat: " ne-habitat " Nest patch: " nest-patch)
        let eu-pollinators pollinators with [sp = species and (eusocial = 2 or eusocial = 3 ) ]
        if eu-pollinators != nobody  [
          ask eu-pollinators [
            set nest nest-patch
            move-to nest
          ]
        ]
      ][ ;; eusocial = 1 solitary species
        let eu-pollinators pollinators with [sp = species and eusocial = 1 ]

        if any? eu-pollinators  [
          ask eu-pollinators [
            let ne-habitat [nest_habitat] of sp-pollinator
            let nest-patch one-of patches with [ habitat = ne-habitat ]
            if nest-patch = nobody  [
              print "Habitat of an eusociality pollinator must be valid !!!!!!!!!!!!!"
              stop
            ]

            set nest nest-patch
            move-to nest
            ;show (word "Pollinator: " sp-pollinator " Habitat: " ne-habitat " Nest patch: " nest-patch)

          ]
        ]
      ]
    ]
  ]

end

to setup-plants

  file-close-all ; close all open files
  if not file-exists? plant-parameters-file-name [

    user-message (word "No file " plant-parameters-file-name  " exists!")
    stop
  ]

  file-open plant-parameters-file-name ; open the file with the turtle data

  ;; To skip the header row
  ;read the header row here to move the cursor down to the next line.
  let headings csv:from-row file-read-line

  while [ not file-at-end? ] [

    ; here the CSV extension grabs a single line and puts the read data in a list
    let plant_data csv:from-row file-read-line
    ; print (word "plant_data: " plant_data )
    let plant_sp item 0 plant_data
    let habitat_str item 1 plant_data
    let density_str item 2 plant_data
    let sp_flower_min item 3 plant_data
    let sp_flower_max item 4 plant_data

    let habitat_list read-from-string habitat_str
    let density_list read-from-string density_str

    let i 0
    ; for each habitat set the plants
    foreach habitat_list [ h ->
      ;
      let density item i density_list
      let npatches count patches with [habitat = h]
      ;print (word "Habitat: " h " Total patches: " npatches )
      set npatches npatches * density
      ;print (word "Species: " plant_sp " patches: " npatches )

      set i i + 1
      ask n-of npatches patches with [habitat = h and plant_species = 0] [

        set plant_species plant_sp
        set flower_max sp_flower_max
        set flower_min sp_flower_min
        ;show (word "Plant species: " plant_sp " habitat: " h " Plant density: " density " Flower_density: " flower_density)
        set pcolor palette:scale-gradient palette:scheme-colors "Divergent" "RdYlGn" 9 plant_sp 0 10   ;; Color assumes 10 plant species
      ]
    ]
    ; setup plants in the landscape
  ]
end

to replenish-flowers
  ;print "Replenish-flowers"
  ask patches with [ plant_species != 0 ][

    set flower_density random ( flower_max - flower_min + 1 ) + flower_min
    ;show (word "Plant species: " plant_species " habitat: " habitat " Flower_density: " flower_density " pcolor:" pcolor )
    paint-flower-density    ;show (word "After pcolor:" pcolor )


  ]
end

to paint-flower-density
    set pcolor palette:scale-gradient palette:scheme-colors "Divergent" "RdYlGn" 9 plant_species 0 10   ;; Color assumes max 10 plant species

    set pcolor map [ i -> flower_density / flower_max * i  ] pcolor
end

to go

  set day ( ticks / minutes-per-day )
  if ( int day ) = day [                              ; replenish-flowers first time and then at the end of the day
    replenish-flowers
    return-all-pollinators
  ]

  if not any? pollinators or day = number-of-days [               ; minutes-per-day ticks per day
    file-close
    if video [
        vid:save-recording "pollinators02.mp4"
    ]

    stop
  ]

  ask pollinators [

    move-pollinators
    eat
    death
  ]
  ;ask patches with [ plant_species != 0 ]
  ;[
  ;  paint-flower-density
    ;patch-flowering
  ;]


  tick
  if video [
        ;vid:record-interface
     vid:record-view
  ]

end

to return-all-pollinators
  ask pollinators [
    ifelse eusocial > 0 [
      ;show (word "End of day on_nest: " on_nest " Nest: " nest )

      ifelse on_nest = 0 [
        set foraging_distance 0
        move-to nest
      ][
        set on_nest 0                                              ; after the night they don't wait and start pollination
      ]
    ][
      set foraging_distance 0
    ]

  ]

end

;;
;; Pollinators detect the plant with max flowers then if the plant is not in their niche they keep their random walk
;; if the plant is in their niche they face the plant and move to the patch
;;
to move-pollinators
  ifelse on_nest > 0 [                                              ; Resting on the nest the amount of time if they should flight foraging_distance back
    set  on_nest on_nest - 1
    set foraging_distance 0
    ;show (word "Decrement on_nest: " on_nest)
  ]
  [
    ifelse found_plant or not active-search [
      ;show (word "Found plant in previous tick:  " adaptative_step  )
      ifelse eusocial > 0 [
        ifelse foraging_distance > max_distance                     ; After max distance they go nest and stay there until foraging_distance/ flight_speed steps
        [
          move-to nest
          set on_nest int ( foraging_distance / flight_speed )
          ;show (word "Foraging distance: " foraging_distance " Max distance: " max_distance " on_nest: " on_nest " nest: " nest )

        ][
          correlated-random-walk
        ]
      ][
        correlated-random-walk
      ]
      set found_plant false
    ][
      ;
      ; Detect the max flower_density of plants in the niche
      ;
      let highest-list map [x  ->  max-one-of ( patches with [plant_species = x ] in-cone perception_range perception_angle )[flower_density] ] niche_list
      ;print (word "highest-list: "  highest-list )

      ;
      ; Then weigth by preferences
      ;
      let highest-patch  patch-set highest-list
      ifelse any? highest-patch [
        ;print (word "Niche_preferences: " Niche_preferences )
        let np niche_preferences
        let nl niche_list
        ask highest-patch  [
          let pos_np position plant_species nl
          let pw item pos_np np
          set pollinators_weigth pw * flower_density
          ;show (word "Niche preference: " pw " pollinators_weigth: " pollinators_weigth " flower_density: " flower_density)
        ]
        let higher-patch max-one-of highest-patch [pollinators_weigth ]
        ;print (word "Higher patch: " [pollinators_weigth ] of higher-patch )

        face higher-patch
        set adaptative_step distance higher-patch
        move-to higher-patch
        set found_plant true

        ;show (word "Found plant move to higher patch " higher-patch )
      ][
        correlated-random-walk
        ;show "Not found plant correlated random walk"
      ]
      ; @Benadi2018 If no flowers are within the pollinator’s perception range, it moves in a correlated random walk
      ; (with turning angles drawn from a normal distribution with mean 0 and standard deviation j) until it perceives at least one flower.
    ]

    ;
    ; energy lost by movement
    ;
    ;let euse adaptative_step * energy_by_distance
    ;set energy (energy - euse)

    set foraging_distance  foraging_distance + adaptative_step
    show (word adaptative_step " - " foraging_distance )
  ]
end

to correlated-random-walk
  rt random-normal 0 stdev_angle                          ; correlated random walk
  set adaptative_step random-poisson flight_speed         ; Instead of min max distances set the speed

  fd adaptative_step
  ;show (word "Correlated rnd walk poisson step " adaptative_step )

end


; pollinator's procedure
; The pollinators only take the pollen of plants inside their niche
;
to eat
  ;if is-flowering = TRUE [
  ;let total_flowers 0
  ;let p_niche niche_list
  ;print (word "Plant sp: " species " niche_list: " p_niche "Flower_density: " flower_density)
  if member? plant_species niche_list [
      if flower_density > 0 [
      count-visits
      ;set energy energy + flower_density * energy_by_distance * 10
      ;if who = 0 [
      ;  show (word "From patch: " patch-here " flower_density: " flower_density "foraging_distance : " foraging_distance " f_plant: " found_plant " a_step: " adaptative_step )
      ;]
      set flower_density flower_density - 1
      set found_plant true

    ]
  ]
  ;show (word "Total_flowers: " total_flowers " Is habitat " habitat )

  ;]
end


; pollinators die after running out of fuel=energy
;
to death
;    if ( energy < 0 ) [ die ]
end

to count-visits
  set number_of_visits number_of_visits + 1
  ;;
  ;; File recording visits
  ;;
  if generate-output-file [

    file-print (word   behaviorspace-run-number ";" precision day 4 ";" who ";" species ";" patch-here ";" plant_species ";" flower_density ";" habitat ";"
      precision foraging_distance 3
    )
  ]
end

;
; Calculate the proportion of each habitat
;
to-report calculate-habitat-area
  let habitats-numbers (range 1 (land-cover-classes + 1 ) )
  report map [

    h ->  precision ( count patches with [ habitat = h ]  / count patches ) 3

  ] habitats-numbers
end

; Calculate the number of plants within each habitat
;
to-report calculate-plants-by-habitat
  let habitats-list (range 1 (land-cover-classes + 1 ) )
  let plantspecies max [plant_species] of patches
  let plant-sp-list (range 1 (plantspecies + 1 ) )
  let number-plants-by-habitat []
  foreach habitats-list [ h ->

    let plants map [

      p ->  ( count patches with [ habitat = h and plant_species = p ]   )

    ] plant-sp-list
    set number-plants-by-habitat lput plants number-plants-by-habitat

    ;print (word "Habitat: " h " Plants " plant-sp-list " count " plants)

  ]
  report number-plants-by-habitat
end


; Calculate the mean free habitat path:  is the mean
; distance from a randomly chosen site of an habitat to
; the closest diferent habitat. It uses a maximum of 100 random places inside the habitat.
;
to-report calculate-mean-free-path
  let habitats (range 1 (land-cover-classes + 1 ) )
  let no_hab map [

    h ->  count patches with [ habitat = h ] * 0.1         ;  it uses the 20% of the total sites to calculate the mean
  ] habitats
  let mfp-habitat []
  ;print (word "Habitat patches: " no_hab)
  foreach habitats [

    h ->
    let list-mfp []
    let nh item (h - 1)  no_hab
    if nh > 100 [ set nh 100 ]
    ask n-of nh  patches with [ habitat = h ] [
      let i 0
      let p-in-r no-patches
      while [not any? p-in-r] [
        set i i + 1
        set p-in-r patches with [habitat != h ] in-radius i
        ;print (word "p-in-r" p-in-r " i: " i)

      ]
      ;show (word "Habitat: " h " radius: " i )
      set list-mfp lput i list-mfp

    ]
    ;print (word "Habitat: " h " Mfp: " (mean list-mfp))
    set mfp-habitat lput precision mean list-mfp 3 mfp-habitat
  ]
  report mfp-habitat
end



to-report calculate-distance-plants

  let plant-species max [plant_species] of patches
  let plant-sp-list (range 1 (plant-species + 1 ) )
  ;let plant-sp-list (range 1 (1 + 1 ) )

  let list-distance []
  let sp-mean-distance []
  foreach plant-sp-list [

    sp ->

    ask patches with [plant_species = sp ][ sprout-observers 1 [set size 2 set color black] ]
    ;display
    ask observers [
        let near-obs  min-one-of other observers [ distance myself ]
        ;show near-obs
        set list-distance lput (distance near-obs) list-distance
    ]
    ;Print (word "Species: " sp " dist: " list-distance)

    set sp-mean-distance lput (mean list-distance ) sp-mean-distance
    ;Print (word "Mean dist: " sp-mean-distance)

    ask observers [ die ]
  ]
  report sp-mean-distance
end
@#$#@#$#@
GRAPHICS-WINDOW
226
21
834
630
-1
-1
3.0
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
199
0
199
1
1
1
ticks
30.0

BUTTON
15
25
198
58
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

SLIDER
875
125
1047
158
seed-percent
seed-percent
0.00001
1
1.0E-5
0.001
1
NIL
HORIZONTAL

SLIDER
875
25
1047
58
land-cover-classes
land-cover-classes
1
12
7.0
1
1
NIL
HORIZONTAL

BUTTON
15
70
105
103
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

CHOOSER
875
70
1045
115
landscape_type
landscape_type
"Regular" "Random natural" "Image"
1

BUTTON
110
70
195
103
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
15
115
195
148
active-search
active-search
0
1
-1000

MONITOR
15
200
155
245
Mean foraging distance
mean [ foraging_distance ]  of pollinators
4
1
11

MONITOR
15
255
72
300
NIL
day
2
1
11

SWITCH
15
155
195
188
generate-output-file
generate-output-file
0
1
-1000

SLIDER
875
215
1045
248
number-of-days
number-of-days
1
20
1.0
1
1
NIL
HORIZONTAL

MONITOR
80
255
170
300
NIL
count pollinators
17
1
11

SWITCH
875
330
978
363
Video
Video
1
1
-1000

BUTTON
15
685
97
718
Profiler
profiler:start         ;; start profiling\nrepeat 2 [ show calculate-mean-free-path ]       ;; run something you want to measure\nprofiler:stop          ;; stop profiling\nprint profiler:report  ;; view the results\nprofiler:reset         ;; clear the data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
15
318
198
378
pol-parameters-file-name
pol_pars_1005905681.csv
1
0
String

INPUTBOX
14
465
198
525
nlrx-experiment
NIL
1
0
String

INPUTBOX
14
391
198
451
plant-parameters-file-name
plant_pars_1005905681.csv
1
0
String

SLIDER
875
170
1047
203
minutes-per-day
minutes-per-day
480
600
480.0
10
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is asscoiated with the paper:

Landscape composition and plant-pollinator network structure interact to influence pollination success in an individual-based model

The model was developed by Leonardo Saravia and Susanne Kortsch 

-----------------------------------------------------------------------------------------

The model description below follows the ODD (Overview, Design concepts, Details) protocol for describing individual- and agent-based models (Grimm et al. 2006, 2010). The model was implemented in NetLogo (Wilensky, 1999), version 6.0.4.

1. Purpose

The main purpose of the model is to understand how the landscape structure influences pollinator visitation rates. By adjusting the arrangements of “habitats” and plants in the landscape, the model serves as a practical tool to mimic the effects of plant intermixing on plant-pollinator interactions. Number of habitats and plants can be adjusted to test different hypotheses. In the Netlogo world, colored grid cells represent different plant types, which are visited by pollinators based on some rules for pollinator movement behavior. Plants with many flowers will be more attractive to pollinators. This variable is also affected by a pollinators’ plant preference, i.e., its diet niche. The pollinators’ visitation rate and plant-pollinator networks will be constructed by recording the pollinators visits to plants. The resulting data that can be analyzed in precisely the same way as conventional field data.

## HOW IT WORKS

2. Entities, state variables and scales

The model has two types of agents, pollinators and plants. Pollinators are the only mobile agents in the simulation, whereas plants are non-mobile. Pollinators and plants are associated with state variables determining their traits and behaviors. Pollinators are divided into species distinguished by a unique set of traits such as body size, eusociality, foraging distance, and niche (i.e., a subset of plants they interact with) and niche preferences. Plants can have varying numbers of flowers, which will determine their attractiveness to pollinators. The more flowers, the more attractive the plant will be to pollinators will be. 
There are seven habitats of which six each contain one plant species. A proportion f of patches contains plant species (with a single plant, but flower density can be varied). The remaining proportion of patches (1-f) are empty. Each patch which contains a plant corresponds to a single plant type indicated by a unique color. The world is set to wrap vertically and horizontally.
The “netlogo world” consists of 1m x 1m grid cells (square patches) and the model landscape comprises 600 x 600 m. 

3. Process overview and scheduling

Time is modelled as discrete steps called ticks in Netlogo. Processes modeled are: 1) movement of pollinators; 2) plants perceived based on attractiveness; 3) visits to plants. 

4. Design concepts

Basic principles 

As currently configured, the model assumes that pollinators can perceive flower density of plants, given that plants are within their niche and perception range. We pre-assigned a fundamental niche to pollinators, i.e., they can interact with a subset of the plant types. To select a plant from its niche, the pollinator checks all plants within its perception range and chooses the one with the most flowers (Benadi and Gaeger 2018), then it moves in a straight line to the chosen plant at a given flight speed (patches per time step). If more than one plant matches this criterion, the pollinator randomly chooses one among them. If no flowers are within a pollinator’s perception range, it continues to move in a correlated random walk until it perceives at least one plant from its niche. 
Pollinators are associated with different parameters (e.g., flight speed, nest, or no nest) which results in somewhat different movement behaviors, foraging distances and hence in different number of plants visited. Generally, larger-sized pollinators will travel further.

Emergence. — The pollination events emerge from 1) the movement of pollinators 2) flower density, 3) niche and niche preferences of pollinators, and 3) the spatial distribution of plant resources.
 
Adaptation. — Pollinators change their foraging behavior in response to clues from their environment, i.e., plant attractiveness. 
Prediction. — The model does not incorporate any prediction by the agents, i.e., agents do not adjust their behavior to future conditions or consequences of decisions.
Sensing. — The only sensing that occurs is the sensing of plants by pollinators at each time step. 

Interaction - No direct interaction between agents was incorporated in the model, i.e., agents do not affect each other.  Pollinators may affect each other indirectly (i.e., compete) as plants visited will lose one flower unit and become less attractive to other pollinators.

Stochasticity. — Movement of pollinators follows a correlated random walk with turning angles drawn from a normal distribution with mean (0) and standard deviation (90). The flight speed is not constant but randomly sampled from a Poisson distribution with mean FS at every time step for every pollinator individual. FS are defined based on allometric body size- foraging distance scaling relationships (Kendall et al. accepted). 
Collectives. — Both the pollinator agents and the plants belong to groups with their own set of traits (sociality, plant densities) which results in different model behaviors, and which will give rise to different plant-pollinator encounter rates and hence pollination events.
Observation. — Output data are counts of the number of visits by each pollinator to each plant species. These visits reported are saved in a format (e.g., csv file), which can then be imported and analyzed in a statistical program such as R (R Core Team, 2021). 

Details

Initialisation. — Simulations are conducted with randomized plant and pollinator input files 

Input data. — The model does currently not use any external information sources, but this could be included. 

Submodels. — At each timestep (i.e., tick), the following processes occur:
Move-pollinators: Movement of pollinators, e.g., its foraging range and flight speeds, depends on pollinator perception range and perception angle and other pollinator species’ traits such as its body size, sociality category and plant pollinator preferences. Foraging ranges scale positively with body size according to an allometric function, and also depend on sociality category (Kendall et al. 2022). Per unit body size, foraging ranges are larger an increase at greater rate with sociality. The exact movement details of pollinators are explained thoroughly in the main article text associated with this model.

Count-visits: The Netlogo patches that are classified as plants (i.e., the colored patches) count the number of visits by each pollinator species during each timestep. A visit is recorded when a pollinator is located on top of a grid cell that is classified as a plant. 

Eat: Agents remove one flower unit after each visit but do not gain energy from visiting a plant. NB! This procedure is currently very simple and could be extended in future work by adding sensing of plants (and their rewards, nectar quantity and quality) by pollinators.

Death: As our simulations runs only for one day, pollinators do not die.  

Outputs: The model is configured such that when an individual pollinator visits an individual flower with a given color, the visit is recorded; model output includes the visits made by each pollinator species to each flower species at each tick. An output file (e.g., Visits_10) with all pairwise interactions is saved to a csv file. The resulting plant-pollinator interactions and bipartite networks can be analyses in R or similar statistical programs.
Furthermore, a csv file with habitat characteristics (size of habitats, number of grid cells with plants in a habitat etc.) is generated.



## HOW TO USE IT


The model needs plant and pollinator input files specifying the parameters of the model.

Input file names are:

plant_pars_replicate_n
pol_pars_replicate_n

Inside the model the replictae number of the input files are specfied, e.g., 10.
In order for the model to read the files, this needs to be reflected in the input file names, e.g.:
 
plant_pars_10
pol_pars_10


Setup:
Sets up the landscape

Go:
Starts the simulation

There are three sliders in the model that can be used to change the landscape:
-land-cover-class, determines number of "habitats"
-seed-percent, determines how intermixed the habitats=plants are
-number-of-days, determines how many days the simulation runs. One day contains 600 ticks

## THINGS TO NOTICE

NOTE! The model setup is very slow. The model landscape size may be decreased to 299*299 grid cells for faster start up under settings in the interface.

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

The model could be extended by inlcuding some population dynamics

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

The landscape diversity model by Wirth et al. (2016) which was used for this study.
http://ccl.northwestern.edu/netlogo/models/community/Landscape%20Diversity

## CREDITS AND REFERENCES

Benadi, G. & Gaeger, R. J. 2018. Adaptive Foraging of Pollinators Can Promote Pollination of a Rare Plant Species. The American Naturalist 192, E81–E92. 

Grimm V, Berger U, Bastiansen F, Eliassen S, Ginot V, Giske J, Goss-Custard J, Grand T, et al. (2006) A standard protocol for describing individual-based and agent-based models. Ecological Modelling 198, 115–126.

Grimm V, Berger U, DeAngelis DL, Polhill JG, Giske J, Railsback SF (2010) The ODD protocol: a review and first update. Ecological Modelling 221, 2760-2768.

Kendall, L., Mola, J., Portman, Z., Cariveau D., Smith, H. & Bartomeus, I. 2022. The potential and realized foraging movements of bees are differentially determined by body size and sociality. Accepted in Ecology, to be published soon.

R Core Team. 2021. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Wilensky U. NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL. 1999.

Wirth, E., Szabó, Gy., and Czinkóczky. 2016 A.: MEASURE OF LANDSCAPE HETEROGENEITY BY AGENT-BASED METHODOLOGY, ISPRS Ann. Photogramm. Remote Sens. Spatial Inf. Sci., III-8, 145-151, http://dx.doi.org/10.5194/isprs-annals-III-8-145-2016
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

bee
true
0
Polygon -1184463 true false 152 149 77 163 67 195 67 211 74 234 85 252 100 264 116 276 134 286 151 300 167 285 182 278 206 260 220 242 226 218 226 195 222 166
Polygon -16777216 true false 150 149 128 151 114 151 98 145 80 122 80 103 81 83 95 67 117 58 141 54 151 53 177 55 195 66 207 82 211 94 211 116 204 139 189 149 171 152
Polygon -7500403 true true 151 54 119 59 96 60 81 50 78 39 87 25 103 18 115 23 121 13 150 1 180 14 189 23 197 17 210 19 222 30 222 44 212 57 192 58
Polygon -16777216 true false 70 185 74 171 223 172 224 186
Polygon -16777216 true false 67 211 71 226 224 226 225 211 67 211
Polygon -16777216 true false 91 257 106 269 195 269 211 255
Line -1 false 144 100 70 87
Line -1 false 70 87 45 87
Line -1 false 45 86 26 97
Line -1 false 26 96 22 115
Line -1 false 22 115 25 130
Line -1 false 26 131 37 141
Line -1 false 37 141 55 144
Line -1 false 55 143 143 101
Line -1 false 141 100 227 138
Line -1 false 227 138 241 137
Line -1 false 241 137 249 129
Line -1 false 249 129 254 110
Line -1 false 253 108 248 97
Line -1 false 249 95 235 82
Line -1 false 235 82 144 100

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
<experiments>
  <experiment name="regular3days" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="land-cover-classes">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_type">
      <value value="&quot;Random natural&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-percent">
      <value value="1.0E-4"/>
      <value value="0.01"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-days">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-pollinators">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="active-search">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generate-output-file">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_2_scenarios">
      <value value="&quot;heterogenous&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="random_narural1day" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="land-cover-classes">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_type">
      <value value="&quot;Random natural&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-percent">
      <value value="1.0E-5"/>
      <value value="1.0E-4"/>
      <value value="0.001"/>
      <value value="0.01"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-days">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="active-search">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generate-output-file">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_1day" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="land-cover-classes">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_type">
      <value value="&quot;Random natural&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-percent">
      <value value="1.0E-5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-days">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="active-search">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generate-output-file">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
1
@#$#@#$#@
