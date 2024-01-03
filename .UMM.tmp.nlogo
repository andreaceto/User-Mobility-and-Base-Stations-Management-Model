breed [users user]
breed [bases base]

users-own [
  nearest-bs
  distance-to-nearest-bs
  linked-bs
  destination
  distance-to-dest
  path-to-dest
]
bases-own [
  capacity
  linked-users
]
patches-own [
  weight ;; helps with the Base Stations distribution based on 5G coverage
  cost ;; weight related, fundamental to determine the path users follow to reach their destination
]

globals [
  ;population-density ;; represents the denisty of users in the world-area, used to determine the number of users
  population-scale ;; represents the model:real world ratio
  number-of-user ;; the number of users in the model defined considering population-density and population-scale
  ;show-linked-users? ;; if TRUE the number of linked users for every base station is shown
  ;show-distance-to-nearest-bs? ;; if TRUE the distance to the closest base station for every user is shown
  world-area ;; fixed world-area defined at setup in km²
  patch-side-length ;; defined following world-side-length, used to track user travel distance in metres
  ;show-bs-range? ;; if TRUE, for each Base Station, the signal range is shown
  _4G-signal-range
  _5G-signal-range
  _5G-coverage
  ;target-5G-coverage ;; defined by user to create different scenarios of world generation, Base Station are created and placed accordingly
  potential-bs-spots
  ;show-path-difference? ;; if TRUE, both shortest-path and lowest-cost-path are shown
]

to setup
  clear-all

  setup-world-metrics

  let number-of-users population-density * world-area * population-scale
  setup-users number-of-users

  setup-bases

  setup-patches-cost

  setup-user-bs-links

  compute-paths

  display-labels

  reset-ticks
end

to go

  if not any? users [
    ;user-message(word " All users reached their destination in " ticks " ticks.")
    stop
  ]

  ask users [reach-destination]

  tick
  update-user-bs-links
  update-labels
end

;; initializes model world and related metrics
to setup-world-metrics
  ;; fixed (model : real world) ratio
  set population-scale 1 / 100
  ;; fixed world area in km², variable is defined so that it's easier to change all related metrics
  set world-area 1
  ;; adjustable world side length in #patches
  let world-side-length world-area * 100
  ;; adjustable patch size
  set-patch-size 600 / world-side-length

  resize-world (world-side-length / 2 * -1) (world-side-length / 2 - 1) (world-side-length / 2 * -1) (world-side-length / 2 - 1)

  ;; intialization for real life metrics, since distance is calculated
  ;; in patches from centre to centre, we can use this measure to convert distance in metres
  set patch-side-length sqrt(world-area * 1000000) / world-side-length
end

;; creates users and initialize their variables
to setup-users [num-users]
  create-users num-users [
    set shape "person"
    set size 2
    set color pink
    move-to one-of patches

    set nearest-bs nobody
    set distance-to-nearest-bs -1
    set linked-bs nobody
    set destination nobody
    set distance-to-dest -1
    set path-to-dest []
  ]
  setup-destinations
end

;; generates a random destination for every user while also making sure it's not to close from their starting positions
to setup-destinations
  ;; stores the generated destination patch
  let destination-patch nobody

  ask users [
    ;; makes sure the generated destination it's not to close
    while[distance-to-dest < (5 * patch-side-length)] [
      ;; makes sure the generated destination it's on the borders
      set destination-patch one-of patches with [abs pxcor = max-pxcor or abs pycor = max-pycor]
      set destination destination-patch
      set distance-to-dest round((distance destination-patch) * patch-side-length)
    ]
  ]
end

;; reports wether a potential Base Station spawn spot is within world borders
to-report is-spot-valid [x y]
  (ifelse
    (x < min-pxcor or x > max-pxcor) [ report false ]
    (y < min-pycor or y > max-pycor) [ report false ]
    [ report true ]
  )

end

;; find potential patches to sprout a Base Station
to find-potential-locations [x y]
  ;; this is just for readability
  let var _5G-signal-range

  let xmin x - var
  let xmax x + var
  let ymin y - var
  let ymax y + var

  ;For each base station we need to find these 24 patches, if their coordinates are valid we add them to a list
  ; '+' points are points defined by the square built around the radius of the 5g signal range
  ; '-' points are points defined by the square built around the radius of double the 5g signal range

  ;-  -	  -	  -	  -
  ;
  ;-	+   +   +	  -
  ;
  ;-	+   X   +	  -
  ;
  ;-	+   +   +	  -
  ;
  ;-	-   -   -	  -

  if(is-spot-valid xmin ymin) [set potential-bs-spots lput patch xmin ymin potential-bs-spots]
  if(is-spot-valid xmin y) [set potential-bs-spots lput patch xmin y potential-bs-spots]
  if(is-spot-valid xmin ymax) [set potential-bs-spots lput patch xmin ymax potential-bs-spots]
  if(is-spot-valid x ymax) [set potential-bs-spots lput patch x ymax potential-bs-spots]
  if(is-spot-valid xmax ymax) [set potential-bs-spots lput patch xmax ymax potential-bs-spots]
  if(is-spot-valid xmax y) [set potential-bs-spots lput patch xmax y potential-bs-spots]
  if(is-spot-valid xmax ymin) [set potential-bs-spots lput patch xmax ymin potential-bs-spots]
  if(is-spot-valid x ymin) [set potential-bs-spots lput patch x ymin potential-bs-spots]

  if(is-spot-valid (xmin - var) (ymin - var)) [set potential-bs-spots lput patch (xmin - var) (ymin - var) potential-bs-spots]
  if(is-spot-valid (xmin - var) ymin) [set potential-bs-spots lput patch (xmin - var) ymin potential-bs-spots]
  if(is-spot-valid (xmin - var) y) [set potential-bs-spots lput patch (xmin - var) y potential-bs-spots]
  if(is-spot-valid (xmin - var) ymax) [set potential-bs-spots lput patch (xmin - var) ymax potential-bs-spots]
  if(is-spot-valid (xmin - var) (ymax + var)) [set potential-bs-spots lput patch (xmin - var) (ymax + var) potential-bs-spots]
  if(is-spot-valid xmin (ymax + var)) [set potential-bs-spots lput patch xmin (ymax + var) potential-bs-spots]
  if(is-spot-valid x (ymax + var)) [set potential-bs-spots lput patch x (ymax + var) potential-bs-spots]
  if(is-spot-valid xmax (ymax + var)) [set potential-bs-spots lput patch xmax (ymax + var) potential-bs-spots]
  if(is-spot-valid (xmax + var) (ymax + var)) [set potential-bs-spots lput patch (xmax + var) (ymax + var) potential-bs-spots]
  if(is-spot-valid (xmax + var) ymax) [set potential-bs-spots lput patch (xmax + var) ymax potential-bs-spots]
  if(is-spot-valid (xmax + var) y) [set potential-bs-spots lput patch (xmax + var) y potential-bs-spots]
  if(is-spot-valid (xmax + var) ymin) [set potential-bs-spots lput patch (xmax + var) ymin potential-bs-spots]
  if(is-spot-valid (xmax + var) (ymin - var)) [set potential-bs-spots lput patch (xmax + var) (ymin - var) potential-bs-spots]
  if(is-spot-valid xmax (ymin - var)) [set potential-bs-spots lput patch xmax (ymin - var) potential-bs-spots]
  if(is-spot-valid x (ymin - var)) [set potential-bs-spots lput patch x (ymin - var) potential-bs-spots]
  if(is-spot-valid xmin (ymin - var)) [set potential-bs-spots lput patch xmin (ymin - var) potential-bs-spots]
end

;; updates color-weight mapping
to update-color-weight-mapping
  ask patches [
    (ifelse
      (weight = 2) [ set pcolor lime ]
      (weight = 1) [ set pcolor yellow ]
      [ set pcolor 3 ] ;; dark gray
    )
  ]
end

;; resets color-weight mapping,
;; useful in the Base Station setup while analyzing potential spots coverage improvement.
;; Since colors are not updated during analysis, this procedure works as a "load last saved mapping".
to reset-color-weight-mapping
  ask patches with [pcolor = lime][set weight 2]
  ask patches with [pcolor = yellow][set weight 1]
  ask patches with [pcolor = 3][set weight 0]
end

;; creates base staions, initializes their variables and places them following a Weighted Distribution System
to setup-bases
  ;; initialization
  ask patches [set weight 0]
  set _4G-signal-range round 50000 / patch-side-length
  set _5G-signal-range round 250 / patch-side-length
  set potential-bs-spots []

  ;; first BS is placed randomly to improve variety in world generation
  ask one-of patches[
    sprout-bases 1 [
      set shape "basestation"
      set size 10
      set color 33

      set capacity 50 ;; TO-BE UPDATED user should be able to modify this parameter
      set linked-users 0
    ]
    ask patches in-radius _4G-signal-range [
      set weight 1
    ]
    ask patches in-radius _5G-signal-range [
      set weight 2
    ]

    find-potential-locations [pxcor] of self [pycor] of self
  ]
  update-color-weight-mapping

  set _5G-coverage count patches with [weight = 2] * 100 / count patches

  ;; while the 5G coverage doesn't meet the target we keep placing new BS
  while[_5G-coverage < target-5G-coverage][
    let potential-locations []
    ;; improvement in coverage
    let delta-coverage []

    ;; foreach potential spot we analyze the improvement in coverage
    foreach potential-bs-spots [
      p ->
       ask p [
        set potential-locations lput self potential-locations
        ask patches in-radius _5G-signal-range [
          set weight 2
        ]
        let current-coverage count patches with [weight = 2] * 100 / count patches
        set delta-coverage lput (current-coverage - _5G-coverage) delta-coverage

        reset-color-weight-mapping
       ]
    ]
    ;; after the anlysis is complete we ask the patch that guarantees the most improvement in coverage to sprout a new BS
    let max-delta-coverage max delta-coverage
    let designated-patch-index position max-delta-coverage delta-coverage
    let designated-patch item designated-patch-index potential-locations

    set potential-bs-spots remove-item designated-patch-index potential-bs-spots

    ask designated-patch [
      sprout-bases 1 [
      set shape "basestation"
      set size 10
      set color 3

      set capacity 50 ;; TO-BE UPDATED user should be able to modify this parameter
      set linked-users 0
      ]
      ask patches in-radius _5G-signal-range [
        set weight 2
      ]

      find-potential-locations [pxcor] of self [pycor] of self
    ]

    update-color-weight-mapping
    set _5G-coverage count patches with [weight = 2] * 100 / count patches
  ]
  update-pcolors
end

;; setups patch costs useful to path finding algorithm
to setup-patches-cost
  ;; 5G covered patches cost less to go through
  ask patches [
    ifelse weight = 2
    [set cost 1]
    [set cost 4]
  ]
end

;; finds nearest Base Station for every user and sets related property
to setup-user-bs-links
  ask users [

    ;; find the nearest base station
    set nearest-bs min-one-of bases [distance myself]

    ;; create a link with the nearest base station
    create-link-with nearest-bs

    ask links [
      hide-link
    ]

    ;; set the linked-bs property
    set linked-bs nearest-bs

    ;; set the distance-to-nearest-base-station variable
    set distance-to-nearest-bs (distance nearest-bs) * patch-side-length
  ]

  ;; update linked-users counter for every Base Station
  update-linked-users
end

;; updates links between users and nearest Base Stations -if needed- and all related properties
to update-user-bs-links
  ask users [
    ;; find nearest Base Station as the user moves
    let actual-nearest-bs min-one-of bases [distance myself]

    ;; if user movement causes a change of the nearest Base Station then update his properties
    if (actual-nearest-bs != nearest-bs) [
      ;; [who] properties of both user and Base Station is used to find the older link and delete it
      ask link [who] of self [who] of nearest-bs [die]

      ;; user properties update
      set nearest-bs actual-nearest-bs
      create-link-with nearest-bs
      ask links [
        hide-link
      ]
      set linked-bs nearest-bs
    ]

    ;; in any case the distance to the nearest-bs is updated following user movement
    set distance-to-nearest-bs (distance nearest-bs) * patch-side-length
  ]

  ;; update linked-users counter for every Base Station
  update-linked-users
end

;; updates Base Stations' linked-users property
to update-linked-users
  ask bases [
    set linked-users count users with [linked-bs = myself]
  ]
end

;; updates patches color to help visualize weighted distribution system
to update-pcolors
  ask patches [
    ifelse not show-bs-range?
    [ set pcolor 3 ] ;; dark gray
    [
      (ifelse
        (weight = 2) [ set pcolor lime ]
        (weight = 1) [ set pcolor yellow ]
        [ set pcolor 3 ] ;; dark gray
      )
    ]
  ]
end

;; A* algortithm implementation for lowest-cost-path finding
to-report find-lowest-cost-path [ source dest ]
  let paths (list (list source))
  let estimated-costs (list [distance dest ] of source)
  let path first paths

  let visited patch-set source
  let encountered patch-set source

  while [ source != dest ] [
    let estimated-cost min estimated-costs
    let path-index position estimated-cost estimated-costs
    set path item path-index paths
    set source last path
    let path-cost estimated-cost - [ distance dest ] of source
    let source-cost [cost] of source

    set paths remove-item path-index paths
    set estimated-costs remove-item path-index estimated-costs

    set visited (patch-set source visited)

    ask [ neighbors with [ not member? self visited ] ] of source [
      ;let patch-cost runresult get-cost
      let patch-cost [cost] of self
      let step-cost distance source * (patch-cost + source-cost) / 2
      let est-cost path-cost + step-cost + distance dest

      let add? true

      if member? self encountered [
        let other-path false
        foreach paths [
          p ->
          if last p = self [
            set other-path p
          ]
        ]
        if other-path != false [
          let other-path-index position other-path paths
          let other-path-cost item other-path-index estimated-costs
          ifelse other-path-cost < est-cost [
            set add? false
          ] [
            set paths remove-item other-path-index paths
            set estimated-costs remove-item other-path-index estimated-costs
          ]
        ]
      ]

      if add? [
        set estimated-costs fput est-cost estimated-costs
        set paths fput (lput self path) paths

        set encountered (patch-set self encountered)
      ]
    ]
  ]
  report path
end

to-report find-shortest-path

  let starting-pos patch-here

  let dest [destination] of self
  let path []

  while[patch-here != dest][
    set path lput patch-here path
    ;; at each step the user moves to the neighbor patch closest to the destination
    move-to min-one-of neighbors [distance dest]
  ]

  move-to starting-pos
  report path
end

;; calculate lowest cost paths for every user
to compute-paths
  ask users [
    set path-to-dest find-lowest-cost-path patch-here destination
  ]
end

;; makes users move towards their destination
to reach-destination ;; --TO BE UPDATED--

  if(patch-here = last path-to-dest)[ die ]

  let current-position position patch-here path-to-dest
  let next-patch item (current-position + 1) path-to-dest

  move-to next-patch

;  let dest [destination] of self
;
;  ;; when users reach their destination we stop considering them
;  if(patch-here = dest) [die]
;
;  ;; at each step the user moves to the neighbor patch closest to the destination
;  move-to min-one-of neighbors [distance dest]
;
;  ;; updates distance to destination at each step
;  ;; that older distance is just decreased by a patch-side-length
;  ;; that's beacause at each step the user moves to the centre of his current patch to the centre of another patch
;  ;; but that equals a patch-side-length distance traveled since patch are squares
;  set distance-to-dest round(distance-to-dest - patch-side-length)
end

to display-path-difference
  ask one-of users with [distance-to-dest > 300][
    let lowest-cost-path find-lowest-cost-path patch-here destination
    foreach lowest-cost-path [
      p ->
       ask p [
        (ifelse
          (p = first lowest-cost-path) [ sprout 1 [ set shape "flag" set size 3 set color white] ]
          (p = last lowest-cost-path) [ sprout 1 [ set shape "target" set size 3 set color red] ]
          (p = item ((length lowest-cost-path) / 2) lowest-cost-path) [ set plabel word (length lowest-cost-path * patch-side-length) "m" set plabel-color 57]
          [ set pcolor green ]
        )
      ]
    ]

    let shortest-path find-shortest-path
    foreach shortest-path [
      p ->
       ask p [
        (ifelse
          (p = item ((length shortest-path) / 2) shortest-path) [ set plabel word (length shortest-path * patch-side-length) "m" set plabel-color 17]
          [ set pcolor red ]
        )
      ]
    ]
  ]
end

;; displays user and base stations properties
to display-labels
  ask turtles [ set label "" ]
  if show-linked-users? [
    ask bases [ set label linked-users ]
  ]
  if show-distance-to-nearest-bs? [
    ask users [ set label round distance-to-nearest-bs ]
  ]
  if show-path-difference? [ display-path-difference ]
end

;; updates user and base stations properties
to update-labels
  ask turtles [ set label "" ]
  if show-linked-users? [
    ask bases [ set label linked-users ]
  ]
  if show-distance-to-nearest-bs? [
    ask users [ set label round distance-to-nearest-bs ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
380
10
988
619
-1
-1
6.0
1
15
1
1
1
0
0
0
1
-50
49
-50
49
0
0
1
ticks
30.0

BUTTON
95
120
176
153
Setup
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

SWITCH
80
215
287
248
show-distance-to-nearest-bs?
show-distance-to-nearest-bs?
1
1
-1000

SWITCH
185
170
337
203
show-linked-users?
show-linked-users?
1
1
-1000

BUTTON
185
120
265
153
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
80
335
285
495
Users in the system
time (ticks)
users (units)
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"Users" 1.0 0 -2064490 true "" "plot count users"

TEXTBOX
125
295
250
313
[Distances are in metres]
10
0.0
1

SWITCH
25
170
175
203
show-bs-range?
show-bs-range?
1
1
-1000

SLIDER
80
60
282
93
population-density
population-density
1
7000
3500.0
1
1
pop./km²
HORIZONTAL

TEXTBOX
45
95
330
121
[Population is scaled following a 100:1 ratio in the model]
10
0.0
1

MONITOR
1055
90
1182
143
5G Coverage (%)
_5G-coverage
17
1
13

TEXTBOX
105
30
255
46
World Area is fixed to 1 km²
12
0.0
1

SWITCH
80
255
285
288
show-path-difference?
show-path-difference?
0
1
-1000

TEXTBOX
285
255
435
273
[shortest-path]
10
15.0
1

TEXTBOX
285
275
435
293
[lowest-cost-path]
10
55.0
1

SLIDER
1035
30
1207
63
target-5G-coverage
target-5G-coverage
25
75
75.0
25
1
NIL
HORIZONTAL

TEXTBOX
1000
15
1250
41
!!this represents a treshold and not a precise target!!
10
0.0
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

basestation
false
6
Rectangle -7500403 true false 141 91 160 256
Polygon -7500403 true false 59 256 80 243 89 246 101 256
Polygon -7500403 false false 90 198 150 228 210 198 150 183
Polygon -7500403 false false 101 144 150 171 198 144 150 126
Polygon -7500403 false false 94 172 151 201 205 172 150 156
Polygon -7500403 true false 172 91 188 85 225 247 210 251
Polygon -7500403 true false 128 91 112 85 75 247 90 251
Polygon -7500403 true false 241 256 220 243 211 246 199 256
Polygon -7500403 true false 143 251 158 251 173 266 128 266
Rectangle -7500403 true false 62 53 240 60
Rectangle -7500403 true false 225 30 240 90
Rectangle -7500403 true false 60 30 75 90
Rectangle -7500403 true false 203 38 215 78
Rectangle -7500403 true false 85 38 97 78
Polygon -13840069 true true 156 26 151 42 114 41 110 55 148 56 137 94 80 94 86 79 122 79 125 67 91 67 105 26
Polygon -13840069 true true 216 26 158 26 140 94 199 95 209 59 171 59 168 71 191 72 189 80 161 80 169 44 210 44

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
Rectangle -7500403 true true 90 15 300 165
Rectangle -16777216 true false 90 45 120 75
Rectangle -16777216 true false 90 105 120 135
Rectangle -16777216 true false 120 135 150 165
Rectangle -16777216 true false 120 75 150 105
Rectangle -16777216 true false 120 15 150 45
Rectangle -16777216 true false 150 45 180 75
Rectangle -16777216 true false 150 105 180 135
Rectangle -16777216 true false 180 135 210 165
Rectangle -16777216 true false 180 75 210 105
Rectangle -16777216 true false 180 15 210 45
Rectangle -16777216 true false 210 45 240 75
Rectangle -16777216 true false 210 105 240 135
Rectangle -16777216 true false 240 135 270 165
Rectangle -16777216 true false 240 75 270 105
Rectangle -16777216 true false 240 15 270 45
Rectangle -16777216 true false 270 45 300 75
Rectangle -16777216 true false 270 105 300 135

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
Circle -1 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -1 true false 90 90 120
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
NetLogo 6.3.0
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
1
@#$#@#$#@
