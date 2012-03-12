; ***UPDATES***
; ___________________________________________________
; 

; ***ISSUES***
; ___________________________________________________
; need to be able to assign schedules with multiple destinations

; ***Questions***
; ___________________________________________________
; what is the $ for in $row, $from, etc.
; is current-node needed as a state variable
; do we need a list to store the status variables
; do we want buttons for batt cap or enrgy eff means and stdv
; should we have a driver satisfaction state variable even though it can be calculated by other variables
; is it possible to read a .csv file or does it have to be white-space delimited
; should we track which destination each driver is located within their schedule

extensions [matrix]

globals
[
  start-hour      ; starting time, an integer (or fractional) hour
  stop-hour       ; time to end simulation, an integer hour
  ;time-step-size  ; time step in minutes -- set by interface slider
  time            ; current time in fractional hours
  display-time    ; current time, a character string hh:mm
  
  od              ; origin-destination matrix assumes the TAZ's are 
                  ; numbered from 1 to n-nodes and the row number is calculated at (FromTAZ-1)*n-nodes+ToTAZ
                  ; The columns correspond to the following definitions:
                  ; 1 - 24 Hour Traffic Demand
                  ; 2 - AM Rush Hour Traffic Demand
                  ; 3 - PM Rush Hour Traffic Demand
                  ; 4 - Distance in miles
                  ; 5 - Distance in travel time (decimal hours)
  ; n-nodes        ; number of nodes set by interface
  ; n-drivers      ; number of drivers set by interface
  temperature     ; ambient temperature
]

breed [drivers driver]
drivers-own [
  ;static state variables
  battery-capacity ; in kWh - should be normally distributed
  ;behavior ; probably will be a efficiency reduction factor *wish list* 
  ;anxiety ; could assign a range of proneness or could keep track of simulated anxiety *wish list*
  ;charge-compatibility ; whether level 1, 2, or 3 charging is supported *wish list*
  ;dynamic state variables
  ;driver-satisfaction ; do we want a variable for this here
  current-node ;this one actually might not be needed
  SoC ; represents battery the state of charge at each timestep
  energy-efficiency ; energy required to travel 1 mile (kWh/mile)
  schedule ; a matrix of departure times, destinations, distances and drive times
  status ; discrete value from list:Home,Traveling,Staging,Charging,Waiting,Stranded
  ;destination-number ;keep track of which destination the vehicle is located
]

breed [chargers charger]
chargers-own[
  TAZ-location ; where the charger is located
  available ; boolean to represent either available(TRUE) or occupied(FALSE) 
]

breed [nodes node]
nodes-own[
  TAZ-ID ; a unique integer identifier for each TAZ
  TAZ-distance ; the distance between each node and every other node
  travel-time ; the time required to travel from each node to every other node
]
to setup
  
  clear-all
  
  ; Set the time parameters
  set start-hour 0        ; starting time, an hour
  set stop-hour  24       ; time to end simulation, in whole or fractional hours
  ;set time-step-size  1   ; time step in minutes -- currently set by interface slider
  
  ; Initialize the time variables
  set time start-hour
  update-display-time
  
  reset-ticks
  
  ;set n-nodes 4 ;number of TAZs
  ;set n-drivers 6500 ;number of drivers
  
  setup-matrix ; create a matrix of fabricated data
  setup-drivers
  setup-nodes
  setup-chargers
 
end ;setup
  
to setup-matrix 
  ; currently just creates a random matrix
  set od matrix:make-constant (n-nodes * n-nodes) 5 0
  
  ; make up some fake data to fill the od matrix for now
  foreach n-values n-nodes [?] [
    let $from ?
    foreach n-values n-nodes [?] [
      let $row ($from * n-nodes + ?)
      matrix:set-row od $row (list random-float 50 random-float 10 random-float 10 random-float 100 random-float 2)
    ]
  ]
  ;print matrix:pretty-print-text od
end ;setup-matrix

to setup-drivers
  ; currently assigns each driver to a random node
  create-drivers n-drivers
  setup-schedule
  ask drivers[
    set shape "car"
    set current-node random n-nodes ;change this to hatch each driver at home
    ; let battery capacity deviate a little bit but no less than 20 kWh
    set battery-capacity max (list min-batt-cap random-normal batt-cap-mean batt-cap-stdv) 
    ; let energy efficiency deviate a little but no less than 0.5
    set energy-efficiency max (list 0.5 random-normal .74 .05)
    set SoC 100
    set status "Home"
  ]
end ;setup-drivers

to setup-schedule
  ;still need to determine the best way to do this
  ifelse ( file-exists? "sample_schedule.txt" )
  [
    ;; This opens the file, so we can use it.
    file-close
    file-open "sample_schedule.txt"
    foreach sort drivers[
    ;; Read in all the data in the file
      ask ? [
        ; read in departure time, destination TAZ, distance, and drive time
        ; this only works if each agent has a single destination 
        ; should be modified to incorporate multiple destinations
        ; maybe read in a variable for number of destinations
        set schedule (list file-read file-read file-read file-read)
        show schedule
      ]
    ]
    user-message "File loading complete!"
    ;; Done reading in schedule.  Close the file.
    file-close
  ]
  [ user-message "There is no sample_schedule.txt file in current directory!" ]
end ;setup-schedule

to setup-nodes
  create-nodes n-nodes
  ask nodes [
    set shape "star"
  ]
    ifelse ( file-exists? "sample_TAZs.txt" )
    [  
      file-close ; is this really necessary?
      file-open "sample_TAzs.txt"      
      ask nodes [
        set TAZ-ID file-read
        setxy file-read file-read
      ]
    user-message "File loading complete!"
    ;; Done reading in schedule.  Close the file.
    file-close
    ]
    [ user-message "There is no sample_schedule.txt file in current directory!" ]   
end ;setup-nodes

to setup-chargers
  ask chargers [set available TRUE]
end ;setup-chargers

to go

  ; Advance the time and update time variables
  tick
  set time time + (time-step-size / 60)  ; Convert time step to hours and add to current time
  if time > stop-hour [ stop ]           ; stop the simulation
  
  update-display-time
  
  ; Put the rest of the schedule here
  
;;;;;  ask drivers[
;;;;;   let $from random n-nodes
;;;;;   let $to $from
;;;;;   while [ $to = $from] [ set $to random n-nodes ]  
;;;;;   ; make sure $to != $from because routes are undefined from and to the same node
;;;;;   matrix-go $from $to
;;;;;   ;routes-go $from $to 
;;;;;   
;;;;;   ; for now just assume the driver immediately goes there
;;;;;   set current-node $to
;;;;;   ]
   
   done-traveling
   query-chargers
   finish-charging
   wait-or-not
   depart
   update-variables
 
;  wait 1  ; This temporary statement pauses execution so you can see the time on the display.
  
end ;go


to matrix-go [from-node to-node]
  ; in reality, something would be done with this info, for now, just access it
  let $row (from-node * n-nodes + to-node)
  let $temp-distance-miles matrix:get od $row 3
  let $temp-distance-hours matrix:get od $row 4
  ;type "from:" type from-node type " to:" type to-node type ";  "
  ;type $row type ", " type $temp-distance-miles type ", " print $temp-distance-hours
end


to update-display-time
  
  let hour floor time
  let minutes round ((time - hour) * 60)
  set display-time (word hour ":" minutes)
  
end

to update-variables
  ; update satisfaction
  ; update duty factor
end ;update-variables

to-report satisfaction 
  ; specify context here
  report 1 ; calculate satisfaction here -- currently just garbage
end ;satisfaction

to-report duty-factor
  ; specify context here
  report 1 ; calculate duty factor here -- currently just garbage
end ;duty-factor

to done-traveling
  
end ;done-traveling

to query-chargers
  
end ;query-chargers

to finish-charging
  
end ;finish-charging

to wait-or-not
  
end ;wait-or-not

to depart
  ; do we need these local variables or should we just modify the current schedule variable
  ; let next-dest 0
  ; let next-dist 0
  ; let next-time 0
  ask drivers [
    if (status = "Home") or (status = "Staging") and (time >= item 0 schedule)[ ;find out which drivers are departing this time step
      ;read destination TAZ
      ;set next-dest item 1 schedule
      ;set next-dist item 2 schedule
      ;set next-time (time + item 3 schedule)
      set schedule replace-item 3 schedule (time + item 3 schedule)
      set status "Traveling"
    ]
    
  ]
end ;wait-or-not
@#$#@#$#@
GRAPHICS-WINDOW
375
19
814
479
16
16
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
1.0

MONITOR
16
426
95
471
NIL
display-time
17
1
11

BUTTON
14
14
81
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

BUTTON
14
50
77
83
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

INPUTBOX
10
96
92
156
time-step-size
1
1
0
Number

INPUTBOX
107
21
198
81
min-batt-cap
20
1
0
Number

INPUTBOX
109
95
199
155
batt-cap-mean
74
1
0
Number

INPUTBOX
210
95
289
155
batt-cap-stdv
0.1
1
0
Number

MONITOR
136
427
298
472
average driver satisfaction
satisfaction
17
1
11

INPUTBOX
13
180
84
240
n-drivers
2
1
0
Number

INPUTBOX
15
256
79
316
n-nodes
4
1
0
Number

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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
