; ***UPDATES***
; 1.6 3-20 ah&jb
; "Query-Chargers" now assigns open chargers to first vehicle to arrive. If unavailable, they currently default to wait.
; Initialize driver satisfaction of 1 in "Setup-drivers"
; Driver satisfaction is now reduced during "query-chargers" if driver must wait for charger.
; Total-driver-satisfaction now reports the mean driver satisfaction
; If "waiting" at end of day, driver becomes "stranded"
; 1.6 3-20dh
; added finish charging procedure
; added update soc for charging vehicles
;JB 1.5 3-20
;fixed so that the driver partners with the appropriate charger
;added a phanom turtle so we can keep taz numbering 1-25 rather than 0-24 
; ___________________________________________________
; 3.13 - ZS
; added from/current TAZ column to schedule and switched the order of destination TAZ and departure time
; added current-taz and driver satisfaction as driver state variables
; added global debug? switch, debug-print procedures, and output interface

; changed satisfaction reporter to total-satisfaction
; added travel-time and travel-dist state variables
; added update-soc procedure

;3.15 JB
; Added coordinates for driver locations
; updated done traveling to be waiting till new status is determined from charge needed
; sorted TAZs from 0-24 and made them stay in position
; positioned drivers at home TAZS
; changed colors and size of nodes and drivers to indicate status
; read in sample text file with originated and destination TAZ, distance, and travel, however this may be cumbersome to use
; Charger number is now set in the TAZ location file. The setup-chargers procedure associates chargers with their 
; appropriate TAZ and plots an open circle in that location. The circle currently covers the car icon.

;3.19
;reduced size of circle to not cover car
;added partner to drivers variable, setup driver
; started querry charger submodel
;JB 3-20
;fixed so that the driver partners with the appropriate charger
;added a phanom turtle so we can keep taz numbering 1-25 rather than 0-24 
; 1.6 dh
; added finish charging procedure
; added update soc for charging vehicles
; ***ISSUES/NEEDS***
; ___________________________________________________
; need to be able to assign schedules with multiple destinations.  
; -- We prob need to make schedule a matrix and not a list.
; finish procedure skeleton code
; start producing output plots


; ***Questions***
; ___________________________________________________
; what to do with current-taz variable when traveling
; if we are not using links, how do we store information about each trip (e.g., avg. speed, energy-efficiency multiplier)
; what is the best way to step through the model one tick (or several ticks) at a time?

extensions [matrix]

globals
[
  start-hour      ; starting time, an integer (or fractional) hour
  stop-hour       ; time to end simulation, an integer hour
  ;time-step-size  ; time step in minutes -- set by interface slider
  time            ; current time in fractional hours
  display-time    ; current time, a character string hh:mm
  
  od              ; origin-destination matrix assumes the taz's are 
                  ; numbered from 1 to n-nodes and the row number is calculated at (Fromtaz-1)*n-nodes+Totaz
                  ; The columns correspond to the following definitions:
                  ; 1 - 24 Hour Traffic Demand
                  ; 2 - AM Rush Hour Traffic Demand
                  ; 3 - PM Rush Hour Traffic Demand
                  ; 4 - Distance in miles
                  ; 5 - Distance in travel time (decimal hours)
  ; n-nodes        ; number of nodes set by interface
  ; n-drivers      ; number of drivers set by interface
  temperature     ; ambient temperature
  ; debug?          ; switch to control output of print statements -- controlled from interface
]

breed [drivers driver]
drivers-own [
  ;static state variables
  battery-capacity ; in kWh - should be normally distributed
  ;behavior ; probably will be a efficiency reduction factor *wish list* 
  ;anxiety ; could assign a range of proneness or could keep track of simulated anxiety *wish list*
  ;charge-compatibility ; whether level 1, 2, or 3 charging is supported *wish list*
  ;dynamic state variables
  driver-satisfaction ; do we want a variable for this here
  partner ;a variable used in to interact a driver and a charger
  total-trip-time ; the total time needed to drive from A to B
  total-trip-dist ; the total distance for a given trip
  state-of-charge ; represents battery the state of charge at each timestep
      ; if soc is calculated from travel-dist and energy-efficiency, is it a state variable still?
  energy-efficiency ; energy required to travel 1 mile (kWh/mile)
  arrival-time ; when a car is supposed to arrive
  ;schedule ; a matrix of: (1)from/current TAZ, (2)destination taz, (3)departure time, (4)distance and (5)drive time
             ; one row for each trip. (row 1, col 1) is the home TAZ
  status ; discrete value from list:Home,Traveling,Staging,Charging,Waiting,Stranded
  ;destination-number ;keep track of which destination the vehicle is located
  current-taz ; keeps track of the current location of each vehicle
  destination-taz ;from the schedule, this is where we're going
  departure-time ;When the vehicle is set to leave the taz
  travel-dist ; used to store distance driven since departure
  travel-time ; used to store traveling time since departure
]   ; travel-dist and travel-time have no real use, we may want to delete.

breed [chargers charger]
chargers-own[
  taz-location ; where the charger is located
  available ; boolean to represent either available(TRUE) or occupied(FALSE) 
 ]

breed [nodes node]
nodes-own[
  taz-id ; a unique integer identifier for each taz
  taz-distance ; the distance between each node and every other node
  taz-time ; the time required to travel from each node to every other node
  taz-chargers ; the number of chargers in a given taz
]
to setup
  
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set debug? TRUE
  ; Set the time parameters
  set start-hour 0        ; starting time, an hour
  set stop-hour  24       ; time to end simulation, in whole or fractional hours
  ;set time-step-size  1   ; time step in minutes -- currently set by interface slider
  
  ; Initialize the time variables
  set time start-hour
  update-display-time
  
  reset-ticks
  
  ;set n-nodes 4 ;number of tazs
  ;set n-drivers 6500 ;number of drivers
  create-turtles 1 [ setxy 0 0 set color black] ;This invisible turtle makes sure we start at node 1 not node 0
  setup-matrix ; create a matrix of fabricated data
  setup-nodes
  setup-drivers
  setup-chargers
 
end ;setup
  
to setup-matrix 
  set od matrix:make-constant (n-nodes * n-nodes) 4 0 ;reading in fake data
  
  ifelse (file-exists? "sample_time&distance_matrix.txt")
    [
      file-close
      file-open "sample_time&distance_matrix.txt"
  ; make up some fake data to fill the od matrix for now
  foreach n-values n-nodes [?] [
    let $from ?
    foreach n-values n-nodes [?] [
      let $row ($from * n-nodes + ?)
      matrix:set-row od $row (list file-read file-read file-read file-read)
    ]
  ]
  file-close
    ]
    [ user-message "There is no sample_time&distance_matrix.txt file in current directory!" ]
  print matrix:pretty-print-text od
end ;setup-matrix

to setup-drivers
  ; currently assigns each driver to a random node
  create-drivers n-drivers
  setup-schedule
  ask drivers[
    set shape "car"
    set color green
    set size 2
    set driver-satisfaction 1  ;initialize driver satisfaction
    ;set current-taz random n-nodes ;change this to hatch each driver at home
    ; let battery capacity deviate a little bit but no less than 20 kWh
    set battery-capacity max (list min-batt-cap random-normal batt-cap-mean batt-cap-stdv) ; is this just a kWh rating?
    ; let energy efficiency deviate a little but no less than 0.5
    set energy-efficiency max (list 0.5 random-normal .74 .05)
    set state-of-charge 100
    set status "Home"
    set partner nobody
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
        ; read in current taz, destination taz, departure time, distance, and drive time
        ; this only works if each agent has a single destination 
        ; should be modified to incorporate multiple destinations
        ; maybe read in a variable for number of destinations
        ; AH: Commented out the "set schedule," reading in each as a variable
        ;set schedule (list file-read file-read file-read file-read file-read)
        set current-taz file-read
        setxy [xcor] of node current-taz [ycor] of node current-taz
        set destination-taz file-read
        set departure-time file-read
        show list "departure time is" departure-time
        set total-trip-dist file-read
        set total-trip-time file-read
        show list "total trip time is "total-trip-time
        set arrival-time 99 ; The arrival time is re-set when a vehicle departs - this prevents an arrival time of 0.
        debug-print-self "has a schedule"
        ;show schedule "Schedule" still exists as a variable, but it is no longer defined
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
    set color yellow
    set size 0.5
  ]
    ifelse ( file-exists? "sample_tazs.txt" )
    [  
      file-close ; is this really necessary?
      file-open "sample_tazs.txt"      
      foreach sort nodes[
        ask ? [
        set taz-id file-read
        setxy file-read file-read
        set taz-chargers file-read ; Sets the number of chargers in each node
        ]
      ]
    user-message "File loading complete!"
    ;; Done reading in schedule.  Close the file.
    file-close
    ]
   
    [ user-message "There is no sample_TAZs.txt file in current directory!" ]   
end ;setup-nodes

to setup-chargers

  foreach sort nodes [                       ; At each node, chargers equal to "taz-chargers"are created.
      create-chargers [taz-chargers] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
  ]  
  ask chargers [
    set available TRUE
    ]
  
end ;setup-chargers

to go

  ; Advance the time and update time variables
  tick
  set time time + (time-step-size / 60)  ; Convert time step to hours and add to current time
  
  if time > stop-hour [ 
    ask drivers with [status = "Waiting"] [set status "Stranded"]
    stop ]           ; stop the simulation
  
  update-display-time
  
  ; Put the rest of the schedule here

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
  ask drivers [
    update-soc
  ]
  ; update total-satisfaction
  ; update duty factor
end ;update-variables

to-report total-satisfaction 
  
  ; For now, total-satisfaction is an average of all driver's satisfaction   
  report mean [driver-satisfaction] of drivers 
  
end ;total-satisfaction

to-report duty-factor
  ; specify context here
  report 1 ; calculate duty factor here -- currently just garbage
end ;duty-factor

to done-traveling
    ask drivers [ 
    if status = "Traveling" [
     
    ; TRYING TO GET TRAVELING CARS TO DO THINGS
      if time >= arrival-time [
        show list "is done traveling" display-time ;; If we've arrived, change status.
        setxy [xcor] of node destination-taz [ycor] of node destination-taz
              
        ;let minimum-acceptable-charge 
     ;   show list "SoC is" state-of-charge ; debugging message
        if state-of-charge > minimum-acceptable-charge [set status "Staging"  set color blue]
        if state-of-charge <= minimum-acceptable-charge [set status "Waiting"
        show "charge needed"  set color red]
        set current-taz destination-taz 
        ;need to set new destination-taz here
        ;minimum acceptable charge could be calculated based on charge needed for next destination
        ] 
    ]
  ]
end ;done-traveling

to query-chargers ;Jb added 3.19
          foreach sort nodes
            [
              let available-chargers chargers with [available = TRUE and taz-location = [taz-id] of ?] 
              ;show count available-chargers
              let waiting-drivers drivers with [status = "Waiting" and current-taz = [taz-id] of ?]
              ifelse count waiting-drivers <= count available-chargers
              [
                ask waiting-drivers 
                [
                  set status "Charging"
                  set color orange
                  debug-print-self status
              
                  ;let singles waiting-drivers with [partner = nobody]
                  ;if not any? singles [ stop ]
              
                  set partner one-of available-chargers 
                  ask partner [set available FALSE]
                ]
              ]
              [
                foreach sort available-chargers 
                [
                  set available FALSE
                  ask one-of waiting-drivers with [status = "Waiting"]
                  
                  [
                    set status "Charging"
                    set partner ?
                  ] 
              
                ]
                foreach sort waiting-drivers with [status = "Waiting"]
                [ ask ? [
                set driver-satisfaction (driver-satisfaction * 0.99)]
                show list "Driver satisfaction decreased at" display-time
                
                ]
                  
              ] 
            ]
                 
end ;query-chargers

to finish-charging ;added by dh and zs 3-20
  ask drivers with [status = "Charging"]
  [
    if state-of-charge >= 100
    [
      ask partner [set available TRUE]
      set partner nobody
      set status "Staging"
    ]
  ]    
end ;finish-charging

to wait-or-not
  
end ;wait-or-not

to depart
  ; do we need these local variables or should we just modify the current schedule variable
  ask drivers [
    if (status = "Home") or (status = "Staging") and ((time + 0.001 >= departure-time) and (time - 0.001 <= departure-time)) [ ;find out which drivers are departing this time step
      set arrival-time (time + total-trip-time) ;calculate arrival time based on length of trip and current time
  ;    show list "Arrival time is"  arrival-time
      set status "Traveling"
      set color grey
  ;    show status
  
    ]
    
  ]
end ;depart

to debug-print[msg]
  if debug?[output-print msg]
end
to debug-print-self[msg]
  debug-print(word self " is " msg)
end

to update-soc ;should be executed each time step by each driver
  if status = "Traveling" [
    set travel-time (time - departure-time) ; calculate traveling time based on current time and departure time
  ;  show list "Travel time is " travel-time
    let speed (total-trip-dist / total-trip-time) ; calculate average speed based on total trip distance and total trip time
    set travel-dist (speed * travel-time) ; calculate the distance the driver has traveled thus far
  ;  show list "Travel distance is " travel-dist
    set state-of-charge (state-of-charge - time-step-size / 60 * speed * energy-efficiency) ; AH: updated this calculaton; was previously
  ;  show list "SoC is " state-of-charge   ; subtracting SoC for entire elapsed trip each time. We just want the single step distance.
    if (state-of-charge <= 0)[
      set status "Stranded" ]; any vehicle that runs out of charge is stranded immediately
  ;    show list "SoC is " state-of-charge]
  ]
  if status = "Charging"
  [
    set state-of-charge state-of-charge + 0.1 * time-step-size ;0.1 needs to be a charger variable
  ]
  
end ;update-soc
@#$#@#$#@
GRAPHICS-WINDOW
375
19
760
425
-1
-1
15.0
1
10
1
1
1
0
0
0
1
0
24
0
24
0
0
1
ticks
30.0

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
total-satisfaction
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
25
1
0
Number

OUTPUT
17
323
335
415
11

SWITCH
115
191
218
224
debug?
debug?
0
1
-1000

INPUTBOX
115
260
289
320
minimum-acceptable-charge
95
1
0
Number

@#$#@#$#@
## ## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## ## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## ## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## ## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## ## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## ## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## ## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## ## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## ## CREDITS AND REFERENCES

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
