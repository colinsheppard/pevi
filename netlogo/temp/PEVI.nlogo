extensions [matrix array dynamic-scheduler]


;; define all globals
globals

[ ;start-hour      ; starting time, an integer (or fractional) hour
  ;stop-hour       ; time to end simulation, an integer hour
  ;;time-step-size  ; time step in minutes -- set by interface slider
  ;time            ; current time in fractional hours
  ;;display-time    ; current time, a character string hh:mm
    
  od-from
  od-to
  od-demand
  od-dist
  od-time
  
  schedule  ;; this global variable holds the dynamic schedule for the PEVI program, appended by the drivers from their itineraries
]

breed [drivers driver]
breed [chargers charger]
breed [nodes node]

drivers-own [
  
;; VEHICLE specific
  battery-capacity ; in kwh - FORCE GAUSSIAN DISTRIBUTION
  state-of-charge ; represents the battery state of charge at each timestep
      ; if soc is calculated from travel-dist and electric-fuel-consumption, is it a state variable still?
  electric-fuel-consumption ; energy required to travel 1 mile (kwh/mile)
  status ; discrete value from list:not-charging,traveling,charging
  current-taz ; keeps track of the current location of each vehicle
  kwh-received ; a count of how much energy each driver has charged
  phev?  ;boolean variable       
      
;; TRIP specific
  partner ;a variable used in to interact a driver and a charger
  total-trip-time ; the total time needed to drive from A to B
  travel-time ; used to store traveling time since departure
  total-trip-dist ; the total distance for a given trip
  next-trip-range
  travel-dist ; used to store distance driven since departure
  departure-time ;When the vehicle is set to leave the taz
  arrival-time ; when a car is supposed to arrive
  minimum-acceptable-charge ; The charge required to reach the next destination
  need-to-charge? ; boolean
  
;; SCHEDULE specific
  itin-from 
  itin-to
  itin-depart
  current-schedule-row ; keeps track of which row in the schedule each driver is on
  destination-taz ;from the schedule, this is where we're going
  wait-time
]

chargers-own[
  taz-location ; TAZ # for each charger
  available ; boolean to represent either available(TRUE) or occupied(FALSE)
  charger-level ; 1, 2, or 3 (low to high charge)
  charger-rate ; The rate of recharge for each charger.
  charger-in-use ; A counter increased every time the charger is in use. Used to calculate duty factor. (Not attached to the name -ah)
  duty-factor ; The fraction of time a charger is in use vs. time idle  
  ;; Unless we calculate a duty factor unique to each charger level, this will bias the results (level 3 will always get more use, as it charges faster / 
  ;; allowing it to charge more vehicles)
  charger-service ; a counter for the number of drivers each charger services
  ;; necessary?
  kwh-charged ; a count of how much energy a charger has given
  ;; Interesting -- temporal charge rate.  How does this overlap with the supply of energy throughout the day?  *****
 ]

nodes-own[
  taz-id ; TAZ id #
  time-and-distance ;matrix containing time and distance info for every other node
  taz-chargers-1 ; the number of level 1 chargers in the taz
  taz-chargers-2 ; the number of level 2 chargers in the taz
  taz-chargers-3 ; the number of level 3 chargers in the taz
]

to setup
  __clear-all-and-reset-ticks
  
  set schedule dynamic-scheduler:create
  
  reset-ticks
  
  create-turtles 1 [ setxy 0 0 set color black] ;This invisible turtle makes sure we start at node 1 not node 0
  
  setup-od-array
  setup-nodes
  setup-drivers
  setup-chargers
end 


to setup-od-array
  print "setup-od-array"
  ; Reads in main driver input file: Origin, destination, # of trips, distance, time
  set od-from array:from-list n-values (n-nodes * n-nodes) [0] ; creates global od-from
  set od-to array:from-list n-values (n-nodes * n-nodes) [0]           ; global od-to
  set od-demand array:from-list n-values (n-nodes * n-nodes) [0]       ; global od-demand
  set od-dist array:from-list n-values (n-nodes * n-nodes) [0]         ; global od-dist
  set od-time array:from-list n-values (n-nodes * n-nodes) [0]         ; global od-time
  
  ifelse (file-exists? "../inputs/OD_Matrix_5.txt") [
    file-close
    file-open "../inputs/OD_Matrix_5.txt"
    foreach n-values (n-nodes * n-nodes) [?] [
     array:set od-from ? file-read array:set od-to ? file-read array:set od-demand ? (round file-read) array:set od-dist ? file-read array:set od-time ? (file-read)
     ; HEVI code claims that the "Drive time is in the file as minutes" -- the example file OD_Matrix_5.txt would then suggest that drivers can
     ; sustain a speed of 600mi/hr, if they travel 10 mi/min.  Changed to reflect drive time in the file as units hrs, even though this currently
     ; reflects an average speed ~ 8mi/hr. ac 9/9
    ]
    file-close
  ]
  [ user-message "File not found: ../OD_Matrix_5.txt" ]
  ;print (word "od-dist = " od-dist)
end 

to setup-nodes
  print "setup-nodes"
  create-nodes n-nodes  ; BUTTON for # of nodes?
  ask nodes [
    set shape "star"
    set color yellow
    set size 0.5
  ]
      ; Select the TAZ input file based on the alternative chooser on the interface. If the file does not exist, stop.
      ; BUTTON??

  ifelse (file-exists? alternative-input-file) [ ; ../inputs/alternative_4_5.txt
    file-close
    file-open alternative-input-file
    foreach sort nodes[ ;this block reads taz-id, location, and charger info into each node
      ask ? [
      set taz-id file-read
      setxy file-read file-read
      set taz-chargers-1 file-read ; Sets the number of level 1 chargers in each node
      set taz-chargers-2 file-read ; Sets the number of level 2 chargers in each node
      set taz-chargers-3 file-read ; Sets the number of level 3 chargers in each node
      ]    
    ]
  ]
  [ user-message (word "Input file " alternative-input-file " not found!")] 
   
end ;setup-nodes

to setup-drivers
  print "setup-drivers"
  ; creating drivers based on GEATM data, in setup-itinerary procedure. 
  setup-itinerary
  ; initialize driver state variables
  ask drivers [
    set phev? false ; TODO needs to be determined during itinerary setup
                    ; what is "TODO" -- new procedure to define? -ac 9/8
    set current-schedule-row 0
    set shape "car"
    set color green
    set size 2
    ifelse phev? [
      set battery-capacity 25 ; TODO replace with values from a vehicle type distribution input file
      set electric-fuel-consumption 0.35   ; kWh/mile
    ][ 
      set battery-capacity 25 ; 
      set electric-fuel-consumption 0.35
    ]
    set state-of-charge 1
    set status "not-charging"
    set partner nobody

    set current-taz array:item itin-from current-schedule-row
    set destination-taz array:item itin-to current-schedule-row
    set departure-time array:item itin-depart current-schedule-row
    setxy [xcor] of node current-taz [ycor] of node current-taz   
    check-charge  ; need to check the charge after defining destinations. ac 9/8
  ]
  ask drivers [
    dynamic-scheduler:add schedule self task depart departure-time
  ]
  
end ;setup-drivers

to setup-itinerary
  print "setup-itinerary"
  ifelse (file-exists? driver-input-file) [ ; ../inputs/p1r1_5.txt  
    file-close
    file-open driver-input-file
    let itin-row 0
    let this-itin true
    ;let dummy-read-line file-read-line  ; <--- added this 09/07, to allow the readfile to skip the first line (comments)
    let next-driver file-read
    let this-driver 0
    while [file-at-end? = false] [
      set this-driver next-driver
      create-drivers 1 [
        set itin-from array:from-list n-values 1 [-99]     ; creates global itin-from
        set itin-to array:from-list n-values 1 [-99]       ; creates global itin-to
        set itin-depart array:from-list n-values 1 [-99]   ; creates global itin-depart
        array:set itin-from   0 file-read
        array:set itin-to     0 file-read
        array:set itin-depart 0	file-read
        if file-at-end? = false [
          set next-driver file-read
          set this-itin true
          while [next-driver = this-driver] [  
            array:add itin-from	file-read
            array:add itin-to	file-read
            array:add itin-depart file-read
            ifelse file-at-end? [ set next-driver -1][ set next-driver file-read ]
          ] ; end while this-itin
        ]
      ] ; end create-drivers
    ] ; end while file-at-end
  ] ; end ifelse
  [ user-message (word "Input file '" driver-input-file "' not found!") ]
  file-close
end ;setup-itinerary

to setup-chargers
  print "setup-chargers"
  ; The charger level, location, and quantity of chargers was read in during setup-nodes.
  ; Now chargers of each level are created at the appropriate node.
  ; Charger-rate is currently a separate state variable from charger level. We may want to combine the two later, if
  ; we do not use "charger level" for anything else.

  foreach sort nodes [                       ; At each node, chargers equal to "taz-chargers"are created.
    create-chargers [taz-chargers-1] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set charger-level 1
      set charger-rate 2.4 ; Charger rate is the charger power in kW. Data from (Markel 2010), see project document
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
    
    create-chargers [taz-chargers-2] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set charger-level 2
      set charger-rate 19.2 ; Charger rate is the charger power in kW. Data from (Markel 2010), see project document
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
    
    create-chargers [taz-chargers-3] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set charger-level 3
      set charger-rate 30 ; Charger rate is the charger power in kW. Data from (Markel 2010), see project document
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
  ]  
  ask chargers [
    set available TRUE
    ]
  
end ;setup-chargers

to go
  dynamic-scheduler:go schedule 
end


;; define all subroutines
;; need:
;; -- TravelTime (schedules EVENT arrive)
;; -- arrive 
;;    -- if end of journey, remove from list
;;    -- for remaining vehicles, enter STATE not-charging

to check-charge

;; This submodel estimates the range of the EV. If the remaining-range is less than next-trip-range, returns a boolean need-to-charge? = false

  
    set next-trip-range array:item od-dist (([destination-taz] of self - 1) * 5 + [current-taz] of self - 1)
    let remaining-range ((state-of-charge) * (battery-capacity)) / (electric-fuel-consumption * safety-factor)
    ;; yields remaining range available in miles
    ; remaining range is high when soc is high, low when soc is low. ac 9/8
    if remaining-range > next-trip-range [set need-to-charge? false]
    if remaining-range <= next-trip-range [set need-to-charge? true
         ;print (word "next trip range = " next-trip-range)
         ]
    ;set minimum-acceptable-charge (elec-fuel-consump * next-trip-range) / batt-cap-mean
   ;???? if phev? = false [if minimum-acceptable-charge > 1 [set phev? true]]
 ;   print (word "next trip range = " next-trip-range)
   ; print (word "remaining range = " remaining-range)
    ;print (word "need to charge? " need-to-charge?)
 
  ;** adapted from find-minimum-charge
  ; To determine minimum-acceptable-charge, we set a local variable equal to the distance of the next trip, multiply that by electric-fuel-consumption to get the required
  ; kwh, and then divide by the battery capacity to get the required state-of-charge. Since the total-trip-dist and total-trip-times need to be set in "to depart" 
  ; so that cars will leave at the start of the day, I do not set those values here. 
end


to depart
  print "departing"
  ask drivers [
    if (status = "not-charging") and (ticks >= departure-time) [
;      ;; step 1 - does the driver check-charge?
      check-charge
      ifelse need-to-charge? = true [   ;; if the driver needs to charge, send to seek-charger
        ; will need to be able to charge en route; many destinations are greater than the max range. ac 9/8
      ]
      [   ;; if the driver does not need to charge, set to "traveling"
          ;; step 2 - when does the driver arrive?
        ;set total-trip-dist array:item od-dist (([destination-taz] of self - 1) * 5 + [current-taz] of self - 1)
        set total-trip-dist next-trip-range  ; temporary -- next-trip-range is the same, and is calculated in check-charge. ac 9/8
                                             ; is this value necessary? 
        set total-trip-time array:item od-time (([destination-taz] of self - 1) * 5 + [current-taz] of self - 1) ; units = hrs? ac 9/9
        let speed total-trip-dist / total-trip-time
        ;print (word "in depart, going from " [current-taz] of self " to " [destination-taz] of self)
        ;print (word "in depart, dist, time = " total-trip-dist ", " total-trip-time)
        ;print (word "in depart, speed = " speed)
        set arrival-time (ticks + total-trip-time)
        ;print (word "in depart, arrival-time (trip time), departure time = " arrival-time ", " ticks)
        set status "traveling"
        dynamic-scheduler:add schedule self task arrive arrival-time
        ;set color white
      ]
    ]
  ]
end
  
to arrive  ; **start here next time -- was working here on getting "arrive" to work. ac 9/8
  ask drivers [
    if status = "traveling" [
      ;print (word "sending to update-soc from arrive at time " ticks)
      update-soc
     set current-schedule-row current-schedule-row + 1
     carefully [  ;; *** needed here?  might be necessary for check-charge to work
       set current-taz destination-taz
       set destination-taz array:item itin-to current-schedule-row
       set departure-time array:item itin-depart current-schedule-row
       setxy [xcor] of node current-taz [ycor] of node current-taz   
       
     ;; determine if the driver will charge at its current location:
       check-charge
       ifelse need-to-charge? = true [ 
         ;; send to seek-charger
       ] 
       [ ;; send to depart -- add next departure time to master schedule
         set status "not-charging"
         dynamic-scheduler:add schedule self task depart departure-time
         ;print (word "new departure time for driver " self " = " departure-time)
       ]
     ]
     [  ; if the first block in 'carefully' does not work -- if the next row in the schedule does not exist (is this a problem now?) 
        ; -- this block is executed as a fail-safe.
       set status "not-charging" ;Driver is home again. Yay!
       set departure-time 99
       set color green
     ]
    ]
  ]
  
end

to update-soc
  ;print (word "entered update-soc")
  ask drivers [
    if status = "traveling" [
  ;print (word "soc before check = " state-of-charge)
      ;set travel-time (ticks - departure-time)  ; note for future: departure-time will correspond to the time on the master schedule
                                                ; at which a driver leaves a taz either after "arriving" or "charging" from changing
                                                ; the intended schedule. ac 9/8
      
      let speed (total-trip-dist / total-trip-time)
      ;;set travel-dist (speed * travel-time)
      if state-of-charge > 0 [
        set state-of-charge (state-of-charge - (total-trip-dist * electric-fuel-consumption) / battery-capacity)
        ] 
    ; State of charge = current soc - (miles traveled [mi] * efficiency (kwh/mi) / capacity (kwh))
  ;print (word "travel-time = " travel-time)
  ;print (word "ticks, departure = " ticks ", " departure-time)
  ;print (word "spead, trip dist, trip time = " speed ", " total-trip-dist ", " total-trip-time)
 
  ;print (word "soc is now = " state-of-charge)

    ]
    if status = "charging" [
    
    
    ]
  ]
end

;DECISION; check-charge:
;;         ***assumes itinerary is never complete**
;;         -- Is ChargeRange sufficient?
;;            Executes check-charge submodel, Section 5.5
;;            -- YES (include random yes) = goto STATE traveling
;;            -- NO = goto DECISION seek-charger

;DECISION; seek-charger:
;;         -- see submodel, Section 5.6
;;         -- NotFound? 
;;            -- goto STATE not-charging, request wait-time
;;         -- Found?
;;            -- goto STATE charging, request ChargeTime


;EventScheduler; itinerary:
;;               -- see submodel, Section 5.1
;;               -- schedule depart

;EventScheduler; wait-time:
;;               -- see submodel, Section 5.2
;;               -- schedule [a time in the future to either] depart -OR- retry-seek
;;                                                            (needs to be dummy variable)

;STATE; not-charging:
;; This will be the state that all drivers enter when parked -- waiting for a charging station, or to depart..
;;      -- if came from STATE traveling -OR- charging:
;;         -- enter EventScheduler itinerary:
;;           -- add itinerary step
;;           -- wait, then depart -> send to DECISION check-charge
;;      -- if came from DECISION seek-charger:
;;         -- enter EventScheduler wait-time:
;;           -- either depart or retry-seek (in wait-time)
;;           -- depart ;;;;-> send to DECISION check-charge
;;           -- retry-seek -> send to DECISION seek-charger
;;      -- if INITIALIZING, send to EventScheduler itinerary
;;         -- send to depart


;EventScheduler; ChargeTime:
;;               -- see submodel, Section 5.4
;;               -- schedule end-charge 

;STATE; charging:
;;      -- goto EventScheduler ChargeTime
;;      -- execute charging algorithm
;;         -- will they always charge to a "full" battery, or disengage prematurely?
;;      -- upon end-charge, enter STATE not-charging


;EventScheduler; TravelTime:
;;               -- see submodel, Section 5.3
;;               -- schedule event arrive

;STATE; traveling:
;;      -- execute EventScheduler TravelTime 
;;      -- upon arrive, enter STATE not-charging



;;in GO:
;;   -- initialize itinerary
;;   -- send to not-charging
@#$#@#$#@
GRAPHICS-WINDOW
375
19
775
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
25
0
24
0
0
1
ticks
30.0

INPUTBOX
149
41
304
101
n-nodes
5
1
0
Number

INPUTBOX
836
30
1071
90
alternative-input-file
../inputs/alternative_4_5.txt
1
0
String

INPUTBOX
26
45
129
105
batt-cap-mean
24
1
0
Number

INPUTBOX
25
111
123
171
batt-cap-stdv
1
1
0
Number

INPUTBOX
24
177
115
237
batt-cap-range
5
1
0
Number

INPUTBOX
24
305
134
365
fuel-economy-stdv
0.05
1
0
Number

INPUTBOX
24
369
141
429
fuel-economy-range
0.1
1
0
Number

INPUTBOX
837
97
1072
157
driver-input-file
../inputs/p1r1_5.txt
1
0
String

INPUTBOX
152
115
307
175
safety-factor
0.1
1
0
Number

BUTTON
163
267
229
300
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
165
317
228
350
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
NetLogo 5.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Alt5_batt-cap-std" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>total-satisfaction</metric>
    <metric>average-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>total-wait</metric>
    <steppedValueSet variable="batt-cap-stdv" first="0" step="0.5" last="4"/>
    <enumeratedValueSet variable="min-batt-cap">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-batt-cap">
      <value value="27"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-mean">
      <value value="0.34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-fuel-economy">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alternative">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-mean">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p2r1.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p4r1.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Alt5_bat_cap_range" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>total-satisfaction</metric>
    <metric>average-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>total-wait</metric>
    <steppedValueSet variable="batt-cap-range" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="alternative">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-mean">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-fuel-economy">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-stdv">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p2r1.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p4r1.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-mean">
      <value value="0.34"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Alt5_batt-cap-mean" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>total-satisfaction</metric>
    <metric>average-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>total-wait</metric>
    <enumeratedValueSet variable="alternative">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="batt-cap-mean" first="24" step="6" last="48"/>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-fuel-economy">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-stdv">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p1r2.txt&quot;"/>
      <value value="&quot;p1r3.txt&quot;"/>
      <value value="&quot;p1r4.txt&quot;"/>
      <value value="&quot;p1r5.txt&quot;"/>
      <value value="&quot;p2r1.txt&quot;"/>
      <value value="&quot;p2r2.txt&quot;"/>
      <value value="&quot;p2r3.txt&quot;"/>
      <value value="&quot;p2r4.txt&quot;"/>
      <value value="&quot;p2r5.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p3r2.txt&quot;"/>
      <value value="&quot;p3r3.txt&quot;"/>
      <value value="&quot;p3r4.txt&quot;"/>
      <value value="&quot;p3r5.txt&quot;"/>
      <value value="&quot;p4r1.txt&quot;"/>
      <value value="&quot;p4r2.txt&quot;"/>
      <value value="&quot;p4r3.txt&quot;"/>
      <value value="&quot;p4r4.txt&quot;"/>
      <value value="&quot;p4r5.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
      <value value="&quot;p5r2.txt&quot;"/>
      <value value="&quot;p5r3.txt&quot;"/>
      <value value="&quot;p5r4.txt&quot;"/>
      <value value="&quot;p5r5.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-mean">
      <value value="0.34"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Alt5_batt-cap-stdv-2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>total-satisfaction</metric>
    <metric>average-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>total-wait</metric>
    <enumeratedValueSet variable="alternative">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-range">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-mean">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="batt-cap-stdv" first="0" step="1" last="4"/>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p1r2.txt&quot;"/>
      <value value="&quot;p1r3.txt&quot;"/>
      <value value="&quot;p1r4.txt&quot;"/>
      <value value="&quot;p1r5.txt&quot;"/>
      <value value="&quot;p2r1.txt&quot;"/>
      <value value="&quot;p2r2.txt&quot;"/>
      <value value="&quot;p2r3.txt&quot;"/>
      <value value="&quot;p2r4.txt&quot;"/>
      <value value="&quot;p2r5.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p3r2.txt&quot;"/>
      <value value="&quot;p3r3.txt&quot;"/>
      <value value="&quot;p3r4.txt&quot;"/>
      <value value="&quot;p3r5.txt&quot;"/>
      <value value="&quot;p4r1.txt&quot;"/>
      <value value="&quot;p4r2.txt&quot;"/>
      <value value="&quot;p4r3.txt&quot;"/>
      <value value="&quot;p4r4.txt&quot;"/>
      <value value="&quot;p4r5.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
      <value value="&quot;p5r2.txt&quot;"/>
      <value value="&quot;p5r3.txt&quot;"/>
      <value value="&quot;p5r4.txt&quot;"/>
      <value value="&quot;p5r5.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-mean">
      <value value="0.34"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Alt5_fuel-econ-mean" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>total-satisfaction</metric>
    <metric>average-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>total-wait</metric>
    <enumeratedValueSet variable="alternative">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-range">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-mean">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-stdv">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p1r2.txt&quot;"/>
      <value value="&quot;p1r3.txt&quot;"/>
      <value value="&quot;p1r4.txt&quot;"/>
      <value value="&quot;p1r5.txt&quot;"/>
      <value value="&quot;p2r1.txt&quot;"/>
      <value value="&quot;p2r2.txt&quot;"/>
      <value value="&quot;p2r3.txt&quot;"/>
      <value value="&quot;p2r4.txt&quot;"/>
      <value value="&quot;p2r5.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p3r2.txt&quot;"/>
      <value value="&quot;p3r3.txt&quot;"/>
      <value value="&quot;p3r4.txt&quot;"/>
      <value value="&quot;p3r5.txt&quot;"/>
      <value value="&quot;p4r1.txt&quot;"/>
      <value value="&quot;p4r2.txt&quot;"/>
      <value value="&quot;p4r3.txt&quot;"/>
      <value value="&quot;p4r4.txt&quot;"/>
      <value value="&quot;p4r5.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
      <value value="&quot;p5r2.txt&quot;"/>
      <value value="&quot;p5r3.txt&quot;"/>
      <value value="&quot;p5r4.txt&quot;"/>
      <value value="&quot;p5r5.txt&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fuel-economy-mean" first="0.26" step="0.04" last="0.43"/>
  </experiment>
  <experiment name="pev135_alt135_Power" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count drivers</metric>
    <metric>count drivers with [phev? = true]</metric>
    <metric>count drivers with [status = "Stranded"]</metric>
    <metric>count drivers with [status = "Traveling"]</metric>
    <metric>count drivers with [status = "Waiting"]</metric>
    <metric>count drivers with [status = "Staging"]</metric>
    <metric>count drivers with [status = "Charging"]</metric>
    <metric>count drivers with [status = "Home"]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.1 and driver-satisfaction &gt; 0]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.4 and driver-satisfaction &gt;= 0.1]</metric>
    <metric>count drivers with [driver-satisfaction &lt; 0.7 and driver-satisfaction &gt;= 0.4]</metric>
    <metric>count drivers with [driver-satisfaction &lt;= 1 and driver-satisfaction &gt;= 0.7]</metric>
    <metric>sum [kWh-received] of drivers</metric>
    <metric>average-duty-factor</metric>
    <metric>level1-duty-factor</metric>
    <metric>level2-duty-factor</metric>
    <metric>average-charger-service</metric>
    <metric>mean [charger-service] of chargers with [charger-level = 1]</metric>
    <metric>mean [charger-service] of chargers with [charger-level = 2]</metric>
    <metric>count chargers with [available = true and charger-level = 1]</metric>
    <metric>count chargers with [available = true and charger-level = 2]</metric>
    <metric>sum [kWh-charged] of chargers</metric>
    <metric>total-wait</metric>
    <metric>total-satisfaction</metric>
    <metric>kw</metric>
    <enumeratedValueSet variable="driver-input-file">
      <value value="&quot;p1r1.txt&quot;"/>
      <value value="&quot;p1r2.txt&quot;"/>
      <value value="&quot;p1r3.txt&quot;"/>
      <value value="&quot;p1r4.txt&quot;"/>
      <value value="&quot;p1r5.txt&quot;"/>
      <value value="&quot;p3r1.txt&quot;"/>
      <value value="&quot;p3r2.txt&quot;"/>
      <value value="&quot;p3r3.txt&quot;"/>
      <value value="&quot;p3r4.txt&quot;"/>
      <value value="&quot;p3r5.txt&quot;"/>
      <value value="&quot;p5r1.txt&quot;"/>
      <value value="&quot;p5r2.txt&quot;"/>
      <value value="&quot;p5r3.txt&quot;"/>
      <value value="&quot;p5r4.txt&quot;"/>
      <value value="&quot;p5r5.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-mean">
      <value value="0.34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-range">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bev-charge-anyway">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alternative">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-step-size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-nodes">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-batt-cap">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-fuel-economy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="safety-factor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-economy-stdv">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-mean">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="phev-charge">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="batt-cap-stdv">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
