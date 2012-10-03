extensions [dynamic-scheduler]
__includes["setup.nls"]

globals [    
  od-from
  od-to
  od-demand
  od-dist
  od-time
  
  n-nodes
  
  schedule  ;; this global variable holds the dynamic schedule for the PEVI program, appended by the drivers from their itineraries
  
;; FILE PATHS
  parameter-file
  charger-input-file
  driver-input-file
  od-input-file
  
;; PARAMETERS
  charge-safety-factor
  wait-time-mean
  
  ;; globals needed for testing
  test-driver
]

breed [drivers driver]
breed [chargers charger]
breed [nodes node]

drivers-own [
;; VEHICLE
  vehicle-type              ; e.g. 'leaf' or 'volt'
  is-phev?                  
  battery-capacity          ; kwh
  electric-fuel-consumption ; kwh / mile
  hybrid-fuel-consumption   ; gallon / mile, for phev charge sustaining mode
  
;; DEMOGRAPHY  
  home-taz
  probability-of-unneeded-charge

;; OPERATION
  state                     ; discrete string value: not-charging, traveling, charging
  current-taz               ; nobody if traveling
  destination-taz
  state-of-charge
  current-charger           ; nobody if not charging
  
  itin-from 
  itin-to
  itin-depart
  itin-trip-type
  itin-change-flag
  itin-delay-amount
  current-itin-row          ; index of current location in the itinerary (referring to next trip or current trip if traveling)

;; CONVENIENCE VARIABLES
;; these are used to implement the model, but are in the model description
  trip-time 
  travel-time 
  trip-distance
  journey-distance
  remaining-range
  travel-dist ; used to store distance driven since departure
  departure-time ;When the vehicle is set to leave the taz
  arrival-time ; when a car is supposed to arrive
  minimum-acceptable-charge ; The charge required to reach the next destination
  itin-complete?
  charger-in-origin-or-destination
  time-until-depart
  trip-charge-time-need
  journey-charge-time-need
  full-charge-time-need
  time-until-end-charge
  willing-to-roam-time-threshold  ;; added 9.28 ac
  willing-to-roam?

;; TRACKING
  num-denials
  
;; CANDIDATE ADDITIONS TO MODEL DESCRIPTION
  kwh-received ; a count of how much energy each driver has charged

]

chargers-own[
  location         ; TAZ # for each charger
  current-driver   ; driver currenlty being serviced, nobody indicates charger is available
  charger-type     ; 1, 2, or 3
  charge-rate      ; kWh / hr
  num-sessions     ; count of charging sessions
  energy-delivered ; kWh
 ]

nodes-own[
  id              ; TAZ id
  chargers-in-taz ; list of all non-home chargers
  home-charger    ; special charger available to all drivers when in their home taz
  drivers-in-taz  ; list of drivers currently in TAZ
  
  n-levels        ; list containing the number of chargers for levels 1,2,3 at index 0,1,2
]

;;;;;;;;;;;;;;;;;;;;
;; SETUP
;;;;;;;;;;;;;;;;;;;;
to setup
  print "setting up"
  __clear-all-and-reset-ticks
 
  set schedule dynamic-scheduler:create
   
  create-turtles 1 [ setxy 0 0 set color black] ;This invisible turtle makes sure we start at node 1 not node 0
  
  set parameter-file "params.txt"
  read-parameter-file
  
  setup-od-data
  setup-nodes
  setup-drivers
  setup-chargers
end 

to go
  dynamic-scheduler:go-until schedule 10
end

;;;;;;;;;;;;;;;;;;;;
;; NEED TO CHARGE
;;;;;;;;;;;;;;;;;;;;
to-report need-to-charge [calling-event]
  set remaining-range (state-of-charge * battery-capacity / electric-fuel-consumption )

  ifelse ( (calling-event = "arrive" and remaining-range < journey-distance * charge-safety-factor) or 
           (calling-event = "depart" and remaining-range < trip-distance * charge-safety-factor) )[ ; TODO add random draw here for people who charge but don't need to
    report true
    print (word precision ticks 3 " " self " NEEDS TO CHARGE! ")
  ][
    report false
  ]
;  ifelse ( (remaining-range < journey-distance * charge-safety-factor) or   
;           (remaining-range < trip-distance * charge-safety-factor) )[ ; TODO add random draw here for people who charge but don't need to
;    report true
;  ][
;    report false
;  ]

end

;;;;;;;;;;;;;;;;;;;;
;; RETRY SEEK
;;;;;;;;;;;;;;;;;;;;
to retry-seek
  print (word precision ticks 3 " " self " retry-seek ")
  ;;set time-until-depart departure-time - ticks  ;; this must be the "correct" departure time -- adjusted if necessary
  ;; not necessary here -- immediately executes this command in seek-charger
  seek-charger
end

;;;;;;;;;;;;;;;;;;;;
;; SEEK CHARGER
;;;;;;;;;;;;;;;;;;;;
to seek-charger
  print (word "ENTER SEEK-CHARGER")
  print (word precision ticks 3 " " self " seek-charger ")
  set time-until-depart departure-time - ticks
  
  
  ;; submodel action 1:
  ifelse time-until-depart < willing-to-roam-time-threshold [
    set willing-to-roam? true
  ]
  [
    set willing-to-roam? false
  ]
    
  ;; submodel action 2:
  ifelse willing-to-roam? [
      ;; driver will roam to find charger -- looks at ALL CHARGERS
      ;; 1st -- look at chargers in current TAZ
    foreach [chargers-in-taz] of current-taz [
      if [current-driver] of ? = nobody [
        print (word precision ticks 3 " " self " found " ?)
        ask ? [
          set current-driver myself 
        ]
        set current-charger ?
        charge-time-event-scheduler
        stop
      ] 
      ;; 2nd -- look at chargers en-route from current TAZ to destination TAZ
      ;; how do we find the TAZs en-route?
      ;; 1. what is the maximum distance the driver can travel?
      ;; = trip-distance, set in UPDATE-ITINERARY, executed in ARRIVE
      ;; 2. current charge = ___
      ;; 3. 
      ;; 4. what are the TAZs the driver will travel through?
      ;foreach [chargers-in-taz] of destination-taz [  NO -- driver cannot reach destination-taz!
      
      ;; 3rd -- look at all other chargers in neighboring TAZs within acceptable range
    ]
  ] 
  [   
      ;; driver will NOT roam to find charger -- looks only in current TAZ
    foreach [chargers-in-taz] of current-taz [
      if [current-driver] of ? = nobody [
        print (word precision ticks 3 " " self " found " ?)
        ask ? [
          set current-driver myself 
        ]
        set current-charger ?
        charge-time-event-scheduler

        print (word "EXIT SEEK-CHARGER")
        stop
      ] 
    ]                        
  ]
    
  if current-charger = nobody [  
    print (word precision ticks 3 " " self " no charger found ") 
    wait-time-event-scheduler  
  ]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WAIT TIME EVENT SCHEDULER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to wait-time-event-scheduler
  print (word precision ticks 3 " " self " wait-time-event-scheduler remaining-range:" remaining-range 
    " trip-distance:" trip-distance " time-until-depart:" time-until-depart)
  set state "not charging"
  ifelse remaining-range / charge-safety-factor < trip-distance [
    dynamic-scheduler:add schedule self task retry-seek ticks + wait-time-mean ;; TODO make wait-time-mean random draw
  ][
    ifelse remaining-range / charge-safety-factor >= journey-distance or time-until-depart <= 1 [
      print (word precision ticks 3 " " self " in wait-time-event-sched, deciding to depart at time " (ticks + time-until-depart))
      dynamic-scheduler:add schedule self task depart (ticks + time-until-depart)
      print (word "in WAIT-TIME-EVENT-SCHED")
      print (word precision ticks 3 " " self " departure scheduled: " departure-time " itin: " itin-depart)
  
    ][
      dynamic-scheduler:add schedule self task retry-seek ticks + wait-time-mean ;; TODO make wait-time-mean random draw
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHARGE TIME EVENT SCHEDULER   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time-until-depart set in SEEK CHARGER = departure-time - ticks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to charge-time-event-scheduler
  set state "charging"
  set charger-in-origin-or-destination true  ; TODO this needs to be updated once en-route/neighbor charging is implemented
  set trip-charge-time-need max sentence 0 ((trip-distance * charge-safety-factor * electric-fuel-consumption - state-of-charge * battery-capacity) / [charge-rate] of current-charger)
  set journey-charge-time-need max sentence 0 ((journey-distance * charge-safety-factor * electric-fuel-consumption - state-of-charge * battery-capacity) / [charge-rate] of current-charger)
  set full-charge-time-need (1 - state-of-charge) * battery-capacity / [charge-rate] of current-charger
  ifelse full-charge-time-need < trip-charge-time-need [  ;; if sufficent time to charge to full
    set time-until-end-charge full-charge-time-need
  ]
  [                                                      ;; not sufficient time to charge to full
    ifelse time-until-depart < trip-charge-time-need [   ;; if NOT sufficient time to charge to reach next destination
      set time-until-end-charge trip-charge-time-need    ;; -->
                                                         ;; WILL need to update departure time
      print (word precision ticks 3 " " self " old itinerary (pre end-charge): " itin-depart)
      change-depart-time (time-until-end-charge + ticks)
      print (word precision ticks 3 " " self " new itinerary (pre end-charge): " itin-depart)
      print (word precision ticks 3 " ticks: " ticks " + time-until-end-charge: " time-until-end-charge " = " (time-until-end-charge + ticks))
    ]
    [                                                    ;; sufficient time to charge to before next departure
                                                         ;;
                                                         ;; don't need to update departure time
      ifelse charger-in-origin-or-destination [          ;; TRUE right now -- charger is in origin
        set time-until-end-charge min sentence time-until-depart full-charge-time-need 
        
        ;print (word precision ticks 3 " " self " old itinerary (pre end-charge): " itin-depart)
        ;change-depart-time (time-until-end-charge + ticks)
        ;print (word precision ticks 3 " " self " new itinerary (pre end-charge): " itin-depart)
        ;print (word precision ticks 3 " ticks: " ticks " + time-until-end-charge: " time-until-end-charge " = " (time-until-end-charge + ticks))      
      ]
      [                                                  ;; NOT in od -- en-route?
        ifelse [charger-type] of current-charger = 3 [   ;; charge for full journey
          set time-until-end-charge min sentence time-until-depart journey-charge-time-need
        ]
        [                                                ;; only charge what you need
          set time-until-end-charge min sentence time-until-depart trip-charge-time-need
        ]
      ]
    ]
  ]
  
  print (word precision ticks 3 " " self " charger-type = " [charger-type] of current-charger)
  ;; if change-depart here?
  ifelse [charger-type] of current-charger < 3 and time-until-end-charge < trip-charge-time-need [
    print (word "time-until-end-charge: " time-until-end-charge ", trip-charge-time-need: " trip-charge-time-need ", time-until-depart: " time-until-depart)
    dynamic-scheduler:add schedule self task retry-seek ticks + min (sentence wait-time-mean (time-until-depart - 0.5));; TODO make wait-time-mean random draw with max of time-until-depart - 0.5
    print (word precision ticks 3 " " self " retry seek in min of " wait-time-mean " and " precision (time-until-depart - 0.5) 3)
  ]
  [
    ifelse [charger-type] of current-charger < 2 and time-until-end-charge < journey-charge-time-need [
      dynamic-scheduler:add schedule self task end-charge ticks + min (sentence wait-time-mean (time-until-depart - 0.5))
      print (word precision ticks 3 " " self " charging for min of " wait-time-mean " and " precision (time-until-depart - 0.5) 3)
    ]
    [
      dynamic-scheduler:add schedule self task end-charge ticks + time-until-end-charge
      print (word precision ticks 3 " " self " charging for " precision time-until-end-charge 3)
    ]
  ]
  
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGE DEPARTURE TIME
;;;;;;;;;;;;;;;;;;;;;;;;;

to change-depart-time [new-depart-time]
  ;print (word "ENTER CHANGE-DEPART-TIME")
  set itin-depart replace-item current-itin-row itin-depart new-depart-time
  print (word precision ticks 3 " " self " new-depart-time: " new-depart-time " new itin-depart: " itin-depart)      
  if current-itin-row < (length itin-depart - 1)[
    foreach n-values (length itin-depart - current-itin-row - 1) [current-itin-row + ? + 1] [ change-depart-time-row ?  ]
  ]
  ;print (word "EXIT CHANGE-DEPART-TIME")
end

to change-depart-time-row [row-num]
  if item row-num itin-depart < item (row-num - 1) itin-depart[
    set itin-depart replace-item row-num itin-depart (0.5 + item (row-num - 1) itin-depart) ;; TODO make sub-model about how itin is adjusted when multiple trips are impacted
  ]
end

;;;;;;;;;;;;;;;;;;;;
;; END CHARGE
;;;;;;;;;;;;;;;;;;;;

  ;; AC doing work here on 10.2 --> need to look into the effect of changing departure time upon end-charge.
                                   ;previous change-depart only changes the depart time BEFORE retry-seek executed.
                                   ;need to also account for multiple retry-seeks, and then an update of change-depart, if necessary

to end-charge
  set state-of-charge (state-of-charge + time-until-end-charge * [charge-rate] of current-charger / battery-capacity)
  print (word precision ticks 3 " " self " ending charge soc:" precision state-of-charge 3)
  ask current-charger [ set current-driver nobody ]
  set current-charger nobody
  ;if (departure-time < ticks)[  ;; added 10.2 ac
  ;  change-depart-time ticks
  ;  set departure-time item current-itin-row itin-depart
  ;]
  ;print (word "CHANGED DEPART TIME AT END-CHARGE")
  itinerary-event-scheduler
  print (word precision ticks 3 " " self " new itinerary (end-charge): " itin-depart)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITINERARY EVENT SCHEDULER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to itinerary-event-scheduler
  set state "not-charging"
  ;if (departure-time < ticks)[    ;; no longer necessary? depart-time is "corrected" in UPDATE-ITINERARY
                                   ;; it's possible the departure time will need to be "corrected" again if the driver
                                   ;; has had to wait a long time for a charger.
  ;  print (word "ENTER ITINERARY-EVENT-SCHEDULER (schedule depart)")
  ;  print (word precision ticks 3 " " self " need to change old departure time:" item (current-itin-row - 1) itin-depart " to new depart time (now):" ticks)
  ;  print (word precision ticks 3 " " self " old itinerary:" itin-depart)      
  ;  change-depart-time ticks  
  ;  set departure-time item current-itin-row itin-depart
  ;  ; print (word precision ticks 3 " " self " need to change depart time to: " precision departure-time 3)
  ;  print (word precision ticks 3 " " self " new itinerary:" itin-depart)
  ;  print (word "EXIT ITINERARY-EVENT-SCHEDULER (schedule depart)")
  ;]  
  
  dynamic-scheduler:add schedule self task depart departure-time
  print (word "in ITIN-EVENT-SCHED")
  print (word precision ticks 3 " " self " departure scheduled: " departure-time " itin: " itin-depart)
end

;;;;;;;;;;;;;;;;;;;;
;; DEPART
;;;;;;;;;;;;;;;;;;;;
to depart
  print (word precision ticks 3 " " self " departing, soc:" state-of-charge)
  ;print (word precision ticks 3 " " self " itinerary (upon departure):" itin-depart)
  ifelse need-to-charge "depart" [  ;; do drivers need to seek charge upon departing?
                                    ;; --> does the driver have enough charge to travel the next leg of the journey? (trip-distance of journey-distance)
                                    ;; shouldn't this be executed alongside the check for ample charge to complete journey-distance?
                                    ;; otherwise, the driver may need to alter the itinerary because he checks his charge upon departure
    ifelse state-of-charge = 1 [ 
      print (word precision ticks 3 " " self " cannot make trip with full battery") ;; TODO this shouldn't happen when PHEV are implemented
    ][
      seek-charger   
    ]
  ]
  [  
    travel-time-event-scheduler  ;; schedules next arrival to occur
    ;print (word precision ticks 3 " " self " next arrival time: " precision arrival-time 3)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAVEL TIME EVENT SCHEDULER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to travel-time-event-scheduler
  set state "traveling"
  ;print (word "ENTER TRAVEL-TIME-EVENT-SCHEDULER (schedule arrive)")
  set trip-time item my-od-index od-time
  set arrival-time (ticks + trip-time)
  dynamic-scheduler:add schedule self task arrive arrival-time
  print (word "next arrival time: " precision arrival-time 3)
  ;print (word "EXIT TRAVEL-TIME-EVENT-SCHEDULER (schedule arrive)")
end

;;;;;;;;;;;;;;;;;;;;
;; ARRIVE
;;;;;;;;;;;;;;;;;;;;
to arrive
  set state-of-charge (state-of-charge - trip-distance * electric-fuel-consumption / battery-capacity)
  set journey-distance journey-distance - trip-distance
  print (word precision ticks 3 " " self " arriving, trip-distance: " trip-distance " soc:" state-of-charge " elec-fc:" electric-fuel-consumption " cap" battery-capacity)
  update-itinerary 
      
  if not itin-complete? [
   
    ifelse need-to-charge "arrive" [   ;; if CHARGE NEEDED, seek-charger
      ifelse state-of-charge = 1 [ 
        print (word precision ticks 3 " " self " cannot make trip with full battery") ;; TODO this shouldn't happen when PHEV are implemented
      ]
      [
        seek-charger  ;; TODO need to re-schedule departure if schedule is delayed by seeking a charger. ac 9.28
      ]
    ]
    [
      itinerary-event-scheduler  ;; if NO CHARGE NEEDED, schedules next departure to occur     
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;
;; UPDATE ITINERARY
;;;;;;;;;;;;;;;;;;;;
to update-itinerary
  ifelse (current-itin-row + 1 < length itin-from) [
    set current-itin-row current-itin-row + 1
    set current-taz node item current-itin-row itin-from
    set destination-taz node item current-itin-row itin-to
    if ((item current-itin-row itin-depart) < ticks)[
      print (word precision ticks 3 " " self " need to change old departure time:" item (current-itin-row - 1) itin-depart " to new depart time (now):" ticks)
      print (word precision ticks 3 " " self " old itinerary:" itin-depart)      
      change-depart-time ticks  
      print (word precision ticks 3 " " self " new itinerary:" itin-depart)
    ]
    set departure-time item current-itin-row itin-depart
    set trip-distance item my-od-index od-dist
  ]
  [
    set itin-complete? true
  ]
end

to-report distance-from-to [from-taz to-taz]
  report item ((from-taz - 1) * n-nodes + to-taz - 1 ) od-dist
end
to-report od-index [destination source]
  report ((destination - 1) * n-nodes + source - 1)
end

to-report my-od-index
  report (([id] of destination-taz - 1) * n-nodes + [id] of current-taz - 1)
end

to-report driver-soc [the-driver]
  report [state-of-charge] of the-driver
end
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
147
86
257
146
fuel-economy-stdv
0.05
1
0
Number

INPUTBOX
147
150
264
210
fuel-economy-range
0.1
1
0
Number

BUTTON
145
41
211
74
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
224
43
287
76
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
