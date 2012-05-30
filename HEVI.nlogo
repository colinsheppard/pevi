;; hi, bug fix

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
                  ; 1 - Origin TAZ
                  ; 2 - Destination TAZ
                  ; 3 - Number of 24hr trips from 1 to 2 minus am trips, scaled for pev penetration
                  ; 4 - Distance in miles
                  ; 5 - Distance in travel time (minutes)

  
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
      ; if soc is calculated from travel-dist and fuel-economy, is it a state variable still?
  fuel-economy ; energy required to travel 1 mile (kWh/mile)
  minimum-acceptable-charge ; The charge required to reach the next destination
  arrival-time ; when a car is supposed to arrive
  schedule ; a matrix of: (1)from/current TAZ, (2)to/destination taz, (3)departure time. One row for each trip. (row 1, col 1) is the home TAZ
  status ; discrete value from list:Home,Traveling,Staging,Charging,Waiting,Stranded
  ;destination-number ;keep track of which destination the vehicle is located
  current-taz ; keeps track of the current location of each vehicle
  current-schedule-row ; keeps track of which row in the schedule each driver is on
  destination-taz ;from the schedule, this is where we're going
  departure-time ;When the vehicle is set to leave the taz
  travel-dist ; used to store distance driven since departure
  travel-time ; used to store traveling time since departure
  kWh-received ; a count of how much energy each driver has charged
  wait-time
  phev?  ;boolean variable 
]

breed [chargers charger]
chargers-own[
  taz-location ; where the charger is located
  available ; boolean to represent either available(TRUE) or occupied(FALSE)
  charger-level ; Sets the charger level of each charger
  charger-rate ; The rate of recharge for each charger.
  charger-in-use ; A counter increased every time the charger is in use. Used to calculate duty factor. (Not attached to the name -ah)
  duty-factor ; The fraction of time a charger is in use vs. time idle  
  charger-service ; a counter for the number of drivers each charger services
  kWh-charged ; a count of how much energy a charger has given
 ]

breed [nodes node]
nodes-own[
  taz-id ; a unique integer identifier for each taz
  time-and-distance ;matrix containing time and distance info for every other node
  taz-chargers-lvl-1 ; the number of level 1 chargers in a given taz
  taz-chargers-lvl-2 ; the number of level 2 chargers in a given taz
  taz-chargers-lvl-3 ; the number of level 3 chargers in a given taz

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
  
  create-turtles 1 [ setxy 0 0 set color black] ;This invisible turtle makes sure we start at node 1 not node 0
  setup-od-matrix 
  setup-nodes
  setup-drivers
  setup-chargers
 
end ;setup
  
to setup-od-matrix 
  ; Reads in main driver input file: Origin, destination, # of trips, distance, time
  set od matrix:make-constant (n-nodes * n-nodes) 5 0 
  
  ifelse (file-exists? "test_input.txt") [
    file-close
    file-open "test_input.txt"
    foreach n-values n-nodes [?] [
      let $from ?
      foreach n-values n-nodes [?] [
        let $row ($from * n-nodes + ?)
        matrix:set-row od $row (list file-read file-read (round file-read) file-read (file-read / 60))
        ; Reads in: origin, destination, number of trips (modified by am trips and PEV penetration and rounded to nearest integer), drive distance, drive time
        ; Drive time is in the file as minutes, so to get into hours, we divide by 60 here.
      ]
    ]
    file-close
  ]
  [ user-message "Input file not found in current directory!" ]

end 
to setup-drivers
  ; creating drivers based on GEATM data, in setup-schedule procedure. 
  setup-schedule
  ask drivers [
    set shape "car"
    set color green
    set size 2
    set driver-satisfaction 1  ;initialize driver satisfaction
    ; let battery capacity deviate a little bit but no less than 20 kWh
    ifelse phev? = false [
      ; set randomness of battery capacity
      set battery-capacity min ( list max (list (batt-cap-mean - batt-cap-range) random-normal batt-cap-mean batt-cap-stdv) (batt-cap-mean + batt-cap-range)) 
      ; set randomness of fuel economy
      set fuel-economy max ( list min (list (fuel-economy-mean - fuel-economy-range) random-normal fuel-economy-mean fuel-economy-stdv) (fuel-economy-mean + fuel-economy-range) )
    ]
    [ set battery-capacity phev-batt-cap
      set fuel-economy phev-fuel-economy
    ]
    set state-of-charge 1
    set status "Home"
    set partner nobody
    find-minimum-charge
  ]
end ;setup-drivers

to setup-schedule
  
  ifelse (file-exists? driver-input-file) [
    file-close
    file-open driver-input-file
    let index1 0
    let index2 0
    let dummy-logic false
    
    while [file-at-end? = false] [
      set index1 index1 + 1
      if index1 = 1 [let dummy file-read]
      create-drivers 1 [
        set phev? false
        set dummy-logic true
        set schedule matrix:make-constant 15 3 99
        set index2 0
        while [dummy-logic] [
          set current-taz file-read
          set destination-taz file-read
          matrix:set-row schedule index2 (list current-taz destination-taz file-read)
          find-minimum-charge
          let dummy-read (file-read)
          set index2 index2 + 1
          ifelse (file-at-end? = false) [
           let next-driver file-read
            if next-driver != index1 [
              set dummy-logic false
            ]
          ]
          [set dummy-logic false]
        ]
        
      ]
    ]
  ]
  [ user-message "Input file not found in current directory!" ]
  
  ;Now each driver has a schedule. Let's place them where they need to be.
  
  ask drivers [
    set current-taz matrix:get schedule 0 0 
    set destination-taz matrix:get schedule 0 1 
    set departure-time matrix:get schedule 0 2
    setxy [xcor] of node current-taz [ycor] of node current-taz   
  ]
    ;; user-message "File loading complete!"
    ;; Done reading in schedule.  Close the file.
    file-close
end ;setup-schedule

to setup-nodes
  create-nodes n-nodes
      ask nodes [
    set shape "star"
    set color yellow
    set size 0.5
      ]
      ; Select the TAZ input file based on the alternative chooser on the interface. If the file does not exist, stop.
    if alternative = 0 [  
      file-close ; is this really necessary?
      file-open "alternative_0.txt" ]
    if alternative = 1 [  
      file-close ; is this really necessary?
      file-open "alternative_1.txt" ]
    if alternative = 2 [  
      file-close ; is this really necessary?
      file-open "alternative_2.txt" ]
    if alternative = 3 [  
      file-close ; is this really necessary?
      file-open "alternative_3.txt" ]
    if alternative = 4 [  
      file-close ; is this really necessary?
      file-open "alternative_4.txt" ]
    if alternative = 5 [  
      file-close ; is this really necessary?
      file-open "alternative_5.txt" ]
         
      foreach sort nodes[ ;this block reads taz-id, location, and charger info into each node
        ask ? [
        set taz-id file-read
        setxy file-read file-read
        set taz-chargers-lvl-1 file-read ; Sets the number of level 1 chargers in each node
        set taz-chargers-lvl-2 file-read ; Sets the number of level 2 chargers in each node
        set taz-chargers-lvl-3 file-read ; Sets the number of level 3 chargers in each node
        ]    
      ]

   
end ;setup-nodes

to setup-chargers
  
  ; The charger level, location, and quantity of chargers was read in during setup-nodes.
  ; Now chargers of each level are created at the appropriate node.
  ; Charger-rate is currently a separate state variable from charger level. We may want to combine the two later, if
  ; we do not use "charger level" for anything else.

  foreach sort nodes [                       ; At each node, chargers equal to "taz-chargers"are created.
      create-chargers [taz-chargers-lvl-1] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set charger-level 1
      set charger-rate 2.4 ; Charger rate is the charger power in kW. Data from (Markel 2010), see project document
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
      
      create-chargers [taz-chargers-lvl-2] of ? [  ; The location of each charger created is then set as the current TAZ location
      set shape "Circle 2"
      set color red
      set size 1
      set charger-level 2
      set charger-rate 19.2 ; Charger rate is the charger power in kW. Data from (Markel 2010), see project document
      set taz-location [taz-id] of ?
      set xcor [xcor] of ?
      set ycor [ycor] of ?]
      
      create-chargers [taz-chargers-lvl-3] of ? [  ; The location of each charger created is then set as the current TAZ location
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

  ; Advance the time and update time variables
  tick
  set time time + (time-step-size / 60)  ; Convert time step to hours and add to current time
  if time > stop-hour [ 
    stop ]           ; stop the simulation
  
  update-display-time
   done-traveling
   query-chargers
   finish-charging
   wait-or-not
   depart
   update-variables
   update-custom-plots
 
;  wait 1  ; This temporary statement pauses execution so you can see the time on the display.
  
end ;go

to update-display-time
  
  let hour floor time
  let minutes round ((time - hour) * 60)
  set display-time (word hour ":" minutes)
end

to update-variables
  ask drivers [
    update-soc
  ]
end ;update-variables

to-report total-satisfaction 
  
  ; For now, total-satisfaction is an average of all driver's satisfaction   
  report mean [driver-satisfaction] of drivers with [phev? = false]
  
end ;total-satisfaction

to-report average-duty-factor
  ; Each charger has a unique duty factor - average them here
  ifelse (count chargers > 0)
    [report mean [duty-factor] of chargers]
    [report 0]
end ;average-duty-factor

to-report level1-duty-factor
  ; Each charger has a unique duty factor - average them here
  ifelse (count chargers with [charger-level = 1] > 0)
    [report mean [duty-factor] of chargers with [charger-level = 1]]
    [report 0]
end ;average-duty-factor

to-report level2-duty-factor
  ; Each charger has a unique duty factor - average them here
  ifelse (count chargers with [charger-level = 2] > 0)
    [report mean [duty-factor] of chargers with [charger-level = 2]]
    [report 0]
end ;average-duty-factor

to-report level3-duty-factor
  ; Each charger has a unique duty factor - average them here
  ifelse (count chargers with [charger-level = 3] > 0)
    [report mean [duty-factor] of chargers with [charger-level = 3]]
    [report 0]
end ;average-duty-factor

to-report average-charger-service
  ifelse (count chargers > 0)
    [report mean [charger-service] of chargers]
    [report 0]
end ;average-charger-service

to-report total-wait
  report sum [wait-time] of drivers
end ;wait time

to-report kw
  report sum [charger-rate] of (chargers with [available = false])
end

to done-traveling
    ask drivers [ 
    if status = "Traveling" [

      if time >= arrival-time [
        ; If we've arrived, change status and location
        setxy [xcor] of node destination-taz [ycor] of node destination-taz   
        
        ; We've arrived. Next: Set our next destination. We can read in our current-taz from our destination-taz,
        ; but the next destiantion and arrival time is based on our schedule. 
        
        set current-schedule-row current-schedule-row + 1 ; Set driver to the next schedule
        
        carefully [ ; If the next row in the schedule matrix does not exist, "carefully" will supress the error and send them home. ah 3.25
          set current-taz destination-taz ; Resets the current-taz. "Carefully" won't hit until the next line, so we don't need to set the current-taz in the next block.
          set destination-taz matrix:get schedule current-schedule-row 1
          set departure-time matrix:get schedule current-schedule-row 2
          
          if phev? = false [ ; added 4-10 dh, reassign bat capacity and fuel economy
            set fuel-economy max ( list min (list (fuel-economy-mean - fuel-economy-range) random-normal fuel-economy-mean fuel-economy-stdv) (fuel-economy-mean + fuel-economy-range) )
          ]          
          
          ifelse departure-time < 99 [
            find-minimum-charge
            ifelse state-of-charge > minimum-acceptable-charge + safety-factor and phev? = false [
              ifelse random-bernoulli bev-charge-anyway [ ; 4-9 added this ifelse- dh
                set status "Waiting"
                set color red
              ] [ set status "Staging" set color blue]
                
            ] [ ; Changed the double if statement to an ifelse - we either have the charge, or we don't.
            set status "Waiting" 
            set color red
            ]
            if phev? = true [ ; added 4-9 dh
              ifelse random-bernoulli phev-charge [
                set status "Waiting"
              ]
              [ set status "Staging" ]
            ]
          ] 
          [
            set status "Home" ;Driver is home again. Yay! 
            set color green
          ] 
                    
        ] [
          set status "Home" ;Driver is home again. Yay!
          set departure-time 99
          set color green
        ]
       
        ] 
    ]
  ]
end ;done-traveling

to query-chargers ;Jb added 3.19, modified by ah 3.25
  ask nodes[ ; 3.25 ah: Changed from "foreach" loop to "ask" loop
    let available-chargers chargers with [available = TRUE and taz-location = [taz-id] of myself] 
    let waiting-drivers drivers with [status = "Waiting" and current-taz = [taz-id] of myself]
    ifelse count waiting-drivers <= count available-chargers [
      while [count waiting-drivers > 0 and count available-chargers > 0] [
        ; Start with the level 3 chargers - have them ask available drivers to pair up.
        ask available-chargers with [taz-location = [taz-id] of myself and charger-level = 3] [
          if count waiting-drivers > 0 [
          ask one-of waiting-drivers [
            set status "Charging"
            set color orange
            ; Once we have the driver, pair them with the partner
            set partner myself
            ask partner [set available FALSE set charger-service charger-service + 1]
            ; Reset available-chargers and waiting drivers to trigger the while loop
            set available-chargers chargers with [available = TRUE and taz-location = [current-taz] of myself] 
            set waiting-drivers drivers with [status = "Waiting" and current-taz = [current-taz] of myself]
          ]
        ]
        ]
        ; Rinse and repeat
        ask available-chargers with [taz-location = [taz-id] of myself and charger-level = 2] [
          if count waiting-drivers > 0 [
          ask one-of waiting-drivers [
            set status "Charging"
            set color orange
            ; Once we have the driver, pair them with the partner
            set partner myself
            ask partner [set available FALSE set charger-service charger-service + 1]
            set available-chargers chargers with [available = TRUE and taz-location = [current-taz] of myself] 
            set waiting-drivers drivers with [status = "Waiting" and current-taz = [current-taz] of myself]
          ]
        ]
        ]

        ask available-chargers with [taz-location = [taz-id] of myself and charger-level = 1] [
          if count waiting-drivers > 0 [
          ask one-of waiting-drivers [
            set status "Charging"
            set color orange
            ; Once we have the driver, pair them with the partner
            set partner myself
            ask partner [set available FALSE set charger-service charger-service + 1]
            set available-chargers chargers with [available = TRUE and taz-location = [current-taz] of myself] 
            set waiting-drivers drivers with [status = "Waiting" and current-taz = [current-taz] of myself]
          ]
        ]
      ]
    ]
    ]
      [
       ask available-chargers [ ; 3.25 ah: Changed from "foreach" loop to "ask" loop
         set available FALSE
         set charger-service charger-service + 1
         ask one-of waiting-drivers with [status = "Waiting"] [
           set status "Charging"
           set partner myself
         ]
       ]
      
      ask waiting-drivers with [status = "Waiting"] [ ; 3.25 ah: Changed from "foreach" loop to "ask" loop
         set driver-satisfaction (driver-satisfaction * 0.99 ^ time-step-size)
         set wait-time wait-time + time-step-size
      ]
    ]
  ]         
end ;query-chargers

to finish-charging ;added by dh and zs 3-20
  ask drivers with [status = "Charging"] [
      ifelse state-of-charge >= 0.99 [; If you've got full charge, stop charging.
      ask partner [set available TRUE]
      set partner nobody
      set status "Staging"
    ][
    if time >= departure-time [ ; First, check and see if it is time to leave.
      ifelse state-of-charge >= minimum-acceptable-charge + safety-factor or phev? = true [ ;If you need to go and have the charge, go.
        ask partner [set available TRUE]
        set partner nobody
        set status "Staging"
        set color blue
      ] [
        set driver-satisfaction driver-satisfaction * 0.98 ^ time-step-size ; If you need to keep charging to get home, punish driver satisfaction.
;        if driver-satisfaction < 0.5 [set status "Stranded" set driver-satisfaction 0 set color grey]   
      ]
    ]
  ]]    
end ;finish-charging

to wait-or-not 
  
end ;wait-or-not

to depart
  ; do we need these local variables or should we just modify the current schedule variable
  ask drivers [
    if (status = "Home") or (status = "Staging") and (time >= departure-time) [ ;find out which drivers are departing this time step
      
       ; Time to depart. Step one: calculate their total trip time, and how far they will drive.
       
       ; ah 3-25: now reading total-trip-dist and total-trip-time from od matrix, not from time&distance matrix.
       set total-trip-dist matrix:get od (([current-taz] of self - 1) * 25 + [destination-taz] of self - 1) 3
       set total-trip-time matrix:get od (([current-taz] of self - 1) * 25 + [destination-taz] of self - 1) 4 
       
      
      ; Now we know how long and far they are driving. Step 2: When do they arrive?
      
      set arrival-time (time + total-trip-time) ;calculate arrival time based on length of trip and current time

      ; And they're off!

      set status "Traveling"
      set color white
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
    let speed (total-trip-dist / total-trip-time) ; calculate average speed based on total trip distance and total trip time
    set travel-dist (speed * travel-time) ; calculate the distance the driver has traveled thus far
    if state-of-charge > 0 [set state-of-charge (state-of-charge - (time-step-size / 60 * speed * fuel-economy) / battery-capacity)] ; Here's the calculation:
    ; State of charge - update factor, update factor = time (hours) * speed (miles/hr) * efficiency (kWh/mi) / capacity (kWh) No longer multiplied by 100 ah 3-25
    if (state-of-charge <= 0 and phev? = false)[
      set status "Stranded"
      set driver-satisfaction 0 
      set color grey]; any vehicle that runs out of charge is stranded immediately
  ]
  if status = "Charging" [
    set state-of-charge state-of-charge + ( ( [charger-rate] of partner * time-step-size / 60 ) / battery-capacity ) ; Charger-rate is set in setup-chargers, based on level.
    ; State of charge + update factor, update factor = time (hours) * charger power (kW) / capacity (kWh) ; no longer multiplied by 100 ah 3-25
    set kWh-received kWh-received + ( [charger-rate] of partner * time-step-size / 60 )
    ask partner [
      set charger-in-use charger-in-use + 1   ;Charger is use - increase the duty factor
      set kWh-charged kWh-charged + charger-rate * time-step-size / 60
      set duty-factor charger-in-use / (ticks) ; Don't calculate before 6AM, and disregard the morning when no one will charge.
    ]
  ]
  
end ;update-soc

to-report random-bernoulli [probability-true]
  if (probability-true < 0.0 or probability-true > 1.0) [
    user-message (word
      "Warning in random-bernoulli: probability-true equals"
      probability-true)
  ]
  report random-float 1.0 < probability-true
end ;random-bernoulli

to update-custom-plots
  set-current-plot "Driver Status"
  set-plot-pen-interval time-step-size / 60
  set-plot-pen-color green
  set-plot-pen-mode 2
  plot count drivers with [status = "Home"]
  set-plot-pen-interval 0
  set-plot-pen-color orange
  plot count drivers with [status = "Charging"]
  set-plot-pen-color black
  plot count drivers with [status = "Traveling"]
  set-plot-pen-color blue
  plot count drivers with [status = "Staging"]
  set-plot-pen-color red
  plot count drivers with [status = "Waiting"]
  set-plot-pen-color grey
  plot count drivers with [status = "Stranded"]
end

to find-minimum-charge
  ; To determine minimum-acceptable-charge, we set a local variable equal to the distance of the next trip, multiply that by fuel-economy to get the required
  ; kWh, and then divide by the battery capacity to get the required state-of-charge. Since the total-trip-dist and total-trip-times need to be set in "to depart" 
  ; so that cars will leave at the start of the day, I do not set those values here.
  
    let next-trip-range matrix:get od (([current-taz] of self - 1) * 25 + [destination-taz] of self - 1) 3
    set minimum-acceptable-charge (fuel-economy-mean * next-trip-range) / batt-cap-mean
    if phev? = false [if minimum-acceptable-charge > 1 [set phev? true]]
    
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

MONITOR
291
381
370
426
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
13
50
76
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
109
95
199
155
batt-cap-mean
24
1
0
Number

INPUTBOX
210
95
309
155
batt-cap-stdv
1
1
0
Number

MONITOR
817
381
980
426
Average driver satisfaction
total-satisfaction
3
1
11

INPUTBOX
229
23
293
83
n-nodes
25
1
0
Number

SWITCH
95
184
198
217
debug?
debug?
0
1
-1000

MONITOR
828
136
885
181
Drivers
count drivers
3
1
11

PLOT
14
429
440
726
Driver Status
Hour
Number of Drivers
0.0
10.0
0.0
25.0
true
true
"" ""
PENS
"pen-0" 1.0 0 -7500403 false "" ""

PLOT
615
436
815
586
State of Charge
State of Charge
Frequency
0.0
1.1
0.0
100.0
true
false
"set-histogram-num-bars 10" "set-plot-pen-mode 1"
PENS
"pen-1" 1.0 0 -16777216 true "" "histogram [state-of-charge] of drivers"

PLOT
614
586
814
736
Driver Satisfaction
Satisfaction
Frequency
0.0
1.1
0.0
100.0
true
false
"set-histogram-num-bars 10" ""
PENS
"default" 0.1 1 -16777216 true "" "histogram [driver-satisfaction] of drivers"

SLIDER
198
184
370
217
safety-factor
safety-factor
0
.25
0.1
0.01
1
NIL
HORIZONTAL

INPUTBOX
825
34
1076
94
driver-input-file
p1r1.txt
1
0
String

MONITOR
1006
136
1111
181
Stranded
count drivers with [status = \"Stranded\" ]
17
1
11

MONITOR
956
381
1033
426
Duty factor
average-duty-factor
3
1
11

CHOOSER
4
175
96
220
alternative
alternative
0 1 2 3 4 5
1

MONITOR
1036
446
1193
491
Average vehicles serviced
average-charger-service
3
1
11

PLOT
814
436
1014
586
Vehicles Serviced per Charger
Number of vehicles serviced
Frequency
0.0
20.0
0.0
10.0
true
false
"set-histogram-num-bars 20" "set-plot-pen-mode 1"
PENS
"default" 1.0 0 -16777216 true "" "histogram [charger-service] of chargers"

PLOT
814
585
1014
735
kWh-charged
kWh-charged
Frequency
0.0
100.0
0.0
10.0
true
false
"" "set-plot-pen-mode 1"
PENS
"default" 1.0 0 -16777216 true "" "histogram [kWh-charged] of chargers"

MONITOR
908
136
1006
181
PHEVs
count drivers with [phev? = true]
0
1
11

MONITOR
823
194
880
239
Home
count drivers with [status = \"Home\"]
17
1
11

MONITOR
822
257
879
302
Waiting
count drivers with [status = \"Waiting\"]
17
1
11

MONITOR
826
318
883
363
Staging
count drivers with [status = \"Staging\"]
17
1
11

MONITOR
883
318
1117
363
Charging
count drivers with [status = \"Charging\"]
17
1
11

MONITOR
880
194
1118
239
Traveling
count drivers with [status = \"Traveling\"]
17
1
11

MONITOR
879
257
945
302
NIL
total-wait
17
1
11

SLIDER
199
216
371
249
phev-charge
phev-charge
0
1
0.1
.05
1
NIL
HORIZONTAL

SLIDER
199
249
373
282
bev-charge-anyway
bev-charge-anyway
0
1
0.1
.05
1
NIL
HORIZONTAL

INPUTBOX
3
224
158
284
fuel-economy-mean
0.34
1
0
Number

INPUTBOX
5
287
113
347
fuel-economy-stdv
0.05
1
0
Number

INPUTBOX
113
355
223
415
phev-fuel-economy
0.5
1
0
Number

INPUTBOX
7
353
103
413
phev-batt-cap
16
1
0
Number

INPUTBOX
109
23
203
83
batt-cap-range
5
1
0
Number

INPUTBOX
120
288
237
348
fuel-economy-range
0.1
1
0
Number

MONITOR
957
257
1046
302
total-kwh
sum [kwh-received] of drivers
4
1
11

MONITOR
1035
504
1151
549
effective stranded
count drivers with [driver-satisfaction < 0.1 and driver-satisfaction > 0]
17
1
11

MONITOR
1035
568
1138
613
high satisfaction
count drivers with [driver-satisfaction <= 1.0 and driver-satisfaction >= 0.7]
17
1
11

MONITOR
1038
631
1151
676
chargers available
count chargers with [available = true]
1
1
11

MONITOR
1043
689
1135
734
kWh delivered
sum [kWh-charged] of chargers
4
1
11

MONITOR
1178
509
1276
554
low satisfaction
count drivers with [driver-satisfaction < 0.4 and driver-satisfaction >= 0.1]
17
1
11

MONITOR
1174
573
1295
618
medium satisfaction
count drivers with [driver-satisfaction < 0.7 and driver-satisfaction >= 0.4]
17
1
11

TEXTBOX
460
441
642
569
Driver Status Legend\n-GREEN	= HOME\n-BLACK	= TRAVELING\n-ORANGE	= CHARGING\n-BLUE	= STAGING\n-RED	= WAITING\n-GREY	= STRANDED
13
0.0
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
