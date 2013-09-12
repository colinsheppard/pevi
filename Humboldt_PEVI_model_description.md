#Plug-in Electric Vehicle Infrastructure Model Description

####Version 2.0

####Colin Sheppard, Allison Campbell, Jim Zoellick, Charles Chamberlin, Andy Harris

####Schatz Energy Research Center

# 1. Purpose
The purpose of this model is to simulate different PEV public charging infrastructure alternatives in Humboldt County, CA. The PEV infrastructure alternatives are described by number, type and location of PEV charging stations throughout the county.  For each alternative, a variety of realistic scenarios should be modeled that represent different PEV adoption rates, technologic advances, driver behaviors, and vehicle types. A measure of driver’s satisfaction and amount of use (duty factor) are outputs from the model needed to determine the preferred alternative. The model must incorporate the traffic data provided by Caltrans and spatially represent an aggregated set of their existing traffic analysis zones (TAZs).
### Assumptions
1. All PEV owners are assumed to have home chargers and vehicles are assumed to begin each day with a full charge.
2. All BEV vehicle parameter values are based on those of the Nissan Leaf (24 kWh battery, 0.34 kWh/mi, level 1-3 charging capabilities).
3. If a scheduled trip is outside the range of an average vehicle, it is assumed that the trip would never be attempted by a BEV.  These vehicles are then modeled as PHEVs with parameters based on the Chevrolet Volt (16 kWh battery, 0.5 kWh/mi, level 1-3 charging capabilities).
4. PEV adoption rates are assumed to follow the temporal and spatial distribution of hybrid electric vehicles in Humboldt County from 2003-2012.
5. All chargers are assumed to have a constant charging rate based on their power specifications. (i.e., no charging algorithms are incorporated).
6. Drivers begin the day with a schedule that determines all TAZs they will attempt to visit (with associated departure times).
7. Charging stations are assumed to be networked and their state (charging vs. available) is known to all drivers via wireless communications.
8. Drivers can choose to make a mid-trip stop for level 3 charging, though the choice must be made before they begin their trip.
9. ~~Once at a destination, drivers only seek available chargers in their current TAZ location.  They do not look for a charger in neighboring TAZs.  TAZs are assumed to be sized large enough that leaving a TAZ to find a charger and walking back to one’s destination is impractical.~~
10. Departure is not permitted for a BEV unless it has a minimum acceptable charge for their next trip plus a safety factor.
11. A fraction of drivers will attempt to charge their vehicle even when a charge is not needed to get to their next destination
12. Drive times between TAZ pairs are supplied as input and are applied to all vehicles equally, thus all vehicles are assumed to drive at the same speed for a given trip.

# 2. Entities, State Variables, and Scales
## 2.1 Traffic Analysis Zones (TAZs)
The TAZs are entities that describe the atomic geographic regions of the environment. All TAZs are interconnected, so a vehicle in one TAZ may travel to any other.  While they represent spatially explicit regions, the PEVI model does not store or track spatial data (polygons, lines, etc.) for each TAZ.  Instead, the spatial relationships between TAZs are encoded as table (see “Environment – Origin Destination Table” below) describing the distances and travel times between all combinations of TAZs. In this document, TAZs are also referred to as “nodes.”  The TAZ agents are described by following variables:

Category          | Variable      | Description
------------      | ------------- | ------------
Identity (static) | ID            | Integer identification code specified in the input data supplied to the model.
Contents          | chargersInTAZ (static)  | A list of chargers contained in the TAZ.
                  | homeCharger   | Every TAZ has a Level II charger which is only available to drivers in their home TAZ.
                  | driversInTAZ (dynamic)| A list of drivers currently in the TAZ.
                  | nLevels       | A 3-value list containing the number of chargers of each level (1, 2, or 3)

## 2.2 Environment
The environment is the entity where all the agents live and interact, in this model it is the geographic region described by the input data. The environment is defined by several global state variables and parameters which are available to all agents in the model for reference or use. 

Category | Variable      | Description
-------- | ------------- | ------------
Global   | time          | Numeric variable containing the decimal hour of the day, where 0 is midnight, 12 is noon, and 1.5 is 1:30am.
         | schedule      | A compound variable containing the active list of scheduled events (see section Process Overview and Scheduling below).
         | odTable (Origin-Destination Table)| distance and time between any two TAZs.  The table has the following columns:          **(Format a list here)**
         | parameters    | A table of parameter values indexed by their name.  See Table ## for a listing of parameters along with their default values.
         
## 2.3 Drivers
Driver agents are used in the model to simulate individual driver and vehicle characteristics combined. These entities are described in the model by the following state variables:

Category | Variable | Description
---------|----------|------------
Vehicle (static)|vehicleType|String variable containing the name of the vehicle model upon which the other variables of this category are based (e.g. “Leaf” or “Volt”).
|isBEV? | A boolean flag indicating whether the vehicle is a BEV, if not, vehicle is assumed to be a PHEV (conventional vehicles are not modeled).
| chargingOnAWhim? | A boolean flag indicating whether the vehicle is seeking a charger because they actually need to charge or for some other, less critical reason.
| batteryCapacity (kWh) | The default quantity of stored energy by the battery bank when fully charged.  If the vehicle is a PHEV, then the battery capacity indicates the amount of energy available to drive the vehicle in charge depleting mode.
| electricFuelConsumption (kWh / mile)|The default amount of battery electricity required to travel 1 mile.
| hybridFuelConsumption (gallon / mile) | The default fuel amount of gasoline required to travel 1 mile for a PHEV in charge sustaining mode.  (N/A for BEVs).
Demography (static) | homeTAZ | The home TAZ of the driver, this is not necessarily where the driver begins the day, but rather is inferred based upon the trip type column in the drivers itinerary (see below). 
| probabilityOfUnneededCharge | The probability that the driver will choose to attempt to charge their vehicle despite not actually needing the charge.
Operation (dynamic) | state | A discrete integer value that represents the current state of a driver (Home, Traveling, Waiting, Charging, Staging or Stranded), and used to decide which procedures to execute.
| currentTAZ | The TAZ where the driver is currently located, set to “nobody” while in transit.
| stateOfCharge | The fraction of useable energy remaining in the vehicle’s battery.  A value of 1 indicates a fully charged battery and a value of 0 indicates the battery is effectively empty.  Note, if the vehicle is a PHEV, then 0 indicates charge sustaining mode which does not imply the battery is fully depleted.
| currentCharger | The charger with which the driver is currently charging.  Set to ‘nobody’ if the driver is not charging.
| itinerary, currentItinRow | A compound variable containing the intended itinerary of the driver for one day.  Each row of the itinerary represents a single trip and includes the following columns: **(format the list here)**
| willingToRoam? | Boolean value that indicates whether the driver would consider traveling to a neighboring or en-route TAZ to charge.
Tracking (dynamic) | numDenials | The number of occurrences when the driver wanted/needed to charge but was unable due to a lack of available chargers.

## 2.4 Chargers
Charging agents represent the electric vehicle supply equipment installed at a give TAZ.  Charging stations can either be level 1, 2 or 3.  In practice, most level 2 chargers will also have level 1 capability, in this model they are represented as two separate chargers.  The charger agents are currently described by the following state variables: 

Category | Variable | Description
---------|----------|------------
Infrastructure (static) | chargerType | Integer variable indicating whether the station is a level I, II, or III.
| location | A variable referencing the TAZ where the charger is located.
| chargeRate (kWh / hr) | The rate at which the charger delivers energy to the vehicle.
| energyPrice ($/kWh) | The price of energy for charging at this charger
Operation (dynamic) | current-driver | The driver currently being served by the charger.  If “nobody” then the charger is considered available for beginning a new charging session.  If the charger is a home charger, then this variable will always have a value of “nobody” to indicate that any driver in their home TAZ can charge at their home.
Tracking (dynamic) | energyDelivered (kWh) | The cumulative amount of energy delivered by the charger up to the current moment.
| numSessions | Integer count of the number of discrete charging sessions with drivers.
    
Scales are used to describe changes in the model’s entities temporally and spatially. The PEVI model has a temporal extent of one 24-hour day.  Time is modeled using discrete event simulation (see section Process Overview and Scheduling below).  The spatial extent of the model is defined by the TAZs.  For the Humboldt County implementation, the region is discretized into ## TAZs.

# 3. Process Overview and Scheduling
In the PEVI model, time and actions are managed using discrete event simulation (DES).  Model processes are maintained as an ordered schedule of events.  An event consists of a time, an agent, and an action.  After initialization, the first event on the schedule is dispatched, at which point the specified agent performs the specified action; then the next event on the schedule is dispatched, and so on.  Events can be created during initialization or dynamically generated during model execution. 

In PEVI, events are principally associated with drivers.  Figure ## presents a flow chart of the driver decision logic.  The chart contains a representation of the different states that a driver can have (red rectangles), the event schedulers that determine when a driver executes an event (yellow triangles), the events that control process flow (arrows labeled with green rectangles), and the decisions that are evaluated to inform the process flow (blue diamonds).  Descriptions of the states, event schedulers, events, and decisions are listed in Table ##.

In Figure ## event schedulers are depicted as attached to states on the upstream side of the process flow.  This placement is intentional and closely tied to the management of PEVI as a DES.  At any time, drivers have complete knowledge about the state of their vehicle (state of charge, fuel consumption, etc.) and their itinerary.  This means, that as drivers enter any state, they can determine the time at which they will exit that state and perform an event.  For example, when the Traveling state is entered, the driver knows where they are going (by virtue of their itinerary) and based on the global origin-destination table, they can determine when they will arrive.  The PEVI model takes advantage of this foresight and model scheduling is structured so that drivers schedule events as they enter a new state. 

```	
(Flow chart image here)
	
Figure 1: This flow chart illustrates the three driver states (red rectangles), the events that control transitions between states (arrows labeled with green rectangles), the decision logic used to inform transitions (blue diamonds) and the event schedulers that dictate events are executed (orange triangles). See Table ## for a description of the key elements in the flow chart. 
```
```
Table : Overview of driver states, event schedulers, events, and decisions.
```

Type | Name | Description | Results
-----|------|-------------|--------
State|Not Charging| This state describes a driver that is parked but not charging.  The driver could be at home or any other TAZ in the model. | N/A
State|Traveling|Drivers in the *Traveling* state are on their way from one TAZ to another.  The model does not track drivers along their path, instead they “appear” at their destination when the *Arrive* event is executed.|N/A
State|Charging|Drivers in the *Charging* state are parked and engaged in a charging session.|N/A
Event Scheduler|Itinerary|As drivers enter the *Not Charging* state through this path, they schedule the *Depart* event based on the *Itinerary* submodel (Section Itinerary). If the next trip on their itinerary was supposed to occur in the past, the driver executes the *Depart* event immediately.|Depart Event Scheduled
Event Scheduler|Wait Time|As drivers enter the *Not Charging* state through this path, they schedule the *Depart* or *Retry Seek* event based on the *Wait Time* submodel (Section Wait Time)|Depart or Retry Seek Event Scheduled
Event Scheduler|Travel Time|As drivers enter the *Traveling* state, they schedule the *Arrive* event based on the *Travel Time* submodel (Section Travel Time).|Arrive Event Scheduled
Event Scheduler|Charge Time|As drivers enter the *Charging* state, they schedule two events to occur based on the *Charge Time* submodel (Section Charge Time).  Either the *End Charge* and *Retry Seek* event are schedule (the latter to immediately follow the former) or the *End Charge* and *Depart *events are schedule.Charge Time|End Charge and either Retry Seek or Depart Event Scheduled
Event|Depart|The driver executes the *Need to Charge* decision and either transitions to the *Traveling* state or executes the *Seek Charger* decision.|Transition to Traveling or Not Charging
Event|Retry Seek|The driver immediately executes the *Seek Charger* decision.|Transition to Charging, Not Charging, or Traveling
Event|Arrive|The driver executes the *Need to Charge?* decision and transitions to a new state accordingly.  If the driver is at home and has finished her itinerary, then she transitions to *Charging* and schedules the *End Charge* event, after which she stops.|Transition to Charging or Not Charging
Event|End Charge|The driver SoC variable is updated to reflect the charging session, then driver transitions to *Not Charging*.|Transition to Not Charging
Decision|Need to Charge?|The driver estimates whether she has sufficient charge for her next trip according to the *Need to Charge?* submodel (Section Need to Charge).|Report Yes or No
Decision|Seek Charger|The driver seeks an available charger according to the *Seek Charger* submodel (Section Seek Charger) and responds accordingly by transitioning to any of the possible states.|Transition to Charging, Not Charging, or Traveling

# 4. Design Concepts
## 4.1 Emergence
## 4.2 Objectives
## 4.3 Adaption
## 4.4 Sensing
Driver agents can sense the availability and distance to a charger node. This information aids decision making about where to seek out available chargers if one is not available at a driver’s current destination.
## 4.5 Interaction
Interaction between vehicles and chargers is incorporated. The vehicle agents directly interact with a charger agent by querying to find out if charging is available. If charging is available the vehicle changes their state to charging and the charger’s state changes to occupied.  Vehicles interact with other vehicles indirectly by competing for a charging resource. When a vehicle interacts with a charger, that charger becomes unavailable for all other vehicles.  
## 4.6 Stochasticity
Several pseudorandom processes are used to introduce variability in the model.  The incorporated drive times between zones are represented as the mean of a normal distribution (wish list item).  Battery capacity, energy efficiency, and anxiety proneness are also normally distributed.  If multiple vehicles are waiting for a charger when one becomes available, a vehicle is selected randomly.  For simple decisions that a driver must make over the course of a day, a Bernoulli random variable is used.  
## 4.7 Initialization
The input to the PEVI model consists of three data sets.  The first set includes the distances and drive times between each TAZ which are used to calculate the reduction in SoC that occurs when vehicles travel to a new zone.  The second data set is the schedule file which provides vehicles with trip scheduling throughout the day.  The third data set identifies the number and type of charging stations located at each TAZ. 
Three driver agent characteristics are set at the start of each modeling day:  schedule, state of charge and satisfaction. The driver schedule establishes the nodes the driver will travel to, along with the departure and arrival times corresponding to each node in their schedule. Based on the assumption that drivers charge their vehicle at night, state of charge is initialized at 100%. Each driver starts the day with a satisfaction of one, which may be reduced as complications arise during the day.
## 4.8 Observation
The model output important for evaluating alternative designs is driver satisfaction and charger station duty factors. The model will output the mean value of all vehicle agents’ mean driver satisfaction over the day. The model will also output a mean value of all charging agents’ duty factor. Additional output used to compare infrastructure alternatives are plots of mean driver satisfaction over time and mean duty factor over time. Overall driver satisfaction is calculated by averaging the end of day driver satisfaction. Additional output used to troubleshoot and understand model operations include:

1. Hourly plots indicating the number of vehicles at each TAZ.
2. Hourly plots indicating the number of chargers occupied within each TAZ.
3. Monitoring of individual vehicle agents’ state changes throughout the day.
4. Monitoring of individual charger agents’ state changes throughout the day.
5. Monitoring the number of stranded vehicles.
6. Plotting a histogram of driver satisfaction.

# 5. Submodel Details
The following sections provide detailed descriptions of the PEVI submodels.

## 5.1 Itinerary
Describe how demand model + travel survey data are combined to produce the itinerary for all drivers.

## 5.2 Wait Time
The wait time submodel is an event scheduler.  It is executed after a driver has performed the Seek Charger decision and found none that are available.  The submodel decides whether the driver will attempt to retry finding a charger or, if sufficient charge is available, abandon the charging attempt and schedule a departure.  
To make this determination, four values are estimated: 

- remainingRange: the number of miles remaining (set to positive infinity if isBEV is false)
- tripDistance: the number of miles to complete the next trip in the driver’s itinerary
- journeyDistance: the number of miles to complete all of the remaining trips in the driver’s itinerary
- timeUntilDepart: the time in hours remaining before the vehicle is due to depart on its next trip

The following table details how either the decision is made and at what time the corresponding event is to be scheduled:

If | Then
---|----- 
remainingRange / chargeSafetyFactor < tripDistance | Schedule *Retry Seek* event to occur after a random amount of time based on an exponential distribution with mean of waitTimeMean.
tripDistance <= remainingRange / chargeSafetyFactor < journeyDistance | If timeUntilDepart > willingToRoamTimeThreshold, schedule *Retry Seek* event to occur after a random amount of time based on an exponential distribution with mean of waitTimeMean and a maximum allowed value of (timeUntilDepart – willingToRoamTimeThreshold); If timeUntilDepart <= willingToRoamTimeThreshold, schedule *Depart* event to occur after timeUntilDepart hours.
remainingRange / chargeSafetyFactor > journeyDistance| Schedule *Depart* event to occur after timeUntilDepart hours.

## 5.3 Travel Time
**Describe how this is based on OD Table and how that table is created using GIS road network data.**

## 5.4 Charge Time
The charge time submodel is an event scheduler.  It is executed after a driver has performed the *Seek Charge*r decision, selected an available charger, and optionally traveled to that charger.  The submodel decides whether the driver will attempt to retry finding a charger later in the day (necessary to allow drivers to make temporary use of lower level chargers when higher levels are currently unavailable) or to schedule the *End Charge* event.
To make this determination, the following values are estimated:
 
- chargerInOriginOrDestination: this Boolean describes whether the charger is located in a TAZ that’s a part of the driver’s itinerary vs. a neighboring TAZ or an en-route TAZ.
- timeUntilDepart: the amount of time before the next trip in the driver’s itinerary.
- tripDistance: the number of miles to complete the next trip in the driver’s itinerary
- tripChargeTimeNeed: the amount of charging time needed to complete the next trip in the itinerary, if isBev is FALSE then set this to 0 to indicate that there is no need for charge to complete the trip, otherwise use the following formula: **Get the formula in here.**
- journeyDistance: the number of miles to complete all of the remaining trips in the driver’s itinerary
- journeyChargeTimeNeed: the amount of charging time needed to complete the remaining trips in the itinerary, **Another equation**
- fullChargeTimeNeed: the amount of charging time to complete a full charge, **Equation : if chargerType = 3, then fullChargeTimeNeed = otherwise, fullChargeTimeNeed = **
- timeUntilEndCharge: the anticipated time in hours remaining before the driver chooses to end charging or the vehicle is fully charged.  The following table describes how this value is calculated:

If|Then timeUntilEndCharge = | Additional Actions
--|--------------------------|-------------------
fullChargeTimeNeed < tripChargeTimeNeed | fullChargeTimeNeed |
timeUntilDepart < tripChargeTimeNeed | tripChargeTimeNeed | Delay itinerary with next trip occurring tripChargeTimeNeed hours from the present moment.
chargerInOriginOrDestination | min(timeUntilDepart, fullChargeTimeNeed) | 
chargerType= 3 | min(timeUntilDepart, fullChargeTimeNeed, journeyChargeTimeNeed) |
otherwise | min(timeUntilDepart, tripChargeTimeNeed) |

The following table details how the decision is made and at what time the corresponding event is to be scheduled:

If | Then
---|-----
isBEV = TRUE AND 0 < timeUntilEndCharge < fullChargeTimeNeed AND chargerType < 3 AND (timeUntilEndCharge > timeUntilDepart OR timeUntilEndCharge < journeyChargeTimeNeed) AND timeUntilDepart > willingToRoamTimeThreshold | Schedule *Retry Seek* event to occur after a random amount of time based on an exponential distribution with mean of waitTimeMean and a maximum allowed value of (timeUntilDepart –willingToRoamTimeThreshold).
otherwise | Schedule *End Charge* event to occur after timeUntilEndCharge hours.

## 5.5 Need to Charge?
First estimate the following values:

- tripDistance: the number of miles to complete the next trip in the driver’s itinerary
- journeyDistance: the number of miles to complete all of the remaining trips in the driver’s itinerary
- remainingRange: the number of miles remaining, see Equation 1

Now base the decision on the following table, where chargingOnAWhim? is initialized to false:

If | Then
:---:|:-----:
calling event is “Arrive” AND remainingRange / chargeSafetyFactor < journeyDistance | Report yes.
Else If	| Then
calling event is “Depart” AND remainingRange / chargeSafetyFactor < tripDistance | Report yes.
Else If | Then
calling event is “Arrive” AND timeUntilDepart > willingToRoamTimeThreshold AND randomDrawFromUniformDist < probabilityOfUnneededCharge | Report yes and set *chargingOnAWhim?* to true.
Otherwise | Report no.

## 5.6 Seek Charger
This submodel is based on an economic model that compares the total cost of charging (including the opportunity cost of a driver’s time) from all relevant charging alternatives, selecting the least cost option.
The submodel consists of the following actions:

1. Set willingToRoam? to *true* if isBEV is *true* AND timeUntilDepart is less than the parameter willingToRoamTimeThreshold AND chargingOnAWhim? is *false*, otherwise set to *false*.

2. Find the number of available charges by type and location within range of the driver.  If willingToRoam? is set to false, then only consider charges in currentTAZ.  Otherwise, include any chargers in currentTAZ, neighboring TAZs (all TAZs within a driving distance set by chargerSearchDistance), and en-route TAZs between the current TAZ and the next destination TAZ in the driver’s itinerary.  The index ‘i’ will be used below to reference each combination of TAZ and charger type with at least one available charger.  Note that some of the variables with the prefix “extra” will be zero for chargers in the current TAZ or en-route as they only apply to travel that’s additional to the driver’s itinerary.  The one exception to this is extraTimeForCharging, which will be non-zero for en-route TAZs because the time spent is an opportunity cost to the driver. If no available chargers are found, then increment the driver variable numDenials, transition to the state *Not Charging* and stop this action.

3. Calculate the following values: 
	a. level3AndTooFull? This boolean value is true if the charger under consideration is level III and the driver’s state of charge is >= 0.8 or, for enroute chargers, will be >= 0.8 when the vehicle reaches the intermediate destination.  If this parameter is true, then the alternative is not considered.
	b. level3TimePenalty  Set this to a value of 1 if the distance to the destination (in the case of enroute charging, from the intermediate TAZ) is greater than vehicle can go on a full level 3 charge (80% state of charge). Otherwise set to 0.  This penalizes level 3 charging when a level 1 or 2 charge might get the driver there without an additional stop or another charging session.
	c. tripOrJourneyEnergyNeed.  This value depends on the amount of time before the next departure in the driver’s itinerary as well as the current state of charge and the charger type. If timeUntilDepart < willingToRoamThreshold, then only the energy needed for the next trip is considered, otherwise the energy needed for the journey is used. If the energy needed for the trip or journey is greater than the energy needed to fill the battery (or in the case of level 3, achieved 80% state of charger) then tripOrJourneyEnergyNeed is set to the battery limiting value. As a formula, the value is calculated as: if timeUntilDepart < willingToRoamThreshold , distance = tripDistance, otherwise distance = journeyDistance,
if level == 3: **(FORMULA MISSING IN OPENOFFICE)** otherwise: **(ANOTHER FORMULA MISSING)**
	d. extraTimeForTraveli, extraDistanceForTraveli: the additional travel time and distance needed to accommodate the detour, equal to the difference between first traveling to the intermediate TAZ, then to the destination TAZ vs. traveling straight to the destination TAZ.
	e. extraEnergyForTraveli: the energy needed to accommodate the extra travel, calculated by:  * chargeSafetyFactor
	f. extraTimeUntilEndChargei: if chargerInOriginOrDestinationi is true, then this value is set to the amount of delay in the driver’s itinerary that would be necessary to use the charging alternative, calculate as max(0, tripChargeTimeNeed – timeUntilDepart) if the charger is in the origin and 0 if the charger is in the destination TAZ, if  chargerInOriginOrDestinationi is false, then the value is an estimate of the extra time a driver would spend charging, equal to the value of timeUntilEndCharge as calculated by the Charge Time submodel (Section Charge Time) with the following modifications:
		f.i. timeUntilDepart is decreased by the time of travel from the origin TAZ to TAZi
		f.ii. stateOfCharge is decreased by  where tripDistancei is the distance in miles from the origin TAZ to TAZi
		f.iii. tripDistance and journeyDistance are assumed to begin at TAZi **(RE-FORMAT)**
4. Estimate the cost of the alternative,	
Equation : Costi = **(Missing formula)**
5. Chose the alternative with the minimum cost.  If TAZi  is the current TAZ, call the *ChargeTime* event scheduler.   Otherwise update the driver’s itinerary to include the new destination TAZ (unless TAZi is the destination TAZ) with a depart time equal to now and call the *TravelTime* event scheduler.

## 5.7 Break Up Trip
- If driver has a full battery and cannot make the next trip (or if the stateOfCharge >= 0.8 and the currentTAZ only has level 3 chargers available), then he attempts to break the trip into smaller trips with intermediate stops for charging.
- The driver only considers en-route TAZs that are reachable given her range.
- The search is first restricted to reachable en-route TAZs that would allow the driver to reach the ultimate destination in one trip after recharging (note that this must be based on a stateOfCharge of 0.8 if only level 3 chargers are available in the candidate TAZ).  If no such TAZs can be found, or all of these TAZs have a score of 0, then all reachable en-route TAZs are considered.
- Each reachable en-route TAZ is assigned a score equal to the number of available chargers or a certain level times the level number (E.g. if two level 3 and one level 2 chargers are available then the score would be 2 * 3 + 1 * 2 = 8).  If the TAZ is the driver’s home, then 8 is added to the score for that TAZ (in other words, a home charger is as valuable as 4 level 2 chargers but not as valuable as 3 level 3 chargers).  
- The TAZ with the highest score is selected (ties are broken by selecting the furthest taz from the current location).  If no en-route TAZs have any available chargers (i.e. if they all have a score of 0), then the driver selects the most distance reachable TAZ.
# 6. Parameters
Name | Description | Default Value
-----|-------------|:--------------:
chargeSafetyFactor | Multiplier used to approximate the safety factor drivers assume necessary to ensure a trip can be made.|1.1
chargerSearchDistance | The distance in miles used to define what TAZs are considered “neighbors” for the purpose of finding a charger. |5
willingToRoamTimeThreshold | The amount of time in hours at which point a driver will consider travelling to neighboring or en-route TAZs in order to charge vs. only considering TAZs in their current location. | 1
timeOpportunityCost | The value of a driver’s time to his or herself in units of $ / hour. | 12.50
fracPHEV | The fractions of PEV vehicles that are PHEV vs BEV. | 0.5
probabilityOfUnneededCharge | The probability that a driver will choose to charge despite not actually needing it. | 0.1
electricFuelConsumptionSD | Standard deviation of the truncated normal distribution used to distribute electric fuel consumption amongst the drivers.  In units of kWh/mile. | 0.02
electricFuelConsumptionRange | Range of the truncated normal distribution used to distribute electric fuel consumption amongst the drivers.  In units of kWh/mile. | 0.1

