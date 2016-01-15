
# specify the reporters used to summarize an experiment and setup results data frame
reporters <- data.frame(num.drivers="(count drivers)",
  num.trips="(sum [ length itin-change-flag - sum itin-change-flag ] of drivers)",
  objective="(objective-function)",
  total.delay="(sum [ sum map weight-delay itin-delay-amount  ] of drivers)",
  mean.delay="(mean-delay)",
  total.delay.cost="(total-delay-cost)",
  total.charger.cost="(total-charger-cost)",
  frac.drivers.delayed="(count drivers with [ sum map abs itin-delay-amount > 0 ] / count drivers)",
  num.unscheduled.trips="(sum [ sum itin-change-flag ] of drivers)",
  energy.charged="(sum [ energy-received ] of drivers)",
  driver.expenses="(sum [ expenses ] of drivers)",
  infrastructure.cost="(total-charger-cost)",
  infrastructure.cost.including.external="(total-charger-cost-including-external)",
  public.infrastructure.cost="(sum [ [installed-cost] of this-charger-type ] of chargers with [level-of self > 0])",
  residential.infrastructure.cost="( (count drivers) * ([installed-cost] of one-of charger-types with [level = 0]) )",
  gasoline.used="(sum [ gasoline-used ] of drivers)",
  miles.driven="(sum [ miles-driven ] of drivers)",
  num.denials="(sum [ num-denials ] of drivers)",
  num.stranded='(num-stranded)',
  num.stranded.by.delay='(num-stranded-by-delay)',
  frac.stranded.by.delay='(num-stranded-by-delay / count drivers)',
  mean.duty.factor="(mean-duty-factor)",
  electric.miles.driven="(electric-miles-driven)",
  frac.denied="(count drivers with [num-denials > 0] / count drivers)",stringsAsFactors=F)

# log files, these all get set to false so logging is deactivated
logfiles<-c("wait-time","charging","charge-time","seek-charger","seek-charger-result","need-to-charge","trip-journey-timeuntildepart","break-up-trip","break-up-trip-choice","charge-limiting-factor","drivers","pain","tazs","trip","summary")

debug.reporters <- data.frame(
  n.tazs="(count tazs)",
  n.charger.types="(count charger-types)",
  n.drivers="(count drivers)",
  charge.safety.factor="(charge-safety-factor)",
  charger.search.distance="(charger-search-distance)",
  wait.time.mean="(wait-time-mean)",
  time.opportunity.cost="(time-opportunity-cost)",
  willing.to.roam.time.threshold="(willing-to-roam-time-threshold)",
  probability.of.unneeded.charge="(probability-of-unneeded-charge)",
  electric.fuel.consumption.sd="(electric-fuel-consumption-sd)",
  electric.fuel.consumption.range="(electric-fuel-consumption-range)",
  charger.input.file="(charger-input-file)",
  charger.type.input.file="(charger-type-input-file)",
  driver.input.file="(driver-input-file)",
  vehicle.type.input.file="(vehicle-type-input-file)",
  stringsAsFactors=F)
