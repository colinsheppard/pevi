# This script collects, zips, and transfers to NERSC all the files needed to run pevi. 

# List of the files needed.

# From params.txt:

# "starting-soc-file"	"data/inputs/starting-soc.txt"
# "ext-dist-time-file"	"data/inputs/external-time-distance/ext-time-dist-mean-25.txt"
# "charger-input-file"	"data/inputs/charger-input-file/upstate/upstate-existing-chargers.txt"
# "charger-type-input-file"	"data/inputs/charger-type-input-file/charger-types-scen-upstate.txt"
# "driver-input-file"	"data/inputs/driver-input-file/upstate-combined/driver-schedule-pen0.5-rep10-20140129.txt"
# "od-input-file"	"data/inputs/OD-upstate/upstate-od-data-update.txt"
# "vehicle-type-input-file"	"data/inputs/vehicle-type-input-file/vehicle-types-scen2.txt"

optim.scenarios <- c('base','res-none')

setwd(pevi.shared)

zip('pevi-files.zip',c('data/inputs/starting-soc','data/inputs/external-time-distance','data/inputs/charger-input-file/delhi/delhi-existing-chargers.txt','data/inputs/charger-type-input-file','data/inputs/driver-input-file/delhi-combined','data/inputs/OD-delhi','data/inputs/vehicle-type-input-file',pp('data/inputs/optim-new/',optim.scenarios)),extras='data/inputs/driver-input-file/delhi-combined/old')

# use the following shell command to push the data up to nersc
#scp pevi-files.zip csheppar@carver.nersc.gov:~/
