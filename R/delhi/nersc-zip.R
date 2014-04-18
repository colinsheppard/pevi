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

optim.scenarios <- c('base','res-none','homeless')
optim.scenarios <- c('homeless')
optim.scenarios <- c('half-homeless')
optim.scenarios <- c('no-homeless')
optim.scenarios <- c('swap')
optim.scenarios <- c('veh-low','veh-high')
optim.scenarios <- c('opp-cost-high','cheap-l1','cheap-l2','cheap-l3','cheap-l4')
optim.scenarios <- c('homeless','half-homeless','no-homeless','veh-low','veh-high')

setwd(pevi.shared)

#zip('pevi-files.zip',c('data/inputs/starting-soc','data/inputs/external-time-distance','data/inputs/charger-input-file/delhi/delhi-existing-chargers.txt','data/inputs/charger-type-input-file','data/inputs/driver-input-file/delhi-combined','data/inputs/OD-delhi','data/inputs/vehicle-type-input-file'),extras='-x data/inputs/driver-input-file/delhi-combined/old')
unlink('pevi-experiments.zip')
zip('pevi-experiments.zip',c(pp('data/inputs/optim-new/delhi-',optim.scenarios)))

# use the following shell command to push the data up to nersc
#scp pevi-files.zip csheppar@carver.nersc.gov:~/pevi-shared/
#scp pevi-experiments.zip csheppar@carver.nersc.gov:~/pevi-shared/
#ssh csheppar@carver.nersc.gov
