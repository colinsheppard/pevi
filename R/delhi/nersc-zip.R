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

# From pevi-shared directory
cd ~/Dropbox/serc/pev-shared

# Be sure to include both upstate files (for testing) and delhi files (for future work).

zip pevi-files.zip data/inputs/starting-soc data/inputs/external-time-distance data/inputs/charger-input-file/upstate data/inputs/charger-type-input-file data/inputs/driver-input-file/upstate-combined data/inputs/OD-upstate data/inputs/OD-delhi data/inputs/vehicle-type-input-file data/inputs/optim-new/params.R data/inputs/optim-new/params.txt data/inputs/optim-new/vary.yaml data/inputs/optim-new/varyshort.yaml