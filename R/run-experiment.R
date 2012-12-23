Sys.setenv(NOAWT=1)
library(colinmisc)
load.libraries(c('plyr','yaml','RNetLogo'))

path.to.pevi <- '/Users/critter/Dropbox/serc/pev-colin/pevi/'
path.to.shared.inputs <- '/Users/critter/Dropbox/serc/pev-colin/pev-shared/data/inputs/'

nl.path <- "/Applications/NetLogo\ 5.0.1"
NLStart(nl.path,nl.version=5, gui=F)
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')
NLLoadModel(model.path)
NLCommand('clear-all-and-initialize')
NLCommand(paste('set parameter-file "',path.to.shared.inputs,'sensitivity/base/params.txt"',sep=''))
NLCommand(paste('set model-directory "',path.to.pevi,'netlogo/"',sep=''))
NLCommand('read-parameter-file')
NLCommand('setup')
NLCommand('go')
NLCommand('summarize')



NLReport('count drivers')
NLReport('sum [ length itin-change-flag - sum itin-change-flag ] of drivers')
NLReport('sum [ sum itin-delay-amount  ] of drivers')

NLReport(c(
'model-directory',
'parameter-file',
'charger-input-file',
'charger-type-input-file',
'driver-input-file',
'od-input-file',
'vehicle-type-input-file',
'outputs-directory'))
NLReport('count drivers')
NLReport('count vehicle-types')
NLQuit()


