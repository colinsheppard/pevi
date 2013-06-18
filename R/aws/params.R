
base.path <- paste(getwd(),'/',sep='')

#optim.scenario <- 'cost'    # vary L2 installed cost
#optim.scenario <- 'battery' # vary batter capacity
optim.scenario <- 'type'    # vary BEV/PHEV ratio

num.cpu <- 8

path.to.outputs.base <- paste(base.path,'pevi/outputs/',sep='')

hot.start <- T
