# Load the conversion table from old TAZ to new TAZ.
load(pp(pevi.shared,'data/UPSTATE/od.converter.Rdata'))

# Cycle through the uncombined files and convert to the new TAZ ids.
for(od.input.file in list.files(pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined/'))){
	od.table <- read.table(pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined/',od.input.file),sep='\t',header=TRUE)
	names(od.table) <- c(';driver','from','to','depart','home')
	for(i in 1:nrow(od.table)) {
		od.table$from[i] <- od.converter$new.id[match(od.table$from[i],od.converter$old.id)]
		od.table$to[i] <- od.converter$new.id[match(od.table$to[i],od.converter$old.id)]
		od.table$home[i] <- od.converter$new.id[match(od.table$home[i],od.converter$old.id)]
	}
	write.table(od.table,file=pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-v2/',substr(od.input.file,1,nchar(od.input.file)-4),'.txt'),sep='\t',row.names=F)
}

# The first line will need to be adjusted to remove the quotes for use in NetLogo. 