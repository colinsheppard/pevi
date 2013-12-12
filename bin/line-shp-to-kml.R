#!/usr/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to convert shape files to KML format for use in Google Earth
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','maptools','stringr','ggplot2','rgdal','plotKML'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-s", "--shapefile"), type="character", default='input_shapefile', help="Path to line shape file with no extension [\"%default\"]"),
  make_option(c("-k", "--kmlfile"), type="character", default='converted_shapefile', help="Path to kml output file with no extension [\"%default\"]"),
  make_option(c("-n", "--shapename"), type="character", default='shapename', help="Name of line set as it should appear in kml file with no extension [\"%default\"]")
)
if(interactive()){
  setwd(pp(pevi.shared,'../ExampleDataSets/Road Network/'))
  args<-c('-s','Road Network.shp','-k','Road Network.kml','-n','Land Use')
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp-to-kml.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp-to-kml.R [options]"),positional_arguments=F)
}

######################################################################################################
# SITE ATTRIBUTES
# 
######################################################################################################
#gpclibPermit()

shapes <- readShapeLines(args$shapefile,proj4string=CRS('+proj=longlat +zone=15 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
writeLinesShape(shapes,pp(args$shapefile,"-new"))

#c.map <- paste(map.color(agg.taz$total.demand,blue2red(50)),'7F',sep='')
#shp.to.kml(agg.taz,pp(pevi.shared,'data/UPSTATE/kml/AggregatedTAZs.kml'),'Aggregated TAZs','','white',2,c.map,'shp.id','name',c('name','agg.id','total.demand'))

writeOGR(spTransform(shapes, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")),args$kmlfile,args$shapename,"KML",overwrite_layer=T)

# the following would later read the poly's back in, but they won't have the attribute data but we don't actually need that if we are maintaining a separate table with the
# custom attributes
#from.kml <- readOGR('sites-from-R.kml','sites')
