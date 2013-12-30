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
  make_option(c("-s", "--shapefile"), type="character", default='input_shapefile', help="Path to polygon shape file with no extension [\"%default\"]"),
  make_option(c("-t", "--shapetype"), type="character", default='polygon', help="Shape type: polygon line point [\"%default\"]"),
  make_option(c("-k", "--kmlfile"), type="character", default='converted_shapefile', help="Path to kml output file with no extension [\"%default\"]"),
  make_option(c("-n", "--name"), type="character", default='shapename', help="Name of polygon set as it should appear in kml file with no extension [\"%default\"]"),
  make_option(c("-p", "--projstring"), type="character", default='+proj=longlat +zone=15 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0', help="Projection string of input [\"%default\"]")
)
if(interactive()){
  setwd(pp(pevi.shared,'data/UPSTATE/ExampleDataSets/'))
  args<-c('-s','AggregatedTAZs.shp','-k','AggregatedTAZs.kml','-n','Aggregated TAZs')
  args<-c('-s','Fake_Roads.shp','-k','Fake_Roads.kml','-n','Fake Roads','-t','line')
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp2kml.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp2kml.R [options]"),positional_arguments=F)
}

# Make shapetype a bit more user-friendly
args$shapetype <- tolower(args$shapetype)
if(substr(args$shapetype,nchar(args$shapetype), nchar(args$shapetype)) == "s"){
  args$shapetype <- substr(args$shapetype,1,nchar(args$shapetype)-1)
}


######################################################################################################
# SITE ATTRIBUTES
# 
######################################################################################################
#gpclibPermit()

if(args$shapetype == "polygon"){
  shapes <- readShapePoly(args$shapefile,proj4string=CRS(args$projstring))
}else if(args$shapetype == "line"){
  shapes <- readShapeLines(args$shapefile,proj4string=CRS(args$projstring))
}

writeOGR(spTransform(shapes, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")),args$kmlfile,args$name,"KML",overwrite_layer=T)

