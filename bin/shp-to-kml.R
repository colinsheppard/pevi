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
  make_option(c("-k", "--kmlfile"), type="character", default='converted_shapefile', help="Path to kml output file with no extension [\"%default\"]"),
  make_option(c("-n", "--shapename"), type="character", default='shapename', help="Name of polygon set as it should appear in kml file with no extension [\"%default\"]")
)
if(interactive()){
  setwd(pp(pevi.shared,'data/google-earth/aggregated-taz-unweighted'))
  args<-c('-s','aggregated-taz-unweighted','-k','test.kml','-n','Shapename')
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp-to-kml.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp-to-kml.R [options]"),positional_arguments=F)
}

######################################################################################################
# SITE ATTRIBUTES
# 
######################################################################################################
#gpclibPermit()

shapes <- readShapePoly(args$shapefile,proj4string=CRS('+proj=longlat +zone=15 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
writePolyShape(shapes,pp(args$shapefile,"-new"))

#c.map <- paste(map.color(agg.taz$total.demand,blue2red(50)),'7F',sep='')
#shp.to.kml(agg.taz,pp(pevi.shared,'data/UPSTATE/kml/AggregatedTAZs.kml'),'Aggregated TAZs','','white',2,c.map,'shp.id','name',c('name','agg.id','total.demand'))

writeOGR(spTransform(shapes, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")),args$kmlfile,"shapes","KML",overwrite_layer=T)

# the following would later read the poly's back in, but they won't have the attribute data but we don't actually need that if we are maintaining a separate table with the
# custom attributes
#from.kml <- readOGR('sites-from-R.kml','sites')
