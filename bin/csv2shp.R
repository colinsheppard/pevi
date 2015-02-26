#!/usr/bin/Rscript
##############################################################################################################################################
# Script to convert shape files to KML format for use in Google Earth
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','maptools','stringr','ggplot2','rgdal','plotKML'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-c", "--csvfile"), type="character", default='input_csvfile', help="Input file in csv format [\"%default\"]"),
  make_option(c("-l", "--latcol"), type="character", default='latitude', help="Name of column in csv file with latitude [\"%default\"]"),
  make_option(c("-g", "--loncol"), type="character", default='longitude', help="Name of column in csv file with longitude [\"%default\"]"),
  make_option(c("-s", "--shapefile"), type="character", default='output_shapefile', help="output point file name with no extension [\"%default\"]")
)
if(interactive()){
  setwd('~/downs/')
  args<-c('-c','stations.csv','-l','Latitude','-g','Longitude','-s','stations')
  args <- parse_args(OptionParser(option_list = option_list,usage = "csv2shp.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "csv2shp.R [options]"),positional_arguments=F)
}

######################################################################################################
#gpclibPermit()

pts <- read.csv(args$csvfile)
pts[,c(args$latcol,args$loncol)]

shp.pts <- SpatialPointsDataFrame(pts[,c(args$loncol,args$latcol)],data=pts,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

writeSpatialShape(shp.pts,args$shapefile)

