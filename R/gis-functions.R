# area.of.union
# this gives you the area of the union of any number of SpatialPolygonsDataFrame objects including multiple rows within
# make sure that both objects are in the same projection before attempting
#
# arguments must be an object of class "SpatialPolygonsDataFrame" or a list of such objects 
# each data frame can have multiple rows and the union over all rows of all data frames is taken

area.of.union <- function(spatial.polys.list){
  if(inherits(spatial.polys.list,"SpatialPolygonsDataFrame")){
    spatial.polys.list <- list(spatial.polys.list)
  }
  first.union <- T
  for(spatial.polys.list.i in 1:length(spatial.polys.list)){
    for(row.i in nrow(spatial.polys.list[[spatial.polys.list.i]]@data)){
      polys <- slot(spatial.polys.list[[spatial.polys.list.i]][row.i,], "polygons")  
      for(polys.i in 1:length(polys)){
        polys.polys <- slot(polys[[polys.i]], "Polygons")
        for(polys.polys.i in 1:length(polys.polys)){
          pp.coords <- slot(polys.polys[[polys.polys.i]], "coords")
          if(first.union){
            result.gpc.poly <- as(pp.coords, "gpc.poly")
            first.union <- F
          }else{
            result.gpc.poly <- gpclib:::union(result.gpc.poly,as(pp.coords, "gpc.poly"))
          }
        }
      }
    }
  }
  area.poly(result.gpc.poly)
}

map.color <- function (x,c.map){
  c.map[round((length(c.map)-1)*(x-min(x))/diff(range(x))+1,0)]
}

shp.to.kml <- function(shp,kml.filename,kmlname="KML Name", kmldescription="<i>Description</i>",borders='white',lwds=1.5,colors='red',id.col='id',name.col='id',description.cols=NA){
  n <- length(shp@polygons)
  if(length(colors)==1)colors <- rep(colors,n)
  if(length(borders)==1)borders <- rep(borders,n)
  if(length(lwds)==1)lwds <- rep(lwds,n)
  kml.data <- sapply(slot(shp, "polygons"), function(x) { 
    row.num = which(as.numeric(slot(x, "ID"))==shp@data[[id.col]])
    descrip = ifelse(is.na(description.cols),'',paste(paste(description.cols,': ',shp@data[row.num,description.cols],sep=''),collapse='<br/><br/>')) 
    kmlPolygon(x,
      name=shp@data[[name.col]][row.num], 
      col=colors[row.num], lwd=lwds[row.num], border=borders[row.num], 
      description=descrip
    )})

  kmlFile <- file(kml.filename, "w")
  cat(kmlPolygon(kmlname=kmlname, kmldescription=kmldescription)$header, 
      file=kmlFile, sep="\n")
  cat(unlist(kml.data["style",]), file=kmlFile, sep="\n")
  cat(unlist(kml.data["content",]), file=kmlFile, sep="\n")
  cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
  close(kmlFile)

  system(paste('open ',kml.filename,sep=''))
}
print('map.color, shp.to.kml loaded')
