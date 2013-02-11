# area.of.union
# NOTE - this fails to properly account for holes in polygons, use gArea and gUnion from package rgeos instead (see TAZ-weighting.R for example)
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
          if(slot(polys.polys[[polys.polys.i]], "hole")){ pp.coords <- pp.coords[nrow(pp.coords):1,] }
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
  res <- c.map[round((length(c.map)-1)*(x-min(x,na.rm=T))/diff(range(x,na.rm=T))+1,0)]
  res[is.na(res)] <- "#000000"
  return(res)
}

#shp <- agg.taz
#kml.filename <- paste(path.to.google,'optim/',optim.code,'-pen',100*pev.penetration,'.kml',sep='')
#kmlname <- "KML Name"
#kmldescription <- "<i>Description</i>"
#borders<-'white'
#lwds<-1.5
#colors<-'red'
#id.col<-'id'
#name.col<-'id'
#description.cols<-NA

chargers.to.kml <- function(shp,kml.filename,kmlname="KML Name", kmldescription="<i>Description</i>",borders='white',lwds=1.5,colors='red',id.col='id',name.col='id',description.cols=NA){

    
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
  shp$long <- coordinates(shp)[,1]
  shp$lat  <- coordinates(shp)[,2]
  shp$newname <- NA
  eka.dense.rows <- c(grep("EKA_",shp$name),which(shp$name %in% c("Bayview","Cutten","Myrtletown","FieldsLanding")))
  mck.dense.rows <- grep("MCK_",shp$name)
  for.dense.rows <- grep("FOR_",shp$name)
  arc.dense.rows <- grep("ARC_",shp$name)
  dense.rows <- c(eka.dense.rows,mck.dense.rows,for.dense.rows,arc.dense.rows)
  shp.sparse <- rbind(collapse.df(shp@data[eka.dense.rows,],"Eureka"),
    collapse.df(shp@data[arc.dense.rows,],"Arcata"),
    collapse.df(shp@data[for.dense.rows,],"Fortuna"),
    collapse.df(shp@data[mck.dense.rows,],"McKinleyville"))
  placemark.header <- "	<Folder>
		<name>Charger Distribution</name>
		<open>1</open>"
  placemark.styles <- ddply(rbind(shp@data,shp.sparse),.(row),function(df){ 
    data.frame(style=paste("<Style id=\"placemark_",df$name,"\">
		<IconStyle>",
		if(sum(round(c(df$L2,df$L3)))<1) "<color>00ffffff</color>
		<scale>2.0</scale>" 
		else 
		"<scale>2.0</scale>","
			<Icon>
				<href>http://www.schatzlab.org/images/pev-charger-icons/charger-icon-",roundC(df$L2,0),"-",roundC(df$L3,0),".png</href>
			</Icon>
			<hotSpot x=\"0.5\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>
		</IconStyle>",
		if(sum(round(c(df$L2,df$L3)))==0) "<LabelStyle>
			<color>00ffffff</color>
			</LabelStyle>","
  </Style>",sep=''),stringsAsFactors=F)
  })$style
  placemark.kml <- c("<Folder><name>Regular Zones</name><open>1</open>
			<Region>
				<LatLonAltBox>
					<north>41.5</north>
					<south>40</south>
					<east>-123.4</east>
					<west>-124.4</west>
				</LatLonAltBox>
				<Lod>
					<minLodPixels>256</minLodPixels>
				</Lod>	
			</Region>",ddply(shp@data[-dense.rows,],.(row),function(df){ 
    data.frame(placemark=paste("
			<Placemark>
				<name>",df$name,"</name>
				<styleUrl>#placemark_",df$name,"</styleUrl>
				<Point>
					<coordinates>",
					ifelse(any(df$name==hard.code.coords$name),paste(hard.code.coords$lon,",",hard.code.coords$lat,",",0),paste(df$long,",",df$lat,",0")),"</coordinates>
				</Point>
			</Placemark>",sep=''),stringsAsFactors=F)
  })$placemark,"</Folder>")
  placemark.kml <- c(placemark.kml,"<Folder>
		<name>Aggregated Zones</name>
		<open>1</open>
        ",ddply(shp.sparse,.(row),function(df){ 
    data.frame(placemark=paste("
			<Placemark>
        <Region>
          <LatLonAltBox>
            <north>",df$lat+0.75,"</north>
            <south>",df$lat-0.75,"</south>
            <east>",df$long+0.5,"</east>
            <west>",df$long-0.5,"</west>
          </LatLonAltBox>
          <Lod>
            <minLodPixels>256</minLodPixels>
            <maxLodPixels>8192</maxLodPixels>
          </Lod>	
        </Region>
				<name>",df$name,"</name>
				<styleUrl>#placemark_",df$name,"</styleUrl>
				<Point>
					<coordinates>",df$long,",",df$lat,",0</coordinates>
				</Point>
			</Placemark>",sep=''),stringsAsFactors=F)
  })$placemark,"</Folder>")
  dense.placemark.kml <- ""
  for(sparse.name in c("Arcata","Eureka","McKinleyville","Fortuna")){
    shp.row <- which(shp.sparse$name == sparse.name)
    dense.placemark.kml <- c(dense.placemark.kml,paste("					<Folder>
              <name>",sparse.name," Chargers</name>
              <open>1</open>
              <Region>
                <LatLonAltBox>
                  <north>",shp.sparse$lat[shp.row]+0.04,"</north>
                  <south>",shp.sparse$lat[shp.row]-0.04,"</south>
                  <east>",shp.sparse$long[shp.row]+0.02,"</east>
                  <west>",shp.sparse$long[shp.row]-0.02,"</west>
                </LatLonAltBox>
                <Lod>
                  <minLodPixels>256</minLodPixels>
                  <maxLodPixels>-1</maxLodPixels>
                </Lod>
              </Region>",sep=''))
    use.rows <- unlist(ifelse(sparse.name=="Arcata",list(arc.dense.rows),
                  ifelse(sparse.name=="Eureka",list(eka.dense.rows),
                    ifelse(sparse.name=="McKinleyville",list(mck.dense.rows),list(for.dense.rows)))))
    dense.placemark.kml <- c(dense.placemark.kml,ddply(shp@data[use.rows,],.(row),function(df){ 
      data.frame(placemark=paste("
        <Placemark>
          <name>",df$name,"</name>
          <styleUrl>#placemark_",df$name,"</styleUrl>
          <Point>
            <coordinates>",
					ifelse(any(df$name==hard.code.coords$name),paste(hard.code.coords$lon,",",hard.code.coords$lat,",",0),paste(df$long,",",df$lat,",0")),"</coordinates>
          </Point>
        </Placemark>",sep=''),stringsAsFactors=F)
    })$placemark)
    dense.placemark.kml <- c(dense.placemark.kml,"</Folder>")
  }

  placemark.footer <- "	</Folder>"
  cat(placemark.header, file=kmlFile, sep="\n")
  cat(placemark.styles, file=kmlFile, sep="\n")
  cat(placemark.kml, file=kmlFile, sep="\n")
  cat(dense.placemark.kml, file=kmlFile, sep="\n")
  cat(placemark.footer, file=kmlFile, sep="\n")
  cat(unlist(kml.data["style",]), file=kmlFile, sep="\n")
  cat(unlist(kml.data["content",]), file=kmlFile, sep="\n")
  cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
  close(kmlFile)

  system(paste('open ',kml.filename,sep=''))
}

collapse.df <- function(df,newname){
  df$newname <- newname
  ddply(data.frame(df),.(newname),function(df){
    res <- df[1,]
    res$name <- df$newname[1]
    res$L2 <- sum(round(df$L2))
    res$L3 <- sum(round(df$L3))
    res$lat <- mean(df$lat)
    res$long <- mean(df$long)
    res
  })
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
