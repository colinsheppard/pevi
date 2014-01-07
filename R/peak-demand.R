load.libraries(c('maptools','plyr','stringr','ggplot2','gdata','doMC','gpclib','RPostgreSQL','rPython','reshape'))
registerDoMC(10)
gpclibPermit()

dbuser<-"pev"
dbpassword<-""
dbname<-"grid"
dbhost<-"localhost"

con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

source(paste(pevi.home,'R/gis-functions.R',sep=''))

############################################################################################################
# The following is only necessary to load the grid database into postgres
############################################################################################################

installXLSXsupport()

grid.dir <- pp(pevi.shared,'/data/GRID/distribution-data/')

file.names <- grep("Rdata",grep("~",list.files(grid.dir),invert=T,value=T),invert=T,value=T)

#sheets <- grep("Substation",sheetNames(pp(grid.dir,file.names[1])),invert=T,value=T)
sheets <- c("Substation","SubBanks","Circuit Load Summary","Line Sections","Conductor Codes","Transformers","Customer Load","Generation","Structures")

# create the db tables
file.name <- file.names[1]
if(!file.exists('columns-names.Rdata')){
  cols <- list()
  for(sheet in sheets){
    r.data.name <- pp(grid.dir,file.name,"-",sheet,".Rdata")
    if(file.exists(r.data.name)){
      load(r.data.name)
    }else{
      dat <- read.xls(pp(grid.dir,file.name),sheet,stringsAsFactors=F)
      save(dat,file=r.data.name)
    }
    names(dat) <- tolower(str_replace(names(dat),"\\.","NUM"))
    dat.types <- lapply(dat[,!apply(dat,2,function(x){ all(is.na(x)) })],class)
    dat.types[dat.types=="numeric"] <- 'double precision'
    dat.types[dat.types=="character"] <- 'text'
    table.name <- str_replace_all(tolower(sheet)," ","_")
    cols[[table.name]] <- names(dat.types)
    sql <- pp('CREATE TABLE IF NOT EXISTS ',table.name,' ( ',pp(pp(names(dat.types),' ',dat.types),collapse=','),' );')
    dbSendQuery(con,sql)
  }
  save(cols,file='columns-names.Rdata')
}else{
  load('columns-names.Rdata')
}

# load the data
tot.trans <- 0
reload.db <- F
# optionally hold onto the data as it loads
sav.dat<-list()
for(sheet in sheets){
  table.name <- str_replace_all(tolower(sheet)," ","_")
  my.cat(table.name)
  system('sleep 0.01')
  sav.dat[[table.name]] <- list()
  for(file.name in file.names){
    r.data.name <- pp(grid.dir,file.name,"-",sheet,".Rdata")
    if(file.exists(r.data.name)){
      load(r.data.name)
    }else{
      my.cat(file.name)
      system('sleep 0.01')
      dat <- read.xls(pp(grid.dir,file.name),sheet,stringsAsFactors=F)
      save(dat,file=r.data.name)
    }
    if(sheet == "Transformers")tot.trans <- tot.trans + nrow(dat)

    names(dat) <- tolower(str_replace(names(dat),"\\.","NUM"))
    dat.types <- lapply(dat[,!apply(dat,2,function(x){ all(is.na(x)) })],class)
    dat.types[dat.types=="numeric"] <- 'double precision'
    dat.types[dat.types=="character"] <- 'text'
    dat <- dat[,names(dat.types)]
    dat[is.na(dat)] <- 'NULL'
    for(i in which(dat.types=='text')){
      dat[,i] <- pp("'",dat[,i],"'")
      dat[dat[,i]=="''",i] <- 'NULL'
    }
    # get rid of dupes
    if(table.name=="transformers"){
      dup.ids <- as.numeric(names(table(dat$device_id))[table(dat$device_id)>1])
      for(dup.id in dup.ids){
        sub <- subset(dat,device_id==dup.id)
        sub.i <- which(sub$summer_load_factor != 'NULL')[1]
        sub.i <- ifelse(is.na(sub.i),1,sub.i)
        to.delete <- as.numeric(rownames(sub)[-sub.i])
        if(length(to.delete)>0) dat <- dat[-to.delete,]
      }
    }
    dat[dat=="'NULL'"] <- 'NULL'
    if(reload.db){
      sql <- pp('TRUNCATE TABLE ',table.name)
      dbSendQuery(con,sql)
      sql <- pp('INSERT INTO ',table.name,' ( ',pp(cols[[table.name]],collapse=','),') \nVALUES \n',
        pp(apply(dat[,cols[[table.name]]],1,function(x){ pp("(",pp(x,collapse=","),")") }),'\n'),';')
      res <- sapply(sql,function(s){ 
        try(dbSendQuery(con,s),T)
      })
    }
    sav.dat[[table.name]][[file.name]] <- dat
  }
  if(sheet == "Transformers")print(pp('# transformers: ',tot.trans))
}

# troubleshooting, load all the transf data
sheet <- 'Transformers'
trans <- data.frame()
for(file.name in file.names){
  r.data.name <- pp(grid.dir,file.name,"-",sheet,".Rdata")
  if(file.exists(r.data.name)){
    load(r.data.name)
  }else{
    my.cat(file.name)
    system('sleep 0.01')
    dat <- read.xls(pp(grid.dir,file.name),sheet,stringsAsFactors=F)
    save(dat,file=r.data.name)
  }
  if(nrow(dat)>0){
    dat$file <- file.name
    trans <- rbind(trans,dat)
  }
}
dat <- trans
names(dat) <- tolower(str_replace(names(dat),"\\.","NUM"))
dat.types <- lapply(dat[,!apply(dat,2,function(x){ all(is.na(x)) })],class)
dat.types[dat.types=="numeric"] <- 'double precision'
dat.types[dat.types=="character"] <- 'text'
dat <- dat[,names(dat.types)]
dat[is.na(dat)] <- 'NULL'
for(i in which(dat.types=='text')){
  dat[,i] <- pp("'",dat[,i],"'")
  dat[dat[,i]=="''",i] <- 'NULL'
}
# get rid of dupes
dat$dev.id <- substr(dat$device_id,2,nchar(dat$device_id)-1)
dup.ids <- as.numeric(names(table(dat$dev.id))[table(dat$dev.id)>1])
to.delete <- c()
for(dup.id in dup.ids){
  sub <- subset(dat,dev.id==dup.id)
  sub.i <- which(sub$summer_load_factor != "NULL" & sub$summer_load_factor != "'NULL'")[1]
  sub.i <- ifelse(is.na(sub.i),1,sub.i)
  to.delete <- c(to.delete,as.numeric(rownames(sub)[-sub.i]))
}
if(length(to.delete)>0) dat <- dat[-to.delete,]
dat <- dat[,grep("dev.id",names(dat),invert=T)]
dat <- dat[,grep("file",names(dat),invert=T)]
if(reload.db){
  table.name <- str_replace_all(tolower(sheet)," ","_")
  my.cat(table.name)
  system('sleep 0.01')
  sql <- pp('TRUNCATE TABLE ',table.name)
  dbSendQuery(con,sql)
  sql <- pp('INSERT INTO ',table.name,' ( ',pp(cols[[table.name]],collapse=','),') \nVALUES \n',
    pp(apply(dat,1,function(x){ pp("(",pp(x,collapse=","),")") }),'\n'),';')
  res <- sapply(sql,function(s){ 
    try(dbSendQuery(con,s),T)
  })
}

# now add a geom to stuctures
sql <- "SELECT AddGeometryColumn('structures','geom',4326,'POINT',2,true);"
dbSendQuery(con,sql)
sql <- 'UPDATE structures SET geom=ST_SetSRID(ST_POINT(longitude,latitude),4326)'
dbSendQuery(con,sql)

# create views that join the geom to the other tables 
for(table.name in c('transformers','substation')){
  sql <- pp('CREATE OR REPLACE VIEW ',table.name,'_sp AS (SELECT foo.*,struc.geom FROM ',table.name,' foo LEFT JOIN structures AS struc ON foo.struc_id=struc.struc_id)')
  dbSendQuery(con,sql)
}

# create views that join the geom to the other tables through transformers
for(table.name in c('customer_load','generation')){
  sql <- pp('CREATE OR REPLACE VIEW ',table.name,'_sp AS (SELECT foo.*,trans.geom FROM ',table.name,' foo LEFT JOIN transformers_sp AS trans ON foo.trf_id=trans.device_id)')
  dbSendQuery(con,sql)
}

# now add a geom to lines
sql <- "UPDATE line_sections SET geom=subquery.newgeom FROM (SELECT line.line_id AS line_id,ST_MakeLine(struc.geom,struc2.geom) AS newgeom FROM line_sections AS line LEFT JOIN structures AS struc ON struc.struc_id=line.src_struc_id LEFT JOIN structures AS struc2 ON struc2.struc_id=line.load_struc_id) AS subquery WHERE line_sections.line_id=subquery.line_id ;"
dbSendQuery(con,sql)

# now add a geom to load_summary
sql <- "SELECT AddGeometryColumn('circuit_load_summary','geom',4326,'MULTILINESTRING',2,true);" 
dbSendQuery(con,sql)
sql <- "SELECT fdr_num FROM circuit_load_summary"
circ <- na.omit(dbGetQuery(con,sql))
for(circ.id in circ$fdr_num){
  sql <- pp("UPDATE circuit_load_summary SET geom=subquery.newgeom FROM (SELECT ST_Union(line.geom) AS newgeom from line_sections AS line where line.fdr_num=",circ.id,") AS subquery WHERE fdr_num=",circ.id)
  dbSendQuery(con,sql)
}

# looks like transformer load factors have different units 
sql <- "SELECT * FROM transformers"
tran <- dbGetQuery(con,sql)
dim(subset(tran, summer_load_factor>1))
sql <- "UPDATE transformers SET summer_load_factor=summer_load_factor/100.0 WHERE summer_load_factor > 1"
dbSendQuery(con,sql)
sql <- "UPDATE transformers SET winter_load_factor=winter_load_factor/100.0 WHERE winter_load_factor > 1"
dbSendQuery(con,sql)
sql <- "UPDATE transformers SET winter_load_factor=-winter_load_factor WHERE winter_load_factor < 0"
dbSendQuery(con,sql)

############################################################################################################
# Analysis
############################################################################################################

# make some heat maps
sql <- "SELECT cust_id, cust_typ AS type, ST_X(geom) AS long, ST_Y(geom) AS lat, sum_aug_kwhr AS load FROM customer_load_sp"
all.load <- na.omit(dbGetQuery(con,sql))
ggplot(all.load,aes(x=load))+geom_histogram()+scale_x_log10()+facet_wrap(~type,scales="free_y")
res <- subset(all.load,type=="DOM" & load < 5000)
res <- subset(all.load,type=="DOM" & load > 3000)
load <- ddply(res,.(cust_id),function(df){ 
  if(df$load<1000)df$load <- 1000
  data.frame(rep=rep(1,df$load/1000),df$long,df$lat)
})

heatmap.kml(res$long,res$lat,filepath="test.kml",dotsize=round(res$load/max(res$load)*100),opacity=100,width=2048*4,height=2048*4)
heatmap.kml(load$df.long,load$df.lat,filepath="test.kml",dotsize=20,opacity=100,width=2048*4,height=2048*4)

# load the line sections
sql <- "SELECT cust_id, cust_typ AS type, ST_X(geom) AS long, ST_Y(geom) AS lat, sum_aug_kwhr AS load FROM customer_load_sp"
all.load <- na.omit(dbGetQuery(con,sql))

# transformers & load
sql <- "SELECT * FROM transformers"
tran <- dbGetQuery(con,sql)
names(tran) <- c('trf_id',tail(names(tran),-1))

sql <- "SELECT * FROM customer_load"
ld <- dbGetQuery(con,sql)
# test out join
join(subset(tran,trf_id%in%as.numeric(names(table(ld$trf_id))[table(ld$trf_id)>1][1])),subset(ld,trf_id%in%as.numeric(names(table(ld$trf_id))[table(ld$trf_id)>1][1])),by='trf_id')
# do join
tran.ld <- ddply(join(tran,ld,by='trf_id'),.(trf_id,cust_typ),function(df){
  data.frame(df[1,c('primary_voltage_code','secondary_voltage_code','nameplate_kva','fdr_num','summer_load_factor','winter_load_factor')],cust_count=sum(df$cust_count),sum_aug_kwhr=sum(df$sum_aug_kwhr),sum_jan_kwhr=sum(df$sum_jan_kwhr))
})

# plot summer vs winter load on transformers
ggplot(tran.ld,aes(x=sum_jan_kwhr,y=sum_aug_kwhr,colour=factor(nameplate_kva)))+geom_point()+facet_wrap(~cust_typ)+scale_x_log10()+scale_y_log10()+geom_abline(intercept=0,slope=1)
# mean of values by customer type
ddply(tran.ld,.(cust_typ),function(df){ ldply(df,mean,na.rm=T) })

ggplot(melt(tran.ld,id.vars='cust_typ',measure.vars=c('sum_aug_kwhr','sum_jan_kwhr')),aes(x=value))+geom_histogram()+facet_grid(cust_typ~variable)+scale_x_log10()

cit <- readShapePoly(pp(pevi.shared,'data/HUMCO/cities-wgs84',sep=''))
cit$name <- cit$NAME

# Associate each circuit load summary with a substation
sql <- "SELECT fdr_num,season,peak_time,total_kw,max_nor_voltage,ckt_bpf,kva_capability,energ_kvar,com_kw,limit_desc,projected,ST_X(ST_Centroid(geom)) AS long,ST_Y(ST_Centroid(geom)) AS lat FROM circuit_load_summary"
circ <- dbGetQuery(con,sql)
circ$substation.name <- NA
circ$circuit.num <- NA
for(file.name in names(sav.dat[['circuit_load_summary']])){
  substation <- pp(head(strsplit(strsplit(file.name,".xlsx")[[1]]," ")[[1]],-1),collapse=" ")
  circuit.num <- tail(strsplit(strsplit(file.name,".xlsx")[[1]]," ")[[1]],1)
  circ$substation.name[match(sav.dat[['circuit_load_summary']][[file.name]]$fdr_num,circ$fdr_num)] <- substation
  circ$circuit.num[match(sav.dat[['circuit_load_summary']][[file.name]]$fdr_num,circ$fdr_num)] <- circuit.num
}
write.csv(circ,pp(pevi.shared,'/data/GRID/distribution-data/circuits.csv'))
