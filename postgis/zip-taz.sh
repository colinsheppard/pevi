# load the zip data into the database 
shp2pgsql ../../data/CA-ZIPS/tl_2010_06_zcta510.shp | psql -U pev -d pev

# load the TAZ data into the database
shp2pgsql ../../data/GEATM-2020/Shape_Files/TAZ.shp | psql -U pev -d pev

raster2pgsql eureka-e.dem | psql -U pev -d pev
raster2pgsql weed-w.dem -a eureka-e | psql -U pev -d pev
raster2pgsql ukiah-w.dem -a eureka-e | psql -U pev -d pev
raster2pgsql redding-w.dem -a eureka-e | psql -U pev -d pev
raster2pgsql noyo_canyon-e.dem -a eureka-e | psql -U pev -d pev
raster2pgsql crescent_city-e.dem -a eureka-e | psql -U pev -d pev

# then issue following psql command to aggregate the rasters

CREATE TABLE "north_coast" AS (select 1 AS rid,st_union(rast,'MEAN') AS rast from "eureka-e");

# export raster to .tif file, note gdal must be installed as: 
# brew install gdal --complete --with-postgres

gdal_translate -a_nodata -9999 -co "TFW=YES" "PG:host=localhost dbname=pev user=pev table=north_coast mode=2" north-coast.tif

# this can be imported into QGIS to verify that it worked

# import roads into DB
shp2pgsql HumRoads | psql -U pev -d pev

# now sample from the north_coast table along the road network

SELECT ST_AsText(points.geom) AS point, 
  ST_Value(img.rast,points.geom) AS elev 
  FROM north_coast AS img, 
    (SELECT ST_Pointn(lines.geom,generate_series(1,ST_NPoints(lines.geom))) AS geom 
      FROM 
      (SELECT (ST_Dump(geom)).geom AS geom 
        FROM humroads LIMIT 5) AS lines) 
    AS points;

# to use pgrouting, install these
#brew install https://raw.github.com/mxcl/homebrew/5f5439c47f1d3d37370b3265163b54e856b7fe23/Library/Formula/gaul.rb
#brew install https://raw.github.com/mxcl/homebrew/72f866597d498a6c0bea302b0ae73d9032736f74/Library/Formula/pgrouting.rb

# enable routing functions on DB
psql -U pev -d pev -f /usr/local/Cellar/pgrouting/1.05/share/postlbs/routing_core.sql
psql -U pev -d pev -f /usr/local/Cellar/pgrouting/1.05/share/postlbs/routing_core_wrappers.sql


CREATE OR REPLACE VIEW humroads_ext AS SELECT *,ST_StartPoint(lines.dumpedgeom) AS startpoint,ST_EndPoint(lines.dumpedgeom) AS endpoint FROM (SELECT *,(ST_Dump(geom)).geom AS dumpedgeom FROM humroads ) AS lines;

CREATE OR REPLACE VIEW roads_ext AS SELECT *,ST_StartPoint(lines.dumpedgeom) AS startpoint,ST_EndPoint(lines.dumpedgeom) AS endpoint FROM (SELECT *,(ST_Dump(geom)).geom AS dumpedgeom FROM roads ) AS lines;

CREATE TABLE node AS
  SELECT row_number() OVER (ORDER BY foo.p)::integer AS id, foo.p AS geom
  FROM (        
     SELECT DISTINCT humroads_ext.startpoint AS p FROM humroads_ext WHERE functional != 'Centroids'
     UNION
     SELECT DISTINCT humroads_ext.endpoint AS p FROM humroads_ext  WHERE functional != 'Centroids'
  ) foo
  GROUP BY foo.p;
CREATE TABLE node AS
  SELECT row_number() OVER (ORDER BY foo.p)::integer AS id, foo.p AS geom
  FROM (        
     SELECT DISTINCT roads_ext.startpoint AS p FROM roads_ext 
     UNION
     SELECT DISTINCT roads_ext.endpoint AS p FROM roads_ext
  ) foo
  GROUP BY foo.p;


CREATE TABLE network AS
  SELECT a.*, b.id as start_id, c.id as end_id
  FROM humroads_ext AS a
     JOIN node AS b ON a.startpoint = b.geom
     JOIN node AS c ON a.endpoint = c.geom
  WHERE a.functional != 'Centroids';

CREATE TABLE network AS
  SELECT a.*, b.id as start_id, c.id as end_id
  FROM roads_ext AS a
     JOIN node AS b ON a.startpoint = b.geom
     JOIN node AS c ON a.endpoint = c.geom;
  
CREATE INDEX spatial_index ON network USING GIST (dumpedgeom);

# also need to make gid a primary key on table network
ALTER TABLE network ADD COLUMN id BIGSERIAL PRIMARY KEY;

# For upstate, I need to create points along the road network

CREATE VIEW test_route AS
SELECT * 
   FROM network
   JOIN
   (SELECT * FROM shortest_path_astar('
      SELECT gid,cost AS id, 
          start_id::int4 AS source, 
          end_id::int4 AS target, 
          (length/ab_speed)::float8 AS cost,
          ST_X(startpoint) AS x1, 
          ST_Y(startpoint) AS y1, 
          ST_X(endpoint) AS x2, 
          ST_Y(endpoint) AS y2
      FROM network',
      4211,
      332,
      false,
      false)) AS route
   ON
   network.gid = route.edge_id;
     
CREATE OR REPLACE VIEW test_route AS
SELECT * 
   FROM network
   JOIN
   (SELECT * FROM shortest_path_astar('
      SELECT gid AS id,
          start_id::int4 AS source, 
          end_id::int4 AS target, 
          id::float8 AS cost,
          ST_X(startpoint) AS x1, 
          ST_Y(startpoint) AS y1, 
          ST_X(endpoint) AS x2, 
          ST_Y(endpoint) AS y2
      FROM network',
      8,
      4,
      false,
      false)) AS route
   ON
   network.gid = route.edge_id;
     

# For UPSTATE/DELHI I ended up building routable DB's using OSM
#
# first I downloaded the .osm file for California:
#http://download.geofabrik.de/north-america/us/california.html

# then I extracted just the region of interest using osmosis (SIS, SHA, TEH)
osmosis --read-xml california-latest.osm enableDateParsing=no --bounding-box top=42.066 left=-123.675 right=-121.264 bottom=39.728 --write-xml upstate.osm

# then I used osm2pgrouting to create the routable tables:
osm2pgrouting -file upstate.osm -conf /Users/sheppardc/Dropbox/bin/osm/mapconfig.xml -dbname upstate_route -user pev -clean

# then I discoverd that the source/target id fields in "ways" did not get filled in by osm2pgrouting, 
# so first we had to change the data type of nodes.id to integer by creating a new field "newid" and using the following to seed with incrementing ints

CREATE SEQUENCE seq;
update nodes set newid=nextval('seq');

# then drop constraint and id field and change name of newid to id and add constraint back

# now update ways
update ways set source=nodes.id from nodes where ways.x1=nodes.lon and ways.y1=nodes.lat;
update ways set source=nodes.id from nodes where ST_Distance(ST_Point(ways.x1,ways.y1),ST_Point(nodes.lon,nodes.lat))=MIN(ST_Distance(ST_Point(ways.x1,ways.y1),ST_Point(nodes.lon,nodes.lat)))
ways.x1=nodes.lon and ways.y1=nodes.lat;
update ways set target=nodes.id from nodes where ways.x2=nodes.lon and ways.y2=nodes.lat;

# finally, make sure gid is a primary key on ways

# then routing queries go like this:

CREATE OR REPLACE VIEW test_route AS
SELECT * 
   FROM ways 
   JOIN
   (SELECT * FROM shortest_path_astar('
      SELECT gid AS id,
          source, 
          target, 
          length::float8 AS cost,
          x1, 
          y1, 
          x2, 
          y2
      FROM ways',
      5,
      202,
      false,
      false)) AS route
   ON
   ways.gid = route.edge_id;

