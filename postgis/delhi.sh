###############################################################
# INSTALL SOFTWARE NEEDED WITH HOMEBREW
###############################################################
brew install postgres
brew install postgis
brew install pgrouting

###############################################################
# CREATE A SPATIAL DB
# 
# this assumes postgres 9.X, postgis ?.? and pgrouting 2.0
###############################################################

# first make the DB, this assumes you have a login role names 'pev'
createdb delhi -O pev

# this may not be necessary
createlang plpgsql delhi

# the following assume you used homebrew to install postgres and postgis
psql delhi -c "create extension postgis"

# Now enable pgrouting on the DB
psql delhi -c "create extension pgrouting"

###############################################################
# LOAD DELHI NETWORK
###############################################################
# Now we load the road network data into the DB
shp2pgsql delhi_network | psql -d delhi -U pev

# These commands are meant to go from the SHELL, but you could just issue the SQL
# from the psql prompt (get there using: psql -d delhi -U pev):
psql -d delhi -U pev -c "CREATE OR REPLACE VIEW roads_ext AS SELECT *,ST_StartPoint(lines.dumpedgeom) AS startpoint,ST_EndPoint(lines.dumpedgeom) AS endpoint FROM (SELECT *,(ST_Dump(geom)).geom AS dumpedgeom FROM delhi_network ) AS lines"

psql -d delhi -U pev -c "CREATE TABLE node AS
  SELECT row_number() OVER (ORDER BY foo.p)::integer AS id, foo.p AS geom
  FROM (        
     SELECT DISTINCT roads_ext.startpoint AS p FROM roads_ext 
     UNION
     SELECT DISTINCT roads_ext.endpoint AS p FROM roads_ext
  ) foo
  GROUP BY foo.p"

psql -d delhi -U pev -c "CREATE TABLE network AS
  SELECT a.*, b.id as start_id, c.id as end_id
  FROM roads_ext AS a
     JOIN node AS b ON a.startpoint = b.geom
     JOIN node AS c ON a.endpoint = c.geom"
  
psql -d delhi -U pev -c "CREATE INDEX spatial_index ON network USING GIST (dumpedgeom)"

# also need to make gid a primary key on table network
psql -d delhi -U pev -c "ALTER TABLE network ADD COLUMN id BIGSERIAL PRIMARY KEY"

# use the following query to do the routing
# note this assumes we're using pgrouting 2.0

CREATE VIEW test_route AS
SELECT * 
   FROM network
   JOIN
   (SELECT * FROM pgr_astar('
      SELECT gid AS id, 
          start_id::int4 AS source, 
          end_id::int4 AS target, 
          runtime::float8 AS cost,
          ST_X(startpoint) AS x1, 
          ST_Y(startpoint) AS y1, 
          ST_X(endpoint) AS x2, 
          ST_Y(endpoint) AS y2
      FROM network',
      501,
      2426,
      false,
      false)) AS route
   ON
   network.gid = route.id2;
     

