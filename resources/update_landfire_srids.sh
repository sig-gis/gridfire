# [[file:../org/GridFire.org::*Importing Rasters into the Database][Importing Rasters into the Database:6]]
#!/bin/sh

DATABASE=$1
SCHEMA=$2
SRID=$3

for LAYER in asp cbd cbh cc ch dem fbfm13 fbfm40 slp
do
    psql -h localhost -U $DATABASE -c \
         "SELECT UpdateRasterSRID('$SCHEMA'::name,'$LAYER'::name,'rast'::name,$SRID);"
done
# Importing Rasters into the Database:6 ends here
