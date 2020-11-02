DATABASE=$1
SCHEMA=$2

for LAYER in ign
do
    raster2pgsql -I -C $LAYER.tif $SCHEMA.$LAYER | psql -h localhost -U $DATABASE
done
