DATABASE=$1
SCHEMA=$2

for LAYER in tmpf wd ws rh
do
    raster2pgsql -t 80x80 -I -C ${LAYER}_to_sample.tif $SCHEMA.$LAYER | psql -h localhost -U $DATABASE
done
