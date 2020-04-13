#!/usr/bin/env bash
set -xeuo pipefail
cd /tmp
wget -O alcaldias.zip "https://datos.cdmx.gob.mx/explore/dataset/alcaldias/download/?format=shp&timezone=America/Mexico_City&lang=es"
unzip alcaldias.zip
shp2pgsql alcaldias.shp | psql --host=$1 -U postgres
rm alcaldias.*
arkon-fetchdata.sh --create $1
psql --host=$1 -U postgres <<'EOF'
CREATE VIEW refined AS
 SELECT bus.gid,
    bus.vehicle_id::integer AS vehicle_id,
    st_aslatlontext(bus.geom) AS lat_lon_text,
    st_asewkb(bus.geom) AS ewkb,
    to_timestamp(bus.date_update::text, 'YYYY-MM-DD HH24:MI:SS'::text) AS time_update,
    alcaldias.nomgeo AS alcaldia
   FROM prueba_fetchdata_metrobus bus,
    alcaldias
  WHERE st_contains(alcaldias.geom, bus.geom);
EOF
