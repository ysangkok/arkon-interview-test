#!/usr/bin/env bash
set -xeuo pipefail
cd /home/janus/Desktop/arkon
rm -f new_data.zip
wget -O new_data.zip "https://datos.cdmx.gob.mx/explore/dataset/prueba_fetchdata_metrobus/download/?format=shp&timezone=America/Mexico_City&lang=es"
rm -f prueba_fetchdata_metrobus.*
unzip new_data.zip
rm new_data.zip
shp2pgsql -a prueba_fetchdata_metrobus.shp | psql
rm -f prueba_fetchdata_metrobus.*
