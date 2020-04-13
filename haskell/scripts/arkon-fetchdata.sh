#!/usr/bin/env bash
set -xeuo pipefail
if [[ "$1" != "--create" && "$1" != "--update" ]]; then
  echo "First argument must be --create or --update."
  exit 1
fi
cd /tmp
mkdir arkon || true
cd arkon
wget -O new_data.zip "https://datos.cdmx.gob.mx/explore/dataset/prueba_fetchdata_metrobus/download/?format=shp&timezone=America/Mexico_City&lang=es"
unzip new_data.zip
rm new_data.zip
if [[ $1 == "--create" ]]; then
  ARG=""
elif [[ $1 == "--update" ]]; then
  ARG=-a
fi
shp2pgsql $ARG prueba_fetchdata_metrobus.shp | psql --host=$2 -U postgres
rm prueba_fetchdata_metrobus.*
