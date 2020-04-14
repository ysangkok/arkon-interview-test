This file covers, in five sections summarized:

* technology STACK and reasoning
* an OVERVIEW of files I developed to implement the requirements
* how to SET UP the project to get it running and be able to test
* EXAMPLES of commands to execute in a set up environment
* a DIAGRAM of the Kubernetes pods and protocols used between them
* IMPROVEMENT IDEAS for how the project could be improved
* TROUBLESHOOTING with tips on how to resolve issues I encountered
  with Kubernetes
* SQL EXAMPLES which shows a useful SQL command that I have not
  implemented in the GraphQL API.

Technology stack
================

I chose Haskell for the server because it has a great type system, so I am
reasonably confident that most corner cases have been taken care of.

I chose PostGIS because it is the most popular GIS package I know, which ties
into a very popular SQL database. SQL is the industry standard language for
data storage, which makes the approach able to scale up to large amounts of
data.

Overview of files
=================

* postgres-*.yaml  Kubernetes resource files for the PostgreSQL server.
                   The Service depends on the Pod in the container provided by
                   the StatefulSet which needs the ConfigMap to function.

* webserver-deployment.yaml Kubernetes resource file which provides the GraphQL
                            server. The server connects to the PostgreSQL
                            database and submits SQL queries. It also has a
                            webserver as its entrypoint, which means it starts
                            when the Pod starts.

* haskell/         Directory with Haskell source solution.
                   This directory specifies the container used by the
                   `webserver-deployment.yaml` resource definition file.

  - arkon.cabal    This is a project definiton file for the build system Cabal.
                   It contains the dependencies list and lists source files
                   like `Demo.hs` and `WebServer.hs`, which are built into
                   executable artifacts called `webserver` and `demo`.

  - Server.hs      This is the main implementation file. It contains a function
                   called `api` which takes a PostgreSQL server DNS name and
                   returns a function taking encode GraphQL queries. The
                   function will parse the GraphQL query against a schema which
                   is hardcoded in this file (but which is also available in
                   `schema.gql`).

  - Demo.hs        Contains a list of sample queries based on the requirements
                   list in the problem description document. This file simply
                   imports the `api` function from `Server.hs` and uses the
                   same GraphQL library that the server uses to generate
                   queries for the API. The results of the queries
                   are reported in the console. See below for instructions
                   on how to run it.

  - WebServer.hs   Offers a very minimal webserver which simply receives
                   GraphQL queries and passes them to the `api` function from
                   `Server.hs`. I have made this since GraphQL is typically
                   used over HTTP.

  - Dockerfile     This file contains the build script for the Docker image
                   used in `webserver-deployment.yaml`. The image contains both
                   Haskell binaries: `webserver` and `demo`.
                   The `webserver` is automatically started when the image
                   launches.
                   Also included are the two scripts for data ingestion:
                   `arkon-init.sh` and `arkon-fetchdata.sh`.
                   The Dockerfile installs the dependencies for these scripts
                   and for the Haskell programs, and also the `libpq-dev`
                   library which is needed to build the Postgres client library
                   for Haskell.

  - schema.gql     This file contains the GraphQL schema implemented by the
                   server. It is used when compiling the client, to generate
                   datatype definitions representing queries, arguments and
                   replies. This schema format could be used by any GraphQL
                   implementation to generate queries.

  - scripts/       This directory contains bash scripts that are used to
                   download and unzip city districts and bus locations.
                   After unpacking, the records are loaded into a database.
                   The two scripts are both installed to `/usr/bin` such that
                   they are on the `$PATH` and easily executable.
                   The data is downloaded in Shapefile format, since that
                   format can easily be converted to PostGIS-flavored
                   SQL statements using the `shp2pgsql` tool which is installed
                   using `apt-get` in the `Dockerfile`.

    + arkon-init.sh   This script can be run to initialize the database,
                      and download the current hours' sample data from the CDMX
                      government. It must be manually executed after starting
                      the system for the first time.
                      This file creates an SQL VIEW, which SELECT's the data
                      we need from the ingested tables and parses the
                      timestamps into datatypes that PostgreSQL can manipulate.
                      It also uses the PostGIS function `ST_Contains` to
                      calculate the corresponding city district (alcaldía)
                      for each location.

    + arkon-fetchdata.sh  This script downloads current data,
                          (new data is released every hour)
                          which is added to the table created by the
                          `arkon-init.sh` script.
                          Make sure you don't run this script too often, as
                          that will result in duplicate data entries. I call
                          this script five minutes past the hour, every hour.

Setup
=====

You need either:
- a Kubernetes installation.
  Kubernetes will automatically download all required containers, since I have
  pushed them to Docker Hub.
  (tested with Minikube 1.9.3 with the Docker driver and Docker v19.03.8)
- or, alternatively a PostGIS / GHC 8.8 setup.
  Cabal (the Haskell package manager) will automatically download all
  dependencies and build them, but this takes quite a while.
  (tested on Ubuntu 18.04, with official PostgreSQL packages from
   http://apt.postgresql.org/pub/repos/apt/README , hvr's ppa with GHC 8.8.3
   and cabal-install-3.2)

Here are instructions for the Kubernetes based setup:

The PostgreSQL container needs a PersisentVolume, which can be automatically
provided, but I think it is dependent on either of the plugins
'default-storageclass' or 'storage-provisioner' being enabled.

To see the list of plugins enabled, you can use

  $ minikube addons list

First, we need to start Minikube (I use the Docker driver):

  $ minikube start

It reports Kubernetes version 18 for me.

Load the PostgreSQL server config map:

  $ kubectl create -f postgres-config.yaml

Start the StatefulSet which will start the PostgreSQL server Pod:

  $ kubectl create -f postgres-stateful.yaml

This will take a while, you can monitor the progress with

  $ kubectl describe pods postgres-demo-0

You can connect the PostgreSQL server like this after it has started:

  $ kubectl exec -i --tty postgres-demo-0 -- psql -U postgres

We need to expose the PostgreSQL server, so that it is reachable from the other
Pod:

  $ kubectl create -f postgres-service.yaml

You can use this command to see the IP of the service:

  $ kubectl describe services postgres

Notice how the endpoint IP and port. You can connect to the PostgreSQL
server from your host machine, using this IP/port, if you have the `psql`
client installed:

  $ psql -h 172.17.0.2 -U postgres postgres

See the last section for some tips on how to use the PostgreSQL prompt. You
can quit it with Control-D.

Now, let's start the GraphQL server:

  $ kubectl create -f webserver-deployment.yaml

Similar to before, you can monitor the progress with

  $ kubectl describe pods arkon-webserver

Make sure you can connect to the SQL server from the webserver:
(note that you can use tab completion for pseudo-random Pod name suffix!)

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- \
      psql --host=postgres-demo-0 -U postgres

Before the webserver is usable, the database needs to be initialized with
sample data from the CDMX government website:

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- \
      arkon-init.sh postgres-demo-0

This command creates a view called `refined`, and it can be run only once
per database! You can use `psql` to check which tables and views exist.
If the command `\d+ refined` shows an error, the database has not been
successfully initialized.

Now, the GraphQL server is ready to use, see the next section for example
usage.

Every hour, the sample data on the CDMX government website is update. To append
the data from the website to the current (already initialized) database, you
can use:

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- \
      arkon-fetchdata.sh --update postgres-demo-0

Commands to try out with a running environment
==============================================

To run the demo, which will show a few example queries
from the problem description (source in Demo.hs):

  kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- \
    sh -c 'LC_ALL=C.UTF-8 demo postgres-demo-0 '

To run an arbitrary GraphQL query:

  kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- \
    curl -d '{"query": "query Q {ubicaciones { uVehicleId } } "}' \
    localhost:3000/api

This query shows the vehicle ID's of all recorded locations.

Diagram of Kubernetes pods
==========================

- - - - - - - - -K-u-b-e-r-n-e-t-e-s- -p-o-d-s- - - - - - - - +
|                                                             |
| +----------------------------+                              |
| | PostgreSQL + PostGIS       |                              |
| | See postgres-stateful.yaml |                              |
| | for the Kubernetes config  |                              |
| +----------------------------+                              |
|           ^                                                 |
|           | PostgreSQL over port 5432                       |
|           v                                                 |
| +---------------------------------------------------------+ |
| | Haskell GraphQL pod with a webserver and a demo program | |
| | (built with Dockerfile, published as                    | |
| |  ysangkok/arkon-webserver on Docker Hub)                | |
| | See webserver-deployment.yaml for the Kubernetes config | |
| +---------------------------------------------------------+ |
|           ^                         ^                       |
+-----------|-------------------------|-----------------------+
            | Read-only               | Commands like 'arkon-update.sh'
            | GraphQL over HTTP       | or data emitted through
            |                         | keystrokes for a PostgreSQL prompt
            v                         v
  +-----------+                     +---------------------------+
  | End users |                     | Administrator with rights |
  +-----------+                     | to make arbitrary changes |
                                    +---------------------------+

The end users may be connected with a web browser, and use a frontend which is
not contained in this project, but it could use one of the many available
GraphQL implementation libraries for JavaScript.

The administrator would connect using the `kubectl` commands shown above.

Ideas for improvment
====================

* GraphQL subscriptions could be added.

  Subscriptions could offer an end user with updates as soon as they are
  available. This would be implemented by defining the `subscriptionResolver`
  in `Server.hs`.

* The database could use indexes.

  Currently, no indexes are created, which means the database has to do
  sequential scans through the data on each request. Indexes would improve
  performance if there were a lot of data in the database. But since I only
  have a few thousand rows, indexes are not needed. PostgreSQL offers ANALYZE
  functionality, which allows debugging whether an index is being used for a
  query with bad performance.

* Duplicate data entries could be avoided.

  Right now, nothing is preventing the user from running `arkon-fetchdata.sh`
  too often, which would result in duplicate entries. This is easy to avoid and
  a bit complicated to prevent, so I didn't mitigate this.
  In my own testing, I used `cron` to run the script according to a schedule of
  5:00-24:00, every hour, since there are presumably no locations to append
  during the night.

* Logging could be added.

  If the client submits a malformed query, or if the database cannot be
  connected to, it is currently unlikely that the administrator would find out.
  By adding a logging solution, better reliability could be achieved. Also, it
  could guide optimization efforts because one could easily discover which
  queries take the most time.

* Unit tests could be added.

  Mainly I would like to test that the business logic returns the expected
  results with a given data set. Currently, the database backend is not
  parameterized, and `postgresql-simple` is used. This makes testing
  cumbersome because it requires a running database server to run the test
  suite. I would prefer to have the test suite run with ephemeral storage.
  It is simpler since you wouldn't have a chance of data-loss by running
  against the wrong instance, for example.

  One way to achieve this would be to use another database library that
  allows for parameterization of the database backend, and one could use
  SQLite's memory backend for testing.

  The issue with this approach is that SQLite does not support PostGIS
  functions.

  So another approach could be to have a separate database server for testing.
  This is maybe easier, but also requires Kubernetes for running the tests.
  I consider Kubernetes pretty heavy-weight, and it would be sad if it was
  necessary to use it just to run the unit tests.

* Test coverage could be determined.

  I have not researched how to increase test coverage. To make sure every
  case has been tested, coverage would be helpful.

Troubleshooting
===============

Error on service and postgres-demo-0 not reachable
--------------------------------------------------

Quote from 'kubectl describe services':

  Error updating Endpoint Slices for Service default/postgres:
    Error deleting postgres-dvk6d EndpointSlice for Service default/postgres:
      endpointslices.discovery.k8s.io "postgres-dvk6d" not found

This is caused when a Service for a Pod created by the StatefulSet remains
after the StatefulSet/Pod is deleted.

Solution: Make sure the Service is deleted when deleting the StatefulSet and
          its Pod.


Error on PostgreSQL pod and postgres-demo-0 not reachable
---------------------------------------------------------

Quote from 'kubectl describe pods':

  Error:
    stat /tmp/hostpath-provisioner/pvc-cfd29fd1-fd6d-4b8f-b0ab-36ba56f53d67:
      no such file or directory

This is caused when PersistentVolumes and PersistentVolumeClaims are lingering
after a StatefulSet is deleted.

Solution: Make sure the PersistentVolume and PersistentVolumeClaim are deleted
          after deleting the StatefulSet. Apparently they are not deleted
          automatically even though they are created automatically...


SQL prompt examples
===================

You can use PostgreSQL commands like `\d+` to examine the tables and
their schemata.

I use this prompt to experiment with commands before they are implemented in
GraphQL. For example:

# SELECT COUNT(vehicle_id) as c, alcaldia
    FROM refined
    GROUP BY alcaldia
    ORDER BY c desc;
 c  |      alcaldia
----+---------------------
 71 | Cuauhtémoc
 71 | Gustavo A. Madero
 17 | Benito Juárez
 13 | Venustiano Carranza
 13 | Iztapalapa
 11 | Iztacalco
 10 | Miguel Hidalgo
  8 | Tlalpan
  7 | Azcapotzalco
  5 | Álvaro Obregón
  5 | Coyoacán
(11 rows)

This shows that Cuauhtémoc is the most visited city district.
