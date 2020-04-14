This file covers, in five sections summarized:

* technology choices and reasoning
* an overview of files I developed to implement the requirements
* how to set up the project to get it running and be able to test
* examples of commands to execute in a set up environment
* a diagram of the Kubernetes pods and protocols used between them
* ways in which the project could be improved

Technology choices
==================
I chose Haskell for the server because it has a great type system, so I am
reasonably confident that most corner cases have been taken care of.

I chose PostGIS because it is the most popular GIS package I know, which ties
into a very popular SQL database, which is the industry standard language for
data storage, which makes the approach be able to scale onto large amounts of
data.

Overview of files
=================

* postgres-*.yaml           Kubernetes resource files for the PostgreSQL server
                            The Service depends on the Pod in the container
                            provided by the StatefulSet which needs the
                            ConfigMap to function.

* webserver-deployment.yaml Kubernetes resource file which provides the GraphQL
                            server. The server connects to the PostgreSQL
                            database, and reads SQL records.  It also has a
                            webserver as it's entrypoint, which means it starts
                            with the Pod.

* haskell/         Directory with Haskell source solution.
                   This directory specifies the container used by the
                   `webserver-deployment.yaml` resource definition file.

  - arkon.cabal    This is a project definiton file for the build system Cabal.
                   It contains the dependencies list and lists source files
                   like `Demo.hs` and `WebServer.hs`, which are built into
                   executable artifacts called 'webserver' and 'demo'.

  - Server.hs      This is the main implementation file. It contains a function
                   called `api` which takes a PostgreSQL server name and
                   returns a function taking encode GraphQL queries. The
                   function will parse the GraphQL query against a schema which
                   is hardcoded in this file (but which is also available in
                   `schema.gql`).

  - Demo.hs        Contains a list of sample queries based on the requirements
                   list in the problem description document. This file simply
                   imports the `api` function from `Server.hs` and uses the
                   same GraphQL library that the server uses to generate
                   queries for the API.

  - WebServer.hs   Offers a very minimal webserver which simply
                   receives GraphQL queries and passes them to
                   the `api` function from `Server.hs`.

  - Dockerfile     This file contains the build script for the Docker image
                   used in `webserver-deployment.yaml`. The image contains both
                   Haskell binaries: `webserver` and `demo`.
                   The `webserver` is automatically started when the image
                   launches.
                   Also included are the two scripts for data ingestion:
                   `arkon-init.sh` and `arkon-fetchdata.sh`.
                   The Dockerfile contains installs the dependencies for these
                   scripts and for the Haskell program, and also the libpq-dev
                   library which is needed to build the Postgres client library
                   for Haskell.

  - schema.gql     This file contains the GraphQL schema implemented by the
                   server. It is used when compiling the client, to generate
                   datatype definitions representing queries, arguments and
                   replies. This schema format could be used by any GraphQL
                   implementation to generate queries.

  - scripts/       This directory contains bash scripts that are used to
                   download, unzip city district and bus locations.
                   After unpacking, the records are loaded into a database.
                   They are both installed to `/usr/bin`.
                   The data is downloaded in Shapefile format, since that
                   format can easily be converted to PostGIS-flavored
                   SQL statements using the `shp2pgsql` tool which is installed
                   using `apt-get` in the `Dockerfile`.

    + arkon-init.sh   This script can be run to initialize the database,
                      and download sample data from the CDMX government.
                      The script is installed to `/usr/bin`, so that it is
                      in the `$PATH`. It must be manually executed after
                      starting the system for the first time.

    + arkon-fetchdata.sh  This script downloads current data,
                          (new data is released every hour)
                          which is added to the table created by the
                          `arkon-init.sh` script.
                          Make sure you don't run this script too often,
                          as that will result in duplicate data entries.

Setup
=====

You need either:
- a Kubernetes installation to run the project
  (tested with Minikube 1.9.3 with the Docker driver and Docker v19.03.8)
- or, alternatively a PostGIS / GHC 8.8 setup
  (tested on Ubuntu 18.04, with hvr's ppa and cabal-install-3.2)

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

Start the StatefulSet which will start the PostgreSQL server pod:

  $ kubectl create -f postgres-stateful.yaml

This will take a while, you can monitor the progress with

  $ kubectl describe pods postgres-demo-0

You can connect the PostgreSQL server like this after it has started:

  $ kubectl exec -i --tty postgres-demo-0 -- psql -U postgres

We need to expose the PostgreSQL server, so that it is reachable
from the other pod:

  $ kubectl create -f postgres-service.yaml

You can use this command to see the IP of the service:

  $ kubectl describe services postgres

Notice how the endpoint IP and port.  You can connect to the PostgreSQL
server from your host machine, using this IP/port, if you have the `psql`
client installed:

  $ psql -h 172.17.0.2 -U postgres postgres

Now, let's start the GraphQL server:

  $ kubectl create -f webserver-deployment.yaml

Similar to before, you can monitor the progress with

  $ kubectl describe pods arkon-webserver

Make sure you can connect to the SQL server from the webserver:
(note that you can use tab completion for pseudo-random pod name suffix!)

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- psql --host=postgres-demo-0 -U postgres

Before the webserver is usable, the database needs to be initialized
with sample data from the CDMX government website:

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- arkon-init.sh postgres-demo-0

This command creates tables and views, so it can be run only once!

Now, the GraphQL server is ready to use, see the next section for example usage.

Every hour, the sample data on the CDMX government website is update.
To append the data from the website to the current (already initialized)
database, you can use:

  $ kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- arkon-fetchdata.sh --update postgres-demo-0

Commands to try out with a running environment
==============================================

To run the demo, which will show a few example queries
from the problem description (source in Demo.hs):

  kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- sh -c 'LC_ALL=C.UTF-8 demo postgres-demo-0 '

To run an arbitrary GraphQL query:

  kubectl exec -i --tty arkon-webserver-869bf6967c-7r9b7 -- curl -d '{"query": "query Q {ubicaciones { uVehicleId } } "}' localhost:3000/api

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
            |                         | Commands like 'arkon-update.sh'
            | GraphQL over HTTP       | or data emitted through
            |                         | keystrokes for a PostgreSQL prompt
            v                         v
  +-----------+                     +---------------------------+
  | End users |                     | Administrator with rights |
  +-----------+                     | to make arbitrary changes |
                                    +---------------------------+

The end users may be connected with a web browser, and use a frontend
which is not contained in this project, but it could use one of the
many available GraphQL implementation libraries for JavaScript.

The administrator would connect using the `kubectl` commands shown above.

Ideas for improvment
====================

* GraphQL subscriptions could be added.

  Subscriptions could offer an end user with updates as soon as they are
  available. This would be implemented by defining the `subscriptionResolver`
  in `Server.hs`.

* Test coverage could be determined.

  I have not researched how to increase test coverage. To make sure every
  case has been tested, coverage would be helpful.

* The database could use indexes.

  Currently, no indexes are created, which means the database has to do
  sequential scans through the data on each request. Indexes would improve
  performance if there were a lot of data in the database. But since I only
  have a few thousand rows, indexes are not needed. PostgreSQL offers ANALYZE
  functionality, which allows debugging whether an index is being used for a
  query with bad performance.

* Duplicate data entries could be avoided.

  Right now, nothing is prevent the user from running `arkon-fetchdata.sh`
  too often, which would result in duplicate entries. This is easy to avoid and
  a bit complicated to prevent, so I didn't mitigate this.
  On my own installation, I used `cron` to run the script according to a
  schedule of 5-24, every hour, since there are presumably no locations to
  append during the night.

* Logging could be added.

  If the client submits a malformed query, or if the database cannot be
  connected to, it is currently unlikely that the administrator would find
  out. By adding a logging solution, better reliability could be achieved.
  Also, it could guide optimization efforts because one could easily discover
  which queries take the most time.
