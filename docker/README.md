![](https://www.docker.com/sites/default/files/Whale%20Logo332_5.png)
# Docker

## Setting up environment 🔨 

1. To run Slamdata on docker with pre populated databases of choice, you will firstly need to install docker on your machine from [here](https://www.docker.com/community-edition).

2. Now you will need Node _(^v6.x.x)_ which can be downloaded from [here](https://nodejs.org/en/download/)

3. In your shell, navigate to root of this repo and run `npm i gulp bower -g && npm i && bower i`

4. Finally `npm run docker:<name of database>` 
for example if you want to start Slamdata with a mongoDb then `npm run docker:mongo`

**databases currently available are:**

&nbsp; `couchbase`
&nbsp; `marklogic`
&nbsp; `mongo`

## Mounting a database on Slamdata ✅ 

1. After setting up environment in the shell you should see `... is running Slamdata & Quasar on http://localhost:<port>`

2. Open your favourite browser and navigate to that address (list of port given bellow)

3. You will now be on the home screen and you can select the mount option and use following details given bellow.

**Couchbase:**
&nbsp; url: `http://localhost:63175`
&nbsp; host: `couchbase` port: `11210`
&nbsp; bucket name: `testDb` password: `couchbase`

**Marklogic:**
&nbsp; url: `http://localhost:63177`
&nbsp; host: `marklogic` port: `8000`
&nbsp; username: `marklogic` password: `marklogic`
&nbsp; database: `testDb` format: `JSON`

**MongoDB:**
&nbsp; url: `http://localhost:63176`
&nbsp; host: `mongo` port: `27017`
&nbsp; username: `mongo` password: `mongo`
&nbsp; database: `testDb`

## Useful scripts :scroll:
To get a quick idea of how docker is structured read the following [article](https://docs.docker.com/engine/docker-overview/#docker-objects).

- `docker ps -a` --  show all current docker containers
- `npm run docker:stop:all` -- this stops all docker containers running
- `npm run docker:remove:all:containers` -- this will stop and remove all containers
- `docker stop <container name>` -- if you want to stop only one container, you can chain names.
- `docker rm <container name>` -- remove single container, can also chain for multiples.
- `docker images` -- this will show you the current images you have locally
- `docker:remove:all:images` -- will remove all images
- `docker rmi <imageID>` -- to remove a specific image

## Running tests on different environments

To run tests locally, you can now do so using docker containers to aid you.
After running tests there is an active container which has in it what ever the tests carried out. Which can be really useful for debugging. When test fails you have a carbon copy of where they failed.

**scripts:**

- 👼 `npm run test:<databasename>` -- this way will remove any existing running docker containers and start from scratch rebuild everything and run tests.
- 😈 `npm run test:<databasename>:fast` -- alternatively you can chose to not rebuild your docker container and `build:psa`. 

To use this script you should be using purescript-ide in the background to compile your PureScript and `npm run watch:bundle` for compiling the frontend.
***WARNING*** Thought this is much faster the downside is you need to make sure that all mounted workspaces and databases are removed manually before running this command again! 

**Travis:**
Travis uses caching for the docker containers, keep in mind if you change the version of Quasar or any other docker related build steps the travis cache will need to be removed.
