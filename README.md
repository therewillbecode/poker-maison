# Poker Maison

## A multiplayer app crafted with Haskell and React

Supports multiple players in realtime on multiple tables.

Player moves are timed in order to ensure that games keep running if players disconnect.

The UI and backend are all implemented in less than seven thousand lines of code.

[![License: Unlicense](https://img.shields.io/badge/license-Unlicense-blue.svg)](http://unlicense.org/)

![alt text](https://s5.gifyu.com/images/ezgif.com-optimize-1e35dcba1eceb51f5.gif "Demo")

## How to run in docker

Skip this section if you would rather avoid docker.

### Docker Prerequisites

In order to use Docker have the following installed.

- [Docker](https://docs.docker.com/compose/install/) (17.12.0+)
- [Docker Compose](https://docs.docker.com/v17.09/engine/installation/)
- [Docker Machine](https://docs.docker.com/machine/install-machine/)

Firstly start Docker Machine

```bash
docker-machine start
```

Then set the correct variables in your terminal so you can connect to Docker Machine

```bash
eval $(docker-machine env)
```

Now build the images. This will take a while.

```
docker-compose up
```

Now go navigate to http://192.168.99.100:3000 in your browser and the app should be running.

The above ip address is the one for your docker-machine VM if you are on the default settings. By default docker-machine doesn't serve localhost but instead uses 192.168.99.100 as the host.

You can simulate multiple players in the same game on on your machine if you navigate to the above url in a few different browser tabs. Eac time you open up a new tab just remember to log out after you have signed in as the browser will cache the access_token for the last logged in user for each new tab as URL is the same.

## Common Docker Problems

### Docker has the wrong TLS setting

If you get the error below then Docker Compose is not using the correct TLS version.

```
Building web
ERROR: SSL error: HTTPSConnectionPool(host='192.168.99.100', port=2376): Max retries exceeded with url: /v1.30/build?q=False&pull=False&t=server_web&nocache=False&forcerm=False&rm=True (Caused by SSLError(SSLError(1, u'[SSL: TLSV1_ALERT_PROTOCOL_VERSION] tlsv1 alert protocol version (_ssl.c:727)'),))
```

You can fix this by setting the following environment variable with the correct TLS version.

```bash
export COMPOSE_TLS_VERSION=TLSv1_2
```

### Container runs out of memory

If the server docker container runs out of memory whilst building. Whis would look like this.

```
--  While building package Cabal-2.4.1.0 using:
      /root/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.4.0.1_ghc-8.6.5 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.4.0.1 build --ghc-options ""
    Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
```

Then set increase the memory available to the VM you are using for docker-machine.
Assuming your VM is named "default", run:

```bash
docker-machine stop default
VBoxManage modifyvm default --memory 4096
docker-machine start default
```

### Slow builds

If you want to speed up builds then replace `n` in the command below
with the number of cores your machine has and run the command.
The command below assumes that "default" is the name of the VM Docker Machine is using.

```bash
docker-machine stop default
VBoxManage modifyvm default --cpus n
docker-machine start default
```

# Building locally from scratch.

The following steps are based on an Ubuntu distribution.

## Back End

Firstly make sure you have ghc and stack installed in order to compile the back end written in Haskell.
If you need to install the Haskell platform then run

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Secondly install libpq (c bindings to postgres)

```bash
sudo apt-get install libpq-dev
```

Next install redis.

```bash
sudo apt-get install redis
```

Navigate to the server/ directory.

```bash
cd server
```

Compile the back end poker server.

```bash
stack build
```

## Now we need to set some config.

Ensure postgresql 10 is installed and running.

Set the env var so that the server has the postgresql connection string.
Of course you will need to change the db connection parameters below to match your local database.

```bash
export dbConnStr='host=0.0.0.0 port=5432 user=postgres dbname=pokerdbtest password=postgres
```

Set env variable with the secret key for generating auth tokens.
Note that this secret must be 32 characters long or it won't work.

```bash
export secret="changeme077cf4e7441c32d2d0a86b4c"
```

Lastly ensure redis-server is running in the background on default port

```bash
redis-server
```

Now run the server locally. The default user API port is 8000 and websocket port is 5000.

```bash
stack run
```

## Front End

Install node version 10.16.3 and then install yarn globally

```bash
npm i -g yarn@1.17.3
```

Install a required system dependency for node-sass .

```bash
sudo apt-get install libpng-dev
```

Navigate to the client/ directory with

```bash
cd client
```

Then just run.

```bash
yarn start
```

Now you are ready to play poker!

### Simulating a multiplayer game locally

You may want to play against yourself when you are developing locally so just
run the clients on two separate ports.

In your first terminal run

```
PORT=8001 yarn start
```

Then open another terminal and run

```
PORT=8002 yarn start
```

Now just open two tabs in your browser navigating to

```
localhost:8001
```

and

```
localhost:8002
```

## Running Tests

To run the test suite on the backend which has over a hundred tests

```bash
cd server && stack test
```

## Contributions Welcome

Have a look at the issues if you want some starting ideas on how to get involved.

Feel free to open any issues with potential enhancements or bugs you have found.

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
