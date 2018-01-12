# openEO Backend in R for proof-of-concept

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

A reference implementation for the openEO core API as a proof-of-concept written in R, utilizing the `plumber` package as a lightweight webserver. The webserver is not final and in terms of
security aspects not optimized. The goal of this package is to provide a simplistic version of local [openEO conformant server backend](https://open-eo.github.io/openeo-api-poc/).

## Installation
Install the package by using `install_github` from the devtools package.

```
library(devtools)
install_github(repo="flahn/openeo-r-backend",ref="master")
```

The package contains a set of example data in `inst/extdata`, so it might take some more time to install as usual. The data can be found in the packages install directory under `/extdata`

## Getting Started
After loading the package an object called _openeo.server_ exists, which can be configured. With the parameters `data.path` and `workspaces.path` you configure where to look for 
the example data sets provided by this package and where to store newly created users, their data and jobs. As a default value for the data the server will set the directory of
the package installation. Note: please remove the '/' suffix from your directory paths. If the `workspaces.path` is not set explicitly, then it will assume to look and/or store the
created data in the current working directory `getwd()`.  
You then need to load the demo data and processes for the server or you need to develop and register your own Processes and Products.  
Also if you are starting the server for the first time, then you might create a user first. If there was already a user created in the specified `workspaces.path`, then it will be
loaded.

```
library(openEO.R.Backend)
openeo.server$data.path =  paste(system.file(package="openEO.R.Backend"),"extdata",sep="/")
openeo.server$workspaces.path = "somewhere/on/computer"

openeo.server$createUser(user_name="test", password="test")
openeo.server$loadDemo()
openeo.server$startup(port = 8000)
```

To stop the server you need to terminate the R session process (e.g. CTRL + C).

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api-poc/)
