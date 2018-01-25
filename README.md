# openEO Backend in R for proof-of-concept

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

A reference implementation for the openEO core API as a proof-of-concept written in R, utilizing the `plumber` package as a lightweight webserver. The webserver is not final and in terms of
security aspects not optimized. The goal of this package is to provide a simplistic version of local [openEO conformant server backend](https://open-eo.github.io/openeo-api-poc/).

## Installation
Install the package by using `install_github` from the devtools package.

```
library(devtools)
install_github(repo="Open-EO/openeo-r-backend",ref="master")
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

When you want to use the server on operational level, meaning you have created your user and just want to start the server for testing, you might be advised to store the code above (without the createUser command) in a separate R file and run it from the command line with the following command:

```
R -f path/to/your_file.R
```

## Authentication / Authorization Behavior
On this local backend we consider three levels of access that require either _open access_, _basic authorization_ and _bearer token authorization_. For _open access_ we consider all meta data services that support exploration of data, processes and other functionalities. Then _basic authorization_ is currently used for the authentication services (login), and finally the _bearer token authorization_ is applied on services for the job management and the user data.

This means that you should be aware to use the proper HTTP headers in your requests. `Authorization: Basic <encoded_credentials>` at the login process and `Authorization: Bearer <token>` at the other authorized services. For the bearer token authorization you will send the token that you have retrieved at the login.

## Notes
There are some minor variations to the openEO API, regarding naming of the endpoints. Due to the different access methods we use multiple _plumber_ routes that run on a shared _root_ route. By doing this we cannot leave an endpoint blank, which means that some enpoints require a trailing `/`. For example, you will need to query `GET http://host:port/api/processes/` to fetch the list of offered processes. Somehow akward it gets at the job endpoint, where you will need to `POST http://host:port/api/jobs/?evaluate=lazy` to upload your job.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api-poc/)
