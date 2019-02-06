# openEO Backend in R for proof-of-concept

A reference implementation for the openEO core API as a proof-of-concept written in R, utilizing the `plumber` package as a 
lightweight webserver. The webserver is not final and in terms of security aspects not optimized. The goal of this package 
is to provide a simplistic version of local [openEO conformant server backend](https://open-eo.github.io/openeo-api/) with the API version 0.3.1.

## Installation
Install the package by using `install_github` from the devtools package. If you run R on Windows then the packages are build from binaries, but on a Linux distribution the packages are compiled. This means that if yon a Linux OS you need to install some required system libraries,first. For Ubuntu this is:

```
sudo apt-get -y install libgdal-dev libcurl4-gnutls-dev libssl-dev libssh2-1-dev libsodium-dev gdal-bin libudunits2-dev
```

But also on Windows it is highly recommended to have GDAL installed and configured in the systems `PATH` environment variable.

After that you can install the R packages by running:

```
library(devtools)
install_github(repo="Open-EO/openeo-r-backend",ref="master")
```

When the back-end is started the first time or if the demo data is not present it will be downloaded from an external source. The demo data set contains two small spatio-temporal raster data sets. One is a NDVI raster time series calculated from Landsat-8 and the other one is a small spatial subset of Sentinel-2 data.

## Getting Started
After loading the package, you can create a server object by calling `createServerInstance()`. The server object is directly available as `openeo.server` in the global environment (`.GlobalEnv`). This object is intended to perform all server relevant tasks like managing references to the server data and processes as well as for starting the server.

The `openeo.server` can be customly configured by configuring and passing a `ServerConfig` object into the `createServerInstance` function. The configuration object has e.g. attributes like `data.path` and `workspace.path` to reference to the demo data or to the folder where the users data and job results are stored. As a default the downloaded demo data will be stored in the subfolder `data` under `config$workspaces.path`.

Note: please remove the '/' suffix from your directory paths. If the `workspaces.path` is not set explicitly, then it will assume to look and/or store the created data in the current working directory `getwd()`.

You then need to load the demo data and processes for the server or you need to develop and register your own Processes and Products. If you haven't already, then `loadDemo()` will download the sample data for you and store it under `/data` in the `workspace.path`
Also if you are starting the server for the first time, then you might create a user first. 

```
library(openEO.R.Backend)

config = ServerConfig()
config$workspaces.path = "path/to/back-end/workspace"
config$mapserver.url = "http://url/to/mapserver" #e.g. http://localhost:8080/cgi-bin/mapserv
config$rudfservice.url = "http://url/to/r-udf-service" #e.g. http://localhost:8010/udf

createServerInstance(configuration = config)

openeo.server$initEnvironmentDefault()
openeo.server$initializeDatabase()
openeo.server$createUser(user_name="test", password="test") #only created if not exists
openeo.server$loadDemo()

openeo.server$startup()
```

To stop the server you need to terminate the R session process (e.g. CTRL + C).

When you want to use the server on operational level, meaning you have created your user and just want to start the server for testing, you might be advised to store the code above (without the createUser command) in a separate R file and run it from the command line with the following command:

```
R -f path/to/your_file.R
```

### Additional Requirements
If you also want to use the R-UDF webservice implementation you need also to install and run [r-udf-service](https://github.com/Open-EO/openeo-r-udf). Also if you want to use
preliminary webservice support, you also need to install [mapserver](https://mapserver.org/).

## Docker installation
As an alternatively to the installation on the local machine, you can run the R backend on a docker machine. We provided an docker-compose file to take care of most of the business. Make sure you are able to run `docker-compose` on the targeted machine and run the following lines to set up the base server and the actual r backend. It is important that you build the baseserver before the openeo-r-server, because it will contain the basic server configuration for the application server (openeo-rserver).

```
docker-compose up -d
```

Note: Starting with version back-end version 0.3.1-X we will also provide docker images for the r-server with demo data and the r-udf-service on docker hub [openeor](https://hub.docker.com/u/openeor)

## Authentication / Authorization Behavior
On this local backend we consider three levels of access that require either _open access_, _basic authorization_ and _bearer token authorization_ depending on the called endpoint (see [api reference](https://open-eo.github.io/openeo-api/apireference/)). But basically we consider all meta data services that support exploration of data, processes and other functionalities as _open access_. Then _basic authorization_ is currently used for the authentication services (login), and finally the _bearer token authorization_ is applied on all services that are linked to the user like user workspace and job and service handling.

This means that you should be aware to use the proper HTTP headers in your requests. `Authorization: Basic <encoded_credentials>` at the login process and `Authorization: Bearer <token>` at the other authorized services. For the bearer token authorization you will send the token that you have retrieved at the login.

## Process Graphs for Proof-of-Concept

### [Use Case 1](https://open-eo.github.io/openeo-api/examples-poc/#use-case-1)
| | |
| --- | --- |
| Endpoint: | POST /preview or POST /jobs |
| Query-Configuration: | Authorization with Bearer-Token |

```JSON
{
    "process_graph": {
      "process_id": "min_time",
      "imagery": {
        "process_id": "NDVI",
        "imagery": {
          "process_id": "filter_bbox",
          "imagery": {
            "process_id": "filter_daterange",
            "imagery": {
              "process_id": "get_collection",
              "name": "sentinel2_subset"
            },
            "extent": ["2017-04-01T00:00:00Z", "2017-05-31T00:00:00Z"]
          },
          "extent": {
            "west": 700000,
            "south": 7898000,
            "east": 702960,
            "north": 7900000,
            "crs": "EPSG:32734"
          }
        },
        "nir": "B8",
        "red": "B4"
      }
    } ,
    "output": {
        "format": "GTiff"
    }
}
```


### [Use Case 3](https://open-eo.github.io/openeo-api/examples-poc/#use-case-3)
| | |
| --- | --- |
| Prerequisites: | An uploaded ["polygons.geojson"](https://raw.githubusercontent.com/Open-EO/openeo-r-client/master/examples/polygons.geojson) file in the users workspace (PUT /users/{user_id}/files/<path>)|
| Endpoint: | POST /jobs or POST /preview |
| Query-Configuration: | Authorization with Bearer-Token |


```JSON
{
    "process_graph": {
      "process_id": "zonal_statistics",
      "imagery": {
        "process_id": "filter_bbox",
        "imagery": {
          "process_id": "filter_bands",
          "imagery": {
            "process_id": "filter_daterange",
            "imagery": {
              "process_id": "get_collection",
              "name": "sentinel2_subset"
            },
            "extent": ["2017-01-01T00:00:00Z", "2017-05-31T23:59:59Z"]
          },
          "bands": "B8"
        },
        "extent": {
          "west": 22.8994,
          "south": -19.0099,
          "east": 22.9282,
          "north": -18.9825
        }
      },
      "regions": "/uc3/polygons.geojson",
      "func": "mean"
    },
    "output": {
        "format": "GPKG"
    }
}
```

If you are interested, then check the [openeo-r-client example](https://github.com/Open-EO/openeo-r-client/blob/master/examples/rbackend-uc3-example.R) for reference.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api/)
* [openEO YouTube channel](https://www.youtube.com/channel/UCMJQil8j9sHBQkcSlSaEsvQ)
