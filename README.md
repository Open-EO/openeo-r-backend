# openEO Backend in R for proof-of-concept

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

A reference implementation for the openEO core API as a proof-of-concept written in R, utilizing the `plumber` package as a lightweight webserver. The webserver is not final and in terms of
security aspects not optimized. The goal of this package is to provide a simplistic version of local [openEO conformant server backend](https://open-eo.github.io/openeo-api/).

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
You then need to load the demo data and processes for the server or you need to develop and register your own Processes and Products. If you haven't already, then `loadDemo()` will download the sample data for you and store it under `/data` in the `workspace.path`
Also if you are starting the server for the first time, then you might create a user first. 

```
library(openEO.R.Backend)
openeo.server$workspaces.path = "somewhere/on/computer"

openeo.server$initEnvironmentDefault()
openeo.server$initializeDatabase()

openeo.server$createUser(user_name="test", password="test")
openeo.server$loadDemo()
openeo.server$startup(port = 8000)
```

To stop the server you need to terminate the R session process (e.g. CTRL + C).

When you want to use the server on operational level, meaning you have created your user and just want to start the server for testing, you might be advised to store the code above (without the createUser command) in a separate R file and run it from the command line with the following command:

```
R -f path/to/your_file.R
```

## Docker installation
As an alternatively to the installation on the local machine, you can run the R backend on a docker machine. We provided an docker-compose file to take care of most of the business. Make sure you are able to run `docker-compose` on the targeted machine and run the following lines to set up the base server and the actual r backend. It is important that you build the baseserver before the openeo-rserver, because it will contain the basic server configuration for the application server (openeo-rserver).

```
docker-compose build openeo-baseserver
docker-compose build openeo-rserver
docker-compose start openeo-rserver
```

Note: preparing the base server will take a considerable amount of time. But when it is done, then you can install newer versions of the backend faster, since the baseserver will contain R and all required dependencies.

## Authentication / Authorization Behavior
On this local backend we consider three levels of access that require either _open access_, _basic authorization_ and _bearer token authorization_. For _open access_ we consider all meta data services that support exploration of data, processes and other functionalities. Then _basic authorization_ is currently used for the authentication services (login), and finally the _bearer token authorization_ is applied on services for the job management and the user data.

This means that you should be aware to use the proper HTTP headers in your requests. `Authorization: Basic <encoded_credentials>` at the login process and `Authorization: Bearer <token>` at the other authorized services. For the bearer token authorization you will send the token that you have retrieved at the login.

## Notes
There are some minor variations to the openEO API, regarding naming of the endpoints. Due to the different access methods we use multiple _plumber_ routes that run on a shared _root_ route. By doing this we cannot leave an endpoint blank, which means that some enpoints require a trailing `/`. For example, you will need to query `GET http://host:port/api/processes/` to fetch the list of offered processes. The basic rule of thump here, is that all the endpoints directly after `/api/xxx` need the trailing slash, but not the basic server endpoints like
`capabilities`.

## Process Graphs for Proof-of-Concept

### [Use Case 1](https://open-eo.github.io/openeo-api/poc/index.html#use-case-1-deriving-minimum-ndvi-measurements-over-pixel-time-series-of-sentinel-2-imagery)
| | |
| --- | --- |
| Endpoint: | POST /jobs or POST /execute |
| Query-Configuration: | Authorization with Bearer-Token |

```JSON
{
    "process_graph": {
      "process_id": "find_min",
      "args": {
        "imagery": {
          "process_id": "calculate_ndvi",
          "args": {
            "imagery": {
              "process_id": "filter_daterange",
              "args": {
                "imagery": {
                  "product_id": "sentinel2_subset"
                },
                "from": "2017-04-01",
                "to": "2017-05-01"
              }
            },
            "nir": 8,
            "red": 4
          }
        }
      }
    },
    "output": {
        format: "GTiff"
    }
}
```


### [Use Case 3](https://open-eo.github.io/openeo-api/poc/index.html#use-case-3-compute-time-series-of-zonal-regional-statistics-of-sentinel-2-imagery-over-user-uploaded-polygons)
| | |
| --- | --- |
| Prerequisites: | An uploaded ["polygons.geojson"](https://raw.githubusercontent.com/Open-EO/openeo-r-client/master/examples/polygons.geojson) file in the users workspace (PUT /users/me/files/<path>)|
| Endpoint: | POST /jobs or POST /execute |
| Query-Configuration: | Authorization with Bearer-Token |

```JSON
{
    "process_graph": {
      "process_id": "zonal_statistics",
      "args": {
        "imagery": {
          "process_id": "filter_daterange",
          "args": {
            "imagery": {
              "process_id": "filter_bands",
              "args": {
                "imagery": {
                  "product_id": "sentinel2_subset"
                },
                "bands": "8"
              }
            },
            "from": "2017-04-01",
            "to": "2017-07-01"
          }
        },
        "regions": "/users/me/files/polygons.geojson",
        "func": "median"
      }
    },
    "output": {
        format: "GPKG"
    }
}
```

If you are interested, then check the [openeo-r-client example](https://github.com/Open-EO/openeo-r-client/blob/master/examples/rbackend-uc3-example.R) for reference.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api/)
* [openEO YouTube channel](https://www.youtube.com/channel/UCMJQil8j9sHBQkcSlSaEsvQ)
