# openEO Backend in R for proof-of-concept

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

A reference implementation for the openEO core API as a proof-of-concept written in R, utilizing the `plumber` package as a lightweight webservice.

## Installation
Copy the `R/api.R` and `data/` to a separate folder and adapt the paths in the `openeo` environment or set `openeo$project.path` to the folder where you have downloaded this repository.

## Getting Started
Customize the port of the webservice via `openeo$api.port`, if not set then 8000 is used as a default.