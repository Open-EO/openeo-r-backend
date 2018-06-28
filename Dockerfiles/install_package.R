library(devtools)

# install the R UDF package from github
install_github("flahn/OpenEO.R.UDF",dependencies=TRUE)

# install the R back-end package
install("/opt/dockerfiles")
