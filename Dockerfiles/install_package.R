library(devtools)

# install the R UDF package from github
install_github("pramitghosh/OpenEO.R.UDF",ref="v0.0.1",dependencies=TRUE)

# install the R back-end package
install("/opt/dockerfiles")
