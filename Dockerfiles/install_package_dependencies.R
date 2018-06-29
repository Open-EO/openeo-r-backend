cran.mirror = "https://cran.uni-muenster.de/"

install.packages("devtools",repos=cran.mirror)
library(devtools)

install_deps("/opt/dockerfiles",repos=cran.mirror)
installation = list.files("/opt/dockerfiles",recursive=TRUE,full.names = TRUE)

# packages for the R UDFs
install_github("r-spatial/stars", dependencies=TRUE)

removed = file.remove(installation)
