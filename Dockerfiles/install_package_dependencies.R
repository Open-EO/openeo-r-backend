cran.mirror = "https://cran.uni-muenster.de/"

install.packages("devtools",repos=cran.mirror)
library(devtools)

install_deps("/opt/dockerfiles",repos=cran.mirror)
install("/opt/dockerfiles")

cat("it worked")