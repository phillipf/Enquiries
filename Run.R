detach("package:ABR", unload=TRUE)

library(devtools)
library(roxygen2)
remove.packages("GNAFAPI")
setwd("G:/Business and Environmental Services/Business Customer Programs/Customers/")
create("RDashboard")
setwd("C:/ABR")
document()
setwd("C:/")
install("GNAFAPI")
library(Enquiries)
setwd("C:/Enquiries")

