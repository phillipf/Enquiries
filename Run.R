detach("package:Enquiries", unload=TRUE)

library(devtools)
library(roxygen2)
remove.packages("Enquiries")
setwd("G:/Business and Environmental Services/Business Customer Programs/Customers/")
create("RDashboard")
setwd("G:/Business and Environmental Services/Business Customer Programs/Customers/Dashboard")
document()
setwd("C:/")
install("Enquiries")
library(Enquiries)
setwd("C:/Enquiries")

