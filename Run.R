detach("package:Enquiries", unload=TRUE)

library(devtools)
library(roxygen2)
remove.packages("Enquiries")
#setwd("C:/")
#create("Enquiries")
setwd("C:/Enquiries")
document()
setwd("C:/")
install("Enquiries")
library(Enquiries)
setwd("C:/Enquiries")

