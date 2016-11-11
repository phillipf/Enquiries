#' Memo clean Function
#'
#' This function splits the MEMO column into MEC1:MEC5 
#' @param MEMO, USES.
#' @keywords MEMO
#' @export
#' @examples
#' MEMO()

Memo <- function(MEMO, USES) {
  
  if (requireNamespace("stringr", quietly = TRUE)) {
    library(stringr)
  }
  

  x2 = tolower(MEMO)
  
  x3 = str_split_fixed(x2, "\\?", 3)
  
  fun <- function(x) {
    str_trim(gsub("^[[:alpha:]]+(\\s+=|:)", "", x))
  }
  
  x5 <- apply(x3, 2, fun)
  
  id = which(grepl("-$", x5[,2]))
  
  text <- str_extract(x5[,3][id], "^[[:alpha:]]+")
  
  x5[,2][id] <- paste(x5[,2][id], text)
  
  x5[,3][id] <- gsub("^[[:alpha:]]+\\?", "", x5[,3][id])
  
  x5[,3] <- gsub("\\?", " ", x5[,3])
  
  x5[,2] <- gsub("'","",x5[,2])
  
  x5 <- cbind(x5, gsub("[^[:alpha:]]","", str_extract(x5[,2], "(-\\s|-)([[:alpha:]]+)$")))
  
  x5[,2] <- gsub("(-\\s|-)([[:alpha:]]+)$","",x5[,2])
  
  x5 <- cbind(x5, gsub("[^[:alpha:]|[:space:]]","", str_extract(x5[,2], "(-\\s|-)([[:alpha:]]|[[:space:]])+$")))
  
  x5[,2] <- gsub("(-\\s|-)([[:alpha:]]|[[:space:]])+$","",x5[,2])
  
  x5 <- apply(x5, 2, str_trim)
  
  colnames(x5) <- rep("MEC", 5)
  
  x5 <- data.frame(x5)
  
  return(x5)
}