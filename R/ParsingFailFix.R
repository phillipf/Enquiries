#' A function to address parsing failures from reading in raw enquiries data
#'
#' This function fixes any parsing fails
#' @param table input data frame
#' @keywords Parsing Fail, table
#' @export
#' @examples
#' ParsingFailFix()
#'

ParsingFailFix <- function(table) {
  if (requireNamespace("stringr", quietly = TRUE)) {
    library(stringr)
  }

  if (requireNamespace("readr", quietly = TRUE)) {
    library(readr)
  }

  if (requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr)
  }

  if (requireNamespace("plyr", quietly = TRUE)) {
    library(plyr)
  }

  if (requireNamespace("tidyr", quietly = TRUE)) {
    library(tidyr)
  }

  errors <- problems(table) %>%
            select(row) %>%
            distinct()

  table$row = seq.int(nrow(table))

  table2 <- table %>%
            inner_join(errors)

  #test if any columns contain a table
    test1fun <- function(y) {
      length(unlist(str_split(y, "\\r\\n"))) > 1
    }

    test1 <- apply(table2, 2, function(x) sapply(x, test1fun))

    test1a <- unlist(apply(test1, 2, which))
    id <- unname(unlist(apply(test1, 2, which)))
    column <- sapply(id, function(x) which(unname(apply(test1, 2, which)) == x))

    error1 <- list()
    error2 <- list()
    error3 <- list()
    error4 <- list()
    for(i in 1:length(id)) {
      error1[[i]] <- table2[[id[i], column[i]]]
      error2[[i]] <- str_split(error1[[i]], "\\r\\n")[[1]]
      error3[[i]] <- error2[[i]][2:length(error2[[i]])]
      error4[[i]] <- ldply(error3[[i]], function(x) str_split(x, "\\r\\n")[[1]]) %>%
                     separate(V1, colnames(table2), sep="\t")
    }

    error5 <- ldply(error4)

      #replace error row
    for(i in 1:length(id)) {
      table2[[id[i], column[i]]] <- error2[[i]][1]
      table2[id[i], column[i]:ncol(table2)] <- str_split(table2[[id[i], column[i]]], "\t")[[1]]
    }

    #rbind the cleaned table onto table 2
      table3 <- rbind(table2, error5) %>%
                distinct()

      return(table3)
}
