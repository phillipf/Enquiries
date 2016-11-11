#' A function to address parsing failures from reading in raw enquiries data
#'
#' This function fixes any parsing fails
#' @param table
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



    fixdelim <- function(id, column) {
      error1 <- table2[[id, column]]
      error2 <- str_split(error1, "\\r\\n")[[1]]
      error3 <- error2[2:length(error2)]

      error4 <- ldply(error3, function(x) str_split(x, "\\r\\n")[[1]]) %>%
                separate(V1, colnames(table2), sep="\t")

      #replace error row
      table2[[id, column]] <- error2[1]
      table2[id, column:ncol(table2)] <- str_split(table2[[id, column]], "\t")[[1]]

    #rbind the cleaned table onto table 2
      table3 <- rbind(table2, error4) %>%
                distinct()
    }

    table3 <- fixdelim(id[1], column[1])

  #test for text ending with ":"

    test2fun <- function(y) {
      grepl(":$", y)
    }

    test2afun <- function(y) {
      grepl("RES|NONRES", y)
    }

    test2 <- apply(table3, 2, function(x) sapply(x, test2fun))

    test <- unlist(apply(test2, 2, which))
    id <- unname(unlist(apply(test2, 2, which)))
    column <- which(unlist(lapply(unname(apply(test2, 2, which)), function(x) length(x) > 0)))
    columnnames <- names(apply(test2, 2, which))

    test2a <- apply(table3, 2, function(x) sapply(x, test2afun))

    test <- unlist(apply(test2a, 2, which))
    id <- unname(unlist(apply(test2a, 2, which)))
    column <- which(unlist(lapply(unname(apply(test2a, 2, which)), function(x) length(x) > 0)))
    columnnames <- names(apply(test2a, 2, which))

    table4 <- table3[id, ] %>%
              unite(MEMO, min(column):max(column), sep="|") %>%
              mutate(MEMO = gsub(":\\||\\|\\?", " ", MEMO)) %>%
              separate(MEMO, columnnames[column], sep = "\\|") %>%
              mutate(MEMO = ifelse((!is.na(USES) & USES != "RES" & USES != "NONRES"),
                                   paste(MEMO, USES, sep = " "),
                                   MEMO),
                     USES = ifelse((!is.na(USES) & USES != "RES" & USES != "NONRES"),
                                   NA,
                                   USES))


    test2afunc <- function(x) {
      i <- max(column)
      while(i >= min(column)) {
        a = i - 1
        y = x[columnnames[c(a,i)]]
      }
    }

    return(table4)
}
