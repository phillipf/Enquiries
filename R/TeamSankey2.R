#' Create a sankey plot of memo beginning and end by team
#'
#' This function creates a sankey plot of an enquiry type beginning and end by team
#' @param df input dataframe of the raw memo data, DEFAULT to CallQual3
#' @param ENQUIRY is the Enquiry type to plot in the sankey
#' @param filename is the name of the saved HTML of the sankey
#' @keywords Sankey plot, sankey diagram
#' @export
#' @examples
#' TeamSankey2()

TeamSankey2 <- function(df = CallQual3, enquiry, filename){

  TeamFlow <- ddply(df %>%
                      filter(SRM == "SRM", USES == "NONRES", ENQUIRY == enquiry) %>%
                      mutate(BEGIN = paste0(BEGIN, "_", Access.x),
                             END = paste0(END, "_", Access.x)),
                    .(BEGIN, END), summarise, Total = length(MEMO_CONSUMER)) %>%
    # mutate(BEGIN = paste(BEGIN, "_end", sep = "")) %>%
    arrange(-Total)


  Filter <- TeamFlow %>%
    arrange(-Total) %>%
    mutate(cumulative = cumsum(Total)/sum(Total)) %>%
    filter(cumulative < 0.93)

  TeamFlow[!(TeamFlow$END %in% Filter$END), ]$END <- "OTHERDEP"
  TeamFlow[!(TeamFlow$BEGIN %in% Filter$BEGIN), ]$BEGIN <- "OTHERDEP"

  Begin_team <- aggregate(Total ~ BEGIN + END, TeamFlow, sum) %>%
    mutate(Total = Total/sum(Total),
           BEGIN = paste(BEGIN, "_begin", sep = ""),
           END = paste(END, "_end", sep = "")) %>%
    arrange(-Total)

  colnames(Begin_team) <- c("source", "target", "value")

  Enquiries::sankeyPlot(Begin_team, name = filename)

  Begin_team <- Begin_team %>%
    mutate(Descr = ifelse(gsub("(.*)_(.*)_(.*)", "\\1\\2", source) == gsub("(.*)_(.*)_(.*)", "\\1\\2", target),
                          paste("Enquiry recieved and SRM closed by a",
                                gsub("(.*)_(.*)_(.*)", "\\2", source),
                                "in the",
                                gsub("(.*)_(.*)_(.*)", "\\1", source),
                                "group",
                                paste0("(", 100 * round(value, 2), "%", ")")),
                          paste("Enquiry recieved by a", gsub("(.*)_(.*)_(.*)", "\\2", source),
                                "in the", gsub("(.*)_(.*)_(.*)", "\\1", source),
                                "group",
                                "and SRM closed by a",
                                gsub("(.*)_(.*)_(.*)", "\\2", target),
                                "in the", gsub("(.*)_(.*)_(.*)", "\\1", target),
                                "group",
                                paste0("(", 100 * round(value, 2), "%", ")"))))

  return(Begin_team)
  }
