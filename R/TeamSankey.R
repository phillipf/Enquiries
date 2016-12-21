#' Create a sankey plot of memo beginning and end by team
#'
#' This function creates a sankey plot of an enquiry type beginning and end by team
#' @param df input dataframe of the raw memo data, DEFAULT to CallQual3
#' @param ENQUIRY is the Enquiry type to plot in the sankey
#' @param filename is the name of the saved HTML of the sankey
#' @keywords Sankey plot, sankey diagram
#' @export
#' @examples
#' TeamSankey()

TeamSankey <- function(df = CallQual3, enquiry, filename){

  TeamFlow <- ddply(df %>% filter(SRM == "SRM", USES == "NONRES", ENQUIRY == enquiry), .(BEGIN, Access.x, Access.y, END), summarise, Total = length(MEMO_CONSUMER)) %>%
    # mutate(BEGIN = paste(BEGIN, "_end", sep = "")) %>%
    arrange(-Total)


  Filter <- ddply(df %>% filter(SRM == "SRM", USES == "NONRES", ENQUIRY == enquiry), .(BEGIN, END), summarise, Total = length(MEMO_CONSUMER)) %>%
    arrange(-Total) %>%
    mutate(cumulative = cumsum(Total)/sum(Total)) %>%
    filter(cumulative < 0.93)

  TeamFlow[!(TeamFlow$END %in% Filter$END), ]$END <- "OTHERDEP"
  TeamFlow[!(TeamFlow$BEGIN %in% Filter$BEGIN), ]$BEGIN <- "OTHERDEP"

  Filter <- ddply(df %>% filter(SRM == "SRM", USES == "NONRES", ENQUIRY == enquiry), .(Access.x, Access.y), summarise, Total = length(MEMO_CONSUMER)) %>%
    arrange(-Total) %>%
    mutate(cumulative = cumsum(Total)/sum(Total)) %>%
    filter(cumulative < 0.93)

  TeamFlow[!(TeamFlow$Access.y %in% Filter$Access.y), ]$Access.y <- "OTHERTEAM"
  TeamFlow[!(TeamFlow$Access.x %in% Filter$Access.x), ]$Access.x <- "OTHERTEAM"

  Begin_team <- aggregate(Total ~ BEGIN + Access.x, TeamFlow, sum) %>%
    mutate(Total = Total/sum(Total),
           BEGIN = paste(BEGIN, "_begin", sep = ""),
           Access.x = paste(Access.x, "_begin", sep = ""))

  colnames(Begin_team) <- c("source", "target", "value")

  Team_Team <- aggregate(Total ~ Access.x + Access.y, TeamFlow, sum) %>%
    mutate(Total = Total/sum(Total),
           Access.y = paste(Access.y, "_end", sep = ""),
           Access.x = paste(Access.x, "_begin", sep = ""))

  colnames(Team_Team) <- c("source", "target", "value")

  Team_END <- aggregate(Total ~ Access.y + END, TeamFlow, sum) %>%
    mutate(Total = Total/sum(Total),
           Access.y = paste(Access.y, "_end", sep = ""),
           END = paste(END, "_end", sep = ""))

  colnames(Team_END) <- c("source", "target", "value")

  Team_Department <- rbind(Begin_team, Team_Team, Team_END)

  Enquiries::sankeyPlot(Team_Department, name = filename)

  }
