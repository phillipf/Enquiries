#' Create a sankey plot
#'
#' This function creates a sankey plot with an input dataframe
#' @param df
#' @keywords Sankey plot, sankey diagram
#' @export
#' @examples
#' MEMO()

sankeyPlot <- function(df, name){
  sankeyPlot <- NULL
  sankeyPlot <- rCharts$new()    #We need to tell R where the Sankey library is.  #I put it as a subdirectory to my current working directory (.)
  sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')    #We also need to point to an HTML template page
  sankeyPlot$setTemplate(script =  "http://timelyportfolio.github.io/rCharts_d3_sankey/layouts/chart.html")
  sankeyPlot$set(
    data = df,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 750,
    height = 500,
    labelFormat = ".1%"
  )
  sankeyPlot$save(paste(name, '.html', sep = ""))}
