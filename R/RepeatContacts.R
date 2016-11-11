#' A function to calculate repeat contact metrics
#'
#' This function fixes calculates repeat contact metrics 
#' @param table 
#' @keywords repeat contacts, table
#' @export
#' @examples
#' RepeatContact()
#' 

RepeatContact <- function(Table) {
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
  }
  
  Table2 <- data.table(Table)   
  
  Table3 <- Table2[, MASTERID := {as.numeric(substring(MEMO_CONSUMER, 3, 9))}][,
                  ENQUIRYDATE := {MEMO_DATE}][,.(
                  MASTERID, ENQUIRYDATE, MEMO_DATE, USES, MEC.1, SRM)]

  
  CWW_DW <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};
                              server=sqldb1dev;
                              database=CWW_DW;
                              uid=report;
                              pwd=report")
  
  PROP <- sqlQuery(CWW_DW,"SELECT distinct 
                    [MASTERID]
                   ,[FINANCIAL_YEAR]
                   ,[Qtr]
                   ,[INVOICEDATE]
                   
                   FROM [CWW_DW].[dbo].[GENTRACK_WaterEfficiency_PROP_CONSUMP]
                   WHERE [FINANCIAL_YEAR] = '2015'
                   OR [FINANCIAL_YEAR] = '2016'
                   ", stringsAsFactors = FALSE) %>%
    as.data.table() 
  
  PROP2 <- PROP[,.(MASTERID, FINANCIAL_YEAR, Qtr, INVOICEDATE)][!is.na(MASTERID)]
  
  setkey(PROP2, MASTERID, INVOICEDATE)
  setkey(CallQual6, MASTERID, ENQUIRYDATE)
  
  Table4 <- PROP2[Table3, roll = "nearest"]
  
  return(Table4)
}