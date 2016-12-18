Customer Enquiries analysis
================

#### eval=FALSE, include=FALSE, echo=FALSE

GitHub Documents
----------------

This is an R Markdown format used for publishing markdown documents to GitHub. When you click the **Knit** button all R code chunks are run and a markdown file (.md) suitable for publishing to GitHub is generated.

read in the annual call qualifier data

``` r
conn <- ABR::PostgreSQLconnect()

CallQual <- read_delim("N:/ABR/GENTRACK DATA/data AGAIN  for phil 22 nov.txt", 
                    delim = "|", quote = "") #quote = "" is important to avoid parsing failures
```

fix any parsing failures due to inconsistent delimeters

``` r
Clean_df <- function(input) {
#     probs_location <- input[problems(input)$row,]
#     # 
#     col <- unique(problems(input)$col[!is.na(problems(input)$col)])
#     # 
#     col <- vapply(col, function(x) which(x == colnames(input)), numeric(1))
#     # 
#     probs <- data.frame(problems(input)) %>% select(row, col) %>% distinct
#     # 
#     # probs$idx <- ""
#     # probs$idx[!is.na(probs$col)] <- as.numeric(vapply(probs$col[!is.na(probs$col)], function(x) col[x], numeric(1))) 
#     # 
#     # probs$idx <- as.numeric(probs$idx)
#     # 
#     # probs$df <- ""
#     # 
#     # probs$df[!is.na(probs$idx)] <- unname(vapply(which(!is.na(probs$idx)), function(i) grepl("\\r\\n", input[[probs$row[i], probs$idx[i]]]), logical(1)))
#   
#     loc <- lapply(CallQual, function(x) grepl("\\r\\n", x)) 
#     
#     test1 <- vapply(loc, function(x) any(x == TRUE), logical(1))
#     
#     probs <- loc[test1]
#     
#     rm(loc)
#     
#     x <- probs[1]
#     
#     y <- CallQual[unlist(x),names(x)]
#     
#     y$tbl <- gsub("(.*)\\r\\n(.*)*", "\\2", y$MEMO_POS3)
#     
#     y$MEMO_POS3 <- gsub("(.*)\\r\\n(.*)", "\\1", y$MEMO_POS3)
#     
#     new <- read_delim(y$tbl[1], 
#                     delim = "|") 
#     test <- y$tbl[1]
#     
#     #loc <- regexec("\\|[^\\|]*(\\r\\n)[^\\|]*\\|", test, value=TRUE)
#     
#     x <- gsub("(\\|[^\\|]*)(\\r\\n)([^\\|]*\\|)*", "\\1 \\3", test)
#     
#     new <- read_delim(x, delim = "|", col_names = FALSE) 
#     
#     gsub("(.*)(\\r\\n)(.*)", "\\3", test)
#     
#     df <- probs[probs$df == TRUE,]
#     
#     if(nrow(df) == 0) {
#       return(NA)
#     }
#     
#     #dfs <- lapply(1:nrow(df), function(i) Clean_df(read_delim(input[[df$row[i], df$idx[i]]], delim = "|", skip = 1, col_names = FALSE)))
#     
#     dfs <- lapply(1:nrow(df), function(i) c(dfs, input[[df$row[i], df$idx[i]]]))
#     
#     dfs2 <- lapply(1:nrow(df), function(i) c(dfs, Clean_df(read_delim(input[[df$row[i], df$idx[i]]], delim = "|", skip = 1, col_names = FALSE))))
#     
#     
# return(unlist(dfs2))
# 
# }
# 
# dfs <- ""
# 
# clean <- Clean_df(CallQual)
# 
# clean2 <- unique(clean[!is.na(clean) & clean != ""])
# 
# correct2 <- lapply(clean2, function(x) read_delim(x, delim = "|", skip = 1, col_names = FALSE))
# 
# #correct3 <- correct2[lapply(correct2, !is.na)]
# 
# cor_time <- function(df) {
# 
#   x <- df[lapply(df, is.POSIXct) == TRUE] 
#   df[lapply(df, is.POSIXct) == TRUE] <- lapply(x, function(x) dmy_hms(format(x, format="%y-%m-%d %H:%M:%S")))
#   
#   return(df)
 }
# 
# correct3 <- ldply(correct2, cor_time) %>% distinct
# 
# colnames(correct3) <- colnames(CallQual)
# 
# CallQual2 <- cor_time(CallQual) %>% distinct
# 
# CallQual3 <- rbind(correct3, CallQual2) %>% distinct
# 
# rm(CallQual, CallQual2, correct2, correct3, clean, clean2)

filter <- !(as.numeric(row.names(CallQual)) %in% problems(CallQual)$row)

CallQual2 <- CallQual[filter,]
```

Extract data form the MEC notes

``` r
names <- unique(randomNames(n = 2000000, which.names = "first"))

Clean <- function(CallQual) { 
    
    ##Create the Enquiry column
  
    CallQual$ENQUIRY <- ""
    
    p.1 <- "(.[^=]*)=(.[^-]*)"
    idx <- grepl(p.1, CallQual$MEMO_POS1)
    CallQual$ENQUIRY[idx] <- gsub(p.1, "\\2",  CallQual$MEMO_POS1[idx])
    
    p.2 <- "(.[^=]*):"
    idx <- grepl(p.2, CallQual$MEMO_POS1)
    #CallQual$ENQUIRY[idx] <- gsub(p.2, "\\2",  CallQual$MEMO_POS1[idx])
    CallQual$ENQUIRY[idx] <- paste(CallQual$MEMO_POS1[idx], CallQual$MEMO_POS2[idx]) 
    
    id <- logical(length(CallQual$ENQUIRY[idx]))
    
    id[grepl("bill", CallQual$ENQUIRY[idx])] <- "ACCOUNT INFORMATION"
    id[grepl("leak", CallQual$ENQUIRY[idx])] <- "METERING"
    id[grepl("update my details", CallQual$ENQUIRY[idx])] <- "ACCOUNT MAINTENANCE"
    id[grepl("payment arrangement", CallQual$ENQUIRY[idx])] <- "PAYMENTS"
    id[grepl("payment[^arrangement]", CallQual$ENQUIRY[idx])] <- "PAYMENTS"
    id[grepl("disconnection and reconnection", CallQual$ENQUIRY[idx])] <- "Water Serv Rbty"
    id[grepl("change the details", CallQual$ENQUIRY[idx])] <- "ACCOUNT MAINTENANCE"
    id[grepl("deceased", CallQual$ENQUIRY[idx])] <- "OWNER"
    id[grepl("final notice", CallQual$ENQUIRY[idx])] <- "ISSUE BILL"
    id[grepl("pension", CallQual$ENQUIRY[idx])] <- "CONCESSIONS & REBATES"
    id[grepl("no longer occupy", CallQual$ENQUIRY[idx])] <- "TENANCY"
    CallQual$ENQUIRY[idx][id == "FALSE"] <- "OTHER"
    
    CallQual$ENQUIRY[idx] <- id
    
    p.3 <- "(.*)-(.*)"
    idx <- grepl(p.3, CallQual$ENQUIRY)
    CallQual$ENQUIRY[idx] <- gsub(p.3, "\\1", CallQual$ENQUIRY[idx])
    
    enquiry <- unique(CallQual$ENQUIRY)
    
    idx <- lapply(enquiry, function(x) which(grepl(x, enquiry[-which(enquiry == x)]) == TRUE) + 1)
    
    names(idx) <- enquiry
    
    idx <- idx[lapply(idx, length) > 0]
    
    idx <- lapply(idx, function(x) which(grepl(paste(enquiry[x], collapse = "|"), CallQual$ENQUIRY)))
    
    lapply(names(idx), function(i) CallQual$ENQUIRY[idx[[i]]] <<- i)
    
    split <- strsplit(enquiry, "\\W+")
    
    # idx <- lapply(split, function(x) any(tolower(x) == tolower(names)))
    
    # wordcount <- vapply(split, length, integer(1))
    # 
    # keys <- enquiry[wordcount <= 5]
    # 
    # CallQual$ENQUIRY <- str_trim(CallQual$ENQUIRY)
    # 
    # lapply(seq_along(idx), function(i) keys[unlist(idx[i])] <- keys[i])
    # 
    # keys <- keys[!tolower(keys) %in% tolower(names)]
    # 
    # id <- !(CallQual$RESULT %in% keys)
    # 
    # CallQual$MEMO_POS2[id] <- str_trim(paste(CallQual$MEMO_POS2[id], CallQual$RESULT[id], sep = " "))
    
    # CallQual$RESULT[id] <- ""
    
    ## Create the action column
    CallQual$ACTION <- ""
    
    fix <- "(-|not|payment)$"
    idx <- grepl(fix, CallQual$MEMO_POS2)
    
    CallQual$MEMO_POS2[idx] <- paste(CallQual$MEMO_POS2[idx], CallQual$MEMO_POS3[idx])
    
    CallQual$MEMO_POS3[idx] <- ""
    
    fix <- "re-send"
    idx <- grepl(fix, CallQual$MEMO_POS2, ignore.case = TRUE)
    
    CallQual$MEMO_POS2[idx] <- gsub("re-send", "resend", CallQual$MEMO_POS2[idx], ignore.case = TRUE)
    
    p.1 <- "(.[^=]*)=(.[^-]*)-(.[^-]*)-(.[^-]*)"
    idx.1 <- grepl(p.1, CallQual$MEMO_POS2)  
    
    CallQual$ACTION[idx.1] <- gsub(p.1, "\\2 \\3", CallQual$MEMO_POS2[idx.1]) 
    
    p.2 <- "(.[^=]*)=(.[^-]*)-(.[^-]*)"
    idx.2 <- grepl(p.2, CallQual$MEMO_POS2)  
    
    CallQual$ACTION[idx.2 & !idx.1] <- gsub(p.2, "\\2", CallQual$MEMO_POS2[idx.2 & !idx.1]) 
    
    p.3 <- "(.[^=]*)=(.[^-]*)"
    idx.3 <- grepl(p.3, CallQual$MEMO_POS2)  
    
    CallQual$ACTION[idx.3 & !idx.2 & !idx.1] <- gsub(p.3, "\\2", CallQual$MEMO_POS2[idx.3 & !idx.2 & !idx.1]) 
    
    #Clean the action column
    
    CallQual$ACTION <- str_trim(CallQual$ACTION)
    
    action <- unique(CallQual$ACTION)
    
    split <- strsplit(action, "\\W+")
    
    wordcount <- vapply(split, length, integer(1))
    
    action <- action[wordcount >= 3]
    
    idx <- lapply(action, function(x) which(grepl(x, action) == TRUE))
    
    names(idx) <- action
    
    idx <- idx[lapply(idx, length) > 1]
    
    idx <- lapply(idx, function(x) action[x])
    
    lapply(names(idx), function(i) CallQual$ACTION[CallQual$ACTION %in% idx[[i]]] <<- i)
    
    idx <- lapply(idx, function(x) which(grepl(paste(action[x], collapse = "|"), CallQual$ACTION)))
    
    lapply(names(idx), function(i) CallQual$ACTION[idx[[i]]] <<- i)
    
    action <- unique(CallQual$ACTION)
    
    split <- strsplit(tolower(action), "\\W+")
    
    wordcount <- vapply(split, length, integer(1))
    
    keys <- action[wordcount >= 7]
    
    filter <- split[wordcount >= 7]
    
    idx <- keys[unlist(lapply(filter, function(x) any((x %in% tolower(names) == TRUE))))]
    
    idx2 <- lapply(action, function(x) which(grepl(x, idx[x != idx]) == TRUE))
    
    names(idx2) <- action
    
    idx2 <- idx2[lapply(idx2, length) > 0 & names(idx2) != ""]
    
    idx3 <- ldply(idx2, function(x) idx[x]) %>%
            group_by(V1) %>%
            filter(.id == .id[which.max(nchar(.id))]) %>%
            ungroup()
    
    lapply(1:nrow(idx3), function(i) CallQual$ACTION[CallQual$ACTION == idx3$V1[i]] <<- idx3$.id[i])
    
    unknown <- idx[!(idx %in% idx3$V1)]
    
    lapply(seq_along(unknown), function(i) CallQual$ACTION[CallQual$ACTION == unknown[i]] <<- "")
    
    # action <- unique(CallQual$ACTION)
    # 
    # split <- strsplit(tolower(action), "\\W+")
    # 
    # wordcount <- vapply(split, length, integer(1))
    # 
    # keys <- action[wordcount >= 8]
    # 
    # id <- !(CallQual$RESULT %in% keys)
    # 
    # CallQual$MEMO_POS2[id] <- str_trim(paste(CallQual$MEMO_POS2[id], CallQual$RESULT[id], sep = " "))
    # 
    # CallQual$RESULT[id] <- ""
    
    ##Create the result column
    CallQual$RESULT <- ""
    
    p.1 <- "(.[^=]*)=(.[^-]*)-(.[^-]*)-(.[^-]*)"
    idx.1 <- grepl(p.1, CallQual$MEMO_POS2)  
    
    CallQual$RESULT[idx.1] <- gsub(p.1, "\\4", CallQual$MEMO_POS2[idx.1]) 
    
    p.2 <- "(.[^=]*)=(.[^-]*)-(.[^-]*)"
    idx.2 <- grepl(p.2, CallQual$MEMO_POS2)  
    
    CallQual$RESULT[idx.2 & !idx.1] <- gsub(p.2, "\\3", CallQual$MEMO_POS2[idx.2 & !idx.1]) 
    
    p.3 <- "(.[^=]*)=(.[^-]*)"
    idx.3 <- grepl(p.3, CallQual$MEMO_POS2)  
    
    CallQual$RESULT[idx.3 & !idx.2 & !idx.1] <- ""
    
    CallQual$RESULT <- str_trim(CallQual$RESULT)
    
    #Clean the result column
    result <- unique(CallQual$RESULT)
    finalised <- paste(result[grepl("^f[^o].*", result, ignore.case = TRUE)], collapse = "|")
    id <- grepl(finalised, CallQual$RESULT, ignore.case = TRUE) 
    CallQual$RESULT[id] <- gsub(finalised, "Finalised", CallQual$RESULT[id], ignore.case = TRUE)
    
    result <- unique(CallQual$RESULT)
    referred <- paste(result[grepl("^ref.*[^L]$", result, ignore.case = TRUE)], collapse = "|")
    id <- grepl(referred, CallQual$RESULT, ignore.case = TRUE) 
    CallQual$RESULT[id] <- gsub(referred, "Referred", CallQual$RESULT[id], ignore.case = TRUE)
    
    result <- unique(CallQual$RESULT)
    crel <- paste(result[grepl("^ref.*[L]$", result, ignore.case = TRUE)], collapse = "|")
    id <- grepl(crel, CallQual$RESULT, ignore.case = TRUE) 
    CallQual$RESULT[id] <- gsub(crel, "ReferredCREL", CallQual$RESULT[id], ignore.case = TRUE)
    
    result <- unique(CallQual$RESULT)
    
    split <- strsplit(result, "\\W+")
    
    wordcount <- vapply(split, length, integer(1))
    
    keys <- result[wordcount <= 2]
    
    keys <- keys[!tolower(keys) %in% tolower(names)]
    
    id <- !(CallQual$RESULT %in% keys)
    
    CallQual$MEMO_POS2[id] <- str_trim(paste(CallQual$MEMO_POS2[id], CallQual$RESULT[id], sep = " "))
    
    CallQual$RESULT[id] <- ""
    
    CallQual[,17:24][is.na(CallQual[,17:24])] <- ""
    
    idx <- grepl("[0-9]+/[0-9]+/[0-9]+", CallQual$USES)
    
    idx.2 <- !is.na(CallQual$MEMO_POS10)
    
    CallQual[idx & idx.2,]$MEMO_POS9 <- paste(CallQual[idx & idx.2,]$MEMO_POS9, CallQual[idx & idx.2,]$MEMO_POS10)
    
    CallQual[idx,25:33] <- CallQual[idx,26:34]
    
    idx <- !grepl("^RES|^NONRES", CallQual$USES)
    
    idx.2 <- !is.na(CallQual$MEMO_POS10)
    
    CallQual[idx & idx.2,]$MEMO_POS9 <- paste(CallQual[idx & idx.2,]$MEMO_POS9, CallQual[idx & idx.2,]$MEMO_POS10)
    
    CallQual[idx,25:33] <- CallQual[idx,26:34]
    
    CallQual <- CallQual %>%
                 mutate(MEMO = paste(MEMO_POS3, 
                                    MEMO_POS4,
                                    MEMO_POS5,
                                    MEMO_POS6,
                                    MEMO_POS7,
                                    MEMO_POS8,
                                    MEMO_POS9,
                                    MEMO_POS10,
                                    sep = " "))
    
    
    
    
return(CallQual)
}

CallQual3 <- Clean(CallQual2) #to do: need to fix ENQUIRY = FALSE#

rm(CallQual2)

# dbWriteTable(con=conn,
#              name="CallQual3",
#              value=CallQual3,
#              row.names=FALSE,
#              append=FALSE)
# 
# CallQual3 <- dbReadTable(con=conn,
#                          name="CallQual3")

# CallQual5 <- CallQual4[!grepl("^RES|NONRES", CallQual4$USES),] 
# 
# row.names(CallQual5) <- CallQual5$MEMO_ID
# 
# x <- apply(CallQual5, 1, function(x) which(grepl("^RES|^NONRES", x) == TRUE))
# 
# loc <- which(colnames(CallQual5) == "MEMO_POS10")
# 
# x[lapply(x,length) == 0] <- 26
# 
# gap <- lapply(x, function(x) seq(from = loc, to=x-1))
# 
# MEMO_POS10 <- lapply(1:nrow(CallQual5), function(i) CallQual5[gap[[i]], ]) 
# 
# x <- unlist(unname(x))
# 
# CallQual[idx,25:33]
# 
# y <- lapply(1:nrow(CallQual5), function(i) CallQual5[[i,x[i]]]) 

# dbWriteTable(con=conn,
#              name="CallQual5",
#              value=CallQual5,
#              row.names=FALSE,
#              append=TRUE)

cols <- vapply(CallQual3, is.character, logical(1))

CallQual3[ , cols] <- apply(CallQual3[ , cols], 2, str_trim)
CallQual3[ , cols] <- apply(CallQual3[ , cols], 2, toupper)

# dbWriteTable(con=conn,
#              name="Callqual_281116",
#              value=CallQual4,
#              row.names=FALSE,
#              append=TRUE)


#CallQual2$ENQUIRY = sapply(CallQual2$MEMO_POS1, Enquiry)
# result = lapply(CallQual2$MEMO_POS2, Result)
# names(result) <- CallQual2$MEMO_CONSUMER
# result_tbl <- ldply(result)
```

Create a call ID

``` r
#CallQual2 <- CallQual3

#CallQual3 <- data.table(CallQual2)

#CallID1 <- CallQual3[, MEMO_DATETIME := ymd_hms(MEMO_DATETIME)][, time_window := MEMO_DATETIME + minutes(5)][,.(MEMO_CONSUMER, MEMO_ID, #MEMO_DATETIME, time_window)]

#setkey(CallID1, MEMO_DATETIME, time_window)

#CallID2 <- CallQual3[, MEMO_DATETIME := ymd_hms(MEMO_DATETIME)][, dummy := MEMO_DATETIME][,.(MEMO_CONSUMER, MEMO_ID, MEMO_DATETIME, dummy)]

#CallID3 <- sqldf("SELECT DISTINCT 
                  #a.MEMO_CONSUMER,
                  #a.MEMO_ID,
                  #a.MEMO_ID AS id,
                  #b.MEMO_ID AS parent
                  #FROM CallQual3 a
                  #LEFT JOIN CallQual3 b ON datetime(b.MEMO_DATETIME) >= (datetime(a.MEMO_DATETIME) - 'interval 5 mins') 
                  #AND datetime(b.MEMO_DATETIME) <= datetime(a.MEMO_DATETIME) 
                  #AND b.MEMO_CONSUMER = a.MEMO_CONSUMER 
                  #AND b.MEMO_ID <> a.MEMO_ID",
                  #drv = 'SQLite')

#Memo_call <- foverlaps(CallID2, 
                       #CallID1, 
                       #by.x=c("MEMO_DATETIME", "dummy"), 
                       #type = "within", 
                       #mult = "all")

# Memo_calls <- dbReadTable(con=conn,
#                          name="memo_calls")
# 
# x <- Memo_calls[Memo_calls$MEMO_CONSUMER == 22513504514,][,3:4]
# x <- Memo_calls[Memo_calls$MEMO_CONSUMER == 22513554612,][,3:4]
# 
# path <- function(x) {
#   #data.frame(id = x[,1], parent = x[,dim(x)[2]])
#   df <- x
#   while(any(!is.na(df[,dim(df)[2]]))) {
#     
#     df <- left_join(x, df, by = c("id" = "parent"))
#     #df <- left_join(x, x[,c(dim(x)[2] -1,dim(x)[2])], by = c("id" = "parent"))
#     
#     #df2 <- df[,c(dim(df)[2], 1)] 
#     
#     #colnames(df2)[1] <- "id"
#     
#     #x <- union_all(df, df2)
#   
#   }
#   
#   df2 <- gather(df, join, child, 4:ncol(df)) %>% filter(is.na(parent))
#   df3 <- df2 %>% filter(!is.na(child)) %>% select(child,id)
#   colnames(df3) <- c("id", "parent")
#   
#   result <- rbind(df2 %>% select(id, parent), df3) %>% distinct()
#  
#   return(result)
# }
# 
# library(nlme)
# x <- Memo_calls[,c(1,3:4)]
# 
# y <- x %>% sample_n(100) %>% mutate(ID = MEMO_CONSUMER) %>% select(ID, id, parent) %>% group_by(ID)
# 
# z <- gapply(y, path)
# 
# result <- path(x)
# 
# 
# gps <- data.frame(lapply(parents, is.na))
# 
# parents <- parents[unlist(lapply(1:nrow(gps), function(i) all(gps[i,3:ncol(gps)] == TRUE))) == FALSE,]
# 
# gps <- data.frame(lapply(parents, is.na))
# 
# parents[!gps] <- NULL
# 
# 
# apply(gps,1,function(x) all())
# 
# path <- function(x) {
#   
#   #if(all(is.na(x[,dim(x)[2]]))) {
#   
#     #x[,dim(x)[2] + 1] <- left_join(x, x, by = c("id" = "parent"))
#     
#   #}
#   
#   if(any(!is.na(x[,dim(x)[2]]))) {  
#   
#     x[,dim(x)[2] + 1] <- left_join(x, path(x), by = c("id" = "parent"))$id.y
#     #df <- %>% distinct
#     }
# 
# }
# 
# #Memo_call<- Memo_call[MEMO_ID == i.MEMO_ID, i.MEMO_ID := 0][MEMO_ID != i.MEMO_ID & MEMO_CONSUMER == i.MEMO_CONSUMER][,.(MEMO_CONSUMER, #MEMO_ID,i.MEMO_ID)][, parent := MEMO_ID]
# 
# #names(Memo_call) <- c("MEMO_CONSUMER","MEMO_ID","parent","id" )
# 
# #Memo_call2 <- Memo_call[order(-rank(MEMO_CONSUMER), MEMO_ID)]
# 
# #Memo_call3 <- Memo_call2[]
```

``` r
PropertyLayer <- read.csv("file:///N:/KarlBlackhall/Gentrack AL/PROPERTY_LAYER_1516FY (Q1-Q4).csv",
                          stringsAsFactors = FALSE) %>%
                 mutate(ACC_NAME = gsub("[[:space:]]|[^[:alnum:]]", "", ACCOUNT_NAME))

KeyAccounts <- read.csv("file:///C:/Enquiries/data/KeyAccounts.csv",
                        stringsAsFactors = FALSE) %>%
               mutate(ACCOUNT_NAME = str_trim(gsub("(.+)(\\(.+)", "\\1", Key.Customer))) %>%
               select(ACCOUNT_NAME)


KeyAccountsName <- unique(gsub("[[:space:]]|[^[:alnum:]]", "", KeyAccounts$ACCOUNT_NAME))

link <- lapply(KeyAccountsName, function(x) PropertyLayer$MASTERID[which(grepl(x, PropertyLayer$ACC_NAME, ignore.case = TRUE) == TRUE)])

KeyAccountsID <- data.frame(MASTERID = unlist(link)) %>%
                 left_join(select(PropertyLayer, MASTERID, ACCOUNT_NAME)) %>%
                 distinct()

write.csv(KeyAccountsID, "C:/Enquiries/data/KeyAccountsID.csv")

KeyAccountsID <- read.csv("C:/Enquiries/data/KeyAccountsID.csv")
```

``` r
CallQual3$SRM <- ifelse(!is.na(CallQual3$SR_SERV_CODE), "SRM", "RESOLVED")   

x <- length(unique(CallQual3$MEMO_CONSUMER))

Filter <- ddply(CallQual3, .(ENQUIRY), summarise, Total = length(MEMO_CONSUMER)/x) %>%
                      arrange(-Total) %>%
                      mutate(cumulative = cumsum(Total)/sum(Total)) %>%
                      filter(cumulative < 0.93)

EnquiryCountAnnual <- CallQual3

EnquiryCountAnnual[!(EnquiryCountAnnual$ENQUIRY %in% Filter$ENQUIRY),]$ENQUIRY <- "OTHER"

EnquiryCountAnnual <- ddply(EnquiryCountAnnual, .(ENQUIRY, USES, SRM), summarise, Total = length(MEMO_CONSUMER)) %>%
                      arrange(-Total) %>%
                      select(USES,
                             SRM,
                             ENQUIRY,
                             Total)

p1Annual <- ggplot(data = EnquiryCountAnnual,
             aes(x = factor(USES),
                 fill= factor(ENQUIRY),
                 y = Total)) 

p1Annual <- p1Annual + geom_bar(stat="identity")

p1Annual <- p1Annual + facet_grid(facets=~SRM)

#Filter for SRM == TRUE

Filter <- ddply(CallQual3 %>% filter(SRM == "SRM"), .(ENQUIRY), summarise, Total = length(MEMO_CONSUMER)/x) %>%
                      arrange(-Total) %>%
                      mutate(cumulative = cumsum(Total)/sum(Total)) %>%
                      filter(cumulative < 0.93)

SRMCountAnnual <- CallQual3

SRMCountAnnual[!(SRMCountAnnual$ENQUIRY %in% Filter$ENQUIRY),]$ENQUIRY <- "OTHER"

p2Annual <- ggplot(data = ddply(SRMCountAnnual %>% filter(SRM == "SRM"), .(USES, ENQUIRY), summarise, Total = length(MEMO_CONSUMER))  %>%
                      arrange(-Total),
             aes(x = factor(USES),
                 fill= factor(ENQUIRY),
                 y = Total)) 

p2Annual <- p2Annual + geom_bar(stat="identity")

#p2Annual <- p2Annual + facet_grid(facets=~SRM)
p2Annual
```

![](EnquiriesAnalysis_files/figure-markdown_github/Annual%20plots-1.png)

``` r
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


x <- length(unique(CallQual3$MEMO_CONSUMER))

EnquiryCountAnnual_sankey <- EnquiryCountAnnual

EndUse_enquiry <- aggregate(Total ~ USES + ENQUIRY, EnquiryCountAnnual, sum) %>%
                  mutate(Total = Total/x)

colnames(EndUse_enquiry) <- c("source", "target", "value")

Enquiry_SRM <- aggregate(Total ~ ENQUIRY + SRM, EnquiryCountAnnual, sum) %>%
                  mutate(Total = Total/x)

colnames(Enquiry_SRM) <- c("source", "target", "value")

Enduse_SRM = rbind(EndUse_enquiry, Enquiry_SRM)

#d <- data.frame(
  #id = grant_id, 
  #source = funding_agency, 
  #target = study_section, 
  #value = total_cost
#)
#devtools::install_github("rCharts", "ramnathv", ref = "dev")

sankeyPlot(Enduse_SRM, name = "Enduse_SRM")

htmltools::includeHTML("C:/Enquiries/data/Enduse_SRM.html")
```

<!--html_preserve-->
<!doctype HTML>
<meta charset = 'utf-8'>
<html>
<head>
    <link rel='stylesheet' href='http://timelyportfolio.github.io/rCharts_d3_sankey/css/sankey.css'>

    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/d3.v3.js' type='text/javascript'></script>
    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/sankey.js' type='text/javascript'></script>

    <style>
    .rChart {
      display: block;
      margin-left: auto; 
      margin-right: auto;
      width: 750px;
      height: 500px;
    }  
    </style>

</head>
<body>
    <div id = 'chart15607fedee2' class = 'rChart rCharts_d3_sankey'></div>    
    ﻿<!--Attribution:

Mike Bostock <https://github.com/d3/d3-plugins/tree/master/sankey> Mike Bostock <http://bost.ocks.org/mike/sankey/> --&gt;

<script>
(function(){
var params = {
 "dom": "chart15607fedee2",
"width":    750,
"height":    500,
"data": {
 "source": [ "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "NONRES", "RES", "ACCOUNT INFORMATION", "ACCOUNT MAINTENANCE", "CUSTOMER CORRESPONDENCE", "FINAL READ / SPECIAL READ", "METERING", "OTHER", "OWNER", "PAYMENTS", "TENANCY", "TRANSFER CALLS", "ACCOUNT INFORMATION", "ACCOUNT MAINTENANCE", "CUSTOMER CORRESPONDENCE", "FINAL READ / SPECIAL READ", "METERING", "OTHER", "OWNER", "PAYMENTS", "TENANCY", "TRANSFER CALLS" ],
"target": [ "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "ACCOUNT MAINTENANCE", "ACCOUNT MAINTENANCE", "CUSTOMER CORRESPONDENCE", "CUSTOMER CORRESPONDENCE", "FINAL READ / SPECIAL READ", "FINAL READ / SPECIAL READ", "METERING", "METERING", "OTHER", "OTHER", "OWNER", "OWNER", "PAYMENTS", "PAYMENTS", "TENANCY", "TENANCY", "TRANSFER CALLS", "TRANSFER CALLS", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "RESOLVED", "SRM", "SRM", "SRM", "SRM", "SRM", "SRM", "SRM", "SRM", "SRM", "SRM" ],
"value": [ 0.033067, 0.22761, 0.02729, 0.38548, 0.027834, 0.13952, 0.00020834, 0.046927, 0.0095792, 0.044212, 0.030137, 0.1511, 0.00017325, 0.098362, 0.058813, 0.66334, 0.010452, 0.10159, 0.035054, 0.053359, 0.24776, 0.4038, 0.15692, 0.035062, 0.039635, 0.13467, 0.098228, 0.71409, 0.11196, 0.086715, 0.012915, 0.0089717, 0.010432, 0.012073, 0.014156, 0.046571, 0.00030703, 0.0080638, 8.1142e-05, 0.0016974 ] 
},
"nodeWidth":     15,
"nodePadding":     10,
"layout":     32,
"labelFormat": ".1%",
"id": "chart15607fedee2" 
};

params.units ? units = " " + params.units : units = "";

//hard code these now but eventually make available
var formatNumber = d3.format("0,.0f"),    // zero decimal places
    format = function(d) { return formatNumber(d) + units; },
    color = d3.scale.category20();

if(params.labelFormat){
  formatNumber = d3.format(".2%");
}

var svg = d3.select('#' + params.id).append("svg")
    .attr("width", params.width)
    .attr("height", params.height);
    
var sankey = d3.sankey()
    .nodeWidth(params.nodeWidth)
    .nodePadding(params.nodePadding)
    .layout(params.layout)
    .size([params.width,params.height]);
    
var path = sankey.link();
    
var data = params.data,
    links = [],
    nodes = [];
    
//get all source and target into nodes
//will reduce to unique in the next step
//also get links in object form
data.source.forEach(function (d, i) {
    nodes.push({ "name": data.source[i] });
    nodes.push({ "name": data.target[i] });
    links.push({ "source": data.source[i], "target": data.target[i], "value": +data.value[i] });
}); 

//now get nodes based on links data
//thanks Mike Bostock https://groups.google.com/d/msg/d3-js/pl297cFtIQk/Eso4q_eBu1IJ
//this handy little function returns only the distinct / unique nodes
nodes = d3.keys(d3.nest()
                .key(function (d) { return d.name; })
                .map(nodes));

//it appears d3 with force layout wants a numeric source and target
//so loop through each link replacing the text with its index from node
links.forEach(function (d, i) {
    links[i].source = nodes.indexOf(links[i].source);
    links[i].target = nodes.indexOf(links[i].target);
});

//now loop through each nodes to make nodes an array of objects rather than an array of strings
nodes.forEach(function (d, i) {
    nodes[i] = { "name": d };
});

sankey
  .nodes(nodes)
  .links(links)
  .layout(params.layout);
  
var link = svg.append("g").selectAll(".link")
  .data(links)
.enter().append("path")
  .attr("class", "link")
  .attr("d", path)
  .style("stroke-width", function (d) { return Math.max(1, d.dy); })
  .sort(function (a, b) { return b.dy - a.dy; });

link.append("title")
  .text(function (d) { return d.source.name + " → " + d.target.name + "\n" + format(d.value); });

var node = svg.append("g").selectAll(".node")
  .data(nodes)
.enter().append("g")
  .attr("class", "node")
  .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; })
.call(d3.behavior.drag()
  .origin(function (d) { return d; })
  .on("dragstart", function () { this.parentNode.appendChild(this); })
  .on("drag", dragmove));

node.append("rect")
  .attr("height", function (d) { return d.dy; })
  .attr("width", sankey.nodeWidth())
  .style("fill", function (d) { return d.color = color(d.name.replace(/ .*/, "")); })
  .style("stroke", function (d) { return d3.rgb(d.color).darker(2); })
.append("title")
  .text(function (d) { return d.name + "\n" + format(d.value); });

node.append("text")
  .attr("x", -6)
  .attr("y", function (d) { return d.dy / 2; })
  .attr("dy", ".35em")
  .attr("text-anchor", "end")
  .attr("transform", null)
  .text(function (d) { return d.name; })
.filter(function (d) { return d.x < params.width / 2; })
  .attr("x", 6 + sankey.nodeWidth())
  .attr("text-anchor", "start");

// the function for moving the nodes
  function dragmove(d) {
    d3.select(this).attr("transform", 
        "translate(" + (
                   d.x = Math.max(0, Math.min(params.width - d.dx, d3.event.x))
                ) + "," + (
                   d.y = Math.max(0, Math.min(params.height - d.dy, d3.event.y))
                ) + ")");
        sankey.relayout();
        link.attr("d", path);
  }
})();
</script>
    <script></script>    

</body>
</html>
<!--/html_preserve-->
``` r
NONRES_SRM <- CallQual3 %>% filter(SRM == "SRM", USES == "NONRES")

NONRES_SRM$KEY_ACCOUNT <- "KEY_ACCOUNT"

NONRES_SRM[!(as.numeric(substr(NONRES_SRM$MEMO_CONSUMER, 3, 9)) %in% KeyAccountsID),]$KEY_ACCOUNT <- "NON_KEY"

NONRES_SRM$ACC_TYPE <- "TENANT"

NONRES_SRM[substr(NONRES_SRM$MEMO_CONSUMER, 1, 2) == 12,]$ACC_TYPE <- "OWNER"

NONRES_SRM[substr(NONRES_SRM$MEMO_CONSUMER, 1, 2) == 32,]$ACC_TYPE <- "TRADE_WASTE"

NONRES_SRM[substr(NONRES_SRM$MEMO_CONSUMER, 1, 2) == 99,]$ACC_TYPE <- "99"

NONRES_SRM[substr(NONRES_SRM$MEMO_CONSUMER, 1, 2) == 99,]$ACC_TYPE <- "99"

Users <- read.csv("file:///N:/ABR/Output_2_RootCauseSRMandRepeats/SRSSreports/GentrackUsers.csv",
                  stringsAsFactors = FALSE) %>%
         select(USER_ID, FULL_NAME, ï..DEPARTMENT, Access)

NONRES_SRM <- NONRES_SRM %>%
               left_join(Users, by = c("MEMO_CREATOR" = "USER_ID")) %>%
               left_join(Users, by = c("SR_CLOSER" = "USER_ID"))

Filter <- ddply(NONRES_SRM, .(ENQUIRY), summarise, Total = length(MEMO_CONSUMER)) %>%
                      arrange(-Total) %>%
                      mutate(cumulative = cumsum(Total)/sum(Total)) %>%
                      filter(cumulative < 0.93)

Filter2 <- ddply(NONRES_SRM, .(SOTDESCRIPTION), summarise, Total = length(MEMO_CONSUMER)) %>%
                      arrange(-Total) %>%
                      mutate(cumulative = cumsum(Total)/sum(Total)) %>%
                      filter(cumulative < 0.9)

SRMCountAnnual <- NONRES_SRM

SRMCountAnnual[!(SRMCountAnnual$ENQUIRY %in% Filter$ENQUIRY),]$ENQUIRY <- "OTHER_ENQUIRY"

SRMCountAnnual[!(SRMCountAnnual$SOTDESCRIPTION %in% Filter2$SOTDESCRIPTION),]$SOTDESCRIPTION <- "OTHER_SRM"

SRM2 <- ddply(SRMCountAnnual, .(USES, ACC_TYPE, `ï..DEPARTMENT.x`, `ï..DEPARTMENT.y`, SR_CALL_METHOD, ENQUIRY, SOTDESCRIPTION, SR_CLOSER), summarise, Total = length(MEMO_CONSUMER)) %>% filter(complete.cases(.))

# x <- length(unique(filter(SRMCountAnnual, SRM == "SRM")$MEMO_CONSUMER))

USES_TYPE <- aggregate(Total ~ USES + ACC_TYPE, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(USES_TYPE) <- c("source", "target", "value")

USES_METHOD <- aggregate(Total ~ ACC_TYPE + SR_CALL_METHOD, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(USES_METHOD) <- c("source", "target", "value")

Enquiry_SRM <- aggregate(Total ~ SR_CALL_METHOD + ENQUIRY, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(Enquiry_SRM) <- c("source", "target", "value")

sankeyPlot(Enquiry_SRM, "Enquiry_SRM")

Enquiry_SOT <- aggregate(Total ~ ENQUIRY + SOTDESCRIPTION, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(Enquiry_SOT) <- c("source", "target", "value")

sankeyPlot(Enquiry_SOT, "Enquiry_SOT")

SOT_CLOSER <- aggregate(Total ~ SOTDESCRIPTION + SR_CLOSER, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(SOT_CLOSER) <- c("source", "target", "value")

SOT = rbind(Enquiry_SRM, USES_TYPE, USES_METHOD, Enquiry_SOT) 

sankeyPlot(SOT, "Enquiry_SOT")

htmltools::includeHTML("C:/Enquiries/data/Enquiry_SOT.html")
```

<!--html_preserve-->
<!doctype HTML>
<meta charset = 'utf-8'>
<html>
<head>
    <link rel='stylesheet' href='http://timelyportfolio.github.io/rCharts_d3_sankey/css/sankey.css'>

    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/d3.v3.js' type='text/javascript'></script>
    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/sankey.js' type='text/javascript'></script>

    <style>
    .rChart {
      display: block;
      margin-left: auto; 
      margin-right: auto;
      width: 750px;
      height: 500px;
    }  
    </style>

</head>
<body>
    <div id = 'chart15603ecb6b62' class = 'rChart rCharts_d3_sankey'></div>    
    ﻿<!--Attribution:

Mike Bostock <https://github.com/d3/d3-plugins/tree/master/sankey> Mike Bostock <http://bost.ocks.org/mike/sankey/> --&gt;

<script>
(function(){
var params = {
 "dom": "chart15603ecb6b62",
"width":    750,
"height":    500,
"data": {
 "source": [ "E", "F", "I", "M", "P", "V", "E", "F", "I", "M", "P", "E", "I", "P", "E", "F", "I", "M", "P", "V", "E", "F", "I", "M", "P", "V", "E", "F", "I", "M", "P", "V", "NONRES", "NONRES", "NONRES", "NONRES", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TRADE_WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "OTHER_ENQUIRY", "TRADE WASTE", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "METERING", "OTHER_ENQUIRY", "TRADE WASTE", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "TRADE WASTE", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "OTHER_ENQUIRY", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "INTERNAL SRM", "OTHER_ENQUIRY", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "METERING", "OTHER_ENQUIRY", "TRADE WASTE" ],
"target": [ "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "ACCOUNT INFORMATION", "CUSTOMER CORRESPONDENCE", "CUSTOMER CORRESPONDENCE", "CUSTOMER CORRESPONDENCE", "CUSTOMER CORRESPONDENCE", "CUSTOMER CORRESPONDENCE", "INTERNAL SRM", "INTERNAL SRM", "INTERNAL SRM", "METERING", "METERING", "METERING", "METERING", "METERING", "METERING", "OTHER_ENQUIRY", "OTHER_ENQUIRY", "OTHER_ENQUIRY", "OTHER_ENQUIRY", "OTHER_ENQUIRY", "OTHER_ENQUIRY", "TRADE WASTE", "TRADE WASTE", "TRADE WASTE", "TRADE WASTE", "TRADE WASTE", "TRADE WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "E", "E", "E", "E", "F", "F", "I", "I", "I", "I", "M", "M", "M", "P", "P", "P", "P", "V", "V", "V", "CAMS SITE VISIT-COMP OFFICER", "CAMS SITE VISIT-COMP OFFICER", "CAMS SITE VISIT-COMP OFFICER", "CAMS SITE VISIT-COMP OFFICER", "CAMS SITE VISIT-COMP OFFICER", "CLOSE TRADE WASTE", "CLOSE TRADE WASTE", "CLOSE TRADE WASTE", "CLOSE TRADE WASTE", "CLOSE TRADE WASTE", "CONSUMPTION CHARGES", "CONSUMPTION CHARGES", "CONSUMPTION CHARGES", "CONSUMPTION CHARGES", "CONSUMPTION CHARGES", "DMS AUDIT", "DMS AUDIT", "DMS AUDIT", "DMS AUDIT", "INCORRECT READING", "INCORRECT READING", "INCORRECT READING", "INCORRECT READING", "INCORRECT READING", "METER READ", "METER READ", "METER READ", "METER READ", "OTHER METERING ISSUES", "OTHER METERING ISSUES", "OTHER METERING ISSUES", "OTHER METERING ISSUES", "OTHER METERING ISSUES", "OTHER_SRM", "OTHER_SRM", "OTHER_SRM", "OTHER_SRM", "OTHER_SRM", "OTHER_SRM", "OWNER DETAILS", "OWNER DETAILS", "OWNER DETAILS", "OWNER DETAILS", "OWNER DETAILS", "POLICY / PROCEDURES", "POLICY / PROCEDURES", "POLICY / PROCEDURES", "RECHECK READ-INVESTIGATION", "RECHECK READ-INVESTIGATION", "RECHECK READ-INVESTIGATION", "REPLACE METER", "REPLACE METER", "REPLACE METER", "REPLACE METER", "STOLEN METER", "STOLEN METER", "TRADE WASTE CHARGES", "TRADE WASTE CHARGES", "TRADE WASTE CHARGES", "TRADE WASTE CHARGES", "TRADE WASTE CHARGES", "TRADE WASTE CHARGES" ],
"value": [ 0.034392, 0.00093712, 0.0031862, 0.00065598, 0.057727, 0.00018742, 0.05604, 0.0021554, 0.0007497, 0.00065598, 0.015837, 0.00056227, 0.40268, 0.05201, 0.021929, 0.00065598, 0.0081529, 0.0007497, 0.099803, 0.00018742, 0.044138, 0.00056227, 0.011433, 0.0017805, 0.075626, 0.0014057, 0.0032799, 9.3712e-05, 0.0026239, 9.3712e-05, 0.08837, 0.011339, 0.013682, 0.39556, 0.46781, 0.12295, 0.0086215, 0.1371, 0.0087152, 0.0059039, 0.004217, 0.00018742, 0.00037485, 0.050604, 0.37522, 0.0026239, 0.0037485, 9.3712e-05, 9.3712e-05, 0.0046856, 0.19951, 0.082935, 0.10224, 0.00037485, 0.00084341, 0.011901, 9.3712e-05, 9.3712e-05, 0.01818, 0.00018742, 9.3712e-05, 0.0022491, 0.0027176, 0.00018742, 0.012839, 0.091931, 0.053603, 0.017524, 9.3712e-05, 0.0024365, 0.0026239, 0.00018742, 0.075719, 0.00037485, 0.00037485, 0.0030925, 0.016118, 0.0087152, 0.0015931, 9.3712e-05, 0.004217, 0.007497, 0.070284, 0.004217, 0.0022491, 0.0037485, 0.00028114, 0.039359, 0.0037485, 0.027176, 0.011901, 0.019867, 0.0088089, 0.034673, 0.00018742, 0.0018742, 0.014057, 9.3712e-05, 0.00037485, 0.059039, 0.0011245, 0.00046856, 0.012464, 0.30025, 0.00028114, 0.0016868, 9.3712e-05, 0.015462, 0.00056227, 0.00028114, 0.024927, 9.3712e-05, 0.001312, 0.0011245, 0.00018742, 9.3712e-05, 0.0012183, 0.013588 ] 
},
"nodeWidth":     15,
"nodePadding":     10,
"layout":     32,
"labelFormat": ".1%",
"id": "chart15603ecb6b62" 
};

params.units ? units = " " + params.units : units = "";

//hard code these now but eventually make available
var formatNumber = d3.format("0,.0f"),    // zero decimal places
    format = function(d) { return formatNumber(d) + units; },
    color = d3.scale.category20();

if(params.labelFormat){
  formatNumber = d3.format(".2%");
}

var svg = d3.select('#' + params.id).append("svg")
    .attr("width", params.width)
    .attr("height", params.height);
    
var sankey = d3.sankey()
    .nodeWidth(params.nodeWidth)
    .nodePadding(params.nodePadding)
    .layout(params.layout)
    .size([params.width,params.height]);
    
var path = sankey.link();
    
var data = params.data,
    links = [],
    nodes = [];
    
//get all source and target into nodes
//will reduce to unique in the next step
//also get links in object form
data.source.forEach(function (d, i) {
    nodes.push({ "name": data.source[i] });
    nodes.push({ "name": data.target[i] });
    links.push({ "source": data.source[i], "target": data.target[i], "value": +data.value[i] });
}); 

//now get nodes based on links data
//thanks Mike Bostock https://groups.google.com/d/msg/d3-js/pl297cFtIQk/Eso4q_eBu1IJ
//this handy little function returns only the distinct / unique nodes
nodes = d3.keys(d3.nest()
                .key(function (d) { return d.name; })
                .map(nodes));

//it appears d3 with force layout wants a numeric source and target
//so loop through each link replacing the text with its index from node
links.forEach(function (d, i) {
    links[i].source = nodes.indexOf(links[i].source);
    links[i].target = nodes.indexOf(links[i].target);
});

//now loop through each nodes to make nodes an array of objects rather than an array of strings
nodes.forEach(function (d, i) {
    nodes[i] = { "name": d };
});

sankey
  .nodes(nodes)
  .links(links)
  .layout(params.layout);
  
var link = svg.append("g").selectAll(".link")
  .data(links)
.enter().append("path")
  .attr("class", "link")
  .attr("d", path)
  .style("stroke-width", function (d) { return Math.max(1, d.dy); })
  .sort(function (a, b) { return b.dy - a.dy; });

link.append("title")
  .text(function (d) { return d.source.name + " → " + d.target.name + "\n" + format(d.value); });

var node = svg.append("g").selectAll(".node")
  .data(nodes)
.enter().append("g")
  .attr("class", "node")
  .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; })
.call(d3.behavior.drag()
  .origin(function (d) { return d; })
  .on("dragstart", function () { this.parentNode.appendChild(this); })
  .on("drag", dragmove));

node.append("rect")
  .attr("height", function (d) { return d.dy; })
  .attr("width", sankey.nodeWidth())
  .style("fill", function (d) { return d.color = color(d.name.replace(/ .*/, "")); })
  .style("stroke", function (d) { return d3.rgb(d.color).darker(2); })
.append("title")
  .text(function (d) { return d.name + "\n" + format(d.value); });

node.append("text")
  .attr("x", -6)
  .attr("y", function (d) { return d.dy / 2; })
  .attr("dy", ".35em")
  .attr("text-anchor", "end")
  .attr("transform", null)
  .text(function (d) { return d.name; })
.filter(function (d) { return d.x < params.width / 2; })
  .attr("x", 6 + sankey.nodeWidth())
  .attr("text-anchor", "start");

// the function for moving the nodes
  function dragmove(d) {
    d3.select(this).attr("transform", 
        "translate(" + (
                   d.x = Math.max(0, Math.min(params.width - d.dx, d3.event.x))
                ) + "," + (
                   d.y = Math.max(0, Math.min(params.height - d.dy, d3.event.y))
                ) + ")");
        sankey.relayout();
        link.attr("d", path);
  }
})();
</script>
    <script></script>    

</body>
</html>
<!--/html_preserve-->
``` r
Enquiry_Creator <- aggregate(Total ~ SR_CALL_METHOD + `ï..DEPARTMENT.x`, SRM2, sum) %>%
                          mutate(Total = Total/sum(Total))

colnames(Enquiry_Creator) <- c("source", "target", "value")

Creator_Closer <- aggregate(Total ~ `ï..DEPARTMENT.x` + `ï..DEPARTMENT.y`, SRM2, sum) %>%
                           mutate(Total = Total/sum(Total),
                                  `ï..DEPARTMENT.y` = paste(`ï..DEPARTMENT.y`, "_end"))
 
colnames(Creator_Closer) <- c("source", "target", "value")

# Creator_Enquiry <- aggregate(Total ~ `ï..DEPARTMENT.x` + ENQUIRY, SRM2, sum) %>%
#                           mutate(Total = Total/sum(Total))
# 
# colnames(Creator_Enquiry) <- c("source", "target", "value")

# Enquiry_Closer <-  aggregate(Total ~ ENQUIRY + `ï..DEPARTMENT.y`, SRM2, sum) %>%
#                           mutate(Total = Total/sum(Total),
#                                  `ï..DEPARTMENT.y` = paste(`ï..DEPARTMENT.y`, "_end"))
# 
# colnames(Enquiry_Closer) <- c("source", "target", "value")

#Users = rbind(USES_TYPE, USES_METHOD, Enquiry_Creator, Creator_Enquiry,  Enquiry_Closer)

Users = rbind(USES_TYPE, USES_METHOD, Enquiry_Creator, Creator_Closer)

sankeyPlot(Users, "Enquiry_users")

htmltools::includeHTML("C:/Enquiries/data/Enquiry_users.html")
```

<!--html_preserve-->
<!doctype HTML>
<meta charset = 'utf-8'>
<html>
<head>
    <link rel='stylesheet' href='http://timelyportfolio.github.io/rCharts_d3_sankey/css/sankey.css'>

    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/d3.v3.js' type='text/javascript'></script>
    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/sankey.js' type='text/javascript'></script>

    <style>
    .rChart {
      display: block;
      margin-left: auto; 
      margin-right: auto;
      width: 750px;
      height: 500px;
    }  
    </style>

</head>
<body>
    <div id = 'chart1560492c5058' class = 'rChart rCharts_d3_sankey'></div>    
    ﻿<!--Attribution:

Mike Bostock <https://github.com/d3/d3-plugins/tree/master/sankey> Mike Bostock <http://bost.ocks.org/mike/sankey/> --&gt;

<script>
(function(){
var params = {
 "dom": "chart1560492c5058",
"width":    750,
"height":    500,
"data": {
 "source": [ "NONRES", "NONRES", "NONRES", "NONRES", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TRADE_WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "E", "I", "P", "E", "I", "P", "E", "I", "P", "E", "F", "I", "M", "P", "V", "E", "I", "M", "P", "V", "E", "I", "P", "E", "I", "M", "P", "V", "CABS", "CDM", "CON", "CREDIT", "SBR", "CABS", "CDM", "CON", "CREDIT", "SBR", "SQE", "CABS", "CALL", "CDM", "CON", "CREDIT", "SBR", "SQE", "CREDIT", "CABS", "CDM", "CON", "SBR", "CABS", "CDM", "CON", "SBR", "SQE", "CABS", "CALL", "CDM", "CON", "CREDIT", "SBR", "SQE" ],
"target": [ "99", "OWNER", "TENANT", "TRADE_WASTE", "E", "E", "E", "E", "F", "F", "I", "I", "I", "I", "M", "M", "M", "P", "P", "P", "P", "V", "V", "V", "CABS", "CABS", "CABS", "CALL", "CALL", "CALL", "CDM", "CDM", "CDM", "CON", "CON", "CON", "CON", "CON", "CON", "CREDIT", "CREDIT", "CREDIT", "CREDIT", "CREDIT", "SBR", "SBR", "SBR", "SQE", "SQE", "SQE", "SQE", "SQE", "CABS _end", "CABS _end", "CABS _end", "CABS _end", "CABS _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CREDIT _end", "MR _end", "MR _end", "MR _end", "MR _end", "SBR _end", "SBR _end", "SBR _end", "SBR _end", "SBR _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end" ],
"value": [ 0.013682, 0.39556, 0.46781, 0.12295, 0.0086215, 0.1371, 0.0087152, 0.0059039, 0.004217, 0.00018742, 0.00037485, 0.050604, 0.37522, 0.0026239, 0.0037485, 9.3712e-05, 9.3712e-05, 0.0046856, 0.19951, 0.082935, 0.10224, 0.00037485, 0.00084341, 0.011901, 0.0011245, 0.022772, 0.051448, 0.00046856, 9.3712e-05, 0.0069347, 9.3712e-05, 0.28779, 0.0029051, 0.13813, 0.0044045, 0.057445, 0.0029988, 0.28442, 0.00037485, 0.0084341, 0.010027, 9.3712e-05, 0.039265, 0.011433, 0.0040296, 0.050417, 0.0039359, 0.0080592, 0.00028114, 0.00084341, 0.00046856, 0.001312, 0.01312, 0.0033736, 0.0024365, 9.3712e-05, 0.0017805, 0.04039, 0.26511, 0.018836, 0.0010308, 0.0059039, 9.3712e-05, 0.010683, 0.0052479, 0.0095586, 0.36566, 0.045263, 0.0046856, 0.0037485, 0.00018742, 0.0077781, 0.001968, 0.0018742, 0.00018742, 0.00018742, 0.0096523, 0.0025302, 0.045544, 0.00018742, 0.0031862, 0.0022491, 0.0011245, 0.09643, 0.022678, 0.00028114, 0.0069347 ] 
},
"nodeWidth":     15,
"nodePadding":     10,
"layout":     32,
"labelFormat": ".1%",
"id": "chart1560492c5058" 
};

params.units ? units = " " + params.units : units = "";

//hard code these now but eventually make available
var formatNumber = d3.format("0,.0f"),    // zero decimal places
    format = function(d) { return formatNumber(d) + units; },
    color = d3.scale.category20();

if(params.labelFormat){
  formatNumber = d3.format(".2%");
}

var svg = d3.select('#' + params.id).append("svg")
    .attr("width", params.width)
    .attr("height", params.height);
    
var sankey = d3.sankey()
    .nodeWidth(params.nodeWidth)
    .nodePadding(params.nodePadding)
    .layout(params.layout)
    .size([params.width,params.height]);
    
var path = sankey.link();
    
var data = params.data,
    links = [],
    nodes = [];
    
//get all source and target into nodes
//will reduce to unique in the next step
//also get links in object form
data.source.forEach(function (d, i) {
    nodes.push({ "name": data.source[i] });
    nodes.push({ "name": data.target[i] });
    links.push({ "source": data.source[i], "target": data.target[i], "value": +data.value[i] });
}); 

//now get nodes based on links data
//thanks Mike Bostock https://groups.google.com/d/msg/d3-js/pl297cFtIQk/Eso4q_eBu1IJ
//this handy little function returns only the distinct / unique nodes
nodes = d3.keys(d3.nest()
                .key(function (d) { return d.name; })
                .map(nodes));

//it appears d3 with force layout wants a numeric source and target
//so loop through each link replacing the text with its index from node
links.forEach(function (d, i) {
    links[i].source = nodes.indexOf(links[i].source);
    links[i].target = nodes.indexOf(links[i].target);
});

//now loop through each nodes to make nodes an array of objects rather than an array of strings
nodes.forEach(function (d, i) {
    nodes[i] = { "name": d };
});

sankey
  .nodes(nodes)
  .links(links)
  .layout(params.layout);
  
var link = svg.append("g").selectAll(".link")
  .data(links)
.enter().append("path")
  .attr("class", "link")
  .attr("d", path)
  .style("stroke-width", function (d) { return Math.max(1, d.dy); })
  .sort(function (a, b) { return b.dy - a.dy; });

link.append("title")
  .text(function (d) { return d.source.name + " → " + d.target.name + "\n" + format(d.value); });

var node = svg.append("g").selectAll(".node")
  .data(nodes)
.enter().append("g")
  .attr("class", "node")
  .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; })
.call(d3.behavior.drag()
  .origin(function (d) { return d; })
  .on("dragstart", function () { this.parentNode.appendChild(this); })
  .on("drag", dragmove));

node.append("rect")
  .attr("height", function (d) { return d.dy; })
  .attr("width", sankey.nodeWidth())
  .style("fill", function (d) { return d.color = color(d.name.replace(/ .*/, "")); })
  .style("stroke", function (d) { return d3.rgb(d.color).darker(2); })
.append("title")
  .text(function (d) { return d.name + "\n" + format(d.value); });

node.append("text")
  .attr("x", -6)
  .attr("y", function (d) { return d.dy / 2; })
  .attr("dy", ".35em")
  .attr("text-anchor", "end")
  .attr("transform", null)
  .text(function (d) { return d.name; })
.filter(function (d) { return d.x < params.width / 2; })
  .attr("x", 6 + sankey.nodeWidth())
  .attr("text-anchor", "start");

// the function for moving the nodes
  function dragmove(d) {
    d3.select(this).attr("transform", 
        "translate(" + (
                   d.x = Math.max(0, Math.min(params.width - d.dx, d3.event.x))
                ) + "," + (
                   d.y = Math.max(0, Math.min(params.height - d.dy, d3.event.y))
                ) + ")");
        sankey.relayout();
        link.attr("d", path);
  }
})();
</script>
    <script></script>    

</body>
</html>
<!--/html_preserve-->
``` r
Users2 = rbind(USES_TYPE, USES_METHOD, Enquiry_Creator, Creator_Closer) %>%
         filter(source != "I", target != "I")

sankeyPlot(Users2, "Enquiry_users2")

htmltools::includeHTML("C:/Enquiries/data/Enquiry_users2.html")
```

<!--html_preserve-->
<!doctype HTML>
<meta charset = 'utf-8'>
<html>
<head>
    <link rel='stylesheet' href='http://timelyportfolio.github.io/rCharts_d3_sankey/css/sankey.css'>

    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/d3.v3.js' type='text/javascript'></script>
    <script src='http://timelyportfolio.github.io/rCharts_d3_sankey/js/sankey.js' type='text/javascript'></script>

    <style>
    .rChart {
      display: block;
      margin-left: auto; 
      margin-right: auto;
      width: 750px;
      height: 500px;
    }  
    </style>

</head>
<body>
    <div id = 'chart156022a67b8a' class = 'rChart rCharts_d3_sankey'></div>    
    ﻿<!--Attribution:

Mike Bostock <https://github.com/d3/d3-plugins/tree/master/sankey> Mike Bostock <http://bost.ocks.org/mike/sankey/> --&gt;

<script>
(function(){
var params = {
 "dom": "chart156022a67b8a",
"width":    750,
"height":    500,
"data": {
 "source": [ "NONRES", "NONRES", "NONRES", "NONRES", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "99", "OWNER", "TENANT", "TRADE_WASTE", "OWNER", "TENANT", "TRADE_WASTE", "E", "P", "E", "P", "E", "P", "E", "F", "M", "P", "V", "E", "M", "P", "V", "E", "P", "E", "M", "P", "V", "CABS", "CDM", "CON", "CREDIT", "SBR", "CABS", "CDM", "CON", "CREDIT", "SBR", "SQE", "CABS", "CALL", "CDM", "CON", "CREDIT", "SBR", "SQE", "CREDIT", "CABS", "CDM", "CON", "SBR", "CABS", "CDM", "CON", "SBR", "SQE", "CABS", "CALL", "CDM", "CON", "CREDIT", "SBR", "SQE" ],
"target": [ "99", "OWNER", "TENANT", "TRADE_WASTE", "E", "E", "E", "E", "F", "F", "M", "M", "M", "P", "P", "P", "P", "V", "V", "V", "CABS", "CABS", "CALL", "CALL", "CDM", "CDM", "CON", "CON", "CON", "CON", "CON", "CREDIT", "CREDIT", "CREDIT", "CREDIT", "SBR", "SBR", "SQE", "SQE", "SQE", "SQE", "CABS _end", "CABS _end", "CABS _end", "CABS _end", "CABS _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CDM _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CON _end", "CREDIT _end", "MR _end", "MR _end", "MR _end", "MR _end", "SBR _end", "SBR _end", "SBR _end", "SBR _end", "SBR _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end", "SQE _end" ],
"value": [ 0.013682, 0.39556, 0.46781, 0.12295, 0.0086215, 0.1371, 0.0087152, 0.0059039, 0.004217, 0.00018742, 0.0037485, 9.3712e-05, 9.3712e-05, 0.0046856, 0.19951, 0.082935, 0.10224, 0.00037485, 0.00084341, 0.011901, 0.0011245, 0.051448, 0.00046856, 0.0069347, 9.3712e-05, 0.0029051, 0.13813, 0.0044045, 0.0029988, 0.28442, 0.00037485, 0.0084341, 9.3712e-05, 0.039265, 0.011433, 0.0040296, 0.0039359, 0.0080592, 0.00084341, 0.00046856, 0.001312, 0.01312, 0.0033736, 0.0024365, 9.3712e-05, 0.0017805, 0.04039, 0.26511, 0.018836, 0.0010308, 0.0059039, 9.3712e-05, 0.010683, 0.0052479, 0.0095586, 0.36566, 0.045263, 0.0046856, 0.0037485, 0.00018742, 0.0077781, 0.001968, 0.0018742, 0.00018742, 0.00018742, 0.0096523, 0.0025302, 0.045544, 0.00018742, 0.0031862, 0.0022491, 0.0011245, 0.09643, 0.022678, 0.00028114, 0.0069347 ] 
},
"nodeWidth":     15,
"nodePadding":     10,
"layout":     32,
"labelFormat": ".1%",
"id": "chart156022a67b8a" 
};

params.units ? units = " " + params.units : units = "";

//hard code these now but eventually make available
var formatNumber = d3.format("0,.0f"),    // zero decimal places
    format = function(d) { return formatNumber(d) + units; },
    color = d3.scale.category20();

if(params.labelFormat){
  formatNumber = d3.format(".2%");
}

var svg = d3.select('#' + params.id).append("svg")
    .attr("width", params.width)
    .attr("height", params.height);
    
var sankey = d3.sankey()
    .nodeWidth(params.nodeWidth)
    .nodePadding(params.nodePadding)
    .layout(params.layout)
    .size([params.width,params.height]);
    
var path = sankey.link();
    
var data = params.data,
    links = [],
    nodes = [];
    
//get all source and target into nodes
//will reduce to unique in the next step
//also get links in object form
data.source.forEach(function (d, i) {
    nodes.push({ "name": data.source[i] });
    nodes.push({ "name": data.target[i] });
    links.push({ "source": data.source[i], "target": data.target[i], "value": +data.value[i] });
}); 

//now get nodes based on links data
//thanks Mike Bostock https://groups.google.com/d/msg/d3-js/pl297cFtIQk/Eso4q_eBu1IJ
//this handy little function returns only the distinct / unique nodes
nodes = d3.keys(d3.nest()
                .key(function (d) { return d.name; })
                .map(nodes));

//it appears d3 with force layout wants a numeric source and target
//so loop through each link replacing the text with its index from node
links.forEach(function (d, i) {
    links[i].source = nodes.indexOf(links[i].source);
    links[i].target = nodes.indexOf(links[i].target);
});

//now loop through each nodes to make nodes an array of objects rather than an array of strings
nodes.forEach(function (d, i) {
    nodes[i] = { "name": d };
});

sankey
  .nodes(nodes)
  .links(links)
  .layout(params.layout);
  
var link = svg.append("g").selectAll(".link")
  .data(links)
.enter().append("path")
  .attr("class", "link")
  .attr("d", path)
  .style("stroke-width", function (d) { return Math.max(1, d.dy); })
  .sort(function (a, b) { return b.dy - a.dy; });

link.append("title")
  .text(function (d) { return d.source.name + " → " + d.target.name + "\n" + format(d.value); });

var node = svg.append("g").selectAll(".node")
  .data(nodes)
.enter().append("g")
  .attr("class", "node")
  .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; })
.call(d3.behavior.drag()
  .origin(function (d) { return d; })
  .on("dragstart", function () { this.parentNode.appendChild(this); })
  .on("drag", dragmove));

node.append("rect")
  .attr("height", function (d) { return d.dy; })
  .attr("width", sankey.nodeWidth())
  .style("fill", function (d) { return d.color = color(d.name.replace(/ .*/, "")); })
  .style("stroke", function (d) { return d3.rgb(d.color).darker(2); })
.append("title")
  .text(function (d) { return d.name + "\n" + format(d.value); });

node.append("text")
  .attr("x", -6)
  .attr("y", function (d) { return d.dy / 2; })
  .attr("dy", ".35em")
  .attr("text-anchor", "end")
  .attr("transform", null)
  .text(function (d) { return d.name; })
.filter(function (d) { return d.x < params.width / 2; })
  .attr("x", 6 + sankey.nodeWidth())
  .attr("text-anchor", "start");

// the function for moving the nodes
  function dragmove(d) {
    d3.select(this).attr("transform", 
        "translate(" + (
                   d.x = Math.max(0, Math.min(params.width - d.dx, d3.event.x))
                ) + "," + (
                   d.y = Math.max(0, Math.min(params.height - d.dy, d3.event.y))
                ) + ")");
        sankey.relayout();
        link.attr("d", path);
  }
})();
</script>
    <script></script>    

</body>
</html>
<!--/html_preserve-->
