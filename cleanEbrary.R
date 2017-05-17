setwd("C:/Users/iakovakis/Documents/Projects/ALA2017")

library(dplyr)
library(stringr)

ebrary_usage <- read.csv("./data/raw/ebrary/ebrary_usage_CSV.csv"
                         , sep = ","
                         , stringsAsFactors = F
                         , colClasses = c("ISBN.Print" = "character"
                                          , "ISBN.Electronic" = "character") # For matching purposes, it is easier to specify the ISBN as character
                         , na.strings = ""
                         , header = TRUE)
# 21612

ebrary_holdings <- read.csv("./data/raw/ebrary/ebrary_holdings_CSV.csv"
                            , sep = ","
                            , stringsAsFactors = F
                            , colClasses = c("ISBN.print" = "character"
                                             , "ISBN.electronic" = "character") # For matching purposes, it is easier to specify the ISBN as character
                            , na.strings = ""
                            , header = TRUE)
# 191499
ebrary_holdings <- rename(ebrary_holdings, Subscription = Subscrption)
ebrary_holdings$Perpetual[is.na(ebrary_holdings$Perpetual)] <- 1  # replace the NA values with 1
ebrary_holdings$Perpetual[which(ebrary_holdings$ebrary.DocID == 10742399)] <- 1  # this should be a 1

#table(ebrary_usage$ebrary.Doc.ID %in% ebrary_holdings$ebrary.DocID) # there are 2,764 ebrary usage titles not found in the ebrary holdings report
#ebrary_noHoldings <- ebrary_usage[ebrary_usage$ebrary.Doc.ID %in% ebrary_holdings$ebrary.DocID == FALSE, ]  # deleting these because they're no longer in our holdings
#write.csv(ebrary_noHoldings, file="./data/processed/ebrary/ebrary_noHoldings.csv", row.names = FALSE)

ebrary_usage <- ebrary_usage[-c(which(ebrary_usage$ebrary.Doc.ID %in% ebrary_holdings$ebrary.DocID == FALSE)), ]  # leaves 18848



# coding a new status variable
z <- ebrary_holdings
z$status <- vector(mode="character", length = nrow(z))


z$status[which(z$Subscription == 1 & z$DDA.Purchase == 1)] <- "Subscription.DDA"
z$status[which(z$Subscription == 1 & z$Perpetual == 1)] <- "Subscription.Perpetual"
z$status[which(z$Subscription == 0 & z$Perpetual == 0 & z$DDA.Purchase == 0 & z$DDA.Pool == 1)] <- "DDA.Pool"
z$status[which(z$Subscription == 0 & z$DDA.Purchase == 1)] <- "DDA.Purchase"
z$status[which(z$Subscription == 0 & z$Perpetual == 1 & z$DDA.Purchase == 0 & z$DDA.Pool == 0)] <- "Perpetual"
z$status[which(z$Subscription == 1 & z$Perpetual == 0 & z$DDA.Purchase == 0 & z$DDA.Pool == 0)] <- "Subscription"


z$Perpetual[which(z$status == "")] <- 1  # this was a mistake on ebrary's side, it should have been a zero
z$DDA.Pool[which(z$status == "")] <- 0  # same here, making these changes that came to light

z$status[which(z$Subscription == 1 & z$DDA.Purchase == 1)] <- "Subscription.DDA"
z$status[which(z$Subscription == 1 & z$Perpetual == 1)] <- "Subscription.Perpetual"
z$status[which(z$Subscription == 0 & z$Perpetual == 0 & z$DDA.Purchase == 0 & z$DDA.Pool == 1)] <- "DDA.Pool"
z$status[which(z$Subscription == 0 & z$DDA.Purchase == 1)] <- "DDA.Purchase"
z$status[which(z$Subscription == 0 & z$Perpetual == 1 & z$DDA.Purchase == 0 & z$DDA.Pool == 0)] <- "Perpetual"
z$status[which(z$Subscription == 1 & z$Perpetual == 0 & z$DDA.Purchase == 0 & z$DDA.Pool == 0)] <- "Subscription"

#write.csv(z, "./data/processed/ebrary/ebrarystatus.csv"
#          , row.names = F)


ebrary <- full_join(ebrary_usage
                    , z
                    , by = c("ebrary.Doc.ID" = "ebrary.DocID"))
# 191499

repl.func <- function(df, var){
  varx <- paste0(var, ".x")
  vary <- paste0(var, ".y")
  
  df$vary <- df[, vary]
  df$varx <- df[, varx]
  
  
  df[[var]] <- ifelse(is.na(df$varx)  # if the one title is NA
                   , as.character(df$vary)  # replace it with the other
                   , as.character(df$varx))
  #df$var <- df[, var]
  
  df$vary <- NULL
  df$varx <- NULL
  
  return(df)
}

ebrary <- repl.func(ebrary, "Title")
ebrary <- repl.func(ebrary, "Author")
ebrary <- repl.func(ebrary, "Publisher")
ebrary <- repl.func(ebrary, "LC.Call")
ebrary <- repl.func(ebrary, "Author")

ebrary$PrintISBN <- ifelse(is.na(ebrary$ISBN.print)  # if the one title is NA
                       , as.character(ebrary$ISBN.Print)  # replace it with the other
                       , as.character(ebrary$ISBN.print))  # otherwise keep it
ebrary$eISBN <- ifelse(is.na(ebrary$ISBN.electronic)  # if the one Author is NA
                       , as.character(ebrary$ISBN.Electronic)  # replace it with the other
                       , as.character(ebrary$ISBN.electronic))  # otherwise keep it

ebrary <- select(ebrary
                 , -Title.x
                 , -Title.y
                 , -Author.x
                 , -Author.y
                 , -Publisher.x
                 , -Publisher.y
                 , -LC.Call.x
                 , -LC.Call.y
                 , -ISBN.Print
                 , -ISBN.print
                 , -ISBN.Electronic
                 , -ISBN.electronic)


