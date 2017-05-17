setwd("C:/Users/iakovakis/Documents/Projects/ALA2017")

library(dplyr)
library(stringr)


ebsco_holdings <- read.csv("./data/raw/ebsco/ebsco_holdings.csv"
                           , sep = ","
                           , stringsAsFactors = F
                           , colClasses = c("ISBN" = "character"
                                            , "eISBN" = "character"
                                            , "Book.ID" = "character") # For matching purposes, it is easier to specify the ISBN as character
                           , na.strings = ""
                           , header = TRUE)
#60321


ebsco_usage <- read.csv("./data/raw/ebsco/ebsco_usage.csv"
                        , sep = ","
                        , stringsAsFactors = F
                        , colClasses = c("issn" = "character") # For matching purposes, it is easier to specify the ISBN as character
                        , na.strings = ""
                        , header = TRUE)
# 9243

table(ebsco_usage$issn %in% ebsco_holdings$ISBN) # 397 false
# table(ebsco_holdings$Title %in% ebsco_usage$Title) # 683
table(ebsco_usage$issn %in% ebsco_holdings$eISBN) # 9166 false

# beautiful easy way to subset data, much easier than []
z <- filter(ebsco_usage, !(ebsco_usage$issn %in% ebsco_holdings$ISBN))  # there are 397 usage ISBNs that are not in the holdings ISBNS
zz <- filter(z, z$issn %in% ebsco_holdings$eISBN)  # out of that 397, 76 are eISBNs.

# I need to create a new ISBN_clean field that keeps the print ISBNs, except for those variables that have a matching eISBN, replace
# the print ISBN with the eISBN

ebsco_holdings$ISBN_clean <- ebsco_holdings$ISBN
ebsco_holdings$ISBN_clean[which(ebsco_holdings$eISBN %in% zz$issn)] <- ebsco_holdings$eISBN[which(ebsco_holdings$eISBN %in% zz$issn)]
  
table(ebsco_usage$issn %in% ebsco_holdings$ISBN_clean)

ebsco <- full_join(ebsco_holdings
               , ebsco_usage
               , by = c("ISBN_clean" = "issn"))
               
# ebsco <- ebsco[!duplicated(ebsco$ISBN), ] #59615 but not running this because there are duplicate isbns with different usage statistics

ebsco$Title <- ifelse(is.na(ebsco$Title)  # if the Title is NA
                      , as.character(ebsco$title)  # replace it with the other one
                      , as.character(ebsco$Title))  # otherwise keep it

ebsco_noHoldings <- ebsco %>%
  filter(is.na(ebsco$Book.ID)) %>%
  select(17:26)
#write.csv(ebsco_noHoldings
#          , "./data/processed/ebsco_noHoldings.csv"
#          , row.names = F)


ebsco <- select(ebsco, -title)
#write.csv(ebsco
#          , file = "./data/results/ebsco.csv"
#          , row.names = FALSE)

