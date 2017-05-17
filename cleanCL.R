# Clean clstk

setwd("C:/Users/iakovakis/Documents/Projects/ALA2017")

source("./code/CollectionAnalysis/ISBNclean.R")
library(dplyr)

cl1 <- read.csv("./data/raw/cl/cl1.txt"  # includes title and ISBN
                   , sep = "^"
                   , stringsAsFactors = F
                   , colClasses = c("X020.a" = "character") # For matching purposes, it is easier to specify the ISBN as character
                   , na.strings = ""
                   , header = TRUE)
cl2 <- read.csv("./data/raw/cl/cl2.txt"  # includes title and ISBN
                , sep = "^"
                , stringsAsFactors = F
                , colClasses = c("X020.a" = "character") # For matching purposes, it is easier to specify the ISBN as character
                , na.strings = ""
                , header = TRUE)

cl <- rbind(cl1, cl2)
cl <- cl[!duplicated(cl$RECORD...ITEM.), ]

rm(cl1, cl2)

cl$TOT.CHKOUT <- as.integer(cl$TOT.CHKOUT)

#x <- cl[complete.cases(cl$TOT.CHKOUT), ]
#nrow(x[x$TOT.CHKOUT > 0, ])/nrow(x) 40% of the collection has circulated, in alignment with Kent Study

cl <- ISBN_clean(cl)  # down to 213,718 (a loss of 146090, which is exactly the number of missing values)

### Converting 10 digit ISBNs to 13 digit ISBNs ###

# First create a brand new csv of 10 digit ISBNs
#write.csv(cl$ISBN_clean[which(nchar(cl$ISBN_clean) == 10)]
#          , file = "./data/processed/isbn10.csv"
#          , quote = FALSE
#          , row.names = F)

# use http://isbnconverter.sampo.co.uk/ to convert them to 13 digits.
# Open the file in Notepad, delete the header and copy all. Paste it into isbn converter.

# copy-paste it into Excel
# then clean it up in Excel:
# delete the extra stuff at the top
# copy-paste the ISBN-10 and the ISBN-13 columns into a new spreadsheet
# replace all Invalid ISBN with nothing 
# THIS CRASHES THE COMPUTER: GoTo Special select blanks, Right click, Delete, Shift cells up
# Leave the blanks
# Ctrl+F replace hyphens with nothing
# Change cell format to number with no decimal places
# Rename  the first column named isbn10 and consisting of the 10 digit ISBNs,
# and the second column named isbn13 and consisting of the converted 13 digit ISBNs
# Save it as a CSV called isbn13

# read in the file
isn13 <- read.csv(file = file.path("./data/processed/isbn13.csv")
                  , sep = ","
                  , stringsAsFactors = F
                  , colClasses = "character" # For matching purposes, it is easier to specify the ISBN as character
                  , na.strings = c("", " ")  # this is to deal with the blanks left by deleting the Invalid ISBN data
                  , header = TRUE)



table(is.na(isn13$isbn13))  # 772 of these did not convert, 166713 did

isn13.1 <- ifelse(is.na(isn13$isbn13)  # if the 13 digit is NA
                  , as.character(isn13$isbn10)  # replace it with the 10
                  , as.character(isn13$isbn13))  # otherwise keep it
cl$ISBN_clean[which(nchar(cl$ISBN_clean) == 10)] <- isn13.1  # do the replacement

rm(clstkRaw, isn13, isn13.1)

table(nchar(cl$ISBN_clean))
# This gives us 212,260 titles with 13 digit ISBNs

# View(clstk$ISBN_clean[which(nchar(clstk$ISBN_clean) == 9)])
cl <- select(cl, -ISN)
cl <- rename(cl
             , ITEM.RECORD = RECORD...ITEM.
             , TITLE = X245.ab
             , AUTHOR = X245.c
             , PUBYEAR = X008.Date.One
             , OCLC = OCLC..
             , ISBN_ORIGINAL = X020.a)

# trying another ISBN10 converter, this one actually seems to be better than sampo http://custompcsoftware.com/isbn13.html
#write.csv(cl$ISBN_clean[which(nchar(cl$ISBN_clean) == 10)]
#          , file = "./data/processed/isbn10v2.csv"
#          , quote = FALSE
#          , row.names = F)

isn13v2 <- read.csv(file = file.path("./data/processed/isbn13v2.csv")
                  , sep = ","
                  , stringsAsFactors = F
                  , colClasses = "character" # For matching purposes, it is easier to specify the ISBN as character
                  , na.strings = c("", " ")  # this is to deal with the blanks left by deleting the Invalid ISBN data
                  , header = TRUE)

table(is.na(isn13v2$isbn13))  # 772 of these did not convert, 166713 did

cl$ISBN_clean[which(nchar(cl$ISBN_clean) == 10)] <- isn13v2$isbn13  # do the replacement

rm(clstkRaw, isn13, isn13.1)

table(nchar(cl$ISBN_clean))  # now there are 213,031

cl <- filter(cl, is.na(VEN.TITLE.))  # this gets rid of the garbage data from faulty Sierra exports


# This is the definitive dataset with clean ISBNs
write.csv(cl
          , file = "./data/results/cl.csv"
          , row.names = F)


View(as.data.frame(cl[!is.na(cl$VEN.TITLE.), ]))

