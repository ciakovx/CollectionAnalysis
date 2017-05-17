library(stringr)
ISBN_clean <- function(df){
  # This function takes a dataframe of books exported from Sierra with the ISN variable, which are full of 
  # junk other than ISBNs. It tries to extract real ISBNs out of all that junk, and then inserts a zero before all
  # 9 digit ISBNs. The purpose is to be able to match these ISBNs to a dataset of with clean ISBN field.
  # 
  # Arg:
  #   A dataframe of items extracted from Sierra including an ISN variable
  #
  # Returns:
  #   A dataframe with a ISBN_clean variable that will have nothing but integers, hopefully ISBNs. All fields that do not
  #   meet the criteria are removed, resulting in a significantly smaller dataset. 
  
  
  # get complete cases of ISBNs. We don't care about anything else  
  ISBNdf <- df[complete.cases(df$X020.a), ] 
  
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$X020.a, "^[^\\~]*")  # get string before tilde
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$ISBN_clean, "^[^\\;]*")  # get string before semicolon
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$ISBN_clean, "^[^\\(]*")  # get string before parentheses
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$ISBN_clean, "^[^\\:]*")  # get string before colon
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$ISBN_clean, "^(?:(?!\\s\\w).)*")  # get string before word
  ISBNdf$ISBN_clean <- str_extract(ISBNdf$ISBN_clean, "^(?:(?!\\s).)*")  # get string before space
  
  # replace anything in the ISBN field that starts with a dollar sign. These typically have 
  # dollar signs alone, not ISBNs
  #ISBNdf$ISBN_clean <- str_replace_all(ISBNdf$X020.a, "^\\$", NA) 
  
  
  
  # Get complete cases again
  ISBNdf <- ISBNdf[complete.cases(ISBNdf$ISBN_clean), ] # 
  
  # Trim whitespace
  ISBNdf$ISN <- str_trim(ISBNdf$ISBN_clean)
  
  # Adding a zero to the beginning of 9 digit ISBNs
  # This requires dealing with ISSNs, which have 9 characters and a hyphen. We want to add a zero to items with 9 characters and no hyphen
  # this should not be necessary if you export only books and not serials.
  ISBNfix <- ISBNdf$ISBN_clean
  # length(ISBNfix[which(nchar(ISBNfix) == 9 & !str_detect(ISBNfix, "-"))])  # How many values 9 characters and no hyphen?
  # length(ISBNfix[which(nchar(ISBNfix) == 10)])  # How many values have ten characters?
  # attach a zero to the beginning of items with 9 characters and no hyphen: these are ISBNs, not ISSNs
  indx <- which(nchar(ISBNfix) == 9 & !str_detect(ISBNfix, "-"))  # create an index of 9 digit ISBNs without a hyphen
  isn9 <- as.character(paste0("0", ISBNfix[which(nchar(ISBNfix) == 9 & !str_detect(ISBNfix, "-"))]))  
  ISBNfix[indx] <- isn9
  ISBNdf$ISBN_clean <- ISBNfix
  
  return(ISBNdf)
}