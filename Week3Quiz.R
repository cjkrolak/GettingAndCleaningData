
Q1Mutate <- function() {
    library(dplyr)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    df <- read.csv(url)
    
    df <- mutate(df, agricultureLogical = (df$ACR==3 && df$AGS ==6))  # create a year column

    }