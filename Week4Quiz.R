Q1Split <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    df <- read.csv(url)
    
    # question 1
    splitNames <- strsplit(names(df), "wgtp")  # split on period
    splitNames[123]
}    
Q2Split <- function() {
    # question 2
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    df <- read.csv(url)
    # filter on ranked
    df <- df[as.numeric(df$Gross.domestic.product.2012) > 0 & as.numeric(df$Gross.domestic.product.2012) < 191,]
    df <- df[!is.na(df$Gross.domestic.product.2012),]  # filter out NA
    dv <- as.double(gsub(",", "", df$X.3))  # remove commas
    mean(dv)  # summary stats
    
    # question 3:
    length(grep("^United", df$x.2, value=TRUE))
}
Q4Match <- function() {
    url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    gdp <- read.csv(url1)
    edu <- read.csv(url2)
    mergedData <- merge(gdp, edu, by.x="X", by.y="CountryCode", all=FALSE)
    mergedData <- mutate(mergedData, rank=as.numeric(Gross.domestic.product.2012))  # rank numeric column
    mergedData <- arrange(mergedData[!is.na(mergedData$rank),], desc(rank))  # remove NA and sort desc
    
    # question 4
    print(paste("number of matches=", length(mergedData$rank)))
    juneData <- mergedData[grep("[fF]iscal year end: [jJ]une ([0-9]*)];", mergedData$Special.Notes, value=FALSE),]
    length(juneData$Special.Notes)
}
Q5quantmod <- function() {
    library(quantmod)
    amzn <- getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes <- index(amzn)
    yearCnt <- sum(format(amzn,"%Y") == "2012")
    mondayCnt <- sum(format(amzn,"%A") == "Monday" & format(amzn,"%Y") == "2012") 
    
}