Q1Mutate <- function() {
    library(dplyr)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    df <- read.csv(url)
    
    df <- mutate(df, agricultureLogical =(df$ACR==3 & df$AGS==6))  # create a year column
    df <- df[which(df$agricultureLogical==TRUE),]  # filter by TRUE
    df[1:3,]
}

Q2Jpeg <- function() {
    library(jpeg)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
    tempfile <- "C:\\Users\\cjkro\\Downloads\\temp.jpg"
    f <- download.file(url, destfile=tempfile, mode='wb')
    img.n <- readJPEG(tempfile, native = TRUE)
    q <- quantile(img.n, probs=c(0.30, 0.80))  # quantiles
}

Q3Match <- function() {
    url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    gdp <- read.csv(url1)
    edu <- read.csv(url2)
    mergedData <- merge(gdp, edu, by.x="X", by.y="CountryCode", all=FALSE)
    mergedData <- mutate(mergedData, rank=as.numeric(Gross.domestic.product.2012))  # rank numeric column
    mergedData <- arrange(mergedData[!is.na(mergedData$rank),], desc(rank))  # remove NA and sort desc
    
    # question 3
    print(paste("number of matches=", length(mergedData$rank)))
    print(paste("13th lowest gdp=",mergedData[13, "Short.Name"]))
    
    # question 4
    tapply(mergedData$rank, mergedData$Income.Group, mean, na.rm=TRUE)
    
    # question 5
    mergedData$rankquartile <- cut(mergedData$rank, breaks=quantile(mergedData$rank,
                                                                    probs = seq(0, 1, 1/5),
                                                                    na.rm=TRUE), na.rm=TRUE)
    table(mergedData$Income.Group, mergedData$rankquartile)
    mergedData
}