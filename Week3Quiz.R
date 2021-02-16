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