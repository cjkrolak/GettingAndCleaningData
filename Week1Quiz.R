q1LoadURL <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    #f = download.file(url)
    dateDownloaded <- date()
    df <- read.csv(url)
    head(df)
}

q3LoadXL <- function () {
    library(xlsx)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    tempfile <- "C:\\Users\\cjkro\\Downloads\\data.xlsx"
    f = download.file(url, destfile=tempfile, mode='wb')
    print("file downloaded")
    dat <- read.xlsx(tempfile, sheetIndex=1, header=TRUE, colIndex=7:15,
                     rowIndex=18:32, as.data.frame = TRUE,
                     colClasses=c(G='numeric', L='numeric'),
                     stringsAsFactors=FALSE)
    print("file read")
    sum(suppressWarnings(as.numeric(dat$Zip))*suppressWarnings(as.numeric(dat$Ext)),na.rm=T)
}

q4LoadXML <- function () {
    url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
    targetZip = 21231

    # insecure URL load using download.file
    library(XML)
    library(expss)
    tempfile <- "C:\\Users\\cjkro\\Downloads\\data.xml"
    url <- sub("https", "http", url)  # convert url to http
    f = download.file(url, destfile=tempfile)
    print(paste("file downloaded using 'download.file()' from", url,
                "to", tempfile))
    doc <- xmlTreeParse(url, useInternal= TRUE)
    print("file parsed into XML")
    rootNode <- xmlRoot(doc)
    name <- xmlName(rootNode)
    print(paste("rootNode name=", name))
    names(rootNode)
    resp <- as.numeric(xpathSApply(rootNode, "//zipcode", fun  = xmlValue))
    print(paste("length of resp=", length(resp)))
    print(resp)
    cnt <- count_if(targetZip, resp)
    print((paste("count of", targetZip, "=", cnt)))

    # secure https load using curl
    library(RCurl)
    library(expss)
    tempfile <- "C:\\Users\\cjkro\\Downloads\\data2.xml"
    xData <- getURL(url)
    print(paste("file downloaded using 'curl' from", url))
    doc2 <- xmlTreeParse(xData, useInternal= TRUE)
    print("file parsed into XML")
    rootNode2 <- xmlRoot(doc2)
    name2 <- xmlName(rootNode2)
    print(paste("rootNode2 name=", name2))
    names(rootNode2)
    resp2 <- as.numeric(xpathSApply(rootNode2, "//zipcode", fun  = xmlValue))
    print(paste("length of resp2=", length(resp2)))
    print(resp2)
    cnt2 <- count_if(targetZip, resp2)
    print((paste("count of", targetZip, "=", cnt2)))
}

q5freadCSV <- function () {
    url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    #tempfile <- "C:\\Users\\cjkro\\Downloads\\data5.csv"
    #f = download.file(url, destfile=tempfile)
    print("file downloaded")
    DT <- fread(url, sep=",")
    print("file downloaded using fread")
    head(DT)
    DT2 <- subset(DT, select=-c(RT, NAICSP, SOCP))  # numeric only data frame
    
    library(rbenchmark)
    benchmark(
    DT[,mean(pwgtp15),by=SEX],  # 0.18
    mean(DT$pwgtp15,by=DT$SEX), # 0.00
    sapply(split(DT$pwgtp15,DT$SEX),mean),  # 0.03
    tapply(DT$pwgtp15,DT$SEX,mean),  # 0.05
    mean(DT[DT$SEX==1,]$pwgtp15),  # 1.27
    mean(DT[DT$SEX==2,]$pwgtp15),  # 1.78
    rowMeans(DT2, na.rm=TRUE)[DT$SEX==1],  # 2.05
    rowMeans(DT2, na.rm=TRUE)[DT$SEX==2])  # 1.88
    
    }