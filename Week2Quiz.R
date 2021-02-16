Q1GitHubAPI <- function() {
    #Register an application with the Github API here https://github.com/settings/applications.
    # Access the API to get information on your instructors repositories
    # (hint: this is the url you want "https://api.github.com/users/jtleek/repos").
    # Use this data to find the time that the datasharing repo was created.
    # What time was it created?

    # tutorial:  https://github.com/r-lib/httr/blob/master/demo/oauth2-github.r
    url = "https://api.github.com/users/jtleek/repos"

    library(httr)
    
    # 1. Find OAuth settings for github:
    #    http://developer.github.com/v3/oauth/
    oauth_endpoints("github")
    
    # 2. To make your own application, register at
    #    https://github.com/settings/developers. Use any URL for the homepage URL
    #    (http://github.com is fine) and  http://localhost:1410 as the callback url
    #
    #    Replace your key and secret below.
    myapp <- oauth_app("github",
                       key = "a7e7ea4ea39a10d8c217",
                       secret = "914306ed01964365b93db5c9f850197f94be7a90"
    )
    
    # 3. Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
    
    # 4. Use API
    gtoken <- config(token = github_token)
    req <- GET(url, gtoken)
    stop_for_status(req)

    
    # OR:
    req <- with_config(gtoken, GET(url))
    stop_for_status(req)
    data <- content(req)
    print(paste("name=", data$name))
    print(paste("created=", data$created_at))
    names(data)
    
    df <- jsonlite::fromJSON(jsonlite::toJSON(data))
    df2 <- df[which (df$name == "datasharing"),]  # filter out only datasharing repo
    df2$created_at  # return created date
    
}

Q2Sqldf <- function() {
    library(sqldf)
    url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    acs <- read.csv(url)
    head(acs)
    sqldf("select pwgtp1 from acs where AGEP < 50")
}

Q4Html <- function(lines) {
    library(XML)
    url <- "http://biostat.jhsph.edu/~jleek/contact.html"
    con <- url(url)
    htmlCode <- readLines(con, n=lines)
    close(con)
    length(htmlCode)
    print(paste("nchar(10)=", nchar(htmlCode[10])))
    print(paste("nchar(20)=", nchar(htmlCode[20])))
    print(paste("nchar(30)=", nchar(htmlCode[30])))
    print(paste("nchar(100)=", nchar(htmlCode[100])))
    htmlCode
    #html <- htmlTreeParse(url, useInternal = T)  # parse data
    #rootNode <- xmlRoot(html)
    #data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
    #df <- data.frame(t(data),row.names=NULL)
}

Q5FixedWidth <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

    coln <- c("Week", "Nino1+2 SST", "Nino1+2 SSTA",
              "Nino3 SST", "Nino1+2 SSTA", 
              "Nino34 SST", "Nino34 SSTA",
              "Nino4 SST", "Nino4 SSTA")
    fc <- read.fwf(url, c(14,5,8,5,8,5,8,5,8), skip=4, col.names=coln)
    sum(fc[,4])
}