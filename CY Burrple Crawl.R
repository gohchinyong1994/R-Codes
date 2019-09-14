library(rvest)
library(dplyr)
library(gtools)
library(readr)
library(httr)
library(stringr)
library(rjson)

# Run these 2 lines after running all the below functions to test the code
# Rolling append feature to CSV in case code fails to connect halfway
df <- burScrapCategory(category = 'Cafes & Coffee',
                       host = 'https://www.burpple.com',
                       country = 'SG',
                       maxLoadMoreComments = 15,
                       maxLoadMorePlaces = 100,
                       csvName = "burScrap.csv",
                       verbose = T)
df <- read.csv("burScrap.csv")


# MAIN FUNCTION: burScrapCategory()
# maxLoadMoreComments: Max no of 'load more' comments to visit
#                      6 comments per load more (note that first page already has 3 comments)
# maxLoadMorePlaces  : Max no of 'load more' places to visit
#                      12 places per load more
burScrapCategory <- function(category = 'Cafes & Coffee',
                             host = 'https://www.burpple.com',
                             country = 'SG',
                             maxLoadMoreComments = 1,
                             maxLoadMorePlaces = 1,
                             csvName = "burScrap.csv",
                             verbose = T) {
  # (1) Initialisation
  if(maxLoadMorePlaces == -1) 
    maxLoadMorePlaces <- Inf
  if(maxLoadMoreComments == -1)
    maxLoadMoreComments <- Inf
  errorCount <- 0
  errorLinks <- c()

  # (2) Attempt to read category page (containing the list of places)
  count <- 0
  catPageURL <- burURLfinder(baseURL = host, country=country,category = category)
  if(is.na(catPageURL)) {
    print("Category Page cannot be found. Enter the correct category using burCategories function.")
    return(NULL)
  }
  catPage <- GET(catPageURL)
  catPageHTML <- catPage$content %>% read_html
  html <- catPageHTML
  
  # (3) Set up headers required for mimic XHR requests
  reqHeaders <- c(
    'User-Agent' = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.45 Safari/537.36",
    'Referer' = host,
    'X-Requested-With' = "XMLHttpRequest"
  )
  
  # (4) Start extracting data (1 loop for every 12 places)
  repeat{
    
    # (4.1) Extract the places and links in the category page for each load more click (usually contains 12 rows per click)
    dfPlaceLinks <- placesParse(html,host)
    
    # (4.2) Initialise a fresh data.frame for each iteration (this reduces memory space required)
    df <- structure(list(Place = character(),
                         Price = character(),
                         District = character(),
                         Address = character(),
                         PostalCode = character(),
                         CustomerName = character(),
                         ReviewTitle = character(),
                         ReviewBody = character(), 
                         Date = character()), 
                    class = "data.frame")
    
    # This initialises a fresh CSV with headers for us to append to
    if(count==0)
      write.csv(df,csvName,row.names = F)
    
    # (4.3) Loop through each place in dfPlaceLinks
    #       Extracts comments and at the end, append to CSV
    for (i in 1:nrow(dfPlaceLinks)) {
      
      tryCatch({
        dff <- burScrapPlace(url = dfPlaceLinks[i,2],host,maxLoadMoreComments)
        df <- bind_rows(df,dff)
        if (verbose)
          paste0(nrow(dff), " comments extracted from ", dfPlaceLinks[i,1], " in ", category, ".") %>% print
      }, error = function(e) {
        errorCount <- errorCount + 1
        errorLinks <- c(errorLinks,dfPlaceLinks[i,2])
        print(e)
        paste0("Error occurred at ",dfPlaceLinks[i,2]) %>% print()
      })
      
    }
    
    # (4.4) Write to CSV (12 places of comments)
    write.table(df,file=csvName,row.names = F,col.names=F,append=T,sep=",",qmethod="double",dec=".")
    if(verbose)
      paste0("Appended ",nrow(df), " rows of comments into ",csvName, ".") %>% print
    
    # (5) Terminating condition 
    # (4.5) Attempts to retrieve the next 'load more' link 
    count <- count + 1    
    nextPageUrl <- html %>% html_nodes(xpath = "//div[@class='masonryViewMore']/a/@href") %>% html_text
    if (length(nextPageUrl) == 0 || count >= maxLoadMorePlaces) {
      paste0("Scrapping has completed with ",errorCount," error(s).") %>% print()
      if(errorCount > 0)
        print(errorLinks)
      return(read.csv(csvName))
    }
    nextPageUrl <- paste0(host,nextPageUrl)
    page <- GET(nextPageUrl,add_headers(reqHeaders))
    html <- content(page)
  }
}

# CORE FUNCTION: placesParse()
# Extracts the links from category page to the review page
placesParse <- function (html,host = 'https://www.burpple.com') {
  if (grepl("searchVenue card feed-item",html)) {
    vHtml <- html %>% gsub("\\\"","",.,fixed=T) %>%
      strsplit("searchVenue card feed-item") %>%  .[[1]] %>% .[2:length(.)]
    place <- c()
    link <- c()
    for (i in 1:length(vHtml)) {
      if(grepl("data-venue-name",vHtml[i])) {
        place <- vHtml[i] %>% read_html %>%
          html_nodes(xpath = "//button[@class='btn--wishBtn']/@data-venue-name") %>%
          html_text %>% c(place,.)
      } else {
        place <- c(place,NA)
      }
      if(grepl("searchVenue-header card-item card-item--header",vHtml[i])) {
        link <- vHtml[i] %>% read_html %>%
          html_nodes(xpath = "//div[@class='searchVenue-header card-item card-item--header']/a/@href") %>% 
          html_text %>% paste0(host,.) %>% c(link,.)
      } else {
        link <- c(link,NA)
      }
    }
    return(data.frame("Place" = place,
                      "Link" = link,
                      stringsAsFactors = F))
  } else {
    return(NULL)
  }
}

# CORE FUNCTION: burScrapPlace()
# Extracts comments from a place
burScrapPlace <- function(url, host = 'https://www.burpple.com',maxLoadMore) {
  # Scraps comments off a place in Burrple
  # host: the host url of BUrrple
  # url: the url of the place in Burrple eg. https://www.burpple.com/sunday-folks
  # OUTPUT: a dataframe
  
  # (1) Retrieve HTML
  firstPage <- GET(url)
  
  firstPageHTML <- firstPage$content %>% read_html()
  
  # (2) Extract relevant NON-COMMENT SPECIFIC content from first page
  
  place <- firstPageHTML %>% html_nodes(xpath="//a[@class='venue-name']") %>% 
    html_text() %>% gsub('[\t\n]','',.)
  
  price <- firstPageHTML %>% html_nodes(xpath="//div[@class='venue-price']") %>% 
    html_text() %>% gsub('[\t\n]','',.)
  
  district <- firstPageHTML %>% html_nodes(xpath="//div[@class='venue-area']") %>% 
    html_text() %>% gsub('[\t\n]','',.)
  
  address <- burAddress(firstPageHTML)
  
  postal <- burPostal(firstPageHTML)
  
  # (4) Extract first few comments from first page
  
  df <- commentParse(firstPageHTML,pos = 1) # pos = 1 is for function to recognise it is a first page
  
  # (5) Scrap the remaining "load more" buttons
  nextPageUrl <-  firstPageHTML %>% html_nodes(xpath = "//a[@class='btn btn--blue btn-see-more']/@href") %>% 
    html_text %>% gsub('[\t\n]','',.)
  #if (nextPageUrl != '') { # Error in if (nextPageUrl != "") { : argument is of length zero
  if (length(nextPageUrl) !=0 ) {
    nextPageUrl <- paste0(host,nextPageUrl)
    df <- bind_rows(df,burScrapPageMore(nextPageUrl,host,maxLoadMore))
  }
  
  # (6) Add the other constant columns (NON-COMMENT SPECIFIC) eg. name of place, price, etc
  
  df$Place <- place
  df$Price <- price
  df$District <- district
  df$Address <- address
  df$Postal <- postal
  
  df <- df[,c(5:9,1:4)]
  
  return(df)
}
# Test inputs:
# url <- 'https://www.burpple.com/sunday-folks?bp_ref=%2Fsearch%2Fsg'
# url <- 'https://www.burpple.com/upsidedown-coffee?bp_ref=%2Fsearch%2Fsg'
# df <- burScrapPlace(url, maxLoadMore = 5)


# CORE FUNCTION: commentParse()
# Defines the output columns to be extracted PER comment post in Burrple
# Extracts and returns data.frame containing comment specific data (less name of place, etc)
# Inputs: (1) html: HTML object containing multiple comments 
#         (2) pos (use 1 for firstpage, 2 for rest of the pages, see function cutStr() for the logic)
commentParse <- function(html,pos=1) {
  if (grepl("card-body",html)) {
    vHtml <- html %>% gsub("\\\"","",.,fixed=T) %>% strsplit("card-body") %>%  .[[1]] %>% .[2:length(.)]
    pTitle <- c()
    pComment <- c()
    pDate <- c()
    pCust <- c()
    for (i in 1:length(vHtml)) {
      if(grepl("food-description-title",vHtml[i])) {
        pTitle <- vHtml[i] %>% read_html %>%
          html_nodes(xpath = "//div[@class='food-description-title']/a") %>% html_text %>%  
          lapply(cutStr) %>% sapply('[[',1) %>% str_trim %>% c(pTitle,.)
      } else {
        pTitle <- c(pTitle,NA)
      }
      if(grepl("food-description-body",vHtml[i])) {
        comment <- vHtml[i] %>% read_html %>% html_nodes('.food-description-body p') %>% html_text
        if(length(comment) >= 2)
          comment <- paste(comment, collapse = '') %>% str_trim 
        comment <- gsub("\\n","",comment,fixed = T) %>% gsub("\\\\","",.,fixed=F)
        pComment <- c(pComment,comment)
      } else {
        pComment <- c(pComment,NA)
      }
      if(grepl("card-item-set--link-subtitle",vHtml[i])) {
        pDate <- vHtml[i] %>% read_html %>%
          html_nodes(xpath = "//div[@class='card-item-set--link-subtitle']") %>% html_text %>%  
          lapply(cutStr,pos=pos) %>% sapply('[[',1) %>% str_trim %>% sapply(burDate) %>% unname() %>% c(pDate,.)
      } else {
        pDate <- c(pDate,NA)
      }
      if(grepl("card-item-set--link-title",vHtml[i])) {
        pCust <- vHtml[i] %>% read_html %>%
          html_nodes(xpath = "//div[@class='card-item-set--link-title']/a") %>% html_text %>%  
          lapply(cutStr) %>% sapply('[[',1) %>% str_trim %>% c(pCust,.)
      } else {
        pCust <- c(pCust,NA)
      }
      
    }
    return(data.frame("CustomerName" = pCust,
                      "ReviewTitle" = pTitle,
                      "ReviewBody" = pComment,
                      "Date" = pDate,
                      stringsAsFactors = F))
  } else {
    return(NULL)
  }
}

# CORE FUNCTION: burScrapPageMore()
# Scrapes "load more" data from specific place in Burrple
# Input link must be the link of the "load more" button
burScrapPageMore <- function(nextPageUrl,host,maxLoadMore) {
  reqHeaders <- c(
    'User-Agent' = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.45 Safari/537.36",
    'Referer' = host,
    'X-Requested-With' = "XMLHttpRequest"
    # Not sure whether below is needed, testing might be needed
    #'X-CSRF-Token' = 'XPJypocwao28aPR+ClhR7M1WBYp69Ev9fwOzDVHTMWyDQb5U/EM42kpBVdYoLD5z/IyeXqLSSzuUqtJIDTqAdw=='
  )
  count <- 0
  df <- structure(list(CustomerName = character(),
                      ReviewTitle = character(),
                      ReviewBody = character(), 
                      Date = character()), 
                      class = "data.frame")
  repeat{
    count <- count + 1
    urlNext <- nextPageUrl
    page <- GET(nextPageUrl,add_headers(reqHeaders))
    html <- content(page) %>% read_html
    
    dff <- commentParse(html,pos = 2)
    df <- bind_rows(df,dff)
    
    # Terminating condition
    if (!grepl("new_load_more_web_foods_path = '",read_html(content(page))) || count >= maxLoadMore) {
      return(df)
      break
    }
    
    nextPageUrl <- html %>% html_text %>% strsplit("new_load_more_web_foods_path = '") %>% 
      .[[1]] %>% .[2] %>% substr(1,(str_locate(.,";")[1,1]-2))
    nextPageUrl <- paste0(host,nextPageUrl)
  }
}

# AUXILLIARY FUNCTION: cutStr()
# Takes the first element of the list and retrieves the specific position (pos) of the vector
cutStr <- function(x,pos=1) {
  strsplit(x,"\\n",fixed=T)[[1]][pos]
}

# AUXILLIARY FUNCTION: burAddress()
# Retrieves address from html using JSON through tryCatch
burAddress <- function(html) {
  address <-  tryCatch({
    json <- html %>% html_nodes(xpath='//script[@type="application/ld+json"]') %>% html_text %>% fromJSON()
    address <- paste(json$address$streetAddress,
                     json$address$addressCountry,
                     json$address$postalCode,
                     sep = " ")
    
  }, error = function(e) {
    address <- NA
  } 
  )
  return(address)
}
# AUXILLIARY FUNCTION: burPostal()
# Retrieves postal code from html using JSON through tryCatch
burPostal <- function(html) {
  postal <-  tryCatch({
    json <- html %>% html_nodes(xpath='//script[@type="application/ld+json"]') %>% html_text %>% fromJSON()
    postal <- json$address$postalCode %>% as.character()
  }, error = function(e) {
    postal <- NA
  } 
  )
  return(postal)
}

# AUXILLIARY FUNCTION: burDate
# Converts a Burpple time stamp into date format dd-mm-yyyy
burDate <- function(str) {
  # Converts a Burpple string date to date format dd-mm-yyyy 
  # Deals with strings like "3d ago", "1 week ago", "Jan 4 at 9:19pm", "Nov 6, 2018"
  str <- str_trim(str)
  currDate <- Sys.Date()
  
  if (!is.na(as.numeric(substr(str,1,1)))) {
    if (grepl("week",str,ignore.case = T))
      date <- currDate - (7 * parse_number(str)) #First character is numeric and contains "week" eg. "1 week ago"
    else if (grepl("d",str))
      date <- (currDate - parse_number(str)) #First character is numeric and contains "d" eg."3d ago"
    else if (grepl("h",str)) # Assume review is dated on system date if it is hours ago
      date <- currDate
    else if (grepl("m",str))
      date <- currDate
    else if (grepl("s",str))
      date <- currDate
    else
      print(paste0("ERROR OCCURED AT STR = ",str))
  } else  {
    if(grepl("at",str))
      date <- (strsplit(str,"at")[[1]][1] %>% str_trim %>% as.Date(format="%b %d")) # Contains "at" eg. "Jan 4 at 9:19pm"
    else
      date <- (as.Date(str,format="%b %d, %Y")) # Ordinary date eg. "Nov 6, 2018"
  }
  date <- as.character(format(date,"%d-%m-%Y"))
}

# AUXILLIARY FUNCTION: burCategories
# Extracts a data.frame containing: (1) Category Name, (2) URL suffixes for each category
burCategories <- function(baseURL = "https://www.burpple.com",country = 'SG') {
  # Returns a data.frame containing:
  # 1. Category Name
  # 2. URL suffixes for each category
  html <- paste0(baseURL, "/categories/") %>%
    paste0(tolower(country)) %>%
    read_html()
  
  # Retrieves category names
  catNames <- html %>%
    html_nodes(xpath="//ul[@class='clearfix']/li/a[@class='a--grey']") %>%
    html_text()
  
  # Retrieves the suffix of the URL for each category
  endingURL <- html %>%
    html_nodes(xpath = "//ul[@class='clearfix']/li/a[@class='a--grey']/@href") %>%
    html_text()
  
  data.frame("Category" = catNames, "URL" = endingURL)
}

# AUXILLIARY FUNCTION: burURLfinder
# Returns a string containing the URL for a country and category
burURLfinder <- function(baseURL = "https://www.burpple.com", country = 'SG', category = "Cafes & Coffee") {
  URL <- tryCatch({  
    URL <- burCategories(baseURL,country) %>%
      filter(Category == category) %>%
      .[,2] %>%
      as.character() %>%
      paste0(baseURL,.) %>% 
      read_html() %>%
      html_nodes(xpath="//div[@class='collection-feed collection-feed--top-venues']
                 /div[@class='collection-feed__footer']
                 /a[@class='collection-feed_see-more-btn feed-see-more']/@href") %>%
      html_text %>%
      paste0(baseURL,.)
      if (URL == baseURL)
        URL <- NA
      URL
    },
      error = function(e) {
        URL <- NA
      }
      )
  return(URL)
}
