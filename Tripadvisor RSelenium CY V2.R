# INSTRUCTIONS:
# (1) Install and run libraries (5 of them)
# (2) Store all of the functions into your environment (starting from scrapAll() to clickSeeMore())
# (3) Run INITALISATION CODE. This will create a browser that can be automated from RSelenium, and navigate to list of places
#     If this fails, the rest of the code WILL NOT work. lmk ur error messages.
# (4) Run START CODE to start the FULL scrap
# (5) RUn STOP CODE to close remDr and chrome

library(wdman)
library(RSelenium)
library(lubridate)
library(dplyr)
library(readr)

# INITIALISATION CODE: START 
#   Starts RSelenium browser
#   Remove '--headless' from chromeOptions if you want to see the webcrawl in action 
#   (suggest dont remove this if crawling for long periods)
#   If this works, u should see a chrome automated browser pop out
#   If this doesn't work for u, it will not work for the rest of the code. U might need to tweak your eCaps/chromeDriver
#   see https://github.com/SeleniumHQ/selenium/wiki/DesiredCapabilities for help regarding eCaps
cDrv <- chrome(version = "73.0.3683.68")
eCaps <- list(
  chromeOptions = list(
    args = c('--no-sandbox', '--disable-gpu', '--window-size=1280,800')
    #args = c('--no-sandbox', '--disable-gpu','--headless', '--window-size=1280,800')
  ), perfLoggingPrefs = list(
    enableNetwork = F,
    enablePage = F
  )
)
remDr<- remoteDriver(browserName = "chrome", port = 4567L,
                     version = "73.0.3683.68",
                     extraCapabilities = eCaps)
remDr$open()
Sys.sleep(5)
#   first page containing list of Cafés in Singapore
listURL <- 'https://www.tripadvisor.com.sg/Restaurants-g294265-c8-Singapore.html' 
remDr$navigate(listURL)
# INITIALISATION CODE: END 


# START CODE: START
#   This basically starts the FULL scrap
#   IMPORTANT: this will OVERWRITE your current csv if it is called csvName
#   remDr should already be navigated to the first page with list of places
csvName = "Tripadvisor Crawl.csv"
df <- scrapAll(remDr,csvName)
# START CODE: END


# STOP CODE: START
# Run these 2 lines at the end to close the server and browser (if u dont run this, u cannot re-run the code)
remDr$close()
cDrv$stop()
# STOP CODE: END


# FUNCTIONS REQUIRED:
# All below are functions --> run all of below before u start running the code
scrapAll <- function(remDr, csvName = "Tripadvisor Crawl.csv") {
  rowStart <- 1
  count <- 0
  errorCount <- 0
  errorLinks <- c()
  
  lastPage <- remDr$findElements(using = "xpath", "//a[@data-page-number]")
  lastPage <- lastPage[[length(lastPage)]]$getElementText() %>% .[[1]] %>% as.integer()
  
  for (j in 0:(lastPage-1)) {
    
    listURL <- paste("https://www.tripadvisor.com.sg/Restaurants-g294265-c8-oa",j*30,"0-Singapore.html",sep="")
    remDr$navigate(listURL)
    links <- remDr$findElements(using="xpath","//div[@class='title']/a") %>% findLinks()
    df <- structure(list(Place = character(),
                         Price = character(),
                         Address = character(),
                         PostalCode = character(), 
                         Rate.Food = numeric(),
                         Rate.Service = numeric(),
                         Rate.Value = numeric(),
                         Rate.Atmosphere = numeric(),
                         CustomerName = character(),
                         CustomerLocation = character(),
                         Date = character(),
                         ReviewTitle = character(),
                         ReviewBody = character()),
                         class = "data.frame")
    links <- remDr$findElements(using="xpath","//div[@class='title']/a") %>% findLinks()
    if(count==0)
      write.csv(df,csvName,row.names = T)
    
    for (i in 1:length(links)) {
      remDr$navigate(links[i])
      dff <- tryCatch({
        scrapPlace(remDr)
      }, error = function(e) {
        print(e)
        NA
      })
      if (!is.na(dff)) {
        df <- bind_rows(df,dff)
        paste0(nrow(dff), " comments extracted from ",links[i]) %>% print
      } else {
        paste0("Error occurred at ",links[i]) %>% print()
        errorCount <- errorCount + 1
        errorLinks <- c(errorLinks,links[i])
      }
    }
    
    rowCount <- nrow(df)
    rowNames <- c(rowStart:(rowCount+rowStart-1)) %>% as.character()
    rowStart <- rowCount+rowStart
    write.table(df,file=csvName,row.names = rowNames,col.names=F,append=T,sep=",",qmethod="double",dec=".")
    paste0("Appended ",nrow(df), " rows of comments into ",csvName, ".") %>% print
    count <- count + 1
  }
  paste0("Scrapping has completed with ",errorCount," error(s).") %>% print()
  if(errorCount > 0)
    print(errorLinks)
  return(read.csv(csvName))
}

# Scrapes all comments and place-specific information about the place in a dataframe
# Input is the remDr navigated to the place
scrapPlace <- function(remDr) {
  # Retrieve place-specific information ie address, name of place, etc.
  suppressMessages({
    place <- tryCatch({
      place <- remDr$findElement(using = "css selector", '.h1')
      place <- place$getElementText() %>% .[[1]]
    }, error = function(e){
      NA
    })
    
    price <- tryCatch({
      price <- remDr$findElement(using = "css selector", '.header_links a')
      price <- price$getElementText() %>% .[[1]]
    }, error = function(e){
      NA
    })
    
    streetAddress <- tryCatch({
      streetAddress <- remDr$findElement(using = "css selector", '.street-address')
      streetAddress <- streetAddress$getElementText() %>% .[[1]]
      locality <- remDr$findElement(using = "css selector", '.locality')
      locality <- locality$getElementText() %>% .[[1]] %>% gsub(",","",.)
      streetAddress <- paste(streetAddress,locality,sep=" ")
    }, error = function(e){
      NA
    })
    
    postalCode <- tryCatch({
      suppressMessages({
        postalCode <- remDr$findElement(using = "css selector", '.locality')
        postalCode <- postalCode$getElementText() %>% .[[1]] %>% parse_number() %>% as.character()
      })
    }, error = function(e){
      NA
    })
    
    # Attempts to retrieve specific ratings for each category ie Food/Service/Value/Atmosphere
    rate <- tryCatch({
      rate <- remDr$findElements(using = "css selector",
                                 '.restaurants-detail-overview-cards-RatingsOverviewCard__ratingBubbles--1kQYC')
    }, error = function(e){
      NA
    })
    
    if (length(rate) > 1) {
      rate.Food <- tryCatch({
        rate.Food <- rate[[1]]$findChildElement(using = "css selector",'.ui_bubble_rating')
        rate.Food <- rate.Food$getElementAttribute('class') %>% .[[1]] %>% substr(25,nchar(.)) %>% as.numeric()
        rate.Food <- rate.Food/10
      }, error = function(e){
        NA
      })
      rate.Service <- tryCatch({
        rate.Service <- rate[[2]]$findChildElement(using = "css selector",'.ui_bubble_rating')
        rate.Service <- rate.Service$getElementAttribute('class') %>% .[[1]] %>% substr(25,nchar(.)) %>% as.numeric()
        rate.Service <- rate.Service/10
      }, error = function(e){
        NA
      })
      rate.Value <- tryCatch({
        rate.Value <- rate[[3]]$findChildElement(using = "css selector",'.ui_bubble_rating')
        rate.Value <- rate.Value$getElementAttribute('class') %>% .[[1]] %>% substr(25,nchar(.)) %>% as.numeric()
        rate.Value <- rate.Value/10
      }, error = function(e){
        NA
      })
      rate.Atmosphere <- tryCatch({
        rate.Atmosphere <- rate[[4]]$findChildElement(using = "css selector",'.ui_bubble_rating')
        rate.Atmosphere <- rate.Atmosphere$getElementAttribute('class') %>% .[[1]] %>% substr(25,nchar(.)) %>% as.numeric()
        rate.Atmosphere <- rate.Atmosphere/10
      }, error = function(e){
        NA
      })
    } else {
      rate.Food <- NA
      rate.Service <- NA
      rate.Value <- NA
      rate.Atmosphere <- NA
    }
  })
  
  # Retrieve comments
  vTitle <- c(); vComment <- c(); vDate <- c(); vRating <- c(); vCustName <- c(); vCustLoc <- c();
  repeat {
    clickSeeMore(remDr)
    listContainers <- remDr$findElements(using = "css selector", '.review-container')
    for(j in 1:length(listContainers)) {
      container <- listContainers[[j]]
      title <- tryCatch({
        suppressMessages({
          title <- container$findChildElement(using="css selector",'.noQuotes')
          title <- title$getElementText() %>% .[[1]]
        })
      }, error = function(e) {
        NA
      })
      vTitle <- c(vTitle,title)
      
      comment <- tryCatch({
        suppressMessages({
          comment <- container$findChildElement(using="css selector",'.entry')
          comment <- comment$getElementText() %>% gsub("\nShow less","",.,fixed=T) %>% gsub("\n\n"," ",.,fixed=T)
        })
      }, error = function(e) {
        NA
      })
      vComment <- c(vComment,comment)

      date <- tryCatch({
        suppressMessages({
          date <- container$findChildElement(using="css selector",'.ratingDate')
          date <- date$getElementAttribute("title") %>% dmy() %>% format('%d-%m-%Y') %>% as.character()
        })
      }, error = function(e) {
        NA
      })
      vDate <- c(vDate,date)
      
      rating <- tryCatch({
        suppressMessages({
          rating <-  container$findChildElement(using="css selector",'.ui_bubble_rating')
          rating <- rating$getElementAttribute('class') %>% .[[1]] %>% substr(25,nchar(.)) %>% as.numeric()
          rating <- rating/10
        })
      }, error = function(e) {
        NA
      })
      vRating <- c(vRating,rating)
      
      custName <- tryCatch({
        suppressMessages({
          custName <- container$findChildElement(using="css selector",'.info_text div')
          custName <- custName$getElementText() %>% .[[1]]
        })
      }, error = function(e) {
        NA
      })
      vCustName <- c(vCustName,custName)
      
      custLoc <- tryCatch({
        suppressMessages({
          custLoc <- container$findChildElement(using="css selector",'.info_text .userLoc')
          custLoc <- custLoc$getElementText() %>% .[[1]]
        })
      }, error = function(e) {
        NA
      })
      vCustLoc <- c(vCustLoc,custLoc)
    }
    
    url <- tryCatch({
      suppressMessages({
        url <- remDr$findElement(using = "xpath", "//link[@rel='next']")
        url <- url$getElementAttribute("href") %>% .[[1]]
      })
    }, error = function(e) {
      NA
    })
    if (is.na(url))
      break
    else
      remDr$navigate(url)
  }
  
  # Arranges everything in a dataframe to be returned
  df <- data.frame('CustomerName' = vCustName,
                   'CustomerLocation' = vCustLoc,
                   'Date' = vDate,
                   'ReviewTitle' = vTitle,
                   'ReviewBody' = vComment
                   )
  df$Place <- place
  df$Price <- price
  df$Address <- streetAddress
  df$PostalCode <- postalCode
  df$Rate.Food <- rate.Food
  df$Rate.Service <- rate.Service
  df$Rate.Value <- rate.Value 
  df$Rate.Atmosphere <- rate.Atmosphere
  
  df <- df[,c(6:13,1:5)]
  return(df)
}


# Extract links from element source
findLinks <- function(x) {
  links <- c()
  for(i in 1:length(x)) {
    links <- c(links, x[[i]]$getElementAttribute("href")[1])
  }
  links <- sapply(links,'[[',1)
}

# Click 'See More' button if exists
clickSeeMore <- function(remDr) {
  tryCatch({
    suppressMessages({
      seeMore <- remDr$findElement(using = "xpath","//span[@class='taLnk ulBlueLinks']")
      seeMore$clickElement()
      wait <- TRUE
      while (wait == T) {
        Sys.sleep(0.01)
        wait <- tryCatch({
        seeMore$getElementText() # if this runs into an error, means the click finished loading
        TRUE
        }, error = function(e) {
          FALSE
        })
      }
    })
  }, error = function(e) {
    NULL
  })
}
