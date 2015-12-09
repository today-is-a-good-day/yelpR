library(httr)
load("yelpOAuth.RData")

# SEARCH 
searchYelp <- function(location, term, limit, offset, sort, category_filter, radius_filter, deals_filter, mySignature) {
    url <- "https://api.yelp.com/v2/search?"
    
    if (!exists(location)) {
        stop("Error: Please give in a location. This parameter is required.")
    }
    
    # in case term paramter has a value
    if (!is.na(term)) {
        # check number of words passed to term 
        lgth <- sapply(gregexpr("\\W+", term1), function(x) sum(x>0) ) + 1)
# if term consists of more then one word, concatenate the single words with plus signs
if lgth > 1 {
    term <- paste(unlist(strsplit(term, split = " ")), collapse = "+")
} 
    }

if (limit > 40) {
    stop("The limit is too high. Please choose a number between 1 and 40.")
} 

search <- GET(URLdecode(url), mySignature)
}


# To-Do`s
## search function/search API 
## Parameters: term, limit, offset, sort, category_filter, radius_filter, deal_filter, 
## location (3 different ways) -> include all recognized neighborhoods + 
## all categories
## parse answer to df
## business API
## Parameters: cc, lang, lang_filter, actionlinks
## Phone API
## Parameters: phone, cc, category
## Create new classes:
## search class
## business class

# neighborhood list: https://www.yelp.com/developers/documentation/v2/neighborhood_list
# category list: https://www.yelp.com/developers/documentation/v2/all_category_list
