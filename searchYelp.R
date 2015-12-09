library(httr)
load("yelpOAuth.RData")

# SEARCH 
searchYelp <- function(location, term, limit, offset, sort, category_filter, radius_filter, deals_filter, mySignature) {
    url <- "https://api.yelp.com/v2/search?"
    url <- parse_url("https://api.yelp.com/v2/search?")
    
    # check for location
    if (!exists(location)) {
        stop("Error: Please give in a location. This parameter is required.")
    } 
    
    # TO-DO: check signature
    
    # bring term in adequate format if it has a value
    if (!is.na(term)) {
        # if term consists of more then one word, concatenate the single words with plus signs
        if ((sapply(gregexpr("\\W+", term), function(x) sum(x>0)) + 1) > 1) {
            term <- paste(unlist(strsplit(term, split = " ")), collapse = "+")
        } 
    }
    

    # check if limit has a valid input and through error message if not
    if (limit > 40) {
        stop("The limit is too high. Please choose a number between 1 and 40.")
    }
    
    # set query parameters if they exist
    if (exists(location)) {
        url$query$location <- location
    }
    if (exists(term)) {
        url$query$term <- term
    }
    if (exists(limit)) {
        url$query$limit <- limit
    }
    if (exists(offset)) {
        url$query$offset <- offset
    }
    if (exists(sort)) {
        url$query$sort <- sort
    }
    if (exists(category_filter)) {
        url$query$category_filter <- category_filter
    }
    if (exists(radius_filter)) {
        url$query$radius_filter <- radius_filter
    }
    if (exists(deals_filter)) {
        url$query$deals_filter <- deals_filter
    }
    
    # build query url and get the data
    url <- URLdecode(build_url((url))
    search <- GET((url), mySignature)
}


# To-Do`s
## search API 
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
