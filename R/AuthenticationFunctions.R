baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
authVersion <- '1.0' ## Current Oauth version used.
oauth_config <- 'HMAC-SHA1'

### This function checks for authentication-related options which must be set using the "options" function.
checkAuthentication <- function(){
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  ops <- Sys.getenv()
  requiredOptions <- c("schoologyConsumerKey", "schoologyConsumerSecret")#, "apiUrl")
  
  for(option in requiredOptions){
    if(Sys.getenv(option) == ''){
      stop(paste0('Required option ', option, ' has not been set! Set its value using Sys.setenv(', option, ' = "{', option, 'Value}").'))
    }
  }
}

# This function retrieves a token for use in calling the API.
getToken <- function(){
  
  checkAuthentication()
  
  require(httr)
  
  TokenZeroLeg <- R6::R6Class("TokenZeroLeg", inherit = Token, list(
    init_credentials = function() {},
    sign = function(method, url) {
      oauth <- oauth_signature(url, method, self$app, token = NULL, token_secret = "")
      
      c(httr:::request(url = url), oauth_header(oauth))
    }
  ))
  
  oauth_zero_leg_token <- function(app) {
    TokenZeroLeg$new(
      app = app, 
      endpoint = NULL,
      credentials = list(),
      cache_path = FALSE
    )
  }
  
  myApp <- oauth_app("Schoology", key = getOption('schoologyConsumerKey'), secret = getOption('schoologyConsumerSecret'))
  myToken <- oauth_zero_leg_token(myApp)
  
  return(myToken)
}

# This function converts a dataframe of varying classes to all character.
characterizeDataFrame = function(df){
  
  # Remove empty columns
  df = df[!(as.logical(lapply(df, length) == 0))]
  df = data.frame(df, stringsAsFactors = FALSE)
  
  # Convert columns to character vectors
  for(i in 1:length(names(df))){
    
    df[,i] = as.character(df[,i])
  }
  
  return(data.frame(df, stringsAsFactors = FALSE))
}

# This function consistently flattens a json list into a dataframe.
flattenJsonList = function(jsonList){
  
  for(item in jsonList){
    
    item = data.frame(item)
    
    if(!exists('df', inherits = FALSE)){
      df <- jsonlite::flatten(item)
    }else{
      df <- bind_rows(df, jsonlite::flatten(item))
    }
  }
  return(df)
}


# This function properly formats parameters for inclusion in a Schoology API request.
addParameters <- function(endpoint, params){

  paramsString = ''
  for(i in 1:length(params)){
    if(i == 1){
      paramsString = '?'
    }else{
      paramsString = paste0(paramsString, '&')
    }
    newString = paste0(names(params)[[i]], '=', params[[i]])
    paramsString = paste0(paramsString, newString)
  }

  return(paste0(endpoint, paramsString))
}

#' Perform API Request to Endpoint
#' 
#' This function performs an HTTP request to a Schoology API endpoint.
#' 
#' This function is called by all Schoology SDK functions.
#' You will, in general, not need to call this function directly.
#' @param endpt The endpoint URL for the request.
#' @param paramsList A list of parameters primarily used as searchFields.
#' @param verb An HTTP request, either GET, PUT, POST or DELETE.
#' @param payload A json object used in PUT and POST requests. \cr Use fromJSON(toJSON(\{yourNamedList\}, pretty = T)).
#' @concept Requests
#' @return The content of the request response.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
#' @export
makeRequest <- function(endpt, paramsList = NULL, verb = 'GET', payload = NULL){
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  checkAuthentication()
  
  Sys.sleep(.2)  
  
  # See https://github.com/r-lib/httr/blob/master/demo/oauth1-nounproject.r for Oauth1.0 1-leg fix.
  Schoology_app <- oauth_app("Schoology",
                           key = Sys.getenv('schoologyConsumerKey'),
                           secret = Sys.getenv('schoologyConsumerSecret')
  )
  
  # Remove ending / if it's present.
  endpt <- gsub('//', '/', ifelse(regexpr('./$', endpt) > 0, substr(endpt, 1, nchar(endpt)-1), endpt))
  if(!is.null(paramsList) & length(paramsList) > 0) {
    endpt <- addParameters(endpt, paramsList)
  }
  
  # Make absolutely sure only one / between apiUrl and endpt.
  apiUrl <- baseUrl
  apiUrl <- ifelse(regexpr('./$', apiUrl) > 0, apiUrl, paste0(apiUrl, '/'))
  endpt <- ifelse(substr(endpt, 1, 1) == '/', substr(endpt, 2, nchar(endpt)), endpt)
  
  url <- paste0(apiUrl, endpt)
  
  sig <- oauth_signature(url, method = verb, app = Schoology_app)
  header_oauth <- oauth_header(sig)
  eval(parse(text = paste0('response <- ', verb, '(url, header_oauth, body = payload, encode = "json", accept_json())')))
  
  if(response$status_code < 300){
    return(content(response))
  }else{
    stop(content(response))
  }
}

allSubjects <- c("Attendances", "Buildings", "Courses", "Enrollments", "Grading Periods", "Groups", "Roles", "Schools", "Sections", "Users")

generatePkgdownYmlFile <- function(subjects = allSubjects){
  
  # Update _pkgdown.yml to aid in reference navigation.
  ymlText <- paste0('url: https://samterfa.github.io/SchoologySDK/
                    
author: Sam Terfa

reference:')
  
  for(subject in subjects){
    
    ymlText <- paste0(ymlText,'
 - title: ', str_to_title(subject) %>% str_replace_all(' ', ''),'
   desc:  Functions involving ', subject %>% str_replace_all(' ', ''), '.
   contents:
   - has_concept("', subject %>% str_replace_all(' ', ''), '")')
  }
  
  writeLines(ymlText, 'pkgdown/_pkgdown.yml')
  
}