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


# # This function properly formats parameters for inclusion in a Schoology API request.
# addParametersOLD <- function(endpoint, params){
#   
#   paramsString = ''
#   for(i in 1:length(params)){
#     if(i == 1){
#       paramsString = '?'
#     }else{
#       paramsString = paste0(paramsString, '&')
#     }
#     newString = paste0(names(params)[[i]], '=', params[[i]])
#     paramsString = paste0(paramsString, newString)
#   }
#   
#   return(paste0(endpoint, paramsString))
# }
# 
# # This function creates an oauth_signature for an API call.
# makeOauthSignatureOLD <- function(url, method, authHeader, consumerSecret, tokenSecret){
#   
#   # Separate url and query.
#   if(regexpr('?', url, fixed = T) > 0){
#     query <- substr(url, regexpr('?', url, fixed = T) + 1, nchar(url))
#     queries <- strsplit(query, '&', fixed = T)[[1]]
#     url <- substr(url, 1, regexpr('?', url, fixed = T) - 1)
#   }
#   
#   baseString <- paste0(toupper(method), '&', URLencode(tolower(url), reserved = T), '&')
#  
#   authList <- strsplit(authHeader, ',')[[1]][-1]
#   oauth_config <- authList[grep('method', authList)]
#   authList <- authList[c(1, 3, 5, 4, 2, 6)]
#   oauth_config <- substr(authList[[3]], regexpr('=', authList[[3]]) + 1, nchar(authList[[3]]))
#   
#   if(exists('queries', inherits = F)){
#     params <- append(authList, queries)
#   }else{
#     params <- authList
#   }
#   params <- params[order(params)]
#   
#   baseString <- paste0(baseString, URLencode(paste(params, collapse = '&'), reserved = T))
#   oauthString <- paste0(consumerSecret, '&', tokenSecret)
#   
#   if(oauth_config == 'PLAINTEXT'){
#     return(oauthString)
#   }
#   
#   if(oauth_config == 'HMAC-SHA1'){
#     # Fixes comma-separated values in request.
#     baseString <- sub('%2C', '%252C', baseString, fixed = T)
#     
#     signature <- URLencode(hmac_sha1(oauthString, baseString), reserved = F)
#     return(signature)
#   }
#   
#   stop('Did not recognize oauth_signature_method')
# }

#' Get Schoology Object
#' 
#' This function retrieves a Schoology object using a GET request.
#' 
#' This function is called by any Schoology SDK function requiring a GET request.
#' You will, in general, not need to call this function directly.
#' @param endpoint See \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{API documentation}
#' for a list of endpoints.
#' @param query See \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{API documentation}
#' for a list of query parameters.
#' @section Additional Arguments: These arguments must be set via the "options" function prior to use of this function. e.g. options(consumerKey = "12345")\cr\cr
#' \strong{consumerKey and consumerSecret}\cr\cr For 2-legged authentication (on your behalf), these values can
#' be found at School Management -> Integration.\cr\cr For 3-legged authentication (on behalf of someone else via
#' your application), these values can be found at (yourSchoologyDomain)/apps/publisher.\cr\cr
#' \strong{token and tokenSecret}\cr\cr For 2-legged authentication, these should be left blank.\cr\cr
#' For 3-legged authentication, these are stored values for the current user.\cr\cr
#' See \href{https://developers.schoology.com/api-documentation/authentication}{authentication documentation} for details.
#' @concept Requests
#' @return The content of the GET request response.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
#' @export
getObject <- function(endpoint, query = NULL){
  
  require(httr)
 
  response <- GET(paste0(baseUrl, endpoint), config(token = getToken()), query = query, accept_json())
  
  if(response$status_code < 300){
    return(content(response))
  }else{
    stop(content(response))
  }
}

#' Update Schoology Object
#' 
#' This function updates a Schoology object using a PUT request.
#' 
#' This function is called by any Schoology SDK function requiring a PUT request.
#' You will, in general, not need to call this function directly.
#' @param endpointWithQuery See \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{API documentation}
#' for a list of endpoints.
#' @param payload A json-formatted payload for this object.
#' @section Additional Arguments: These arguments must be set via the "options" function prior to use of this function. e.g. options(consumerKey = "12345")\cr\cr
#' \strong{consumerKey and consumerSecret}\cr\cr For 2-legged authentication (on your behalf), these values can
#' be found at School Management -> Integration.\cr\cr For 3-legged authentication (on behalf of someone else via
#' your application), these values can be found at (yourSchoologyDomain)/apps/publisher.\cr\cr
#' \strong{token and tokenSecret}\cr\cr For 2-legged authentication, these should be left blank.\cr\cr
#' For 3-legged authentication, these are stored values for the current user.\cr\cr
#' See \href{https://developers.schoology.com/api-documentation/authentication}{authentication documentation} for details.
#' @concept Requests
#' @return The PUT request response.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
#' @export
updateObject <- function(endpointWithQuery, payload){
     
  checkAuthentication()
  consumerKey <- getOption('consumerKey')
  consumerSecret <- getOption('consumerSecret')
  token <- getOption('token')
  tokenSecret <- getOption('tokenSecret')
  
  url <- paste0(baseUrl, endpointWithQuery)
  method <- 'PUT'
  
  timestamp <-  as.numeric(Sys.time())
  nonce <- timestamp
  authHeader <- paste0('OAuth ',
                      'realm=Schoology API',
                      ',oauth_consumer_key=', consumerKey,
                      ',oauth_token=', token,
                      ',oauth_nonce=', nonce,
                      ',oauth_timestamp=', timestamp,
                      ',oauth_signature_method=', oauth_config,
                      ',oauth_version=1.0')
  
  signature <- makeOauthSignature(url, method, authHeader, consumerSecret, tokenSecret)
  authHeader <- paste0(authHeader, ',oauth_signature=', URLencode(signature, reserved = T))
  
  response <- PUT(url, add_headers(Authorization = authHeader), body = payload, encode = 'json')
     
  # If we receive a 2## response...
  if(substr(response$status_code, 1, 1) == '2'){
    
    # There's typically no response content for update requests.
    return(response)
  }else{
    stop(content(response))
  }
}

#' Create Schoology Object
#' 
#' This function creates a Schoology object using a POST request.
#' 
#' This function is called by any Schoology SDK function requiring a POST request.
#' You will, in general, not need to call this function directly.
#' @param endpointWithQuery See \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{API documentation}
#' for a list of endpoints.
#' @param payload A json-formatted payload for this object.
#' @section Additional Arguments: These arguments must be set via the "options" function prior to use of this function. e.g. options(consumerKey = "12345")\cr\cr
#' \strong{consumerKey and consumerSecret}\cr\cr For 2-legged authentication (on your behalf), these values can
#' be found at School Management -> Integration.\cr\cr For 3-legged authentication (on behalf of someone else via
#' your application), these values can be found at (yourSchoologyDomain)/apps/publisher.\cr\cr
#' \strong{token and tokenSecret}\cr\cr For 2-legged authentication, these should be left blank.\cr\cr
#' For 3-legged authentication, these are stored values for the current user.\cr\cr
#' See \href{https://developers.schoology.com/api-documentation/authentication}{authentication documentation} for details.
#' @concept Requests
#' @return The newly created object.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
#' @export
createObject <- function(endpointWithQuery, payload){
     
  checkAuthentication()
  consumerKey <- getOption('consumerKey')
  consumerSecret <- getOption('consumerSecret')
  token <- getOption('token')
  tokenSecret <- getOption('tokenSecret')
  
  url <- paste0(baseUrl, endpointWithQuery)
  method <- 'POST'
     
  timestamp <-  as.numeric(Sys.time())
  nonce <- timestamp
  authHeader <- paste0('OAuth ',
                      'realm=Schoology API',
                      ',oauth_consumer_key=', consumerKey,
                      ',oauth_token=', token,
                      ',oauth_nonce=', nonce,
                      ',oauth_timestamp=', timestamp,
                      ',oauth_signature_method=', oauth_config,
                      ',oauth_version=1.0')
     
  signature <- makeOauthSignature(url, method, authHeader, consumerSecret, tokenSecret)
  authHeader <- paste0(authHeader, ',oauth_signature=', URLencode(signature, reserved = T))
     
  response <- POST(url, add_headers(Authorization = authHeader), body = payload, encode = 'json')
     
  # If we receive a 2## response...
  if(substr(response$status_code, 1, 1) == '2'){
    return(content(response))
  }else{
    stop(content(response))
  }
}

#' Delete Schoology Object
#' 
#' This function deletes a Schoology object using a DELETE request.
#' 
#' This function is called by any Schoology SDK function requiring a DELETE request.
#' You will, in general, not need to call this function directly.
#' @param endpointWithQuery See \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{API documentation}
#' for a list of endpoints.
#' @section Additional Arguments: These arguments must be set via the "options" function prior to use of this function. e.g. options(consumerKey = "12345")\cr\cr
#' \strong{consumerKey and consumerSecret}\cr\cr For 2-legged authentication (on your behalf), these values can
#' be found at School Management -> Integration.\cr\cr For 3-legged authentication (on behalf of someone else via
#' your application), these values can be found at (yourSchoologyDomain)/apps/publisher.\cr\cr
#' \strong{token and tokenSecret}\cr\cr For 2-legged authentication, these should be left blank.\cr\cr
#' For 3-legged authentication, these are stored values for the current user.\cr\cr
#' See \href{https://developers.schoology.com/api-documentation/authentication}{authentication documentation} for details.
#' @concept Requests
#' @return The DELETE request response.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
#' @export
deleteObject <- function(endpointWithQuery, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
  checkAuthentication()
  consumerKey <- getOption('consumerKey')
  consumerSecret <- getOption('consumerSecret')
  token <- getOption('token')
  tokenSecret <- getOption('tokenSecret')
  
  url <- paste0(baseUrl, endpointWithQuery)
  method <- 'DELETE'
     
  timestamp <-  as.numeric(Sys.time())
  nonce <- timestamp
  authHeader <- paste0('OAuth ',
                      'realm=Schoology API',
                      ',oauth_consumer_key=', consumerKey,
                      ',oauth_token=', token,
                      ',oauth_nonce=', nonce,
                      ',oauth_timestamp=', timestamp,
                      ',oauth_signature_method=', oauth_config,
                      ',oauth_version=1.0')
  
  signature <- makeOauthSignature(url, method, authHeader, consumerSecret, tokenSecret)
  authHeader <- paste0(authHeader, ',oauth_signature=', URLencode(signature, reserved = T))
  
  response <- DELETE(url, add_headers(Authorization = authHeader), encode = 'json')
     
  # If we receive a 2## response...
  if(substr(response$status_code, 1, 1) == '2'){
    return(response)
  }else{
    stop(content(response))
  }
}

"
getRequestToken <- function(userId, consumerKey, consumerSecret){
     
     signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
     
     baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
     
     apiUrl <- 'oauth/request_token/'
     
     url <- paste0(baseUrl, apiUrl)
     
     authVersion <- '1.0' ## Current Oauth version used.
     
     timestamp <-  as.numeric(Sys.time());
     nonce <- timestamp
     authHeader <- paste0('OAuth ',
                          'realm=Schoology API',
                          ',oauth_consumer_key=', consumerKey,
                          ',oauth_nonce=', nonce,
                          ',oauth_timestamp=', timestamp,
                          ',oauth_signature_method=PLAINTEXT',
                          ',oauth_version=1.0',
                          ',oauth_signature=', consumerSecret, '%26')
     
     response <- GET(url, add_headers(Authorization = authHeader))
     
     content <- content(response)
     
     return(content)
}

getAuthorization <- function(uid, oauth_token, appUrl, realm, domainBaseUrl){
     
     signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
     
     baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
     
     authVersion <- '1.0' ## Current Oauth version used.
     
     timestamp <-  as.numeric(Sys.time());
     
     nonce <- timestamp
     
     apiUrl <- 'oauth/authorize'
     
     url <- paste0(domainBaseUrl, apiUrl, '?return_url=', appUrl, '?realm=', realm, '&realm_id=', uid, '&app_id=', appId, '&is_ssl=0', '&oauth_token=', oauth_token)
    
     browseURL(url)
}

getAccessToken <- function(userId,oauth_token, oauth_secret, consumerKey = appConsumerKey, consumerSecret = appConsumerSecret){
     
     signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
     
     baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
     
     authVersion <- '1.0' ## Current Oauth version used.
     
     timestamp <-  as.numeric(Sys.time());
     nonce <- timestamp
    
     apiUrl <- 'oauth/access_token'
     
     url <- paste(baseUrl, apiUrl, '?oauth_consumer_key=', consumerKey, '&oauth_token=', oauth_token, '&oauth_signature_method=', 'PLAINTEXT', '&oauth_timestamp=', timestamp, '&oauth_nonce=', nonce, '&oauth_version=', authVersion, '&oauth_signature=', consumerSecret, '%26', oauth_secret, sep=''); 
     
     response <- GET(url)
     
     content <- content(response)
     
     return(response)
}
"