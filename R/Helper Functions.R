signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
authVersion <- '1.0' ## Current Oauth version used.
#appConsumerKey <- '266de2520e08b212cbe0a6c9e24b0b9f058274804'
appConsumerKey <- '28aca4dffadb8ef4e19d80ac566d7f5305836499f'
#appConsumerSecret <- '5845ba0845a3a72dc469f865ead8f5e2'
appConsumerSecret <- '0a52488fdf16d6ebd3c38403e2940fd3'
myConsumerKey <- '44da8dffeba4f148bd9a2e9ff6f35b65056e0a579' ## Can be found at http://schoology.yourDomainName.whatever/api
myConsumerSecret <- 'f8ab24b57199616c140530c32a303dae' ## Can be found at http://schoology.yourDomainName.whatever/api
currentToken <- 'fd8a2c443487d6e90f29277b9c7fdc7c05a7b0af9'
currentTokenSecret <- 'c3b5be8e51a875c47f9d1e480ba522cf'
myAppId <- '890866238'

makeOauthSignature <- function(url, method, authHeader, consumerSecret, tokenSecret){
  
  # Separate url and query.
  if(regexpr('?', url, fixed = T) > 0){
    query <- substr(url, regexpr('?', url, fixed = T) + 1, nchar(url))
    queries <- strsplit(query, '&', fixed = T)[[1]]
    url <- substr(url, 1, regexpr('?', url, fixed = T) - 1)
  }
  
  baseString <- paste0(toupper(method), '&', URLencode(tolower(url), reserved = T), '&')
  
  authList <- strsplit(authHeader, ',')[[1]][-1]
  oauth_config <- authList[grep('method', authList)]
  authList <- authList[c(1, 3, 5, 4, 2, 6)]
  oauth_config <- substr(authList[[3]], regexpr('=', authList[[3]]) + 1, nchar(authList[[3]]))
  
  if(exists('queries', inherits = F)){
    authList <- append(authList, queries)
  }else{
    params <- authList
  }
  params <- params[order(params)]
  
  baseString <- paste0(baseString, URLencode(paste(params, collapse = '&'), reserved = T))
  
  oauthString <- paste0(consumerSecret, '&', tokenSecret)
  
  if(oauth_config == 'PLAINTEXT'){
    return(oauthString)
  }
  
  if(oauth_config == 'HMAC-SHA1'){
    signature <- URLencode(hmac_sha1(oauthString, baseString), reserved = F)
    
    return(signature)
  }
  
  stop('Did not recognize oauth_signature_method')
}

getObject <- function(endpointWithQuery, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
  
  require(httr)
  require(jsonlite)
 
  url <- paste0(baseUrl, endpointWithQuery)
  method <- 'GET'
  oauth_config <- 'HMAC-SHA1'
  
  timestamp <-  as.numeric(Sys.time())
  nonce <- timestamp
  
  authHeader <- paste0('OAuth ',
                       'realm=Schoology API',
                       ',oauth_consumer_key=', consumerKey,
                       ',oauth_token=', token,
                       ',oauth_nonce=', nonce,
                       ',oauth_timestamp=', timestamp,
                       ',oauth_signature_method=', oauth_config,
                       ',oauth_version=1.0'
                        )
  
  signature <- makeOauthSignature(url, method, authHeader, consumerSecret, tokenSecret)
  authHeader <- paste0(authHeader, ',oauth_signature=', URLencode(signature, reserved = T))
  
  response <- GET(url, add_headers(Authorization = authHeader))
  
  # If we receive a 2## response...
  if(round(response$status_code, digits = -2) == 200){
    return(content(response))
  }else{
    return(response)
  }
}

updateObject <- function(endpointWithQuery, payload, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
     require(httr)
     require(jsonlite)
     
     url <- paste0(baseUrl, endpointWithQuery)
     
     timestamp <-  as.numeric(Sys.time())
     nonce <- timestamp
     authHeader <- paste0('OAuth ',
                          'realm=Schoology API',
                          ',oauth_consumer_key=', consumerKey,
                          ',oauth_token=', token,
                          ',oauth_nonce=', nonce,
                          ',oauth_timestamp=', timestamp,
                          ',oauth_signature_method=PLAINTEXT',
                          ',oauth_version=1.0',
                          ',oauth_signature=', consumerSecret, '%26', tokenSecret)
     
     response <- PUT(url, add_headers(Authorization = authHeader), body = payload, encode = 'json')
     
     return(response)
}


createObject <- function(endpointWithQuery, payload, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
     require(httr)
     require(jsonlite)
     
     url <- paste0(baseUrl, endpointWithQuery)
     
     timestamp <-  as.numeric(Sys.time())
     nonce <- timestamp
     authHeader <- paste0('OAuth ',
                          'realm=Schoology API',
                          ',oauth_consumer_key=', consumerKey,
                          ',oauth_token=', token,
                          ',oauth_nonce=', nonce,
                          ',oauth_timestamp=', timestamp,
                          ',oauth_signature_method=PLAINTEXT',
                          ',oauth_version=1.0',
                          ',oauth_signature=', consumerSecret, '%26', tokenSecret)
     
     response <- POST(url, add_headers(Authorization = authHeader), body = payload, encode = 'json')
     
     # If we receive a 2## response...
     if(round(response$status_code, digits = -2) == 200){
          return(content(response))
     }else{
          return(response)
     }
}


deleteObject <- function(endpointWithQuery, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
     require(httr)
     require(jsonlite)
     
     url <- paste0(baseUrl, endpointWithQuery)
     
     timestamp <-  as.numeric(Sys.time())
     nonce <- timestamp
     authHeader <- paste0('OAuth ',
                          'realm=Schoology API',
                          ',oauth_consumer_key=', consumerKey,
                          ',oauth_token=', token,
                          ',oauth_nonce=', nonce,
                          ',oauth_timestamp=', timestamp,
                          ',oauth_signature_method=PLAINTEXT',
                          ',oauth_version=1.0',
                          ',oauth_signature=', consumerSecret, '%26', tokenSecret)
     
     response <- DELETE(url, add_headers(Authorization = authHeader), encode = 'json')
     
     # If we receive a 2## response...
     if(round(response$status_code, digits = -2) == 200){
          return(content(response))
     }else{
          return(response)
     }
}

addParameters <- function(endpoint, params){
     
     params = params[as.character(params) != 'NULL']
     
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

flattenJsonList = function(jsonList){
     
     require(dplyr)
          
     for(item in jsonList){
          
          item = data.frame(item)
          
          if(!exists('df', inherits = FALSE)){
               df <- flatten(item)
          }else{
               df <- bind_rows(df, flatten(item))
          }
     }
     return(df)
}



getRequestToken <- function(userId, consumerKey = appConsumerKey, consumerSecret = appConsumerSecret){
     
     require(httr)
     
     require(jsonlite)
     
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

getAuthorization <- function(uid, oauth_token, appId, appUrl, realm, testingPort = NULL){
     
     require(httr)
     
     require(jsonlite)
     
     signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
     
     baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
     
     authVersion <- '1.0' ## Current Oauth version used.
     
     timestamp <-  as.numeric(Sys.time());
     
     nonce <- timestamp
     
     domainBaseUrl <- 'http://schoology.minnehahaacademy.net/'
     
     apiUrl <- 'oauth/authorize'
     
     url <- paste0(domainBaseUrl, apiUrl, '?return_url=', appUrl, '?realm=', realm, '&realm_id=', uid, '&app_id=', appId, '&is_ssl=0', '&oauth_token=', oauth_token)
    
     browseURL(url)
}

getAccessToken <- function(userId,oauth_token, oauth_secret, consumerKey = appConsumerKey, consumerSecret = appConsumerSecret){
     
     require(httr)
     
     require(jsonlite)
     
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

getCurrentUser <- function(){
  
  httr::GET('https://api.schoology.com/v1/users/me')
  
}