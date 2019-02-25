baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
authVersion <- '1.0' ## Current Oauth version used.

require(httr)
require(jsonlite)
require(dplyr)

getObject <- function(endpointWithQuery, consumerKey, consumerSecret, token = '', tokenSecret = ''){
  
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
      params <- append(authList, queries)
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
    stop(content(response))
  }
}

updateObject <- function(endpointWithQuery, payload, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
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