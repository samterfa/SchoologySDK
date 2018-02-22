signatureMethod <- 'PLAINTEXT' ## This is the signature generation method used for Oauth. For https connection, plaintext works fine. See https://developers.schoology.com/api-documentation/authentication for details. 
baseUrl <- 'https://api.schoology.com/v1/' ## API calls are made to urls which start this way.
authVersion <- '1.0' ## Current Oauth version used.
myConsumerKey <- '44da8dffeba4f148bd9a2e9ff6f35b65056e0a579' ## Can be found at http://schoology.yourDomainName.whatever/api
myConsumerSecret <- 'f8ab24b57199616c140530c32a303dae' ## Can be found at http://schoology.yourDomainName.whatever/api

# This function returns a data frame of schools.
listSchools = function(startingRecord = 0, maxRecords = 200){
     
     endpoint = 'schools'

     endpoint = addParameters(endpoint, as.list(environment()))
     resource = getEndpoint(endpoint)
     
     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          return(flattenJsonList(endpoint))
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

viewSchool = function(id){
     
     
     
}

addParameters <- function(endpoint, params){
     
     paramsString = ''
     for(i in 1:length(params)){
          if(paramsString == ''){
               paramsString = '?'
          }
          newString = paste0(names(params)[[i]], '=', params[[i]])
          paramsString = paste0(paramsString, newString)
     }
     return(paste0(endpoint, paramsString))
}

getEndpoint <- function(endpointWithQuery, consumerKey = myConsumerKey, consumerSecret = myConsumerSecret, token = '', tokenSecret = ''){
     
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
    
     response <- GET(url, add_headers(Authorization = authHeader))
     
     # If we receive a 2## response...
     if(round(response$status_code, digits = -2) == 200){
          return(content(response))
     }else{
          return(response)
     }
}

flattenJsonList = function(jsonList){
     
     require(dplyr)
     
     for(item in jsonList){
          if(!exists('df', inherits = FALSE)){
               df <- flatten(data.frame(item))
          }else{
               df <- bind_rows(df, flatten(data.frame(item)))
          }
     }
     return(df)
}