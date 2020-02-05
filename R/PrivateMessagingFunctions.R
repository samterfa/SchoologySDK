
source('R/AuthenticationFunctions.R')

#' List Messages for School
#' 
#' This function lists all messages for a school district.
#' 
#' A message is like a campus of a school district. 
#' @param schoolId The ID of the school (district) for which to list messages.
#' @concept Messages
#' @return A dataframe of message details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation}
#' @export
listMessages = function(uid, messageFolder = 'inbox'){
  
  params <- list()
  
  endpoint = paste0('/messages/', messageFolder)
 
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 1
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$message), flatten = TRUE)
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}

#' Get Message Details
#' 
#' This function returns details about a message: a campus of a school district.
#' 
#' @param messageId Can be found by navigating to the Schoology message information page.
#' @concept Messages
#' @return A dataframe of message details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation}
#' @export
viewMessage = function(messageId, messageFolder){
  
  endpoint = paste0('messages/', messageFolder, '/', messageId)
  
  resource = makeRequest(endpoint, verb = 'GET')
  
  # If there's no error...
  if(!exists('status_code', where = resource)){
    # ... Return resource.
    resource = fromJSON(toJSON(resource$message), flatten = TRUE)
    resource = characterizeDataFrame(resource)
    return(resource)
  }else{
    # Otherwise return server response if there's an error.
    return(resource)
  }
}


'/analytics/users/%uid'
viewHistory = function(uid, startTime, endTime){
  
  endpoint = paste0('/analytics/users/', uid, '?start_time=', startTime, '&endTime=', endTime)
  print(endpoint)
  resource = makeRequest(endpoint, verb = 'GET')
  
  # If there's no error...
  if(!exists('status_code', where = resource)){
    # ... Return resource.
    resource = fromJSON(toJSON(resource), flatten = TRUE)
    resource = characterizeDataFrame(resource)
    return(resource)
  }else{
    # Otherwise return server response if there's an error.
    return(resource)
  }
}

#' Update Message Details
#' 
#' This function modifies one or more attributes of a message.
#' 
#' @param messageId Can be found by navigating to the Schoology message information page.
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation} for a description 
#' of each parameter.
#' @concept Messages
#' @return A dataframe of updated message details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation}
#' @export
updateMessage = function(messageId, title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
  
  params <- as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('messages/', messageId)
  
  response = makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  if(is.null(response)){
    
    return(viewMessage(messageId, messageFolder = 'inbox'))
    
  }else{
    
    return(response)
    
  }
}

#' Create a School Message
#' 
#' This function creates a new school message.
#' 
#' @param schoolId The ID of the school to which the message will be added.
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation} for a description 
#' of each parameter.
#' @concept Messages
#' @return A dataframe of details for the newly created message.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation}
#' @export
# Creates a message in a school.
createMessage = function(schoolId, title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
  
  params <- as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('messages/', schoolId, '/messages/')
  
  response = makeRequest(endpoint, verb = 'POST', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  # If there's no error...
  if(!exists('status_code', where = response)){
    # ... Return resource.
    resource = fromJSON(toJSON(response), flatten = TRUE)
    resource = characterizeDataFrame(resource)
    return(resource)
  }else{
    # Otherwise return server response if there's an error.
    return(response)
  }
}

#' Deletes a school message.
#' 
#' @param messageId The ID of the message which will be deleted.
#' @concept Messages
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/message}{API Documentation}
#' @export
deleteMessage = function(messageId){
  
  endpoint = paste0('messages/', messageId)
  
  messageName = viewMessage(messageId)$title
  
  userResponse = readline(prompt = paste0("Are you SURE you want to delete ", messageName, "? This cannot be undone! \nType '", messageName, "' to delete, anything else not to delete."))
  
  if(userResponse == messageName){
    response = makeRequest(endpoint, verb = 'DELETE')
    return(paste(messageName, 'successfully deleted.'))
  }else{
    return("Message deletion canceled.")
  }
}
