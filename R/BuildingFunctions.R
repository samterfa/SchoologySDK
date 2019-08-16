source('R/AuthenticationFunctions.R')

#' List Buildings for School
#' 
#' This function lists all buildings for a school district.
#' 
#' A building is like a campus of a school district. 
#' @param schoolId The ID of the school (district) for which to list buildings.
#' @concept Buildings
#' @return A dataframe of building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
listBuildings = function(schoolId){
  
  params <- list()
  
  endpoint = paste0('schools/', schoolId, '/buildings')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 1
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$building), flatten = TRUE)
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}

#' Get Building Details
#' 
#' This function returns details about a building: a campus of a school district.
#' 
#' @param buildingId Can be found by navigating to the Schoology building information page.
#' @concept Buildings
#' @return A dataframe of building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
viewBuilding = function(buildingId){
  
  endpoint = paste0('schools/', buildingId)
  
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

#' Update Building Details
#' 
#' This function modifies one or more attributes of a school building.
#' 
#' @param buildingId Can be found by navigating to the Schoology building information page.
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation} for a description 
#' of each parameter.
#' @concept Buildings
#' @return A dataframe of updated building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
updateBuilding = function(buildingId, title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
  
  params <- as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('schools/', buildingId)
  
  response = makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  if(is.null(response)){
    
    return(viewBuilding(buildingId))
    
  }else{
    
    return(response)
    
  }
}

#' Create a School Building
#' 
#' This function creates a new school building.
#' 
#' @param schoolId The ID of the school to which the building will be added.
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation} for a description 
#' of each parameter.
#' @concept Buildings
#' @return A dataframe of details for the newly created building.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
# Creates a building in a school.
createBuilding = function(schoolId, title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
 
    params <- as.list(environment())[-1]
    params <- as.list(unlist(params))
  
     if(length(params) == 0){
          stop('ERROR: No changes requested.')
     }
     
     endpoint = paste0('schools/', schoolId, '/buildings/')
    
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

#' Deletes a school building.
#' 
#' @param buildingId The ID of the building which will be deleted.
#' @concept Buildings
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
deleteBuilding = function(buildingId){
     
     endpoint = paste0('schools/', buildingId)
     
     buildingName = viewBuilding(buildingId)$title
     
     userResponse = readline(prompt = paste0("Are you SURE you want to delete ", buildingName, "? This cannot be undone! \nType '", buildingName, "' to delete, anything else not to delete."))
     
     if(userResponse == buildingName){
          response = makeRequest(endpoint, verb = 'DELETE')
          return(paste(buildingName, 'successfully deleted.'))
     }else{
          return("Building deletion canceled.")
     }
}
