source('R/AuthenticationFunctions.R')

#' Create Building Object
#' 
#' This function creates a Schoology building object.
#' 
#' A building object is necessary for creation and modification of a Schoology building. 
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation} for a description 
#' of each parameter.
#' @return A named list of building attributes.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
createBuildingObject = function(title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
  
  buildingObject <- as.list(environment())
  buildingObject <- buildingObject[as.character(buildingObject) != 'NULL']
  
  return(buildingObject)
}

#' List Buildings for School
#' 
#' This function lists all buildings for a school district.
#' 
#' A building is like a campus of a school district. 
#' @param start Index of first record to return. Min value is 0.
#' @param limit Number of records to return. 
#' Default is 20. Max value is 200.
#' @return A dataframe of building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
listBuildings = function(schoolId, start = 0, limit = 200){
  
  params = as.list(environment())
  
  endpoint = paste0('schools/', schoolId, '/buildings')
  
  resource = getObject(addParameters(endpoint, params))
  resource <- fromJSON(toJSON(resource), flatten = TRUE)
  resource <- characterizeDataFrame(resource)
  
  return(resource)
}

#' Get Building Details
#' 
#' This function returns details about a building: a campus of a school district.
#' 
#' @param buildingId Can be found by navigating to the Schoology building information page.
#' @return A dataframe of building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
viewBuilding = function(buildingId){
  
  endpoint = paste0('schools/', buildingId)
  
  resource = getObject(endpoint)
  
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
#' @param object Must be created via createBuildingObject().
#' @return A dataframe of updated building details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
updateBuilding = function(buildingId, object = createBuildingObject()){
  
  if(length(object) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('schools/', buildingId)
  
  response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
  
  # If there's no error...
  if(substr(response$status_code, 1, 1) == '2'){
    # ... Return updated school info.
    response2 <- viewSchool(schoolId)
    
    # If there's no error...
    if(!exists('status_code', where = response2)){
      # ... Return resource.
      
      resource = fromJSON(toJSON(response2), flatten = TRUE)
      resource = characterizeDataFrame(resource)
      return(resource)
    }else{
      # Otherwise return server response if there's an error.
      return(response2)
    }
  }
  else{
    # Otherwise return server response if there's an error.
    return(response)
  }
}

#' Create a School Building
#' 
#' This function creates a new school building.
#' 
#' @param schoolId The ID of the school to which the building will be added.
#' @param object Must be created via createBuildingObject().
#' @return A dataframe of details for the newly created building.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
# Creates a building in a school.
createBuilding = function(schoolId, object = createBuildingObject()){
 
     if(length(object) == 0){
          stop('ERROR: No changes requested.')
     }
     
     endpoint = paste0('schools/', schoolId, '/buildings/')
    
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
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
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/building}{API Documentation}
#' @export
deleteBuilding = function(buildingId){
     
     endpoint = paste0('schools/', buildingId)
     
     buildingName = viewBuilding(buildingId)$title
     
     userResponse = readline(prompt = paste0("Are you SURE you want to delete ", buildingName, "? This cannot be undone! \nType '", buildingName, "' to delete, anything else not to delete."))
     
     if(userResponse == buildingName){
          response = deleteObject(endpoint)
          return(paste(buildingName, 'successfully deleted.'))
     }else{
          return("Building deletion canceled.")
     }
}
