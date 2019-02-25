# SEE https://developers.schoology.com/api-documentation/rest-api-v1/building FOR MORE DETAILS.

# This function creates a school object which can be used to create or update a school.
createBuildingObject = function(title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){
     
     buildingObject = as.list(environment())
     buildingObject = buildingObject[as.character(buildingObject) != 'NULL']
     
     return(buildingObject)
}

# This function returns a data frame of school buildings.
listBuildings = function(schoolId){
     
     endpoint = paste0('schools/', schoolId, '/buildings')
     
     resource = getObject(endpoint)
     
     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource = fromJSON(toJSON(resource$building), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

# This function returns details about a building.
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

# This function modifies one or more attributes of a building.
updateBuilding = function(buildingId, object = createBuildingObject()){
     
     endpoint = paste0('schools/', buildingId)
     
     response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# Creates a building in a school.
createBuilding = function(schoolId, object = createBuildingObject()){
     
     object = object[as.character(object) != 'NULL']
    
     if(length(object) == 0){
          return('ERROR: No changes requested.')
     }
     
     endpoint = paste0('schools/', schoolId, '/buildings/')
    
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function deletes a building.
deleteBuilding = function(buildingId){
     
     endpoint = paste0('schools/', buildingId)
     
     buildingName = viewBuilding(buildingId)$title
     
     userResponse = readline(prompt = paste0("Are you SURE you want to delete ", buildingName, "? This cannot be undone! Type 'Y' to delete, anything else not to delete."))
     
     if(userResponse == 'Y'){
          response = deleteObject(endpoint)
     }else{
          return("Building deletion canceled.")
     }
     
     return(response)
}
