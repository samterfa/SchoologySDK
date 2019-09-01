source('R/AuthenticationFunctions.R')

#' List Roles for District
#' 
#' This function lists all roles for a district.
#' 
#' A "role" is a thing in Schoology.
#' @concept Roles
#' @return A dataframe of role details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/role}{API Documentation}
#' @export
listRoles = function(){
  
  endpoint = 'roles'
  
  resource = makeRequest(endpoint, verb = 'GET')
  
  # If there's no error...
  if(!exists('status_code', where = resource)){
    # ... Return resource.
    resource <- fromJSON(toJSON(resource$role), flatten = TRUE)
    resource <- characterizeDataFrame(resource)
    return(resource)
  }else{
    # Otherwise return server response if there's an error.
    return(resource)
  }
}

#' Get Role Details
#' 
#' This function returns details about a role (district).
#' 
#' @param roleId Can be found by navigating to the Roleogy role (district) information page.
#' @concept Roles
#' @return A dataframe of role (district) details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/role}{API Documentation}
#' @export
viewRole = function(roleId){
  
  endpoint = paste0('roles/', roleId)
  
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