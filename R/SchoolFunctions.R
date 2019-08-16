source('R/AuthenticationFunctions.R')

#' List Schools for District
#' 
#' This function lists all schools for a district.
#' 
#' A "school" is a district in Schoology so this will simply
#'    return details about the (one) school district. 
#' @concept Schools
#' @return A dataframe of school details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation}
#' @export
listSchools = function(){

     endpoint = 'schools'

     resource = makeRequest(endpoint, verb = 'GET')

     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource <- fromJSON(toJSON(resource$school), flatten = TRUE)
          resource <- characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

#' Get School Details
#' 
#' This function returns details about a school (district).
#' 
#' @param schoolId Can be found by navigating to the Schoology school (district) information page.
#' @concept Schools
#' @return A dataframe of school (district) details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation}
#' @export
viewSchool = function(schoolId){

     endpoint = paste0('schools/', schoolId)

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

#' Update School Details
#' 
#' This function modifies one or more attributes of a school (district).
#' 
#' @param schoolId Can be found by navigating to the Schoology district information page.
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation} for a description 
#' of each parameter.
#' @concept Schools
#' @return A dataframe of updated school details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation}
#' @export
updateSchool = function(schoolId, title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){

     params = as.list(environment())[-1]
     params <- as.list(unlist(params))
  
     if(length(params) == 0){
          stop('ERROR: No changes requested.')
     }

     endpoint = paste0('schools/', schoolId)

     response = makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
     
     if(is.null(response)){
       
       return(viewSchool(schoolId))
       
     }else{
       
       return(response)
       
     }
}


######################## EXPERIMENTAL!!! ##################################

# This function creates a school. I am not able to test. Returns 403 error.
createSchool = function(title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){

    params = as.list(environment())
    params <- as.list(unlist(params))

    endpoint = 'schools/'

    response = makeRequest(endpoint, verb = 'POST', payload = fromJSON(toJSON(newObject, pretty = TRUE)))

    return(response)
}

# This function deletes a school. I have never tested this. Use at your own risk!
deleteSchool = function(schoolId){

     endpoint = paste0('schools/', schoolId)

     schools = listSchools();
     school = schools$title[which(schools$id == schoolId)]

     userResponse = readline(prompt = paste0('Are you SURE you want to delete ', school, '? This cannot be undone! Type Y to delete, anything else not to delete.'))

     if(tolower(userResponse) == 'y'){
       return("I'm not letting you do that!")
      ######    response = makeRequest(endpoint, verb = 'DELETE')
     }else{
          return('School deletion canceled.')
     }

     return(response)
}