source('R/AuthenticationFunctions.R')

#' Create School Object
#' 
#' This function creates a schoology school (district) object.
#' 
#' A school object is necessary for creation and modification of a Schoology school (district). 
#' @param title,address1,address2,city,state,postal_code,country,website,phone,fax,picture_url
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation} for a description 
#' of each parameter.
#' @concept Schools
#' @return A named list of school attributes.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation}
#' @export
createSchoolObject = function(title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){

     schoolObject = schoolObject[as.character(schoolObject) != 'NULL']

     return(schoolObject)
}

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

     resource = getObject(endpoint)

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

#' Update School Details
#' 
#' This function modifies one or more attributes of a school (district).
#' 
#' @param schoolId Can be found by navigating to the Schoology district information page.
#' @param object Must be created via createSchoolObject().
#' @concept Schools
#' @return A dataframe of updated school details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/school}{API Documentation}
#' @export
updateSchool = function(schoolId, object = createSchoolObject()){

     if(length(object) == 0){
          stop('ERROR: No changes requested.')
     }

     endpoint = paste0('schools/', schoolId)

     response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     # If there's no error...
     if(response$status_code > 200){
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

# This function creates a school. I am not able to test. Returns 403 error.
createSchool = function(object = createSchoolObject()){

     newObject = as.list(environment())
     newObject = newObject[as.character(newObject) != 'NULL']

     endpoint = 'schools/'

     response = createObject(endpoint, fromJSON(toJSON(newObject, pretty = TRUE)))

     return(response)
}

# This function deletes a school. I have never tested this. Use at your own risk!
deleteSchool = function(schoolId){

     endpoint = paste0('schools/', schoolId)

     schools = listSchools();
     school = schools$title[which(schools$id == schoolId)]

     userResponse = readline(prompt = paste0('Are you SURE you want to delete ', school, '? This cannot be undone! Type Y to delete, anything else not to delete.'))

     if(tolower(userResponse) == 'y'){
          response = deleteObject(endpoint)
     }else{
          return('School deletion canceled.')
     }

     return(response)
}