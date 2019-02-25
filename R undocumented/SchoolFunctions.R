"# This function creates a school object which can be used to create or update a school.
createSchoolObject = function(title = NULL, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){

     schoolObject = as.list(environment())[-1]
     schoolObject = schoolObject[as.character(schoolObject) != 'NULL']

}
"


#' List Schools for District.
#' 
#' \code{sum} lists all schools for a district.
#' 
#' A "school" is a district in Schoology so this will simply
#'    return details about the school district. 
#' @section References
#' 
#' \url{https://developers.schoology.com/api-documentation/rest-api-v1/school}
listSchools = function(start = 0, limit = 200){

     params = as.list(environment())

     endpoint = 'schools'

     resource = getObject(addParameters(endpoint, params))

     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource = fromJSON(toJSON(resource$school), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

"
# This function returns details about a school.
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

# This function modifies one or more attributes of a school.
updateSchool = function(id, object = createSchoolObject()){

     if(length(object) == 0){
          return('ERROR: No changes requested.')
     }

     endpoint = paste0('schools/', id)

     response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))

     return(response)
}

# Returns 403 error.
createSchool = function(title, address1 = NULL, address2 = NULL, city = NULL, state = NULL, postal_code = NULL, country = NULL, website = NULL, phone = NULL, fax = NULL, picture_url = NULL){

     newObject = as.list(environment())
     newObject = newObject[as.character(newObject) != 'NULL']

     endpoint = 'schools/'

     response = createObject(endpoint, fromJSON(toJSON(newObject, pretty = TRUE)))

     return(response)
}

# Never tested. Afraid to test! # This function deletes a school.
deleteSchool = function(id){

     endpoint = paste0('schools/', id)

     schools = listSchools();
     school = schools$title[which(schools$id == id)]

     userResponse = readline(prompt = paste0('Are you SURE you want to delete ', school, '? This cannot be undone! Type Y to delete, anything else not to delete.'))

     if(userResponse == 'Y'){
          response = deleteObject(endpoint)
     }else{
          return('School deletion canceled.')
     }

     return(response)
}
"