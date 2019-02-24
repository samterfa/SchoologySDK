# SEE https://developers.schoology.com/api-documentation/rest-api-v1/user FOR MORE DETAILS.

require(jsonlite)
source('R/Helper Functions.R')

# This function creates a User object. NOTE: Documentation is incorrect. id NOT school_uid is required for bulk updating users.
createUserObject = function(id = NULL, school_id = NULL, building_id = NULL, school_uid = NULL, name_title = NULL, name_title_show = NULL, name_first = NULL, name_first_preferred = NULL, name_middle = NULL, name_middle_show = NULL, name_last = NULL, name_display = NULL, username = NULL, primary_email = NULL, position = NULL, gender = NULL, grad_year = NULL, birthday_date = NULL, password = NULL, role_id = NULL, email_login_info = NULL, profiel_url = NULL, tz_name = NULL, parents = NULL, parent_uids = NULL, advisor_uids = NULL, child_uids = NULL, send_message = NULL, synced = NULL, profile_picture_fid = NULL, additional_buildings = NULL){
     
     userObject = as.list(environment())
     userObject = userObject[as.character(userObject) != 'NULL']
     
     return(userObject)
}

# This function creates a user.
createUser = function(object = createUserObject()){
     
     object = object[as.character(object) != 'NULL']
     
     endpoint = 'users/'
     
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function creates up to 50 users at once.
createUsers = function(userObjects = list(createUserObject()), update_existing = 0, ignore_email_conflicts = 0, email_conflict_resolution = 1){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     params = as.list(environment())[-1]
     
     indicesToRemove = integer()
     for(i in 1:length(userObjects)){
          if(length(userObjects[[i]]) == 0){
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
    
     if(length(indicesToRemove) > 0){
          userObjects = userObjects[-indicesToRemove]
     }
     
     userObjects = paste0('{"users":{"user":', toJSON(userObjects), '}}')
    
     endpoint = 'users/'
     
     resource = addParameters(endpoint, params)
   
     response = createObject(resource, fromJSON(userObjects))
     
     return(response)
}


# This function returns a data frame of users.
listUsers = function(active = TRUE, start = 0, limit = 200, building_id = NULL, role_ids = NULL, parent_access_codes = NULL, school_uids = NULL, extended = NULL){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     params = as.list(environment())[-1]
     
     if(active){
          endpoint = paste0('/users')
     }else{
          endpoint = paste0('/users/inactive')
     }
     
     resource = getObject(addParameters(endpoint, params))
     
     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource = fromJSON(toJSON(resource$user), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

# This function returns details about a user.
viewUser = function(userId, active = TRUE, extended = FALSE){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     if(active){
          endpoint = paste0('users/', userId)
     }else{
          endpoint = paste0('users/inactive/', userId)
     }
     
     if(!extended){
          resource = getObject(endpoint, consumerKey = appConsumerKey, consumerSecret = appConsumerSecret, token = tempAccessToken$oauth_token, tokenSecret = tempAccessToken$oauth_token_secret)
     }else{
          params = as.list(environment())['extended']
          resource = getObject(addParameters(endpoint, params))
     }
     
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

# This function modifies one or more attributes of a user.
updateUser = function(userObject = createUserObject()){
    
     require(jsonlite)
     source('R/Helper Functions.R')
     
     endpoint = paste0('users/', id)
     
     response = updateObject(endpoint, fromJSON(toJSON(userObject, pretty = TRUE)))
     
     return(response)
}

# This function modifies one or more attributes of up to 50 users at a time.
updateUsers = function(userObjects = list(createUserObject())){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     indicesToRemove = integer()
     for(i in 1:length(userObjects)){
          if(length(userObjects[[i]]) == 0){
               print(paste('No User Information found. Skipping userObjects index', i))
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
     
     if(length(indicesToRemove) > 0){
          userObjects = userObjects[-indicesToRemove]
     }
     
     userObjects = paste0('{"users":{"user":', toJSON(userObjects), '}}')
     print(userObjects)
     endpoint = 'users/'
     
     response = updateObject(endpoint, fromJSON(userObjects))
     
     return(response)
}

# This function deletes a user.
deleteUser = function(userId, option_comment = '', option_keep_enrollments = 1, email_notification = 0){
     
     endpoint = paste0('users/', userId)
     
     params = as.list(environment())[-1]
     
     resource = getObject(addParameters(endpoint, params))
     
     response = deleteObject(resource)
     
     return(response)
}

# This function deletes up to 50 users at once.
deleteUsers = function(ids = character(), option_comment = '', option_keep_enrollments = 1, email_notification = 0){
     
     endpoint = 'users/'
     params = as.list(environment())
     params = lapply(params, function(x) paste(x, collapse = ','))
     resource = addParameters(endpoint, params)
     
     response = deleteObject(resource)
     
     return(response)
}

# This functionc creates an association object for use in creating and deleting associations between students and adults.
createAssociationObject = function(student_school_uid = NULL, adult_school_uid = NULL, delete = 0){
     
     associationObject = as.list(environment())
     associationObject = associationObject[as.character(associationObject) != 'NULL']
     
     return(associationObject)
}

# This function creates parent-child or advisor-advisee associations.
createAssociations = function(associationObjects = list(createAssociationObject()), adultType = c('advisor', 'parent')){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     indicesToRemove = integer()
     for(i in 1:length(associationObjects)){
          if(length(associationObjects[[i]]) == 0){
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
     
     if(length(indicesToRemove) > 0){
          associationObjects = associationObjects[-indicesToRemove]
     }
     
     associationObjects = paste0('{"associations":{"association":', toJSON(associationObjects), '}}')
     associationObjects = gsub('adult', adultType, associationObjects)
     
     endpoint = paste0('users/import/associations/', adultType, 's')
    
     response = createObject(endpoint, fromJSON(associationObjects))
     
     return(response)
}

# This function returns the current API user.
viewApiUser = function(currentAccessToken, currentAccessTokenSecret, currentConsumerKey, currentConsumerSecret){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     endpoint = 'users/me'  
     resource = getObject(endpoint, consumerKey = currentConsumerKey, consumerSecret = currentConsumerSecret, token = currentAccessToken, tokenSecret = currentAccessTokenSecret)
  
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

# This function returns the current App user.
viewAppUserInfo = function(currentAccessToken, currentAccessTokenSecret, currentConsumerKey, currentConsumerSecret){
     
     require(jsonlite)
     
     endpoint = 'app-user-info'   
     resource = getObject(endpoint, consumerKey = currentConsumerKey, consumerSecret = currentConsumerSecret, token = currentAccessToken, tokenSecret = currentAccessTokenSecret)
     
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

# This function returns all interfaces languages.
listLanguages = function(){
     
     require(jsonlite)
     source('R/Helper Functions.R')
     
     endpoint = 'users/languages'
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