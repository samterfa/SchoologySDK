source('R/AuthenticationFunctions.R')

#' List Users for a School
#' 
#' This function returns a potentially filtered list of users for a school district or building.
#' 
#' A user is a person associated with a school district. 
#' @param active Whether to return active users only. Defaults to True.
#' @param start Index of first record to return. Min value is 0.
#' @param limit Number of records to return. 
#' Default is 20. Max value is 200.
#' @param building_id Specifies the building from which to return students. Leave NULL to return users from all school buildings.
#' @param role_ids Specifies the roles for which to return students. Leave NULL to return users from of all roles.
#' @param parent_access_codes Specifies whether parent access codes should be returned for each user.
#' @param school_uids Specifies whether school uids should be returned for students.
#' @param extended Whether to return extended details about a user.
#' 
#' @concept Users
#' @return A dataframe of user details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
# This function returns a data frame of users.
listUsers <- function(active = T, building_id = NULL, role_ids = NULL, parent_access_codes = NULL, school_uids = NULL, extended = NULL){
  
  params = as.list(environment())[-1]
  
  if(active){
    endpoint = paste0('/users')
  }else{
    endpoint = paste0('/users/inactive')
  }
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 0
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$user), flatten = TRUE)
    if(length(newData) == 0) break()
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}

#' Get User Details
#' 
#' This function returns details about a Schoology user.
#' 
#' @param userId Can be found by navigating to the Schoology user's information page.
#' @param active Whether to return active users only. Defaults to true.
#' @param extended Whether to return extended details about a user. Defaults to false.
#' @concept Users
#' @return A dataframe of user details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
viewUser = function(userId, active = T, extended = 0){
  
  if(active){
    endpoint = paste0('users/', userId)
  }else{
    endpoint = paste0('users/inactive/', userId)
  }
  
  if(!extended){
    resource = makeRequest(endpoint, verb = 'GET')
  }else{
    params = as.list(environment())['extended']
    resource = makeRequest(endpoint, verb = 'GET', paramsList = params)
  }
  
  # ... Return resource.
  resource = fromJSON(toJSON(resource), flatten = TRUE)
  resource = characterizeDataFrame(resource)
  
  return(resource)
}

#' Update User Details
#' 
#' This function modifies one or more attributes of a user.
#' 
#' @param userId Can be found by navigating to the Schoology user information page.
#' @param object Must be created via createUserObject().
#' @concept Users
#' @return A dataframe of updated user details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
updateUser = function(userId, school_id = NULL, building_id = NULL, school_uid = NULL, name_title = NULL, name_title_show = NULL, name_first = NULL, name_first_preferred = NULL, name_middle = NULL, name_middle_show = NULL, name_last = NULL, name_display = NULL, username = NULL, primary_email = NULL, position = NULL, gender = NULL, grad_year = NULL, birthday_date = NULL, password = NULL, role_id = NULL, email_login_info = NULL, profiel_url = NULL, tz_name = NULL, parents = NULL, parent_uids = NULL, advisor_uids = NULL, child_uids = NULL, send_message = NULL, synced = NULL, profile_picture_fid = NULL, additional_buildings = NULL){
  
  params <- as.list(environment())[-1]
  params <- params[!is.null(params)]
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('users/', userId)
  
  response = makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  if(is.null(response)){
    
    return(viewUser(userId))
    
  }else{
    
    return(response)
    
  }
}

#' Update Multiple Users
#' 
#' This function modifies one or more attributes of up to 50 users at once.
#' 
#' @param userObjects A list of userObjects. These must be created via createUserObject().
#' @concept Users
#' @return A dataframe of updated user details for each user modified.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
updateUsers = function(userId_list, school_id_list = NULL, building_id_list = NULL, school_uid_list = NULL, name_title_list = NULL, name_title_show_list = NULL, name_first_list = NULL, name_first_preferred_list = NULL, name_middle_list = NULL, name_middle_show_list = NULL, name_last_list = NULL, name_display_list = NULL, username_list = NULL, primary_email_list = NULL, position_list = NULL, gender_list = NULL, grad_year_list = NULL, birthday_date_list = NULL, password_list = NULL, role_id_list = NULL, email_login_info_list = NULL, profiel_url_list = NULL, tz_name_list = NULL, parents_list = NULL, parent_uids_list = NULL, advisor_uids_list = NULL, child_uids_list = NULL, send_message_list = NULL, synced_list = NULL, profile_picture_fid_list = NULL, additional_buildings_list = NULL){
     
     # indicesToRemove = integer()
     # for(i in 1:length(userObjects)){
     #      if(length(userObjects[[i]]) == 0){
     #           print(paste('No User Information found. Skipping userObjects index', i))
     #           indicesToRemove[[length(indicesToRemove) + 1]] <- i
     #      }
     # }
     # 
     # if(length(indicesToRemove) > 0){
     #      userObjects = userObjects[-indicesToRemove]
     # }
     
    for(i in 1:length(userId_list))  
  
     userObjects = paste0('{"users":{"user":', toJSON(userObjects), '}}')

     endpoint = 'users/'
     
     response = makeRequest(endpoint, payload = fromJSON(userObjects))
     
     # If there's no error...
     if(substr(response$status_code, 1, 1) == '2'){
       
       # ... Return updated user info.
       resource <- content(response)
       resource = fromJSON(toJSON(resource), flatten = TRUE)
       resource = characterizeDataFrame(resource)
       schoolUIDs <- resource$user.school_uid
       
       response2 <- listUsers(school_uids = paste(schoolUIDs, collapse = ','))
       
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



#' Create a New User
#' 
#' This function creates a new Schoology user.
#' 
#' @param object Must be created via createUserObject().
#' @concept Users
#' @return A dataframe of details for the newly created user.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
createUser = function(object = createUserObject()){
  
  if(length(object) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = 'users/'
  
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

#' Create Multiple Users
#' 
#' This function creates one or more attributes of up to 50 users at once.
#' 
#' @param userObjects A list of userObjects. These must be created via createUserObject().
#' @param update_existing 
#' @concept Users
#' @return A dataframe of updated user details for each user created.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
createUsers = function(userObjects = list(createUserObject()), update_existing = F, ignore_email_conflicts = F, email_conflict_resolution = T){
  
  params = as.list(environment())[-1]
  params <- lapply(params, function(x) ifelse(x, 1, 0))
 
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
  endpoint = addParameters(endpoint, params)
  
  response = createObject(endpoint, fromJSON(userObjects))
  
  # If there's no error...
  if(!exists('status_code', where = response)){
    # ... Return resource.
    resource = fromJSON(toJSON(response), flatten = TRUE)
    resource = characterizeDataFrame(resource)
    schoolUIDs <- resource$user.school_uid
    
    response2 <- listUsers(school_uids = paste(schoolUIDs, collapse = ','))
    
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
  
  return(response)
}

#' Deletes a User.
#' 
#' @param userId The ID of the user which will be deleted.
#' @concept Users
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
#' @export
deleteUser = function(userId, option_comment = '', option_keep_enrollments = 1, email_notification = 0){
  
  params = as.list(environment())[-1]
  
  endpoint = paste0('users/', userId)
  
  resource = getObject(addParameters(endpoint, params))
  
  userName = viewUser(userId)$display_name
  
  userResponse = readline(prompt = paste0("Are you SURE you want to delete ", userName, "? This cannot be undone! \nPress enter to continue."))
  
  if(userResponse == ''){
    
    response = deleteObject(resource)
    return(paste(userName, 'successfully deleted.'))
  }else{
    return("User deletion canceled.")
  }
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