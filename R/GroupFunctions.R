source('R/AuthenticationFunctions.R')

#' List Groups for a School
#' 
#' This function returns a list of groups for a school past and present.
#' 
#' A group is a group meeting. 
#' @param building_id Return only groups for the given building_id.
#' @concept Groups
#' @return A dataframe of group details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/groups}{API Documentation}
#' @export
# This function returns a data frame of groups.
listGroups <- function(building_id = NULL){
  
  params = as.list(environment())
  
  endpoint = paste0('/groups')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 1
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$group), flatten = TRUE)
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}


#' Get Group Details
#' 
#' This function returns details about a Schoology group.
#' 
#' @param groupId Can be found by navigating to the Schoology group's information page.
#' @concept Groups
#' @return A dataframe of group details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/groups}{API Documentation}
#' @export
viewGroup = function(groupId){
  
  endpoint = paste0('groups/', groupId)
  
  resource = makeRequest(endpoint, verb = 'GET')
  
  # ... Return resource.
  resource = fromJSON(toJSON(resource), flatten = TRUE)
  resource = characterizeDataFrame(resource)
  
  return(resource)
}


#' Update Group Details
#' 
#' This function modifies one or more attributes of a group.
#' 
#' @param groupId Can be found by navigating to the Schoology group information page.
#' @param building_id The internal Schoology ID of the school building to which the group belongs.
#' @param school_id The internal Schoology ID of the school to which the group belongs.
#' @param title The title of the group.
#' @param picture_url The URL of the group's profile picture.
#' @param description The group description.
#' @param website The group website.
#' @param access_code The access code that users can use to join the group (only admins can see this value).
#' @param privacy_level The privacy of the group. One of 'everyone', 'school', 'building', or 'group'.
#' @param category The category of the group.
#' @param options_invite_type How members can join the group. 0: Invite only, 1: Request to join, 2: Anyone can join. Default 0.
#' @param options_member_post Whether or not a group member can post a group update. Default 1.
#' @param options_member_post_comment Whether or not a group member can post comments to group udpates. Default 1.
#' @param options_create_discussion Whether or not a group member can create a discussion thread. Default 0.
#' @param options_create_files Whether or not members can create resources for the group. Default 0.
#' @concept Groups
#' @return A dataframe of updated group details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/groups}{API Documentation}
#' @export
updateGroup = function(groupId, building_id = NULL, school_id = NULL, title = NULL, picture_url = NULL, description = NULL, website = NULL, access_code = NULL, privacy_level = NULL, category = NULL, options_invite_type = NULL, options_member_post = NULL, options_member_post_comment = NULL, options_create_discussion = NULL, options_create_files = NULL){
  
  params = as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('groups/', groupId)
  
  response <- makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  if(is.null(response)){
    
    return(viewGroup(groupId))
    
  }else{
    
    return(response)
  }
}

listGroupEnrollmentsViaCSVexport <- function(uid = T, school_uid = T, name_first = T, name_last = T, mail = T, title = T, group_code = T, type = T, status = T){
  
  require(xml2)
  require(tidyverse)
  
  params <- as.list(environment())
  params <- params[unlist(params)]
  
  endpoint <- 'csvexport/group_enrollments'
  
  if(length(params) > 0){
    
    endpoint <- paste0(endpoint, '?fields=')
    
    endpoint <- paste0(endpoint, paste(names(params), collapse = ','))
  }
  
  resource <- makeRequest(endpoint)
  
  users <- xml_text(resource) %>% read_csv()
  
  return(users)
}
