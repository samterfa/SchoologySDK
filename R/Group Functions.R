# SEE https://developers.schoology.com/api-documentation/rest-api-v1/group FOR MORE DETAILS.

# This function creates a User object.
createGroupObject = function(building_id = NULL, school_id = NULL, title = NULL, description = NULL, picture_url = NULL, website = NULL, access_code = NULL, privacy_level = NULL, category = NULL, options__invite_type = NULL, options__member_post = NULL, options__member_post_comment = NULL, options__create_discussion = NULL, options__create_files = NULL, group_code = NULL){
     
     groupObject = as.list(environment())
     groupObject = groupObject[as.character(groupObject) != 'NULL']
     names(groupObject) = gsub('__', '/', names(groupObject))
     
     return(groupObject)
}

# This function creates a group.
createGroup = function(object = createGroupObject()){
     
     object = object[as.character(object) != 'NULL']
     
     endpoint = 'groups/'
     
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function returns a data frame of school groups.
listGroups = function(schoolId){
     
     endpoint = 'groups'
     
     resource = getObject(endpoint)
     
     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource = fromJSON(toJSON(resource$group), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

# This function returns details about a group.
viewGroup = function(groupId){
     
     endpoint = paste0('groups/', groupId)
     
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

# This function modifies one or more attributes of a group.
updateGroup = function(id, object = createGroupObject()){
     
     endpoint = paste0('groups/', id)
     
     response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function deletes a group.
deleteGroup = function(id){
     
     endpoint = paste0('groups/', id)
     
     groups = listGroups();
     groupName = groups$title[which(groups$id == id)]
     
     userResponse = readline(prompt = paste0("Are you SURE you want to delete ", groupName, "? This cannot be undone! Type 'Y' to delete, anything else not to delete."))
     
     if(userResponse == 'Y'){
          response = deleteObject(endpoint)
     }else{
          return("Group deletion canceled.")
     }
     
     return(response)
}

# This function returns all interfaces languages.
listGroupCategories = function(){
     
     require(jsonlite)
     source('~/Google Drive/2017-2018/SchoologyApi/Helper Functions.R')
     
     endpoint = 'groups/categories'
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