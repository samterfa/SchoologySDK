source('R/AuthenticationFunctions.R')

#' Create Section Object
#' 
#' This function creates a Schoology section object.
#' 
#' A section object is necessary for creation and modification of a Schoology section. 
#' @param id,school_id,building_id,school_uid,name_title,name_title_show,name_first,name_first_preferred,name_middle,name_middle_show,name_last,name_display,sectionname,primary_email,position,gender,grad_year,birthday_date,password,role_id,email_login_info,profiel_url,tz_name,parents,parent_uids,advisor_uids,child_uids,send_message,synced,profile_picture_fid,additional_buildings
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation} for a description 
#' of each parameter.
#' @concept Sections
#' @return A named list of section attributes.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation}
#' 
#' @export
createSectionObject = function(id = NULL, section_title = NULL, section_code = NULL, section_school_code = NULL, access_code = NULL, grading_periods = NULL, description = NULL, profile_url = NULL, location = NULL, meeting_days = NULL, start_time = NULL, end_time = NULL, class_periods = NULL, synced = NULL){
  
  sectionObject = as.list(environment())
  sectionObject = sectionObject[as.character(sectionObject) != 'NULL']
  
  return(sectionObject)
}

#' Update Section Details
#' 
#' This function modifies one or more attributes of a section.
#' 
#' @param sectionId Can be found by navigating to the Schoology section information page.
#' @param object Must be created via createSectionObject().
#' @concept Sections
#' @return A dataframe of updated section details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation}
#' @export
updateSection = function(sectionId, object = createSectionObject()){
  
  if(length(object) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('sections/', sectionId)
  
  response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
  
  # If there's no error...
  if(substr(response$status_code, 1, 1) == '2'){
    # ... Return updated section info.
    response2 <- viewSection(sectionId)
    
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