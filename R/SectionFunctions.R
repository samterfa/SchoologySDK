source('R/AuthenticationFunctions.R')

#' List Sections for a Course
#' 
#' This function returns a list of sections for a course past and present.
#' 
#' A section is a course meeting. 
#' @param include_past Whether to return past sections or only current sections.
#' @concept Sections
#' @return A dataframe of section details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation}
#' @export
# This function returns a data frame of sections.
listSections <- function(course_id, include_past = 0){
  
  params = as.list(environment())[-1]

  endpoint = paste0('/courses/', course_id, '/sections')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 0
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$section), flatten = TRUE)
    if(length(newData) == 0) break()
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}


#' Get Section Details
#' 
#' This function returns details about a Schoology section.
#' 
#' @param sectionId Can be found by navigating to the Schoology section's information page.
#' @concept Sections
#' @return A dataframe of section details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation}
#' @export
viewSection = function(sectionId){
  
  endpoint = paste0('sections/', sectionId)

  resource = makeRequest(endpoint, verb = 'GET')
  
  # ... Return resource.
  resource = fromJSON(toJSON(resource), flatten = TRUE)
  resource = characterizeDataFrame(resource)
  
  return(resource)
}


#' Update Section Details
#' 
#' This function modifies one or more attributes of a section.
#' 
#' @param sectionId Can be found by navigating to the Schoology section information page.
#' @param section_title The section title (e.g. Section 10b).
#' @param section_code The section code must be unique across the course and grading period (e.g. "Spring 2010" can only have one "10b" for course "ENG101").
#' @param section_school_code The section school code must be unique across the school.
#' @param grading_periods The grading period IDs with which this course is associated.
#' @param description The section description.
#' @param location The location of the course section.
#' @param meeting_days Days of the week that this course section meets. Each day is represented by a number (Sun is 0, Sat is 7).
#' @param start_time The time this section starts on the specified meeting days, from 00:00 to 23:59 (local time). Not applicable if your school uses class period blocks (i.e. sections meet at different times on different days).
#' @param end_time The time this section ends on the specified meeting days.
#' @param class_periods The class period IDs with which this course is associated. Not applicable if your school does not use class period blocks (see start_time).
#' @param synced Whether or not this course section was synced with an external system (eg, Student Information System). The default value is 0. For synced course sections, the Section School Code field is not editable through Schoology.
#' @concept Sections
#' @return A dataframe of updated section details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-section}{API Documentation}
#' @export
updateSection = function(sectionId, section_title = NULL, section_code = NULL, section_school_code = NULL, access_code = NULL, grading_periods = NULL, description = NULL, profile_url = NULL, location = NULL, meeting_days = NULL, start_time = NULL, end_time = NULL, class_periods = NULL, synced = NULL){
  
  params = as.list(environment())[-1]
  params <- params[!is.null(params)]
 
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('sections/', sectionId)
  
  response <- makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  return(response)
}

#' Deletes a school section.
#' 
#' @param sectionId The ID of the section which will be deleted.
#' @concept Sections
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/section}{API Documentation}
#' @export
deleteSection = function(sectionId){
  
  endpoint = paste0('schools/', sectionId)
  
  section = viewSection(sectionId)
  
  sectionName <- paste(section$course_title[[1]], section$section_title[[1]])
  
  userResponse = readline(prompt = paste0("Are you SURE you want to delete ", sectionName, "? This cannot be undone! \nType '", sectionName, "' to delete, anything else not to delete."))
  
  if(userResponse == sectionName){
    response = makeRequest(endpoint, verb = 'DELETE')
    return(paste(sectionName, 'successfully deleted.'))
  }else{
    return("Section deletion canceled.")
  }
}
