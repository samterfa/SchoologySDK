source('R/AuthenticationFunctions.R')

#' List Enrollments for a Course
#' 
#' This function returns a list of enrollments for a course past and present.
#' 
#' A enrollment is specific user assigned to the realm object. 
#' @param realm One of 'schools', 'buildings', 'users', 'groups', 'courses', or 'sections'.
#' @param realm_id The id of the realm object.
#' @param uid Filter enrollments for a given user.
#' @param enrollment_status Filter enrollments for a given enrollment status 1-5.
#' @param type Filter enrollments by an enrollment type. Possible values: 'admin' or 'member'.
#' @param picture_size Specify size of profile picture returned with this user. Possible values: 'big', 'reg', 'sm', or 'tiny'.
#' @concept Enrollments
#' @return A dataframe of enrollment details.
#' @enrollment References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-enrollment}{API Documentation}
#' @export
# This function returns a data frame of enrollments.
listEnrollments <- function(realm, realm_id, uid = NULL, enrollment_status = NULL, type = NULL, picture_size = NULL){
  
  params <- as.list(environment())[-(1:2)]
  
  endpoint <- paste0(realm, '/', realm_id, '/enrollments')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 0
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$enrollment), flatten = TRUE)
    if(length(newData) == 0) break()
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}


#' Get Enrollment Details
#' 
#' This function returns details about a Schoology enrollment.
#' 
#' @param realm One of 'schools', 'buildings', 'users', 'groups', 'courses', or 'sections'.
#' @param realm_id The id of the realm object.
#' @param enrollmentId Can be found by navigating to the Schoology enrollment's information page.
#' @concept Enrollments
#' @return A dataframe of enrollment details.
#' @enrollment References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-enrollment}{API Documentation}
#' @export
viewEnrollment = function(realm, realm_id, enrollmentId){
  
  endpoint = paste0(realm, '/', realm_id, '/enrollments/', enrollmentId)
  
  resource = makeRequest(endpoint, verb = 'GET')
  
  # ... Return resource.
  resource = fromJSON(toJSON(resource), flatten = TRUE)
  resource = characterizeDataFrame(resource)
  
  return(resource)
}


#' Update Enrollment Details
#' 
#' This function modifies one or more attributes of a enrollment.
#' 
#' @param enrollmentId Can be found by navigating to the Schoology enrollment information page.
#' @param enrollment_title The enrollment title (e.g. Enrollment 10b).
#' @param enrollment_code The enrollment code must be unique across the course and grading period (e.g. "Spring 2010" can only have one "10b" for course "ENG101").
#' @param enrollment_school_code The enrollment school code must be unique across the school.
#' @param grading_periods The grading period IDs with which this course is associated.
#' @param description The enrollment description.
#' @param location The location of the course enrollment.
#' @param meeting_days Days of the week that this course enrollment meets. Each day is represented by a number (Sun is 0, Sat is 7).
#' @param start_time The time this enrollment starts on the specified meeting days, from 00:00 to 23:59 (local time). Not applicable if your school uses class period blocks (i.e. enrollments meet at different times on different days).
#' @param end_time The time this enrollment ends on the specified meeting days.
#' @param class_periods The class period IDs with which this course is associated. Not applicable if your school does not use class period blocks (see start_time).
#' @param synced Whether or not this course enrollment was synced with an external system (eg, Student Information System). The default value is 0. For synced course enrollments, the Enrollment School Code field is not editable through Schoology.
#' @concept Enrollments
#' @return A dataframe of updated enrollment details.
#' @enrollment References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-enrollment}{API Documentation}
#' @export
updateEnrollment = function(enrollmentId, enrollment_title = NULL, enrollment_code = NULL, enrollment_school_code = NULL, access_code = NULL, grading_periods = NULL, description = NULL, profile_url = NULL, location = NULL, meeting_days = NULL, start_time = NULL, end_time = NULL, class_periods = NULL, synced = NULL){
  
  stop('Not coded yet.')
  
  params = as.list(environment())[-1]
  params <- params[!is.null(params)]
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('enrollments/', enrollmentId)
  
  response <- makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  return(response)
}