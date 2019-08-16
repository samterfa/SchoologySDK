source('R/AuthenticationFunctions.R')

#' List Courses for a School
#' 
#' This function returns a list of courses for a school past and present.
#' 
#' A course is a course meeting. 
#' @param include_past Whether to return past courses or only current courses.
#' @concept Courses
#' @return A dataframe of course details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/courses}{API Documentation}
#' @export
# This function returns a data frame of courses.
listCourses <- function(building_id = NULL){
  
  params = as.list(environment())
  params <- as.list(unlist(params))
  
  endpoint = paste0('/courses')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 1
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$course), flatten = TRUE)
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}


#' Get Course Details
#' 
#' This function returns details about a Schoology course.
#' 
#' @param courseId Can be found by navigating to the Schoology course's information page.
#' @concept Courses
#' @return A dataframe of course details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/courses}{API Documentation}
#' @export
viewCourse = function(courseId){
  
  endpoint = paste0('courses/', courseId)
  
  resource = makeRequest(endpoint, verb = 'GET')
  
  # ... Return resource.
  resource = fromJSON(toJSON(resource), flatten = TRUE)
  resource = characterizeDataFrame(resource)
  
  return(resource)
}


#' Update Course Details
#' 
#' This function modifies one or more attributes of a course.
#' 
#' @param courseId Can be found by navigating to the Schoology course information page.
#' @param course_title The course title (e.g. Course 10b).
#' @param course_code The course code must be unique across the course and grading period (e.g. "Spring 2010" can only have one "10b" for course "ENG101").
#' @param course_school_code The course school code must be unique across the school.
#' @param grading_periods The grading period IDs with which this course is associated.
#' @param description The course description.
#' @param location The location of the course course.
#' @param meeting_days Days of the week that this course course meets. Each day is represented by a number (Sun is 0, Sat is 7).
#' @param start_time The time this course starts on the specified meeting days, from 00:00 to 23:59 (local time). Not applicable if your school uses class period blocks (i.e. courses meet at different times on different days).
#' @param end_time The time this course ends on the specified meeting days.
#' @param class_periods The class period IDs with which this course is associated. Not applicable if your school does not use class period blocks (see start_time).
#' @param synced Whether or not this course course was synced with an external system (eg, Student Information System). The default value is 0. For synced course courses, the Course School Code field is not editable through Schoology.
#' @concept Courses
#' @return A dataframe of updated course details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/courses}{API Documentation}
#' @export
updateCourse = function(courseId, course_title = NULL, course_code = NULL, course_school_code = NULL, access_code = NULL, grading_periods = NULL, description = NULL, profile_url = NULL, location = NULL, meeting_days = NULL, start_time = NULL, end_time = NULL, class_periods = NULL, synced = NULL){
  
  params = as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('courses/', courseId)
  
  #  response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
  response <- makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  return(response)
  
  # If there's no error...
  if(substr(response$status_code, 1, 1) == '2'){
    # ... Return updated course info.
    response2 <- viewCourse(courseId)
    
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