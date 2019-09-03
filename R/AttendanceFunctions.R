source('R/AuthenticationFunctions.R')

attendanceStatuses <- c('present', 'absent', 'late', 'excused')

#' List Attendances for a Course
#' 
#' This function returns a list of attendances for a course past and present.
#' 
#' An attendance entry is a student record of being absent, late or excused late. 
#' @param section_id The id of the section for which the attendance entries are requested.
#' @param start Filter statuses for a given date range, inclusive YYYY-MM-DD.
#' @param end Filter statuses for a given date range, inclusive YYYY-MM-DD.
#' @param enrollment_id Filter statuses for a given enrollment.
#' @concept Attendances
#' @return A dataframe of attendance details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-attendance}{API Documentation}
#' @export
# This function returns a data frame of attendances.
listAttendances <- function(section_id, start = NULL, end = NULL, enrollment_id = NULL){
  
  if((!is.null(start) | !is.null(end)) & !is.null(enrollment_id)) stop('Attendances can be listed with respect to time OR enrollment_id, but not both!')
  
  params = as.list(environment())[-1]
  
  endpoint = paste0('/sections/', section_id, '/attendance')
  
  resource <- makeRequest(endpoint, params, verb = 'GET')

  # If there's no error...
  if(!exists('status_code', where = resource)){
    # ... Return resource.
    
    if(length(resource$date) == 0){
      return(NULL)
    }
    
    attendanceEntries <- NULL
    for(dateIndex in 1:length(resource$date)){
      
      attendancesForDate <- resource$date[[dateIndex]]
      
      for(statusTypeIndex in 1:length(attendancesForDate$statuses$status)){
        
        newAttendanceEntries <- attendancesForDate$statuses$status[[statusTypeIndex]]$attendances$attendance %>% toJSON() %>% fromJSON()
        
        attendanceEntries <- bind_rows(attendanceEntries, newAttendanceEntries %>% mutate(status = attendanceStatuses[status %>% unlist()]))
      }
    }
    
    return(attendanceEntries)
    
  }else{
    # Otherwise return server response if there's an error.
    return(resource)
  }
}

#' Update Attendance Details
#' 
#' This function modifies one or more attributes of a attendance.
#' 
#' @param section_id The id of the section for which the attendance entry will be added.
#' @param enrollment_id The id of the enrollment for the student in the course section.
#' @param date The date of the attendance entry in YYYY-MM-DD format.
#' @param status 1, 2, 3, or 4 corresponding to 'present', 'absent', 'late', or 'excused'.
#' @concept Attendances
#' @return An array of attendance ids and statuses
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/course-attendance}{API Documentation}
#' @export
updateAttendance = function(section_id, enrollment_id, date, status, comment = ''){
  
  payload <- list()
  payload$attendances <- list()
  payload$attendances$attendance <- list()
  payload$attendances$attendance[[1]] <- as.list(environment())[-1]

  endpoint = paste0('sections/', section_id, '/attendance')
  
  resource <- makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(payload, pretty = TRUE)))
  
  # If there's no error...
  if(!exists('status_code', where = resource)){
    # ... Return resource.
    resource = fromJSON(toJSON(resource$attendance), flatten = TRUE)
    resource = characterizeDataFrame(resource)
    return(resource)
  }else{
    return(resource) 
  }
}