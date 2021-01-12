
#' List Grades for Section
#' 
#' Return all grades for all enrollments of a section.
#' @param section_id The id of the course section.
#' @param assignment_id Filter grades for a given assignment
#' @param enrollment_id Filter grades for a given enrollment
#' @param timestamp Return only grades that have been changed since the given timestamp, according to the server time.
#' @return A list of section grades.
#' @concept Grading
#' @export
listGrades <- function(section_id, assignment_id = NULL, enrollment_id = NULL, timestamp = NULL){
  
  endpoint = glue::glue('/sections/{section_id}/grades')
  
  params <- list(start = 0, limit = 200, assignment_id = assignment_id, enrollment_id = enrollment_id, timestamp = timestamp)
  resource <- makeRequest(endpoint, params, verb = 'GET')
  
  return(resource)
}

#' Update Grades for Section
#' 
#' Update grades for a section.
#' @param section_id The id of the course section.
#' @return The updated grades.
#' @concept Grading
#' @export
updateGrades <- function(section_id, grades){
  
  endpoint = glue::glue('/sections/{section_id}/grades')
  
  body <- list(grades = grades)
  resource <- makeRequest(endpoint, verb = 'PUT', payload = body)
  
  return(resource)
}



#' Update Final Comments
#' 
#' Set/modify comments for students for a grading period and the overall course.
#' @param section_id The id of the course section.
#' @param grades The updated grades object using patch semantics.
#' @return The updated final comments.
#' @concept Grading
#' @export
updateFinalComments <- function(section_id, period_id, enrollment_ids, comments, comment_statuses){
  
  assertthat::assert_that(length(enrollment_ids) == length(comments) & length(comments == length(comment_statuses)))
  
  payload <- list(final_comments = list(final_comment = list()))
  
  for(i in 1:length(enrollment_ids)){
    
    payload$final_comments$final_comment <- payload$final_comments$final_comment %>% append( 
      list(list(enrollment_id = enrollment_ids[[i]], 
                period_id = period_id, 
                comment = comments[[i]], 
                comment_status = comment_statuses[[i]])))
  }
  
  endpoint = glue::glue('/sections/{section_id}/grades')
  
  resource <- makeRequest(endpoint, verb = 'PUT', payload = payload)
  
  return(resource)
}


#' List Grading Scales
#' 
#' List grading scales for a section
#' @param section_id The id of the section.
#' @return The grading scales for the section.
#' @concept Grading
#' @export
listGradingScales <- function(section_id){
  
  endpoint = glue::glue('/sections/{section_id}/grading_scales')
  
  params <- list(start = 0, limit = 200)
  resource <- makeRequest(endpoint, params, verb = 'GET')
  
  return(resource)
}
