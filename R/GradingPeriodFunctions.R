source('R/AuthenticationFunctions.R')

#' List GradingPeriods for School
#' 
#' This function lists all gradingPeriods for a school district.
#' 
#' A gradingPeriod is like a semester or trimester for a given school year. 
#' @param title Specifies an exact title to search for.
#' @param startswith Set this value to 1 to match all grading periods whose titles begin with the title provided.
#' @concept GradingPeriods
#' @return A dataframe of gradingPeriods details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation}
#' @export
listGradingPeriods = function(title = NULL, startswith = NULL){
  
  params <- as.list(environment())
  
  endpoint = paste0('/gradingPeriods')
  
  toReturn <- data.frame()
  total <- 1000000
  while(nrow(toReturn) < total - 1){
    params$start <- nrow(toReturn) + 1
    params$limit <- 200
    resource <- makeRequest(endpoint, params, verb = 'GET')
    total <- as.integer(resource$total)
    newData <- jsonlite::fromJSON(toJSON(resource$gradingperiods), flatten = TRUE)
    toReturn <- bind_rows(toReturn, characterizeDataFrame(newData))
    if(length(total) == 0) break()
  }
  
  return(toReturn)
}

#' Get GradingPeriods Details
#' 
#' This function returns details about a gradingPeriods: a campus of a school district.
#' 
#' @param gradingPeriodsId Can be found by navigating to the Schoology gradingPeriods information page.
#' @concept GradingPeriods
#' @return A dataframe of gradingPeriods details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation}
#' @export
viewGradingPeriod = function(gradingPeriodId){
  
  endpoint = paste0('gradingPeriods/', gradingPeriodId)
  
  resource = makeRequest(endpoint, verb = 'GET')
  
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

#' Update GradingPeriods Details
#' 
#' This function modifies one or more attributes of a school gradingPeriods.
#' 
#' @param gradingPeriodsId Can be found by navigating to the Schoology gradingPeriods information page.
#' @param title The grading period title. This title must be unique across all grading periods in the school.
#' @param start The grading period start date YYYY-MM-DD
#' @param end The grading period end date YYYY-MM-DD
#' See \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation} for a description 
#' of each parameter.
#' @concept GradingPeriods
#' @return A dataframe of updated gradingPeriod details.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation}
#' @export
updateGradingPeriods = function(gradingPeriodId, title = NULL, start = NULL, end = NULL){
  
  params <- as.list(environment())[-1]
  params <- as.list(unlist(params))
  
  if(length(params) == 0){
    stop('ERROR: No changes requested.')
  }
  
  endpoint = paste0('gradingPeriods/', gradingPeriodId)
  
  response = makeRequest(endpoint, verb = 'PUT', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
  if(is.null(response)){
    
    return(viewGradingPeriod(gradingPeriodId))
    
  }else{
    
    return(response)
    
  }
}

#' Create a School GradingPeriods
#' 
#' This function creates a new school gradingPeriods.
#' 
#' @param title The grading period title. This title must be unique across all grading periods in the school.
#' @param start The grading period start date YYYY-MM-DD
#' @param end The grading period end date YYYY-MM-DD
#' @concept GradingPeriods
#' @return A dataframe of details for the newly created gradingPeriod.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation}
#' @export
# Creates a gradingPeriod for a school.
createGradingPeriod = function(title, start, end){
  
  params <- as.list(environment())
  
  endpoint = paste0('/gradingPeriods/')
  
  response = makeRequest(endpoint, verb = 'POST', payload = fromJSON(toJSON(params, pretty = TRUE)))
  
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

#' Deletes a school gradingPeriod.
#' 
#' @param gradingPeriodsId The ID of the gradingPeriods which will be deleted.
#' @concept GradingPeriods
#' @return The success status of the DELETE request.
#' @section References:
#' \href{https://developers.schoology.com/api-documentation/rest-api-v1/gradingPeriods}{API Documentation}
#' @export
deleteGradingPeriod = function(gradingPeriodId){
  
  endpoint = paste0('gradingPeriods/', gradingPeriodId)
  
  gradingPeriodName = viewGradingPeriod(gradingPeriodId)$title
  
  userResponse = readline(prompt = paste0("Are you SURE you want to delete ", gradingPeriodName, "? This cannot be undone! \nType '", gradingPeriodsName, "' to delete, anything else not to delete."))
  
  if(userResponse == gradingPeriodName){
    response = makeRequest(endpoint, verb = 'DELETE')
    return(paste(gradingPeriodName, 'successfully deleted.'))
  }else{
    return("GradingPeriod deletion canceled.")
  }
}

