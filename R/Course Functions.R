
# This function creates a Course object for use in creating and modifying Course objects.
createCourseObject = function(building_id  = NULL, title  = NULL, course_code  = NULL, department  = NULL, description  = NULL, credits  = NULL, synced  = NULL, grade_level_range_start  = NULL, grade_level_range_end  = NULL, subject_area  = NULL){
	
     object = as.list(environment())
	object = object[as.character(object) != "NULL"]
	
	return(object)
}


# This function creates a course.
createCourse = function(object = createCourseObject()){
     
     object = object[as.character(object) != 'NULL']
     
     endpoint = 'courses/'
     
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function creates up to 50 courses at once.
createCourses = function(courseObjects = list(createCourseObject()), update_existing = 0){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     params = as.list(environment())[-1]
     
     indicesToRemove = integer()
     for(i in 1:length(courseObjects)){
          if(length(courseObjects[[i]]) == 0){
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
     
     if(length(indicesToRemove) > 0){
          courseObjects = courseObjects[-indicesToRemove]
     }
     
     courseObjects = paste0('{"courses":{"course":', toJSON(courseObjects), '}}')
     
     endpoint = 'courses/'
     
     resource = addParameters(endpoint, params)
     
     response = createObject(resource, fromJSON(courseObjects))
     
     # If there's no error...
     if(!exists('status_code', where = response)){
          # ... Return resource.
          resource = fromJSON(toJSON(response$course), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
     return(response)
}


# This function returns a data frame of courses.
listCourses = function(active = TRUE, start = 0, limit = 200, building_id = NULL, role_ids = NULL, parent_access_codes = NULL, school_uids = NULL, extended = NULL){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     params = as.list(environment())[-1]
     
     if(active){
          endpoint = paste0('/courses')
     }else{
          endpoint = paste0('/courses/inactive')
     }
     
     resource = getObject(addParameters(endpoint, params))
     
     # If there's no error...
     if(!exists('status_code', where = resource)){
          # ... Return resource.
          resource = fromJSON(toJSON(resource$course), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
}

# This function returns details about a course.
viewCourse = function(courseId){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     endpoint = paste0('courses/', courseId)

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

# This function modifies one or more attributes of a course.
updateCourse = function(id, object = createCourseObject()){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     unmodifiedObject <- as.list(viewCourse(id))
     object$title = unmodifiedObject$title
     object$course_code = unmodifiedObject$course_code
     endpoint = paste0('courses/', id)
     
     response = updateObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function modifies one or more attributes of up to 50 courses at a time.
updateCourses = function(courseObjects = list(createCourseObject())){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     indicesToRemove = integer()
     for(i in 1:length(courseObjects)){
          if(length(courseObjects[[i]]) == 0){
               print(paste('No Course Information found. Skipping courseObjects index', i))
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
     
     if(length(indicesToRemove) > 0){
          courseObjects = courseObjects[-indicesToRemove]
     }
     
     courseObjects = paste0('{"courses":{"course":', toJSON(courseObjects), '}}')
     print(courseObjects)
     endpoint = 'courses/'
     
     response = updateObject(endpoint, fromJSON(courseObjects))
     
     return(response)
}

# This function deletes a course.
deleteCourse = function(id){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     endpoint = paste0('courses/', id)
     
     response = deleteObject(endpoint)
     
     return(response)
}

# This function deletes up to 50 courses at once.
deleteCourses = function(course_ids = character()){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     params = as.list(environment())
     params = lapply(params, function(x) paste(x, collapse = ','))
     
     endpoint = 'courses'
     resource = addParameters(endpoint, params)
    
     response = deleteObject(resource)
     
     return(response)
}