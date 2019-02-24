# This function creates a Section object for use in creating and modifying Section Sections objects.
createSectionObject = function(title  = NULL, section_code  = NULL, section_school_code  = NULL, access_code  = NULL, grading_periods  = NULL, description  = NULL, profile_url  = NULL, location  = NULL, meeting_days  = NULL, start_time  = NULL, end_time  = NULL, class_periods  = NULL, options__section_format  = NULL, options__weighted_grading_categories  = NULL, options__upload_document  = NULL, options__create_discussion  = NULL, options__member_post  = NULL, options__member_post_comment  = NULL, synced  = NULL){
     
     object = as.list(environment()) 
     object = object[as.character(object) != "NULL"] 
     names(object) = gsub('__', '/', names(object))
     
     return(object) 
}

# This function creates a section.
createSection = function(courseId, object = createSectionObject()){
     
     object = object[as.character(object) != 'NULL']
     
     endpoint = paste0('courses/', courseId, '/sections')
     
     response = createObject(endpoint, fromJSON(toJSON(object, pretty = TRUE)))
     
     return(response)
}

# This function creates up to 50 sections at once.
createSections = function(sectionObjects = list(createSectionObject()), update_existing = 0){
     
     require(jsonlite)
     source('Helper Functions.R')
     
     params = as.list(environment())[-1]
     
     indicesToRemove = integer()
     for(i in 1:length(sectionObjects)){
          if(length(sectionObjects[[i]]) == 0){
               indicesToRemove[[length(indicesToRemove) + 1]] <- i
          }
     }
     
     if(length(indicesToRemove) > 0){
          sectionObjects = sectionObjects[-indicesToRemove]
     }
     
     sectionObjects = paste0('{"sections":{"section":', toJSON(sectionObjects), '}}')
     
     endpoint = 'sections/'
     
     resource = addParameters(endpoint, params)
     
     response = createObject(resource, fromJSON(sectionObjects))
     
     # If there's no error...
     if(!exists('status_code', where = response)){
          # ... Return resource.
          resource = fromJSON(toJSON(response$section), flatten = TRUE)
          resource = characterizeDataFrame(resource)
          return(resource)
     }else{
          # Otherwise return server response if there's an error.
          return(resource)
     }
     return(response)
}