# This function creates a Section object for use in creating and modifying Course Sections objects.
createSectionObject = function(title  = NULL, section_code  = NULL, section_school_code  = NULL, access_code  = NULL, grading_periods  = NULL, description  = NULL, profile_url  = NULL, location  = NULL, meeting_days  = NULL, start_time  = NULL, end_time  = NULL, class_periods  = NULL, options/course_format  = NULL, options/weighted_grading_categories  = NULL, options/upload_document  = NULL, options__create_discussion  = NULL, options__member_post  = NULL, options__member_post_comment  = NULL, synced  = NULL){
     
     object = as.list(environment()) 
     object = userObject[as.character(userObject) != "NULL"] 
     names(object) = gsub('__', '/', names(object))
     
     return(userObject) 
 }