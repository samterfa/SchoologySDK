% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CourseFunctions.R
\name{listCourses}
\alias{listCourses}
\title{List Courses for a School}
\usage{
listCourses(building_id = NULL)
}
\arguments{
\item{include_past}{Whether to return past courses or only current courses.}
}
\value{
A dataframe of course details.
}
\description{
This function returns a list of courses for a school past and present.
}
\details{
A course is a course meeting.
}
\section{References}{

\href{https://developers.schoology.com/api-documentation/rest-api-v1/courses}{API Documentation}
}

\concept{Courses}
