% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AuthenticationFunctions.R
\name{makeRequest}
\alias{makeRequest}
\title{Perform API Request to Endpoint}
\usage{
makeRequest(endpt, paramsList = NULL, verb = "GET", payload = NULL)
}
\arguments{
\item{endpt}{The endpoint URL for the request.}

\item{paramsList}{A list of parameters primarily used as searchFields.}

\item{verb}{An HTTP request, either GET, PUT, POST or DELETE.}

\item{payload}{A json object used in PUT and POST requests. \cr Use fromJSON(toJSON(\{yourNamedList\}, pretty = T)).}
}
\value{
The content of the request response.
}
\description{
This function performs an HTTP request to a Schoology API endpoint.
}
\details{
This function is called by all Schoology SDK functions.
You will, in general, not need to call this function directly.
}
\section{References}{

\href{https://developers.schoology.com/api-documentation/rest-api-v1/}{Schoology Rest API Documentation}
}

\concept{Requests}
