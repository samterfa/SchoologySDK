% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UserFunctions.R
\name{viewApiUser}
\alias{viewApiUser}
\title{This function returns the current API user.}
\usage{
viewApiUser(
  currentAccessToken,
  currentAccessTokenSecret,
  currentConsumerKey,
  currentConsumerSecret
)
}
\arguments{
\item{currentAccessToken}{The currentAccessToken of the current API user.}

\item{currentAccessTokenSecret}{The currentAccessTokenSecret of the current API user.}

\item{currentConsumerKey}{The currentConsumerKey of the application.}

\item{currentConsumerSecret}{The currentConsumerSecret of the application.}
}
\value{
User information for the current API user.
}
\description{
This function returns the current API user.
}
\section{References}{

\href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
}

\concept{Users}
