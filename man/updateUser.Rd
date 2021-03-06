% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UserFunctions.R
\name{updateUser}
\alias{updateUser}
\title{Update User Details}
\usage{
updateUser(
  userId,
  school_id = NULL,
  building_id = NULL,
  school_uid = NULL,
  name_title = NULL,
  name_title_show = NULL,
  name_first = NULL,
  name_first_preferred = NULL,
  name_middle = NULL,
  name_middle_show = NULL,
  name_last = NULL,
  name_display = NULL,
  username = NULL,
  primary_email = NULL,
  position = NULL,
  gender = NULL,
  grad_year = NULL,
  birthday_date = NULL,
  password = NULL,
  role_id = NULL,
  email_login_info = NULL,
  profile_url = NULL,
  tz_name = NULL,
  parents = NULL,
  parent_uids = NULL,
  advisor_uids = NULL,
  child_uids = NULL,
  send_message = NULL,
  synced = NULL,
  profile_picture_fid = NULL,
  additional_buildings = NULL
)
}
\arguments{
\item{userId}{Can be found by navigating to the Schoology user information page.}

\item{object}{Must be created via createUserObject().}
}
\value{
A dataframe of updated user details.
}
\description{
This function modifies one or more attributes of a user.
}
\section{References}{

\href{https://developers.schoology.com/api-documentation/rest-api-v1/user}{API Documentation}
}

\concept{Users}
