library(httr)
library(jsonlite)

getSmSurveys <- function(auth_token, page, per_page) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your auth token for SurveyMonkey: ')
  }
  
  # defaults
  if (missing(page)) {
    page <- 1
  }
  if (missing(per_page)) {
    per_page <- 250
  }

  auth <- paste("bearer", auth_token, sep=" ");

  url <- paste('https://api.surveymonkey.net/v3/surveys?page=', page, '&per_page=', per_page, sep='')

  survey_list_response <- GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))

  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', http_status(survey_list_response)))
  }

  json <- content(survey_list_response, as = 'text')
  survey_list <- fromJSON(json)

  invisible(survey_list)
}

getSmResponses <- function(auth_token, survey_id, page, per_page) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your auth token for SurveyMonkey: ')
  }
  if (missing(survey_id)) {
    stop('Survey ID is required')
  }
 
  # defaults
  if (missing(page)) {
    page <- 1
  }
  if (missing(per_page)) {
    per_page <- 250
  }

  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste('https://api.surveymonkey.net/v3/surveys/', survey_id, '/responses?page=', page, '&per_page=', per_page, sep='')

  survey_responses_response <- GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))

  if (survey_responses_response$status_code != 200) {
    stop(c('Bad response from server: ', http_status(survey_responses_response)))
  }

  json <- content(survey_responses_response, as = 'text')
  survey_responses <- fromJSON(json)

  invisible(survey_responses)
}

