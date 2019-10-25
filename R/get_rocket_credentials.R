#' Get Rocket.chat user credentials
#'
#' \code{get_rocket_credentials} Connects with the Rocket.chat REST API and returns the user ID and token.
#'
#' This function connects with the Rocket.chat REST API to gather the users ID and token.
#' This sort of information will be needed to any function, since the "contract" between R
#' and the API of rocket depends on Token Authentication
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https)
#' @param user The username of login which will be used to extract ID and Token
#' @param password This argument can be set manually, but as default it opens a popup in RStudio for the user to type the password.
#'
#' @return Given a valid url, user and password, this function returns a named list
#' containing the user_id argument and the auth_token argument.
#'
#' @examples
#'
#' \dontrun{
#' get_rocket_credentials('http://localhost:8000/', user = 'me')
#' }
get_rocket_credentials <-
  function(url,
           user,
           password = rstudioapi::askForPassword()){

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    `%>%` <- magrittr::`%>%`

    credentials_return <-
      httr::VERB(
        verb = 'POST',
        url = paste0(url, 'api/v1/login'),
        body = list(
          username = user,
          password = password
        ),
        encode = 'form'
      ) %>%
      httr::content()

    if(credentials_return$status != 'success') {

      stop('Não foi possível autenticar no rocket.chat')

    }

    lst_tokens <-
      list(
        user_id = credentials_return$data$userId,
        auth_token = credentials_return$data$authToken
      )

    return(lst_tokens)

  }
