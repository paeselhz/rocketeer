#' Retrieve a list of all users in a Rocket.chat server
#'
#' \code{get_rocket_users} Gets a dataframe with all the users in a Rocket.chat server.
#'
#' This is a simple function, that connects with the Rocket.chat server
#' and returns a dataframe with information about all users registered.
#'
#' The return comprises a few information about the users such as:
#' * username  = Username used to login.
#' * name      = Actual name of the user.
#' * user_id   = ID used to identify other users within Rocket.chat.
#' * is_ldap   = If this registry comes from LDAP servers or not.
#' * is_active = If this user is still active.
#' * type      = Can be one of two, or user, or bot.
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https).
#' @param user_id The users ID retrieved from `get_rocket_credentials`.
#' @param auth_token The users authentication Token retrieved from `get_rocket_credentials`.
#'
#' @return The return of this function is a dataframe.
#'
#' @examples
#'
#' \dontrun{
#' get_rocket_users(url        = 'http://localhost:8000/',
#'                  user_id    = '<ROCKET_USER_ID>',
#'                  auth_token = '<ROCKET_USER_AUTH_TOKEN>')
#' }
get_rocket_users <-
function(url,
         user_id,
         auth_token) {

  if(substr(url, nchar(url), nchar(url)) != '/') {

    url <- paste0(url, '/')

  }

  `%>%` <- magrittr::`%>%`

  users_list <-
    httr::GET(
      url = paste0(url, '/api/v1/users.list?count=0'),
      httr::add_headers(
        c(
          'X-Auth-Token' = auth_token,
          'X-User-Id'    = user_id
        )
      )
    ) %>%
    httr::content() %>%
    .$users %>%
    lapply(
      function(user_) {

        return_df <-
          data.frame(
            user = user_$username,
            name = ifelse(is.null(user_$name), '', user_$name),
            user_id = user_$`_id`,
            is_ldap = ifelse(is.null(user_$ldap), FALSE, user_$ldap),
            is_active = ifelse(is.null(user_$active), FALSE, user_$active),
            type = user_$type
          )

        return(return_df)
      }
    ) %>%
    dplyr::bind_rows()

  return(users_list)

}
