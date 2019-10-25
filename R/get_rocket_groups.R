#' Retrieve a list of all groups which the user is invited in a Rocket.chat server
#'
#' \code{get_rocket_groups} Gets a dataframe with all the groups which the user is allowed in a Rocket.chat server.
#'
#' This function connects to the Rocket.chat server and returns a dataframe
#' containing the name of the group, the group's ID, number of users and number of
#' messages exchanged there.
#'
#' The function may be similar (some might say is equal) to `get_rocket_channels`,
#' but Rocket.chat has a few differences in the API regarding public/open channels
#' and private groups, this is the reason there are two functions.
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
#' get_rocket_groups(url        = 'http://localhost:8000/',
#'                   user_id    = '<ROCKET_USER_ID>',
#'                   auth_token = '<ROCKET_USER_AUTH_TOKEN>')
#' }
get_rocket_groups <-
  function(url,
           user_id,
           auth_token){

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    `%>%` <- magrittr::`%>%`

    groups_list <-
      httr::GET(
        url = paste0(url, 'api/v1/groups.list'),
        httr::add_headers(
          c(
            'X-Auth-Token' = auth_token,
            'X-User-Id'    = user_id
          )
        )
      ) %>%
      httr::content() %>%
      .$groups %>%
      lapply(
        function(item) {

          return_df <-
            data.frame(
              name = item$name,
              id = item$`_id`,
              users_count = item$usersCount,
              n_messages = item$msgs,
              stringsAsFactors = FALSE
            )

          return(return_df)

        }
      ) %>%
      dplyr::bind_rows()

    return(groups_list)

  }
