#' Retrieve a list of `n`` messages exchanged in private groups of a Rocket.chat server
#'
#' \code{get_rocket_groups_history} Gets a dataframe with the messages
#' exchanged in a specific group of a Rocket.chat server.
#'
#' This function returns a dataframe containing `n` messages exchanged in
#' a private group in a Rocket.chat server.
#'
#' The return of this function contains the content of the message, who sent it (username and name),
#' the ID of the sender, and a time stamp.
#'
#' The function may be similar (some might say is equal) to `get_rocket_channels_history`,
#' but Rocket.chat has a few differences in the API regarding public/open channels
#' and private groups, this is the reason there are two functions.
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https).
#' @param user_id The users ID retrieved from `get_rocket_credentials`.
#' @param auth_token The users authentication Token retrieved from `get_rocket_credentials`.
#' @param group The name of the group to get the history from
#' @param n_messages The number of messages to be returned (default is 0, which brings the complete history)
#'
#' @return The return of this function is a dataframe.
#'
#' @examples
#'
#' \dontrun{
#' get_rocket_groups_history(url        = 'http://localhost:8000/',
#'                           user_id    = '<ROCKET_USER_ID>',
#'                           auth_token = '<ROCKET_USER_AUTH_TOKEN>',
#'                           group = 'bot_tests',
#'                           n_messages = 0)
#' }
get_rocket_groups_history <-
  function(url,
           user_id,
           auth_token,
           group,
           n_messages = 0) {

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    if(grepl('#', channel)){

      group <- stringr::str_remove(group, '#')

    }

    `%>%` <- magrittr::`%>%`

    message_history <-
      httr::GET(
        paste0(url, 'api/v1/groups.history?count=', n_messages, '&roomName=', group),
        httr::add_headers(
          c(
            'X-Auth-Token' = auth_token,
            'X-User-Id'    = user_id
          )
        )
      ) %>%
      httr::content() %>%
      .$messages %>%
      lapply(
        function(message_corpus) {

          return_df <-
            data.frame(
              message = message_corpus$msg,
              user = ifelse(is.null(message_corpus$u$name), '', message_corpus$u$name),
              user_name = message_corpus$u$username,
              user_id = message_corpus$u$`_id`,
              time_stamp = message_corpus$ts
            )

          return(return_df)

        }
      ) %>%
      dplyr::bind_rows()

    return(message_history)

  }
