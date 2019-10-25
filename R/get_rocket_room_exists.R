#' Validates whether a room exists or not
#'
#' \code{get_rocket_room_exists} Validates whether a room exists
#'
#' This function returns a logical response to whether a room exists or not
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https).
#' @param user_id The users ID retrieved from `get_rocket_credentials`.
#' @param auth_token The users authentication Token retrieved from `get_rocket_credentials`.
#' @param room_id The id of the room to be checked
#'
#' @return The return of this function is a boolean
#'
#' @examples
#'
#' \dontrun{
#' get_rocket_room_exists(url        = 'http://localhost:8000/',
#'                        user_id    = '<ROCKET_USER_ID>',
#'                        auth_token = '<ROCKET_USER_AUTH_TOKEN>',
#'                        room_id    = '<ROOM_ID>')
#' }
get_rocket_room_exists <-
  function(url,
           user_id,
           auth_token,
           room_id) {

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    `%>%` <- magrittr::`%>%`

    room_return <-
      httr::GET(
        url = paste0(url, 'api/v1/rooms.info?roomId=', room_id),
        httr::add_headers(
          c(
            "X-Auth-Token" = auth_token,
            "X-User-Id" = user_id
          )
        )
      ) %>%
      httr::content() %>%
      .$success

    return(room_return)

  }
