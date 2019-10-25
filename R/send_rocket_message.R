#' Send Rocket.chat messages to users, channels or groups
#'
#' \code{send_rocket_message} Uses the users ID and Token to communicate with Rocket.chat server and send messages to specific groups, users or channels
#'
#' This function connects with the Rocket.chat with users ID and Token and send
#' a text message to a receiver.
#'
#' To send a message to specific user, is essential to use the `@` before the user name.
#' To send a message to groups or channels, you should use `#`
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https).
#' @param user_id The users ID retrieved from `get_rocket_credentials`.
#' @param auth_token The users authentication Token retrieved from `get_rocket_credentials`.
#' @param receiver The receiver of the message (if user mentioned with `@`, otherwise use `#`)
#' @param message A text string containing the text to be sent
#'
#' @return This function has no return since it posts the message to the server
#'
#' @examples
#'
#' \dontrun{
#' send_rocket_message(url        = 'http://localhost:8000/',
#'                     user_id    = '<ROCKET_USER_ID>',
#'                     auth_token = '<ROCKET_USER_AUTH_TOKEN>',
#'                     receiver   = '#general',
#'                     message    = 'hello')
#' }
send_rocket_message <-
  function(url,
           user_id,
           auth_token,
           receiver,
           message) {

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    body_ <-
      paste0('{"channel": "', receiver, '", ',
             '"text": "', message, '"}')

    httr::POST(
      url = paste0(url, 'api/v1/chat.postMessage'),
      httr::add_headers(
        c(
          'X-Auth-Token' = auth_token,
          'X-User-Id'    = user_id,
          'Content-type' = 'application/json'
        )
      ),
      body = body_
    )

  }
