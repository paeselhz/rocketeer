#' Sends an attachment to a rocket user/channel
#'
#' \code{send_rocket_attachment} Sends an attachment to a user or channel or group
#'
#' This function only returns the status code of the POST request to upload an
#' attachment for any user or channel or group in a Rocket.chat server.
#'
#' @param url The url of the rocket.chat server (has to be complete, with http or https).
#' @param user_id The users ID retrieved from `get_rocket_credentials`.
#' @param auth_token The users authentication Token retrieved from `get_rocket_credentials`.
#' @param receiver The id of the user/channel/group that will receive the attachment (with `@` for users and `#` for channels and users)
#' @param filename The path of the file to be uploaded.
#'
#' @return This function only returns the answer of the POST request
#'
#' @examples
#'
#' \dontrun{
#' send_rocket_attachment(url        = 'http://localhost:8000/',
#'                        user_id    = '<ROCKET_USER_ID>',
#'                        auth_token = '<ROCKET_USER_AUTH_TOKEN>',
#'                        receiver   = '#general',
#'                        filename   = 'path_to_file')
#' }
send_rocket_attachment <-
  function(url,
           user_id,
           auth_token,
           receiver,
           filename) {

    if(substr(url, nchar(url), nchar(url)) != '/') {

      url <- paste0(url, '/')

    }

    if(grepl('@', receiver)) {

      rec_id <-
        get_rocket_users_list(
          url = url,
          user_id = user_id,
          auth_token = auth_token
        ) %>%
        dplyr::filter(
          stringr::str_detect(user,
                              stringr::str_remove(receiver, '@'))
        ) %>%
        .$user_id

      room_exists <-
        get_rocket_room_exists(
          url = url,
          user_id = user_id,
          auth_token = auth_token,
          room_id = paste0(user_id, rec_id)
        )

      if(room_exists) {

        room <- paste0(user_id, rec_id)

      } else {

        room <- paste0(rec_id, user_id)

      }

    } else if(grepl('#', receiver)) {

      room <-
        get_rocket_channels(
          url = url,
          user_id = user_id,
          auth_token = auth_token
        ) %>%
        dplyr::bind_rows(
          get_rocket_groups(
            url = url,
            user_id = user_id,
            auth_token = auth_token
          )
        ) %>%
        dplyr::filter(
          stringr::str_detect(name, stringr::str_remove(destination, '#'))
        ) %>%
        .$id
    }

    httr::POST(
      url = paste0(url, 'api/v1/rooms.upload/', room),
      httr::add_headers(
        c(
          "X-Auth-Token" = auth_token,
          "X-User-Id" = user_id
        )
      ),
      body = list(
        file = httr::upload_file(filename)
      )
    )

  }
