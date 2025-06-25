#' Extract error message from JSON body of an HTTP response
#'
#' @param resp An HTTP response object.
#' @return A named character vector representing the flattened JSON content of the HTTP response body.
#' @examples
#' \dontrun{
#' # Assuming an HTTP response object 'response' with a JSON error body
#' error_body <- ecopi_error_body(response)
#' }
#' @export

ecopi_error_body <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    unlist()
}


#' Make API requests to the Ecopi API
#'
#' This function sends an API request to the Ecopi API with the specified resource, parameters, and optional arguments.
#'
#' @param resource A character string specifying the API resource to request.
#' @param ... Path parameters, that point to a specific resource, to be passed to the req_template() function.
#' @param params Query parameters that modify the reuqest.
#' @param new_data A named list of parameters to be updated/ patched.
#' @param file_path A path currently only important to patch an image to a recorder endpoint (Watchout! If file_path given, new_data gets ignored (cannot patch multiple types)).
#' @return A `response` object from the httr2 package containing the API response.
#' @examples
#' \dontrun{
#' # Send a request to the 'detections' endpoint. Only get detections in the project '017_neeri'
#' response <- ecopi_api("GET /detections/", params = list("project" = "017_neeri"))
#' }
#' @import httr2
#' @export
ecopi_api <- function(resource, ...,
                      params = list(),
                      new_data = list(),
                      file_path,
                      base_url = getOption("ecopiapi.base_url", "http://127.0.0.1:8008/api/v0.1")) {
  params <- lapply(params, paste, collapse = ",")
  new_data <- lapply(new_data, function(x) if (identical(x, "")) jsonlite::unbox(NULL) else paste(x, collapse = ","))

  if (!missing(file_path) && length(unlist(new_data)) > 0) {
    warning("The 'new_data' parameter is ignored when 'file_path' is provided.")
  }

  req <- request(base_url) |>
    req_headers(Authorization = paste("Token", get_ecopiapi_key())) |>
    req_user_agent("ecopiapi") |>
    req_error(body = ecopi_error_body) |>
    req_template(resource, ...) |>
    req_url_query(!!!params)

  if (missing(file_path)) {
    req <- req_body_json(req, new_data)
    req_perform(req)
  } else {
    req <- req_body_multipart(req, image = curl::form_file(file_path))
    req_perform(req)
  }
}


#' Convert API Response Body to Data Frame
#'
#' This function takes an API response object and converts its JSON body to a data frame if the response contains a body. If the response does not contain a body, a warning is issued with the response status code.
#'
#' @param api_response Response object from the API, expected to be of class `httr2_response`.
#' @param get_response Logical. If TRUE, returns a list containing both the original response and the data frame.
#'
#' @return If get_response is FALSE (default), returns a data frame containing the JSON content of the response body if it exists; otherwise, `NULL`.
#'         If get_response is TRUE, returns a list with two elements: `data` (the data frame) and `response` (the original API response).
#' @export
#' @examplesIf interactive()
#' response <- ecopi_api("GET /detections/", params = list("project" = "017_neeri"))
#' resp_body_json_to_df(response)
#'
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_status
#' @importFrom httr2 resp_has_body
resp_body_json_to_df <- function(api_response, get_response = FALSE) {
  resp_body_not_empty <- resp_has_body(api_response)
  if (resp_body_not_empty) {
    data <- api_response |>
      resp_body_json(simplifyVector = TRUE)

    if (get_response) {
      return(list(data = data, response = api_response))
    } else {
      return(data)
    }
  } else {
    warning("The response does not contain a body. Status code: ", resp_status(api_response))
    if (get_response) {
      return(list(data = NULL, response = api_response))
    } else {
      return(NULL)
    }
  }
}

# Detections ------------------------------------------------------------------------------------------------------


#' Get detections list.
#'
#' Wrapper around the 'detections_list' endpoint to retrieve a list of detections based on the specified query parameters.
#'
#' @param ... query paramaters. See \url{https://api.ecopi.de/api/docs/#tag/v0.2/operation/v0.2_detections_list}
#' @param get_response Logical. If TRUE, returns a list containing both the original response and the data frame.
#'
#' @examples
#' # Retrieve a list of detections for project '017_neeri' that occurred in March (month=3)
#' \dontrun{
#' get_detections(project_name = "017_neeri", datetime__month = 3)
#' }
#'
#' @return If get_response is FALSE (default), returns a data frame containing the detections.
#'         If get_response is TRUE, returns a list with 'data' (the data frame) and 'response' (the original API response).
#'
#' @export
get_detections <- function(..., get_response = FALSE) {
  params <- list(...)
  ecopi_api("GET /detections/", params = params) |>
    resp_body_json_to_df(get_response = get_response)
}



#' Post a new detection
#'
#' Wrapper around the 'detections_create' endpoint.
#' If you want to create a new detection and include a media file, you need to do it in two steps.
#' First create the detection using `post_detection()` and then upload the mediafile with `patch_detections()`
#' TIP: Assigning your own uuid before posting makes it easier to patch a file.
#'
#' @param ... Find required parameters here \url{https://api.ecopi.de/api/docs/#tag/v0.2/operation/v0.2_detections_create}
#'
#' @examples
#' \dontrun{
#' post_detection(
#'   recorder_name = "000000002e611dde",
#'   datetime = lubridate::now() |> lubridate::with_tz(tzone = "UTC") |> format("%Y-%m-%dT%H:%M:%S") |> paste0("Z") |> as.character(), # lubridate::now(tzone = "UTC") |> format("%Y-%m-%dT%H:%M:%S%Z") |> as.character(), # "2019-08-24T14:15:22Z",
#'   start = 1,
#'   end = 4,
#'   species_code = "Frosch",
#'   confidence = -1
#' )
#' }
#' @return httr2_response
#' @export
post_detection <- function(...) {
  params <- list(...)
  ecopi_api("POST /detections/", new_data = params)
}


#' PATCH detection
#'
#' Wrapper around the 'detections_partial_update' endpoint to update detections parameters based on the specified query parameters.
#'
#' @param ... query paramaters. See \url{https://api.ecopi.de/api/docs/#tag/v0.2/operation/v0.2_detections_partial_update}
#' @param id_or_uid The database ID or UID of the respective detection
#' @param file_path Path to file to upload (Watchout! If file_path given, new_data gets ignored (cannot patch multiple types)).
#'
#' @examples
#' # Update the parameter confirmed of an example detection
#' \dontrun{
#' patch_detections(id_or_uid = "64733fbc-7cc8-49f6-adf1-c9ec2d676959", confirmed = "YES")
#' }
#'
#' @return httr2_response
#'
#' @export

patch_detections <- function(..., id_or_uid, file_path) {
  params <- list(...)
  # important to make emppty/ blank comments (i.o.w. deleting old comments)
  if ("comment" %in% names(params) && params[["comment"]] == "") {
    params[["comment"]] <- "" # Explicit empty string for 'comment'
  }
  ecopi_api("PATCH /detections/{id_or_uid}/", id_or_uid = id_or_uid, new_data = params, file_path = file_path)
}
