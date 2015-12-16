library(bitops)
library(jsonlite)
library(RCurl)
library(curl)

#' Get user credentials from a file
#'
#' Extracts the user credentials from a sepcified file
#' and returns it.
#' @param fileName - The name of the file containing user credentials (username and password)
#' @return The contents of the file
#' @export
get_creds <- function(fileName) {
  readCmd <- paste("cat ", fileName)
  creds <- system(readCmd, intern=TRUE)
  return (creds)
}

#' Get User Token from Keystone
#'
#' Makes a call to keystone server to get a token based on the user credentials
#' @parm credsFileName - File containing user credentials
#' @return The auth token for the user
#' @export
get_token <- function (credsFileName) {
  creds <- get_creds(credsFileName)
  #
  username <- creds[1]
  password <- creds[2]
  tenantName <- creds[3]
  #
  h <- new_handle()
  handle_setheaders(h, "Content-Type" = "application/json", "Accept: application/json")
  httpheader <- "Content-Type: application/json"
  #
  auth_data1 <- "{\"auth\":{\"passwordCredentials\":{ \"username\":\""
  auth_data2 <- "\",\"password\":\""
  auth_data3 <- "\"},\"tenantName\":\""
  auth_data4 <- "\"}}"
  auth_data <- paste(auth_data1, username, auth_data2, password, auth_data3, tenantName, auth_data4, sep="")
  #
  handle_setopt(h, ssl_verifypeer = 0, post = 1, postfields = auth_data)
  #
  #con <- jsonlite::fromJSON(curl("https://10.245.123.2:5000/v2.0/tokens", handle=h))
  con <- jsonlite::fromJSON(getURL("https://10.245.123.2:5000/v2.0/tokens",
                                   ssl.verifypeer = 0, httpheader = httpheader, post = 1, postfields = auth_data))
  token <- con$access$token$id
  return (token)
}

#' Issue swift commands
#'
#' Issues client commands against the opoenstack swift service
#' @param credsFileName - Filename where user credentials are stored
#' @parm - swiftCmd - The specified swift command
#' @parm - container - The container name
#' @parm - object - The object name
#' @export
swift <- function (credsFileName, swiftCmd=NULL, container=NULL, object=NULL) {
  Xauth_token = paste("X-AUTH-TOKEN: ", get_token(credsFileName))
  if (is.null(swiftCmd) || swiftCmd == "list") {
    req <- getURL("https://10.245.123.2:8080/swift/v1", ssl.verifypeer = 0, httpheader = Xauth_token)
  } else if (swiftCmd == "get") {
    url <- paste("https://10.245.123.2:8080/swift/v1/", container, "/", object, sep="")
    req <- getURL(url, ssl.verifypeer = 0, httpheader = Xauth_token)
  } else if (swiftCmd == "put") {
    url <- paste("https://10.245.123.2:8080/swift/v1/", container, "/", object, sep="")
    req <- getURL(url, ssl.verifypeer = 0, httpheader = Xauth_token)
  }
  return (req)
}
