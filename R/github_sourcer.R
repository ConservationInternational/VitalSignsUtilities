#' CIRepo
#' 
#' A class that lets you source directly from the CI github repo rather than
#' download and source() or parse()
#'
#' @section Methods:
#' \itemize{
#'  \item \code{sourceRCode(repo_path): Downloads R code from github, then
#'  sources it.}
#'  \item \code{parseRCode(repo_path): Like sourceRCode, but parse instead.}
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name CIRepo-class
CIRepo <- R6::R6Class("CIRepo",
  public = list(
    github_token = NULL,
    
    initialize = function(pat, repo = "vs-indicators-calc-private") {
      self$github_token <- pat
    },
    sourceRCode = function(repo_path) {
      github_url <- paste0(gsub("REPO", repo, private$RCode.url), repo_path)
      print(github_url)
      github_response <- httr::GET(github_url,
                                   httr::add_headers("Authorization" =
                                                     paste("token",
                                                           self$github_token)))
      response_content <- httr::content(github_response)
      if (response_content != "Not Found") {
        nutrition.r <- tempfile(fileext = ".R")
        writeLines(response_content, nutrition.r)
        source(nutrition.r)
      } else {
        stop("Something went wrong with loading the file")
      }
    },
    parseRCode = function(repo_path) {
      github_url <- paste0(gsub("REPO", repo, private$RCode.url), repo_path)
      print(github_url)
      github_response <- httr::GET(github_url,
                                   httr::add_headers("Authorization" =
                                                       paste("token",
                                                             self$github_token)))
      response_content <- httr::content(github_response)
      if (response_content != "Not Found") {
        nutrition.r <- tempfile(fileext = ".R")
        writeLines(response_content, nutrition.r)
        parse(nutrition.r)
      } else {
        stop("Something went wrong with loading the file")
      }
    }
  ),
  private = list(
    RCode.url = "https://raw.githubusercontent.com/ConservationInternational/REPO/master/"
  )
)


#' checkGithubToken
#' 
#' Checks if Github PAT is set.
#'
#' @return Returns CIRepo object if Github PAT set, otherwise returns an error.
#' @export
#'
#' @examples
#' 
#' # ci.repo<- checkGithubToken() # Returns CIRepo object if Github PAT set.
checkGithubToken <- function() {
  if (Sys.getenv("GITHUB_PAT") != "") {
    return(CIRepo$new(Sys.getenv("GITHUB_PAT")))
  } else {
    stop("Missing github personal access token. See https://github.com/settings/tokens")
  }
}


#' setGithubToken
#'
#' Sets Github PAT so user can use the CIRepo object.
#'
#' @param pat Github PAT available in Github user settings
#'
#' @return Nothing
#' @export
#'
#' @examples
#' # setGithubToken("MY_GITHUB_PAT")
setGithubToken <- function(pat) {
  Sys.setenv("GITHUB_PAT" = pat)
  message("Github PAT set!")
}
