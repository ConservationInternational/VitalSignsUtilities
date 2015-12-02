#' S3Handler class and handler objects.
#'
#' This handler object allows users to operate with S3 at a very basic level.
#' It is entirely a wrapper for AWS CLI.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{writeCredentials(aws_access_key_id, aws_secret_access_key,
#'                               json_file, profile_name): 
#'    There are three potential modes of use with this method:
#'    1) The user provides AWS access key id and secret access key
#'    2) The user provides a JSON file with the credentials
#'    3) The user provides a profile name that already exists in the default
#'    credentials file (~/.aws/credentials)
#'    
#'    Then, the method operates appropriately and initializes an object that can
#'    interact with AWS
#'  }
#'  \item \code{writeS3(bucket, source_path, target_path, overwrite = FALSE): 
#'    This method allows the user to write to S3, provided a bucket, source file
#'    path and target path in the bucket,
#'    along with whether or not the user wishes to overwrite the data.}
#'  \item \code{listS3(bucket, path = ""): 
#'    Given a bucket and path (optional), the user gets an entire, recursive
#'    listing of files and folders from the
#'    combined path of the bucket and path.}
#'  \item \code{checkFilesS3(bucket, path):
#'    Given a bucket and path, the user can use this method to check whether a
#'    file already exists in the S3 bucket.}
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name S3Handler-class
S3Handler <- R6::R6Class("S3Handler",
                         public = list(
                           aws_creds_file = "~/.aws/credentials",
                           profile_name = NULL,
                           dirlist_s3 = list(),
                           initialize = function(access_key_id,
                                                 secret_access_key,
                                                 credentials_file,
                                                 profile_name) {
                             self$writeCredentials(access_key_id,
                                                   secret_access_key,
                                                   credentials_file,
                                                   profile_name)
                           },
                           writeCredentials = function(aws_access_key_id,
                                                       aws_secret_access_key,
                                                       json_file,
                                                       profile_name) {
  
                             credentials_file <- readChar(self$aws_creds_file,
                                            file.info(self$aws_creds_file)$size)
                             if (!missing(profile_name) &
                                 grepl(profile_name, credentials_file) &
                                 missing(aws_access_key_id) &
                                 missing(aws_secret_access_key) &
                                 missing(json_file)) {
                               message("AWS profile exists!")
                               self$profile_name <- profile_name
                               return(NULL)
                             }  

                             else if (!missing(json_file)) {
                               credentials <- jsonlite::fromJSON(json_file)
                               if (!("aws_access_key_id" %in%
                                     names(credentials)) &
                                   !("aws_secret_access_key" %in%
                                     names(credentials)))
                                 stop("JSON file missing credentials.")
                               aws_access_key_id <-
                                 credentials$aws_access_key_id
                               aws_secret_access_key <-
                                 credentials$aws_secret_access_key
                             }
                             else if (missing(aws_access_key_id) ||
                                      missing(aws_secret_access_key))
                               stop(paste0("Missing: ",
                                           ifelse(missing(aws_access_key_id),
                                                  ifelse(missing(
                                                    aws_secret_access_key),
                                   "aws_access_key_id and aws_secret_access_key",
                                   "aws_access_key_id"),
                                   "aws_secret_access_key")))
                               
                               if (!missing(profile_name) ||
                                   !grepl(sprintf("[ruser-%s]",
                                                  Sys.info()[["user"]]),
                                          credentials_file)) {
                                 if (missing(profile_name))
                                   profile_name <- paste0("ruser-", 
                                                          Sys.info()[["user"]])
                                 credentials_text <-
    sprintf("[%s]\naws_access_key_id=%s\naws_secret_access_key=%s",
            profile_name,
            aws_access_key_id,
            aws_secret_access_key)
                                 self$profile_name <- profile_name
                                 writeLines(self$aws_creds_file,
                                            credentials_text,
                                            append=TRUE)
                               } else {
                                 message("It looks like you already have, ",
                                         "credentials stored. ",
                                         "Set the profile_name variable to ",
                                         "override the default.")
                               }
                           },
                           writeS3 = function(bucket, source_path,
                                              target_path,
                                              overwrite = FALSE) {
                             if (!overwrite & self$checkFileS3(bucket,
                                                               target_path))
                               stop(paste0("Write path already exists ",
                                           "and overwrite = FALSE"))

                             system(sprintf("aws s3 cp %s s3://%s --profile %s",
                                            source_path,
                                            file.path(bucket, target_path),
                                            self$profile_name),
                                    intern = T) -> s3_write_results
                             message(s3_write_results)
                           },
                           listS3 = function(bucket, path = "") {
                             s3_path <- file.path(bucket, path)
                             system(sprintf(
                               "aws s3 ls %s --recursive --profile %s",
                                            s3_path,
                                            self$profile_name),
                                    intern = T) -> s3_listing
                             self$dirlist_s3[[bucket]] <- sapply(s3_listing,
                                                            function(path) {
                               folder_regex <- "\\s+PRE\\s"
                               file_regex <-
"[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}\\s+[0-9]+\\s"
                               
                               if (grepl(folder_regex, path))
                                 out_path <- gsub(folder_regex, "", path)
                               else if (grepl(file_regex, path))
                                 out_path <- gsub(file_regex, "", path)
                               else
                                 out_path <- NULL
                               
                               return(out_path)
                             })
                             names(self$dirlist_s3[[bucket]]) <- NULL
                           },
                           checkFileS3 = function(bucket, path) {
                             if (is.null(self$dirlist_s3[[bucket]]))
                               self$listS3(bucket)
                             
                             if (path %in% self$dirlist_s3[[bucket]])
                               return(TRUE)
                             else
                               return(FALSE)
                           }
),
private = list())


#' newS3
#' 
#' A wrapper function for generating S3Handler objects.
#'
#' @param access_key_id An AWS Access Key ID
#' @param secret_access_key An AWS Secret Access Key
#' @param credentials_file A JSON file with AWS access key ID and key
#' @param profile_name A profile name for or from ~/.aws/credentials
#'
#' @return An S3Handler object
#' @export
#'
#' @examples
#' # s3bucket <- newS3(credentals_file = "myawscreds.json")
#' 
#' # s3bucket <- newS3(profile_name = "default")
#' 
#' # s3bucket <- newS3(access_key_id = "Some_Access_Key_ID",
#' #                   secret_access_key = "Some_Secret_Access_Key")

#' # s3bucket <- newS3(access_key_id = "Some_Access_Key_ID",
#' #                   secret_access_key = "Some_Secret_Access_Key",
#' #                   profile_name = "newS3Profile")
#'                   
#' # s3bucket$listS3("mybucket")
#' 
#' # s3bucket$writeS3("mybucket", "~/Desktop/myfile.txt", "some_s3_file.txt",
#' #                  overwrite = TRUE)
newS3 <- function(access_key_id,
                  secret_access_key,
                  credentials_file,
                  profile_name) {
  return(S3Handler$new(access_key_id,
                       secret_access_key,
                       credentials_file,
                       profile_name))
}
