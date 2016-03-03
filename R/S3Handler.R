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
                           data_dir = "~/.VitalSignsUtilities/data_files/",
                           profile_name = NULL,
                           dirlist_s3 = list(),
                           initialize = function(access_key_id,
                                                 secret_access_key,
                                                 credentials_file,
                                                 profile_name,
                                                 data_dir = NULL) {
                             if (!is.null(data_dir)) self$data_dir <- data_dir
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

                             system2("aws", sprintf("s3 cp %s s3://%s --profile %s",
                                            source_path,
                                            file.path(bucket, target_path),
                                            self$profile_name),
                                     stdout = TRUE, stderr = TRUE) -> s3_write_results
                             message(s3_write_results)
                           },
                           listS3 = function(bucket, path = "") {
                             s3_path <- file.path(bucket, path)
                             system2("aws", sprintf("s3 ls %s --recursive --profile %s",
                                            s3_path,
                                            self$profile_name),
                                     stdout = TRUE, stderr = TRUE) -> s3_listing
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
                           },
                           sync = function(src_files, destination = ".", dryrun=NULL, include=NULL, exclude=NULL, 
                                             acl=NULL, no.follow.symlinks=NULL, no.guess.mime.type=NULL, sse=NULL, 
                                             sse.c=NULL, sse.c.key=NULL, sse.kms.key.id=NULL, sse.c.copy.source=NULL, 
                                             sse.c.copy.source.key=NULL, storage.class=NULL, grants=NULL, website.redirect=NULL, 
                                             content.type=NULL, cache.control=NULL, content.disposition=NULL, content.encoding=NULL, 
                                             content.language=NULL, expires=NULL, source.region=NULL, only.show.errors=NULL, 
                                             page.size=NULL, ignore.glacier.warnings=NULL, metadata=NULL, metadata.directive=NULL, 
                                             size.only=NULL, exact.timestamps=NULL, delete=NULL) {

                             # arguments as documented in s3 CLI documentation: http://docs.aws.amazon.com/cli/latest/reference/s3/sync.html
                             setwd(self$data_dir)
                             print(include)
                             args <- c("s3", "sync", src_files, destination,
                                       ifelse (!is.null(dryrun),	"--dryrun", ""),
                                       ifelse (!is.null(exclude), paste0("--exclude \"", exclude, "\""), ""),
                                       ifelse (!is.null(include), paste0("--include \"", include, "\""), ""),
                                       ifelse (!is.null(acl), c("--acl", acl), ""),
                                       ifelse (!is.null(no.follow.symlinks), "--no-follow-symlinks", ""), # default behavior is to follow
                                       ifelse (!is.null(no.guess.mime.type), "--no-guess-mime-type", ""),
                                       ifelse (!is.null(sse), c("--sse", sse), ""),
                                       ifelse (!is.null(sse.c), c("--sse-c", sse.c), ""),
                                       ifelse (!is.null(sse.c.key), c("--sse-c-key", sse.c.key), ""),
                                       ifelse (!is.null(sse.kms.key.id),	c("--sse-kms-key-id", sse.kms.key.id), ""),
                                       ifelse (!is.null(sse.c.copy.source), c("--sse-c-copy-source", sse.c.copy.source), ""),
                                       ifelse (!is.null(storage.class), c("--storage-class", storage.class), ""),
                                       ifelse (!is.null(grants),	c("--grants", grants), ""),
                                       ifelse (!is.null(website.redirect),	c("--website-redirect", website.redirect), ""),
                                       ifelse (!is.null(content.type),	c("--content-type", content.type), ""),
                                       ifelse (!is.null(cache.control), c("--cache-control", cache.control), ""),
                                       ifelse (!is.null(content.disposition), c("--content-disposition", content.disposition), ""),
                                       ifelse (!is.null(content.encoding),	c("--content-encoding", content.encoding), ""),
                                       ifelse (!is.null(content.language),	c("--content-language", content.language), ""),
                                       ifelse (!is.null(expires), c("--expires", expires), ""),
                                       ifelse (!is.null(source.region), c("--source-region", source.region), ""),
                                       ifelse (!is.null(only.show.errors),	c("--only-show-errors"), ""),
                                       ifelse (!is.null(page.size), c("--page-size", page.size), ""),
                                       ifelse (!is.null(ignore.glacier.warnings), "--ignore-glacier-warnings", ""),
                                       ifelse (!is.null(metadata), c("--metadata", metadata), ""),
                                       ifelse (!is.null(metadata.directive),	c("--metadata-directive", metadata.directive), ""),
                                       ifelse (!is.null(size.only), "--size-only", ""),
                                       ifelse (!is.null(exact.timestamps), "--exact-timestamps", ""),
                                       ifelse (!is.null(delete), "--delete", "")				
                             )

                             commonFilePath <- function (path1, path2) {
                               
                               path_chunks <- strsplit(c(path1, path2), "/")
                               reversed_path_chunks <- lapply(path_chunks, rev)
                               
                               i <- 1
                               repeat({
                                 current_chunk <- sapply(reversed_path_chunks, function(x) x[i])
                                 if(any(current_chunk != current_chunk[1])) break
                                 i <- i + 1
                               })
                               
                               longest_common <- paste(rev(reversed_path_chunks[[1]][seq_len(i-1)]), collapse="/")
                               return(longest_common)
                               
                             }
                             
                             # NOTE: s3 cli output is ALWAYS text - no json to be found unfortunately
                             # use system2 to enable getting exit status to detect errors
                             sync_output <- system2("aws", args, stdout=TRUE, stderr=TRUE)
                             
                             # if system2 has attributes (this will be sync_output$status if it exists), an error has occurred
                             if (!is.null(attributes(sync_output))) stop(paste("S3 CLI exited with error: ", sync_output))
                             
                             if (!is.null(debug)) print(sync_output)
                             
                             changed_files <- sapply(sync_output, function(output_line) {
                               sync_output_words <- strsplit(output_line, " ")[[1]]
                               file_indices <- match("to", sync_output_words)
                               commonFilePath(sync_output_words[file_indices-1], sync_output_words[file_indices+1])
                             }, USE.NAMES=FALSE)
                             
                             return(ifelse(length(changed_files)==0, "Files already up to date!", changed_files))
                             
                           },
                           syncAll = function(s3_bucket,
                                              s3_dir = "",
                                              source_dir = self$data_dir) {
                             self$sync(file.path(s3_bucket, s3_dir), source_dir)
                           },
                           syncS3 = function(s3bucket, sync_file) {
                             setwd(self$data_dir)
                             self$sync(s3bucket, exclude = "*", include = sync_file)
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
                  profile_name = "default",
                  data_dir) {
  return(S3Handler$new(access_key_id,
                       secret_access_key,
                       credentials_file,
                       profile_name,
                       data_dir))
}
