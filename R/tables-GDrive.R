# This is a package for Conservation International's Vital Signs project.
# The package can be found at:
# github.com/ConservationInternational/RCode/rpackages/vitalsigns/
# 
# The purpose of this package is to provide:
# 1) Streamlined access to Vital Signs Data on Google Drive
# 2) General enhancements to Vital Signs code
#
# To install this package, install the devtools R package:
#
## install.packages("devtools")
#
# Then generate a token on github:
# 1) Make sure you're logged in,
# 2) Then go to this url: https://github.com/settings/tokens
# 3) Name the token something like "vitalsigns R package"
# 4) copy the provided token and save it in R using this command
#    (with your token in place of YOUR_TOKEN, of course):
#
## Sys.setenv("GITHUB_PAT" = "YOUR_TOKEN")
# 
# 5) And install using this command
#    (again, with your github username in place of YOUR_USERNAME):
#
## install_github("ConservationInternational/Rcode/packages/vitalsigns",
##                username = "YOUR_USERNAME",
##                auth_token = github_pat())
#
# 6) And then load it like any other package:
#
## library(vitalsigns)
#


#' vital_signs_tables
#'
#' Wrapper function for VitalSignsData
#'
#' @param client_id A Google App Engine client ID
#' @param client_secret A Google App Engine client secret
#' @param creds_file A JSON file with Google App Engine credentials in it
#' (preferred)
#' @param scopes A set of API scopes for Google Drive
#' @param service A boolean of whether or not the credentials are for a service
#' account
#'
#' @return Returns a VitalSignsData class object
#' @export
#'
#' @examples
#' 
#' # vstables <- vital_signs_tables(creds_file = "credentials.json")
#' 
#' # vstables <- vital_signs_tables(creds_file = "service_credentials.json",
#' #                                service = T)
#'
#' # vstables <- vital_signs_tables(client_id = "blablabla",
#' #                                client_secret = "blublublu")
vital_signs_tables <- function(client_id, client_secret, creds_file,  scopes, service = F) {
if ((missing(client_id) || missing(client_secret))) {
  auth_type <- -1
  if (missing(creds_file)) {
    auth_type <- NULL
    while(!(auth_type %in% c("1","2"))) {
      message("Would you like to provide a file or the client id and
               secret?")
      message("Please make a choice:")
      message("1) Credentials file")
      message("2) Client ID and Secret")
      auth_type <- readline("Selection: ")
    }
  }
  else
    credentials <- jsonlite::fromJSON(txt=creds_file)
  if (auth_type == 1) {
    message("Please supply the path to the JSON file:")
    creds_file <- readline("> ")
    credentials <- jsonlite::fromJSON(txt=creds_file)
  }
  if ((!missing(creds_file) || auth_type == 1) & (!service))
    return(VitalSignsData$new(credentials$web$client_id, credentials$web$client_secret))
  else if (service & !missing(creds_file))
    return(VitalSignsData$new(service_credentials = credentials))
  else if (auth_type == 2) {
    message("Please supply your")
    client_id <- readline("client ID: ")
    client_secret <- readline("client secret: ")
    return(VitalSignsData$new(client_id, client_secret))
  }
}
else if (missing(scopes))
  return(VitalSignsData$new(client_id, client_secret))
else
  return(VitalSignsData$new(client_id, client_secret, scopes))
}

#' Class used to warehouse tables. Uses VitalSignsTable to handle tables.
#'
#'
#' @section Methods:
#' \itemize{
#'  \item \code{checkToken(): Checks whether oauth token is still valid.}
#'  \item \code{tableListMaker(datatables, tableType, tableStore): Deprecated
#'  function meant to serve getTables.}
#'  \item \code{getTables(tableType, initialFolderID, tableSave): Retrieves
#'  tables and stores them in a list as VitalSignsTable objects.}
#'  \item \code{getInternalData(): Loads only data from the Internal Data
#'  Folder.}
#'  \item \code{getExternalData(): Loads only data from the External Data
#'  Folder.}
#'  \item \code{showTables(): Shows the names of loaded tables.}
#'  \item \code{saveData = function(newData,
#'                                  newTitle,
#'                                  dataType,
#'                                  parent_folder = self$vsoutputfolderid,
#'                                  file_loc = NULL): Saves data based on
#'  the parameters provided by the user.}
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name VitalSignsData-class
VitalSignsData <- R6::R6Class("VitalSignsData",
 public = list(
   token = NULL,
   tables = list(),
   output.tables = list(),
   initialize = function(client_id,
                         client_secret,
                         auth_scopes =
                             paste0("https://www.googleapis.com/auth/",
                                    c("drive",
                                      "drive.file",
                                      "drive.readonly",
                                      "drive.metadata.readonly",
                                      "drive.appdata",
                                      "drive.apps.readonly",
                                      "drive.metadata")),
                         service_credentials) {
     if (!missing(service_credentials)) {
       print(service_credentials)
       self$token <- httr::oauth_service_token(httr::oauth_endpoints("google"),
                                               service_credentials,
                                               paste(auth_scopes, collapse = " "))
     } else {
       gapp <- httr::oauth_app("google", client_id, client_secret)
       self$token <- httr::oauth2.0_token(httr::oauth_endpoints("google"),
                                          gapp,
                                          auth_scopes)
     }
    
   },
   checkToken = function() {
     if (self$token$validate() == FALSE)
       tryCatch(self$token$refresh(),
                error = tryCatch(self$token$init_credentials(),
                                 error = stop("Unable to authenticate")))
   },
   tableListMaker = function(datatables, tableType, tableStore) {
     tables <- sapply(datatables$items,
                         function(X) {
                           X$table_type <- tableType
                           tableOut <- list(VitalSignsTable$new(X, self$token))
                           print(X$title)
                           names(tableOut) <- X$title
                           return(tableOut)
                         })
     tableStore <- append(tableStore, tables)
     return(tableStore)
   },
   getTables = function(tableType, initialFolderID, tableSave) {
     self$checkToken()
     items <- list()
     initial.tables.list <- httr::GET(
       "https://www.googleapis.com/drive/v2/files",
       query = list(access_token = self$token$credentials$access_token,
                    "q" = gsub("FOLDER_ID",
                               initialFolderID,
                               '"FOLDER_ID" in parents')))
     initial.tables.content <- httr::content(initial.tables.list)
     itemData <- do.call(rbind,
                         lapply(initial.tables.content$items,
                          function(X) {
                            return(data.frame("id" = X$id,
                                              "mimeType" = X$mimeType,
                                              "title" = X$title))
                          }))
     print(initial.tables.list)
     folders <- subset(itemData, mimeType == private$foldermimeType)
     items <- append(items, Filter(function(Y) {
                                     return(Y$mimeType != private$foldermimeType)
                                   }, initial.tables.content$items))
     while(nrow(folders) > 0 & !is.null(folders)) {
       plyr::ddply(folders, names(folders), function(folder){
         print(folder)
         tables.list <- httr::GET(
           "https://www.googleapis.com/drive/v2/files",
           query = list(access_token = self$token$credentials$access_token,
                        "q" = gsub("FOLDER_ID",
                                   folder$id,
                                   '"FOLDER_ID" in parents')))
         tables.content <- httr::content(tables.list)
         sub.items <- Filter(function(Y) {
                               return(Y$mimeType != private$foldermimeType)
                             },
                             tables.content$items)
         sub.items <- lapply(sub.items,
                             function(item) {
                               item$title <- paste0(folder$title, "/", item$title)
                               return(item)
                             })
         items  <<- append(items, sub.items)
         #print(items)
         folders.items <- Filter(function(Y) {
                                   return(Y$mimeType == private$foldermimeType)
                                 },
                                 tables.content$items)
         folders.next <- do.call(rbind,
                                 lapply(folders.items,
                                        function(Z) {
                                          return(data.frame("id" = Z$id,
                                                            "mimeType" = Z$mimeType,
                                                            "title" = paste0(folder$title,
                                                                             "/",
                                                                             Z$title)))
                                        }))
         return(folders.next)
       }, .progress = "text") -> folders.next
       folders <- folders.next
     }
     
     tableNames <- sapply(items,
                          function(X) {
                            return(X$title)
                          })
     print(tableNames)
     tableSave.temp <- plyr::llply(items,
                           function(item) {
                             item$table_type <- tableType
                             return(VitalSignsTable$new(item, self$token))
                           })
     names(tableSave.temp) <- tableNames
     return(append(tableSave, tableSave.temp))
   },
   getInternalData = function() {
     self$tables <- self$getTables("InternalData", private$internaldatafolderid, self$tables)
   },
   getExternalData = function() {
     self$tables <- self$getTables("ExternalData", private$externaldatafolderid, self$tables)
   },
   showTables = function() {
     if (is.null(self$tables) || (is.list(self$tables) & length(self$tables) == 0))
       stop("No tables loaded.")
     else
       return(names(self$tables))
   },
   saveData = function(newData, newTitle, dataType, parent_folder = self$vsoutputfolderid, file_loc = NULL, ... ) {
     self$getTables("VSOutputData", private$vsoutputfolderid, self$output.tables)
     drive_url <- "https://www.googleapis.com/upload/drive/v2/files"
     if (newTitle %in% names(self$output.tables))
       drive_url <- paste0(drive_url, "/", self$output.tables[[newTitle]]$id)
     
     if (dataType == "csv") {
       data_buffer <- tempfile(fileext = ".csv")
       write.csv(newData, data_buffer, row.names = F)
       mime_type <- "text/csv"
     } else if (dataType == "raster") {
       data_buffer <- tempfile(fileext = ".tif")
       if (!missing(raster_format))
         raster::writeRaster(newData, data_buffer, format = raster_format)
       else
         raster::writeRaster(newData, data_buffer)
       mime_type <- "image/tiff"
     } else if (dataType == "shapefile") {
       if (!(typeof(newData) %in% c("SpatialLinesDataFrame",
                                    "SpatialPolygonsDataFrame",
                                    "SpatialPointsDataFrame")))
         stop("The object trying to be saved cannot be saved as a shapefile.")
       temp_dir <- tempdir()
       data_buffer <- tempfile(fileext = ".shp")
       rgdal::writeOGR(newData,
                       data_buffer,
                       data_buffer,
                       driver = "ESRI Shapefile")
       mime_type <- "application/octet-stream"
     } else {
       mime_type <- mime::guess_type(file_loc)
       data_buffer <- file_loc
     }
     
     
     drive_json <- list(
       parents = list(list("id" = parent_folder)),
       title = ifelse(is.null(newTitle), basename(file_loc), newTitle),
       mimeType = mime_type
     )
     
     metadata <- tempfile(fileext = ".json")
     writeLines(jsonlite::toJSON(drive_json), metadata)
     metadata_upload <- httr::upload_file(metadata,
                                          type = "application/json; charset=UTF-8")
     upload_buffer <- httr::upload_file(data_buffer, mime_type)
     
     driveresponse <- httr::POST(drive_url,
                                 query = list(access_token = self$token$credentials$access_token),
                                 encode = "multipart",
                                 httr::add_headers(
                                   "Content-Type" = "multipart/related"),
                                 body=list(
                                   metadata = metadata_upload,
                                   media = upload_buffer))

     print(driveresponse)
     message(newTitle, " saved.")
   }),
 private = list(
   foldermimeType = "application/vnd.google-apps.folder",
   internaldatafolderid =
       "0B6cImLYRWuMZSjNYQ1JWdmptOEE",
   externaldatafolderid =
       "0B_xWBYveFIdUfmxXSktNcDRwSXRGMlBtSTRiTkoxRVdRMWdhNzJORDdhVldiVHl6ZUNHdjA",
   vsoutputfolderid = "0B_xWBYveFIdUfkd1UGk2R2pIWHpBSlhjM1Z6STI5MTh5VGxyUU5oUWtRWlhqeWd5N2V4b3M")
)


#' VitalSignsTable
#'
#' Class used to manage individual tables.
#'
#'
#' @section Methods:
#' \itemize{
#'  \item \code{getData(returnData, raw): Loads the data from Google Drive.
#'  Optionally returns the data, but defaults to not returning it. Also allows
#'  the user to choose whether they want the data in raw binary or auto-
#'  matically processed as a .csv, .dta or .tiff geotiff raster file.}
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name VitalSignsTable-class
VitalSignsTable <- R6::R6Class("VitalSignsTable",
  public = list(
    table_type = NULL,
    title = NULL,
    newtitle = NULL,
    id = NULL,
    newid = NULL,
    downloadurl = NULL,
    table = NULL,
    auth_token = NULL,
    mimeType = NULL,
    parents = NULL,
    initialize = function(item, auth_token) {
      self$auth_token <- auth_token
      self$table_type <- item$table_type
      self$id <- item$id
      self$title <- item$title
      self$mimeType = item$mimeType
      self$parents = sapply(item$parents, function(X) return(X$id))
      self$downloadurl <- item$downloadUrl
      if (self$mimeType == "application/vnd.google-apps.spreadsheet")
        self$downloadurl <- item$exportLinks$`text/csv`
    },
    getData = function(returnData = FALSE, raw = FALSE) {
      print(self$auth_token)
      self$table <- httr::content(httr::GET(self$downloadurl,
                                  query = list(access_token = self$auth_token$credentials$access_token)))
      if (self$mimeType == "text/plain" & !raw) {
        vstf <- tempfile()
        writeLines(self$table, vstf)
        self$table <- read.table(vstf, header = T, sep = "\t")
      }
      else if (self$mimeType == "application/octet-stream" &
          substr(self$title,
                 nchar(self$title) - 2,
                 nchar(self$title)) == "dta" & !raw) {
        vstf <- tempfile()
        writeBin(self$table, vstf)
        self$table <- foreign::read.dta(vstf)
      }
      else if (self$mimeType == "image/tiff" & !raw) {
        vstf <- tempfile(fileext = ".tif")
        writeBin(self$table, vstf)
        self$table <- raster::raster(vstf)
      }
      if (returnData == TRUE)
        return(self$table)
      else
        return(str(self$table))
  }),
  private = list(
    "IndicatorOutput" =
      "0B_xWBYveFIdUfkd1UGk2R2pIWHpBSlhjM1Z6STI5MTh5VGxyUU5oUWtRWlhqeWd5N2V4b3M"
  )
)
