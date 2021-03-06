% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/S3Handler.R
\name{newS3}
\alias{newS3}
\title{newS3}
\usage{
newS3(access_key_id, secret_access_key, credentials_file, profile_name)
}
\arguments{
\item{access_key_id}{An AWS Access Key ID}

\item{secret_access_key}{An AWS Secret Access Key}

\item{credentials_file}{A JSON file with AWS access key ID and key}

\item{profile_name}{A profile name for or from ~/.aws/credentials}
}
\value{
An S3Handler object
}
\description{
A wrapper function for generating S3Handler objects.
}
\examples{
# s3bucket <- newS3(credentals_file = "myawscreds.json")

# s3bucket <- newS3(profile_name = "default")

# s3bucket <- newS3(access_key_id = "Some_Access_Key_ID",
#                   secret_access_key = "Some_Secret_Access_Key")
# s3bucket <- newS3(access_key_id = "Some_Access_Key_ID",
#                   secret_access_key = "Some_Secret_Access_Key",
#                   profile_name = "newS3Profile")

# s3bucket$listS3("mybucket")

# s3bucket$writeS3("mybucket", "~/Desktop/myfile.txt", "some_s3_file.txt",
#                  overwrite = TRUE)
}

