#' Download ACT data base.
#'
#' \code{ACTupdate} connects to the
#' \href{http://www.theactnetwork.com/}{ACT Network} Dropbox data base,
#' downloads the transmitter list, and converts the list to RData format. In
#' order to access Dropbox, you'll need to log in and create an authorization
#' token. The token can be saved for later use.
#'
#' The function checks if a local version of the ACT transmitter data base
#' exists. If it does not, or if the local version is older than the Dropbox
#' version, it downloads the file from Dropbox. The .xlsx is converted to RData
#' and deleted unless keep == T.
#'
#' @param keep logical. Should you keep the downloaded xlsx? Default is \code{F}.
#' @return Saves a copy of the current "Active transmitters mm-dd-yy.xlsx"
#'    spreadsheet to your working directory in RData format as "ACTtrans.rda".
#'    If keep == T, it also will save the orignial .xlsx.
#' @seealso \code{\link{vemsort}}, \code{\link{ACTsplit}}
#' @export
#' @examples
#' ACTupdate()

ACTupdate <- function(keep = F){
  rdrop2::drop_auth()
  drop.ACT <- rdrop2::drop_search('Active')

  ACT_version_check <- function(){
    local.ACT <- list.files(getwd(), recursive = T,
                            pattern = 'ACTtrans.rda')

    if(length(local.ACT) > 0){
      local.ctime <- file.info(local.ACT)$ctime
      drop.ctime <- drop.ACT$client_mtime
      drop.ctime <- lubridate::dmy_hms(strsplit(drop.ctime, ',|[+]')[[1]][2])
      drop.ctime > local.ctime
      }else{
        TRUE
      }
  }

  if(ACT_version_check() == T){
    drop.loc <- drop.ACT %>% dplyr::select(path) %>% as.character()

    suppressMessages(rdrop2::drop_get(drop.loc, overwrite = T, progress = T))
    file <- grep('Active', unlist(strsplit(drop.loc, '/')), value = T)

    # For some reason, read_excel() doesn't read the numeric columns as numeric,
    # even when specifying explicitly. Read as text and convert to numeric later.
    ACTtrans <- readxl::read_excel(file,
                                   col_types = c(rep('text', 6),
                                                 'date', 'text', 'date',
                                                 rep('text', 15)))
    if(keep == F){
      file.remove(file)
    }

    numeric.columns <- c('ID Standard', 'ID Sensor I',
                         'ID Sensor II', 'Tag Life')
    ACTtrans[, numeric.columns]<- sapply(ACTtrans[, numeric.columns],
                                         as.numeric)
    names(ACTtrans) <- gsub(' ', '.', names(ACTtrans))

    save(ACTtrans, file = 'ACTtrans.rda')
  } else{
    message("You're using the current version of the ACT data base.")
  }
}
