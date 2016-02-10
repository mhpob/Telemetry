#' Download ACT data base.
#'
#' \code{ACTupdate} connects to the
#' \href{http://www.theactnetwork.com/}{ACT Network} Dropbox data base using
#' \code{rdrop2}, downloads the selected sheet, and saves the sheet to RData
#' format. Note that you will have to log in and create an authorization token.
#' The token can be saved for later use.
#'
#' The function checks if a local version of the selected spreadsheet exists.
#' If it does not, or if the local version is older than the Dropbox version,
#' it downloads the file from Dropbox. The .xlsx is converted to RData and
#' deleted unless keep == T.
#'
#' @param sheet character. Name, or part of the name, of the desired sheet to
#'    download. This will be appended to the abbreviation "ACT" to name the
#'    local version of the downloaded ACT spreadsheet. Defaults to "active".
#' @param local.ACT character. Name of the local ACT spreadsheet within the
#'    working directory. Default is NULL.
#' @param keep logical. Should you keep the downloaded xlsx? Default is \code{F}.
#' @return Saves a copy of the current spreadsheet to your working directory in
#'    RData format. If keep == T, it also will save the orignial .xlsx.
#'
#' @seealso \code{\link{vemsort}}, \code{\link{ACTsplit}}
#' @export
#' @examples
#' ACTupdate()
#' ACTupdate(sheet = 'res')

ACTupdate <- function(sheet = 'active', local.ACT = NULL, keep = F){
  local.ACT <- ifelse(is.null(local.ACT),
                      paste0('ACT', sheet, '.rda'),
                      local.ACT)

  rdrop2::drop_auth()
  drop.ACT <- rdrop2::drop_search(sheet,
              path = 'ACT Network/ACT Transmitters, Researchers, and Arrays/')

  ACT_version_check <- function(){
    local.ACT.check <- list.files(getwd(), recursive = T,
                            pattern = local.ACT)

    if(length(local.ACT.check) > 0){
      local.mtime <- file.info(local.ACT.check)$mtime
      drop.mtime <- drop.ACT$client_mtime
      drop.mtime <- lubridate::dmy_hms(strsplit(drop.mtime, ',|[+]')[[1]][2])
      drop.mtime > local.mtime
      }else{
        TRUE
      }
  }

  if(ACT_version_check() == T){
    drop.loc <- drop.ACT %>% dplyr::select(path) %>% as.character()

    suppressMessages(rdrop2::drop_get(drop.loc, overwrite = T, progress = T))
    file <- strsplit(drop.loc, '/')[[1]][4]

    # For some reason, read_excel() doesn't read the numeric columns as numeric,
    # even when specifying explicitly. Read as text and convert to numeric later.
    # Allow read_excel to convert dates, though.
    classes <- switch(substr(file, 1, 3),
                      Act = c(rep('text', 6), 'date', 'text', 'date',
                              rep('text', 15)),
                      All = c(rep('text', 6), 'date', 'text', 'date',
                              rep('text', 16)),
                      Arc = c(rep('text', 6), 'date', 'text', 'date',
                              rep('text', 14)),
                      Res = rep('text', 9),
                      Unk = c(rep('text', 6), 'date', 'text', 'date',
                              rep('text', 15)))

    new.local.ACT <- readxl::read_excel(file, col_types = classes)

    if(keep == F){
      file.remove(file)
    }

    numeric.columns <- c('ID Standard', 'ID Sensor I',
                         'ID Sensor II', 'Tag Life')
    new.local.ACT[, names(new.local.ACT) %in% numeric.columns] <-
      sapply(new.local.ACT[, names(new.local.ACT) %in% numeric.columns],
             as.numeric)
    names(new.local.ACT) <- gsub(' ', '.', names(new.local.ACT))

    local.ACT <- strsplit(local.ACT, '[.]')[[1]][1]
    assign(local.ACT, new.local.ACT)
    eval(substitute(save(name, file = paste0(local.ACT, '.rda')),
                    list(name = local.ACT)))

  } else{
    message("You're using the current version of the ACT data base.")
  }
}
