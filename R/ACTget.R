
ACTupdate <- function(){
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
    file.remove(file)

    numeric.columns <- c('ID Standard', 'ID Sensor I',
                         'ID Sensor II', 'Tag Life')
    ACTtrans[, numeric.columns]<- sapply(ACTtrans[, numeric.columns],
                                         as.numeric)

    save(ACTtrans, file = 'ACTtrans.rda')
  } else{
    message('Using current version of ACT data base.')
  }
}
