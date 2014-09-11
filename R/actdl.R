# library(repmis); library(xlsx)
# 
# test <- source_data('https://www.dropbox.com/s/fmaylsaju8v0w8r/Active%20transmitters%209-2-14.xlsx?dl=0')
# 
# test <- read.xlsx2('https://dl.dropboxusercontent.com/s/fmaylsaju8v0w8r/Active%20transmitters%209-2-14.xlsx', sheetIndex = 1, header = T)
# test <- read.xlsx('https://dl.dropboxusercontent.com/s/fmaylsaju8v0w8r/Active%20transmitters%209-2-14.xlsx', sheetName = 'Active_Transmitters')
# 
# test <- testfun(file = 'Active%20transmitters%209-2-14.xlsx',
#                            key = 'fmaylsaju8v0w8r', sheet = 1)
# 
# #source code of source_DropboxData
# 
# testfun <-
# function (file, key, sheet = NULL, sha1 = NULL, cache = FALSE, clearCache = FALSE,
#           ...){
#     stopifnot(!is.null(sheet))
#     if (length(sheet) != 1) {
#         sheet <- sheet[1]
#         warning("Only one sheet can be returned per call. The first sheet will be returned.\n", 
#             call. = FALSE)
#     }
#     url <- paste0("https://dl.dropboxusercontent.com/s/", key, 
#         "/", file)
#     temp_file <- tempfile()
#     on.exit(unlink(temp_file))
#     key <- list(url, sheet)
#     if (isTRUE(clearCache)) {
#         Found <- findCache(key = key)
#         if (is.null(Found)) {
#             message("Data not in cache. Nothing to remove.")
#         }
#         else if (!is.null(Found)) {
#             message("Clearing data from cache.")
#             file.remove(Found)
#         }
#     }
#     if (isTRUE(cache)) {
#         data <- loadCache(key)
#         if (!is.null(data)) {
#             message("Loading cached data.\n")
#             message("Use clearCache = TRUE if you would like to have different arguments passed to read.xlsx.\n")
#             return(data)
#         }
# #         fullData <- download_data_intern(url = url, sha1 = sha1, 
# #             temp_file = temp_file)
#         if (class(sheet) == "character") {
#             data <- read.xlsx(fullData, sheetName = sheet, ...)
#         }
#         else if (class(sheet) != "character") {
#             data <- read.xlsx(fullData, sheetIndex = sheet, ...)
#         }
#         saveCache(data, key = key)
#         data
#     }
#     else if (!isTRUE(cache)) {
# #         fullData <- download_data_intern(url = url, sha1 = sha1, 
# #             temp_file = temp_file)
#         if (class(sheet) == "character") {
#             data <- read.xlsx(url, sha1, sheetName = sheet, ...)
#         }
#         else if (class(sheet) != "character") {
#             data <- read.xlsx(url, sha1, sheetIndex = sheet, ...)
#         }
#         return(data)
#     }
# }