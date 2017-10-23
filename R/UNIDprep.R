#' Assemble unidentified detections, VRL files, and VRL-RLD files for submission
#' to VEMCO
#'
#' \code{UNIDprep} creates a new folder containing a CSV file of unknown
#' transmitters along with the VRL and VRL-RLD files containing the original
#' detections
#'
#' The function aims to easily provide the files requested by
#' \href{http://www.vemco.com/}{VEMCO} to identify tag owners through its
#' \href{https://vemco.com/customer-service/?cs-unknown-ids}{unknown ID
#' service}. Transmitters that were not identified by the
#' \href{http://www.theactnetwork.com/}{ACT Network} (via
#' \code{\link{ACTsplit}}) are listed in a CSV file and the directory is
#' searched for corresponding VRL and VRL-RLD files. These, along with the tag
#' list, are exported directly into a new folder on your computer.
#'
#' @param unids Data frame. Output of \code{\link{ACTsplit}} containing unknown
#'   transmitters.
#' @param directory String. Location of files that contain the unidentified
#'   detections--most likely the same argument passed to \code{\link{ACTsplit}}
#'   or \code{\link{vemsort}}. Defaults to the working directory.
#' @param out String. Where do you want the new folder to be placed? Defaults to
#'   the working directory.
#' @return Output is a logical vector indicating whether VRL/VRL-RLD files were
#'   successfully found and copied (there are issues if they're not all
#'   \code{TRUE}), as well as a folder in the designated location called
#'   "Unknown for VEMCO" containing: \itemize{ \item A CSV file containing the
#'   unknown IDs ('unknown_ids.csv') \item VRL files containing the unknown
#'   detections and \item VRL-RLD files containing the unknown detections }
#'
#' @seealso \code{\link{vemsort}}, \code{\link{ACTsplit}}
#' @export
#' @examples
#' unknown <- ACTsplit('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs',
#'      write = F)
#' UNIDprep(unids = unknown,
#'      directory = 'C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs',
#'      out = 'C:/Users/mypcname/Desktop')

UNIDprep <- function(unids, directory = getwd(), out = getwd()){
  output_location <- file.path(out, 'Unknown for VEMCO')
  dir.create(output_location)

  Unks <- data.frame(Transmitters = unique(unids$transmitter))

  # List VRL and VRL-RLD files
  vrls <- gsub('csv', 'vrl',
                   unique(unids$file))
  vrl_rlds <- strsplit(vrls, '_')
  vrl_rld_front <- lapply(vrl_rlds, function(x) paste0(x[1], '-RLD'))
  vrl_rld_back <- lapply(vrl_rlds, function(x) {
    paste(unlist(x[2:length(x)]), collapse = '_')
    })
  vrl_rlds <- paste(vrl_rld_front, vrl_rld_back, sep = '_')
  vrl_all <- c(vrls, vrl_rlds)

  # Find location of files within directory
  vrl_locs <- unlist(
    lapply(vrl_all, function(x) list.files(path = directory, pattern = x,
                                           recursive = T, full.names = T))
    )

  # Output UNID list, copy VRL and VRL-RLDs into new folder.
  write.csv(Unks, file.path(output_location, 'unknown_ids.csv'),
            row.names = F)

  file.copy(from = vrl_locs,
            to = file.path(output_location, vrl_all))
}
