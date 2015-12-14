#' Habitat selectivity through quotient analysis
#'
#' \code{quo_an} executes the quotient approach defined in Overholtz et
#' al. 2011. Proportion of fish incidence in a water quality bin is divided by
#' the proportion of stations within that bin. Values greater than 1 indicate
#' preference.
#'
#' This approach was outlined in:
#'
#' Van Der Lingen, CD, L Hutchings, D Merkle, JJ van der Westhuizen, and J
#' Nelson. 2001. Comparative spawning habitats of anchovy (\emph{Engraulis
#' capensis}) and sardine (\emph{Sardinops sagax}) in the southern Benguela
#' upwelling ecosystem. 185-209. Spatial processes and management of marine
#' populations. Alaska Sea Grant College Program, AK-SG-01-02, Fairbanks.
#'
#' It was defined, as it appears here, in:
#'
#' Overholtz, W.J., Hare, J.A. and Keith, C.M., 2011. Impacts of interannual
#' environmental forcing and climate change on the distribution of Atlantic
#' mackerel on the US Northeast continental shelf. Marine and Coastal
#' Fisheries, 3(1), pp.219-232.
#'
#' The proportion of sites in each water quality bin (\emph{pSe}) is
#' determined by dividing the number of stations within the bin by the total
#' number of stations. The porportion of stations with fish (\emph{M}) in
#' each bin (\emph{pMe}) is determined by dividing the number of stations in the
#' bin with fish by the total number of stations with fish. The quotient value
#' for the water quality bin (\emph{Qe}) is calculated as \emph{pMe/pSe}.
#'
#' Values greater than 1 \dQuote{indicate a greater number of positive (fish)
#' stations than expected based on sampling effort}.
#'
#' @param wq Numeric. Water quality data at each station.
#' @param det Numeric. Number of detections at each station.
#' @param bin_width Numeric. Size of water quality bins. Default is 1.
#' @param pres_abs Logical. Should the data be reduced to presence/absence?
#'                Default is false.
#' @return Output is a data frame with bin labels, number of detections or
#'                number sites with detections (depending on \code{pres_abs}
#'                input) within each bin, number of sites within each bin, and
#'                pMe, pSe, and Qe as defined above.
#' @export


quo_an <- function(wq, det, bin_width = 1, pres_abs = F){
  # Create breaks, allowing for very small values.
  lims <- NULL
  lims[1] <- ifelse(abs(min(wq, na.rm = T)) < 1,
                    min(wq, na.rm = T) - abs(min(wq, na.rm = T)) / 5,
                    floor(min(wq, na.rm = T)))
  lims[2] <- ifelse(abs(max(wq, na.rm = T)) < 1,
                    max(wq, na.rm = T) + abs(max(wq, na.rm = T)) / 5,
                    ceiling(max(wq, na.rm = T)))
  brks <- seq(lims[1], lims[2], bin_width)
  brks <- if(lims[2] > max(brks)) c(brks, max(brks) + bin_width) else brks

  # Create grouping classes.
  classes <- cut(wq, brks)

  # Aggregate by wqironmental classes.
  if(pres_abs == T){
    # Presence/Absence
    fish <- data.frame(det, classes)
    fish <- fish[det > 0,]
    fish <- aggregate(det ~ classes, data = fish, FUN = length)
  } else{
    # Incidence
    fish <- aggregate(det ~ classes, FUN = sum)
  }

  station <- aggregate(wq ~ classes, FUN = length)

  q_an <- merge(fish, station, all = T)
  q_an[is.na(q_an)] <- 0

  q_an$pme <- q_an$det/sum(q_an$det)
  q_an$pse <- q_an$wq/sum(q_an$wq)

  q_an$qe <- q_an$pme/q_an$pse

  names(q_an) <- c('bin', 'detections', 'water.quality', 'pMe', 'pSe', 'Qe')
  q_an
}
