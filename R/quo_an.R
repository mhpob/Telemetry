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


quo_an <- function(wq, det, bin_width = 1, pres_abs = F, R = 999){
  # Create breaks.
  minval <- min(wq, na.rm = T)
  maxval <- max(wq, na.rm = T)

  brks <- seq(minval, maxval, bin_width)
  brks <- if(maxval > max(brks)) c(brks, max(brks) + bin_width) else brks

  # Create grouping bins.
  bins <- cut(wq, brks)

  # Aggregate by environmental bins.
  boot_func <- function(x, index){
    strap <- x[index]
    if(pres_abs == T){
      # Presence/Absence
      strap <- strap > 0
      fish <- aggregate(strap ~ bins, FUN = sum)
    } else{
      # Incidence
      fish <- aggregate(strap ~ bins, FUN = sum)
    }
    as.vector(fish$strap)/sum(fish$strap)
  }

  strap <- boot::boot(det, boot_func, R)

  #Confidence Interval
  c_i <- NULL
  for(i in 1:length(strap$t0)){
    hold <- boot::boot.ci(strap, type = 'perc', index = i)$percent[4:5]
    c_i <- rbind(c_i, hold)
  }
  row.names(c_i) <- NULL


  if(pres_abs == T){
    # Presence/Absence
    p_ab <- det > 0
    fish <- aggregate(p_ab ~ bins, FUN = sum)
    names(fish) <- c('bins', 'det')
  } else{
    # Incidence
    fish <- aggregate(det ~ bins, FUN = sum)
  }

  station <- aggregate(wq ~ bins, FUN = length)

  # Merge data and correctly order bins.
  q_an <- merge(fish, station)
  q_an[is.na(q_an)] <- 0

  # Quotient analysis
  q_an$pme <- q_an$det / sum(q_an$det)
  q_an$pse <- q_an$wq / sum(q_an$wq)

  q_an$qe <- q_an$pme / q_an$pse

  q_an$ci.025 <- c_i[, 1] / q_an$pse
  q_an$ci.975 <- c_i[, 2] / q_an$pse

  names(q_an) <- c('bin', 'detections', 'wq.var', 'pMe', 'pSe',
                   'Qe', 'CI_0.025', 'CI_0.975')
  q_an
}


test <- read.csv('c:/users/secor lab/downloads/q_an_test.csv')
wq <- test$temp
det <- test$catch
bin_width = 0.2
pres_abs = T
ci = T
R=999
#take these and divide by pse to get qe bounds