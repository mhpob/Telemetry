#' @export trans_loss

trans_loss <- function(data, dates, group, stdate = NULL, enddate = NULL){

  if(is.null(stdate)){
    stdate <- lubridate::floor_date(min(data.frame(data)[, dates]),
                                    unit = 'day')
  }
  if(is.null(enddate)){
    enddate <- lubridate::floor_date(max(data.frame(data)[, dates]),
                                     unit = 'day')
  }

  data <- data %>%
    dplyr::group_by_(group) %>%
    dplyr::summarize_(last.record = lazyeval::interp(~max(x),
                                                     x = as.name(dates)))

  data$interval <- lubridate::interval(stdate, data$last.record)

  date.seq <- data.frame(date = seq(stdate, enddate, by = 'day'))
  date.seq$remaining <- sapply(date.seq$date,
              function(x) sum((lubridate::'%within%' (x, data$interval)) == T))

  date.seq
}
