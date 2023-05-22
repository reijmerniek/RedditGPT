

#' Title
#'
#' @param subreddit
#' @param nsubmissions
#' @param max_error_conseq
#'
#' @return
#' @export
#'
#' @examples
get_submission_data <- function( subreddit, nsubmissions=10,max_error_conseq=3) {


  timestamp <- round(as.numeric(as.POSIXct(Sys.time(), tz = "UTC")),digits =0)
  data_submission_main <- NULL

  threshold_submissions <- nsubmissions*1000
  total_length <- 0
  i <- 1
  error_counter<-0
  error_indicator <- FALSE

  while (total_length < threshold_submissions) {

    tryCatch({
      response <- httr::GET(paste0("https://api.pushshift.io/reddit/search/submission/?subreddit=", subreddit, "&size=1000&before=", timestamp), timeout(10))
      result <- httr::content(response, as = "text", encoding = "UTF-8")
      data_submission <- result %>% jsonlite::fromJSON() %>%
        .$data %>% jsonlite::flatten() %>%
        select(selftext, author_fullname, title, created_utc, id,
               num_comments, score, permalink) %>% as_tibble()
      data_submission$datum <- as.POSIXct(data_submission$created_utc, origin = "1970-01-01")
      data_submission_main <- rbind(data_submission_main, data_submission)
      timestamp <- last(data_submission$created_utc)
      total_length <- length(data_submission_main$datum)
      i <- i + 1  # Increment the iterator
      error_indicator <- FALSE
      print(i)

    }, error = function(e) {
      print(paste0("Error occurred for iteration ", i))
      error_indicator <<- TRUE
      if (error_indicator == TRUE) {
        error_counter <<- error_counter +1
      }
      if (error_indicator == TRUE & error_counter > max_error_conseq ) {
        stop('stop')
      }else{

        error_counter<-0
      }
    })
  }

return(data_submission_main)

}
