


#' Title
#'
#' @param subreddit
#' @param nsubmissions
#' @param ncomments
#' @param max_error_conseq
#' @param subset
#' @param subsetn
#'
#' @return
#' @export
#'
#' @examples
#'
get_full_dataset<- function( subreddit="",nsubmissions=10, ncomments=10,max_error_conseq=3,subset=TRUE,subsetn =13000) {

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

data_submission_main$link_id <- paste0("t3_",data_submission_main$id)

timestamp <- round(as.numeric(as.POSIXct(Sys.time(), tz = "UTC")),digits =0)
data_comment_main <- NULL


threshold_comments <- ncomments*1000
total_length <- 0
i <- 1
error_counter<-0
error_indicator <- FALSE

while (total_length < threshold_comments) {

  tryCatch({
    response <- httr::GET(paste0("https://api.pushshift.io/reddit/search/comment/?subreddit=", subreddit, "&size=1000&before=", timestamp), timeout(10))
    result <- httr::content(response, as = "text", encoding = "UTF-8")

    data_comment <- result %>% jsonlite::fromJSON() %>%
      .$data %>% jsonlite::flatten() %>%
      select(body, author_fullname, created_utc, id,
             permalink, link_id) %>% as_tibble()
    data_comment$datum <- as.POSIXct(data_comment$created_utc, origin = "1970-01-01")
    data_comment_main <- rbind(data_comment_main, data_comment)
    timestamp <- last(data_comment_main$created_utc)
    submission_length <- nrow(data_comment)  # Get the length of the current data_comment
    total_length <- total_length + submission_length  # Update the total length
    i <- i + 1  # Decrement the iterator
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



suppressWarnings({
  full_df <- left_join(data_submission_main, data_comment_main, by= c("link_id"))
})



full_df$author_title_text <- paste0("Forumpost Author: ", full_df$author_fullname.x," \nForumpost Title: ", full_df$title,"\nForumpost text: ", full_df$selftext )
full_df$author_comment <- paste0("\nComment Author: ", full_df$author_fullname.y,"\nComment text: ", full_df$body )

full_df_grouped <- full_df %>% group_by(link_id) %>% summarise(author_title_text = first(author_title_text),
                                                               author_comment =paste(author_comment,collapse = " "))

full_df_grouped$author_comment <-gsub("\nComment Author: \nComment text: NA","",full_df_grouped$author_comment)

full_df_grouped$text <- paste(full_df_grouped$author_title_text, "\n", full_df_grouped$author_comment)
full_df_grouped$length <- nchar(full_df_grouped$text)

if(subset==TRUE){
  full_df_grouped <- subset(full_df_grouped, full_df_grouped$length < subsetn)
  return(full_df_grouped)

} else {
  return(full_df_grouped)
}


}
