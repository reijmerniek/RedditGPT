


get_comments_data<- function( subreddit, ncomments=10,max_error_conseq=3, grouped=FALSE,subset=TRUE,subsetn =13000) {


  timestamp <- round(as.numeric(as.POSIXct(Sys.time(), tz = "UTC")),digits =0)
  data_comment_main <- NULL
  threshold_comments <- ncomments*1000
  total_length <- 0
  i <- 1
  error_counter<-0
  error_indicator <- FALSE

  while (total_length < threshold_comments) {

    tryCatch({
      response <- GET(paste0("https://api.pushshift.io/reddit/search/comment/?subreddit=", subreddit, "&size=1000&before=", timestamp), timeout(10))
      result <- content(response, as = "text", encoding = "UTF-8")

      data_comment <- result %>% jsonlite::fromJSON() %>%
        .$data %>% jsonlite::flatten() %>%
        select(body, author_fullname, created_utc, id,
               permalink, link_id) %>% as_tibble()
      data_comment$datum <- as.POSIXct(data_comment$created_utc, origin = "1970-01-01")
      data_comment_main <- rbind(data_comment_main, data_comment)
      timestamp <- last(data_comment_main$created_utc)
      submission_length <- nrow(data_comment)  # Get the length of the current data_comment
      total_length <- total_length + submission_length  # Update the total length
      error_indicator <- FALSE
      print(i)
      i <- i + 1  # Decrement the iterator


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

  if(grouped== TRUE){

    data_comment_main$author_comment <- paste0("\nComment Author: ", data_comment_main$author_fullname,"\nComment text: ", data_comment_main$body )
    data_comment_main_grouped <- data_comment_main %>% group_by(link_id) %>% summarise(author_comment =paste(author_comment,collapse = " "))
    data_comment_main_grouped$length <- nchar(data_comment_main_grouped$author_comment)
    data_comment_main_grouped$author_comment <-gsub("\nComment Author: \nComment text: NA","",data_comment_main_grouped$author_comment)
    if(subset==TRUE){
      data_comment_main_grouped <- subset(data_comment_main_grouped, data_comment_main_grouped$length < subsetn)

    }

    return(data_comment_main_grouped)


  } else {
    return(data_comment_main)
  }

}
