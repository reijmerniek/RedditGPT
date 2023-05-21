


#' Title
#'
#' @param vector
#' @param temperature
#' @param openai_api_key
#' @param model
#' @param questions
#' @param sleep
#'
#' @return
#' @export
#'
#' @examples
analyze_data_chatgpt <-
  function(vector,
           temperature = 0.3,
           openai_api_key,
           model = "gpt-3.5-turbo",
           questions = "question 1: summarise this:",
           sleep = 5) {


comments.vector <- as.vector(vector)

time <- Sys.time()
reacties <-NULL

for (i in 1:length(comments.vector)){
  Sys.sleep(sleep)
  list <- list(list("role"="user",
                    "content"=paste(questions,
                                    comments.vector[i])))

  tryCatch({

    response <- create_chat_completion(model = model,
                                       messages = list,
                                       temperature = temperature,
                                       n = 1,
                                       openai_api_key = openai_api_key)

    reactie <- response$choices$message.content
    reacties <-rbind(reacties,reactie)





  }, error = function(e) {
    print(paste0("Error occurred for iteration ", i))
    reactie <- "error occured"
    reacties <-rbind(reacties,reactie)
  })

}
}













