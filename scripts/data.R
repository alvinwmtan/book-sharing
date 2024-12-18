SOURCES <- c("Book text", "Read-aloud", "Spont-book", 
             "Spont-other")

get_data <- function(dir_path, source_name, file_name = NULL){
  full_tokens_df <- data.frame()
  full_tokens_list <- list()
  
  # get files
  if(is.null(file_name)) { 
    files <- grep("_tokens.csv$", 
                  list.files(path = here(dir_path, source_name)), 
                  value = TRUE)
  }
  
  else files <- file_name
  
  for (file_no in seq_along(files)) {
    message("Loading ", files[file_no])
    # load dataframes and puts them into a list
    full_tokens_df_new <- read.csv(here(dir_path, source_name, files[file_no]))
    full_tokens_list[[file_no]] <- full_tokens_df_new
    file_no = file_no+1
  }
  
  full_tokens_df <- bind_rows(full_tokens_list) |> 
    mutate(source = source_name)
  
  return(full_tokens_df)
}