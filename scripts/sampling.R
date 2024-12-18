simulate_samples <- function(data_df, 
                             size_start = 100, 
                             size_increment = 100, 
                             size_end = 50000, 
                             simulations = 100) {
  if (size_end > nrow(data_df)) {
    stop("End size larger than number of tokens in data")
  }
  
  # initialise variables
  size = size_start
  sim_no = 1
  sim_df_list <- list()
  
  # run simulations
  while (size <= size_end) {
    if (size %% (20 * size_increment) == 0) {
      message("Simulating ", size)
    }
    
    sim_data_df_new <- matrix(ncol = simulations + 3)
    sim_data_df_new[1] <- size
    sim_no = 1
    for (sim_no in seq_len(simulations)) {
      # generate a random sample: 
      # get random start point and the following 'size' tokens
      token_sample_start <- sample(1:(nrow(data_df) - size), 1) 
      token_sample <- data_df[token_sample_start:
                                (token_sample_start + size - 1),] 
      
      # get number of unique word types
      sim_data_df_new[sim_no + 1] <- token_sample$stem |> unique() |> length()
      
      sim_no = sim_no + 1
    }
    
    # calculate mean
    sim_data_df_new[simulations + 2] <- mean(sim_data_df_new[2:(simulations+1)]) 
    # calculate type-token ratio
    sim_data_df_new[simulations + 3] <- sim_data_df_new[simulations + 2] / size 
    sim_df_list <- sim_df_list |> append(list(sim_data_df_new))
    
    size = size + size_increment
  }
  
  cn <- c("size", paste("sim_", seq_len(simulations), sep=""), 
          "types", "ttr")
  sim_data_df <- sim_df_list |> 
    lapply(as.data.frame) |> 
    bind_rows() |> 
    set_names(cn)
  
  return(sim_data_df)
}