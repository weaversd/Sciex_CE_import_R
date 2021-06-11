create_sciex_table_txt <- function(file, channels = 3, filterby = NA, name = NA, csv = F){
  
  meta_data <- readLines(file, n = 13)
  
  channel_title_raw <- str_split(str_split(meta_data[[11]], pattern = " ")[[1]][3], pattern = "\t")
  channels_n <- length(channel_title_raw[[1]]) - 1
  channel_list <- channel_title_raw[[1]][2:(channels_n+1)]
  
  #get rid of weird conversion symbol that happens for current. Current is just 'uA'
  channel_list <- str_replace(channel_list, 'ï½µ', '')
  channel_list <- str_replace(channel_list, 'µ', 'u')
  
  hz_raw <- str_split(meta_data[[8]], "\t")
  hz <- as.numeric(hz_raw[[1]][2])
  
  #create empty dataframe for the data with the channels as column names
  df_plot <- data.frame(matrix(ncol = channels_n, nrow = 0))
  colnames(df_plot) <- channel_list

  
  timepoints_raw <- str_split(meta_data[[9]], "\t")
  n_timepoints <- as.numeric(timepoints_raw[[1]][2])
  
  raw_data <- readLines(file, n = (n_timepoints*channels_n)+13)
  datapoints <- raw_data[14:length(raw_data)]
  datapoints <- as.numeric(datapoints)
  
  
  # for each channel (in order) pull the datapoints corresponding to that channel from the dataframe
  # put them in the empty plot_df made earlier
  # z is the 'first' value for the next channel (starts at 1)
  for (i in 1:channels_n){
    z <- ((i-1)*n_timepoints)+1
    df_plot[1:n_timepoints,i] <- datapoints[z:(z+n_timepoints-1)]
  }
  
  
  correction_raw <- str_split(str_split(meta_data[[13]], pattern = " ")[[1]][3], pattern = "\t")
  correction_list <- correction_raw[[1]][2:(channels_n+1)]
  correction_list <- as.numeric(correction_list)
  
  
  #multiply each value by the corresponding correction factor
  for (i in 1:channels_n){
    df_plot[[i]] <- df_plot[[i]]*correction_list[i]
  }
  
  #check if filtering is present (only if value is supplied)
  #if it is covert to an odd value, and add a column that is each column with 'filtered_' added
  if (!is.na(filterby)){
    
    if((filterby %% 2) == 0) {
      filterby <- filterby - 1
    }
    
    for (i in 1:channels_n){
      filtered_channel_temp <- paste0('filtered_', channel_list[i])
      df_plot[[filtered_channel_temp]] <- runmed(df_plot[[channel_list[i]]], filterby)
    }
  }
  
  
  #make a vector for indexing all of the time_points
  points <- 1:(n_timepoints)
  
  #add this vector to the dataframe
  df_plot$points <- points
  
  #add columns that show the time in minutes and seconds using the frequencing
  df_plot$time_s <- (df_plot$points - 1)/hz
  df_plot$time_m <- df_plot$time_s/60
  
  #add a column with the name of the run. If not supplied, use the file name
  if (is.na(name)){
    #obtain file name
    trace_name_path <- tools::file_path_sans_ext(file)
    trace_name <- basename(trace_name_path)
  } else {
    trace_name <- name
  }
  
  #add file name to df
  df_plot$trace <- trace_name
  
  if (csv == T) {
    write.csv(df_plot, file = paste0(file, ".csv"), row.names = F)
  }
  
  #return the dataframe
  return(df_plot)
}
