#Libraries required for all functions in the package
library(readxl)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(plotly)

#Prior to any plotting, you must create a dataframe from the asc file,
#and save it as a dataframe object using 'create_sciex_table'
#file = .xlsx file (asc file opened in excel, and then saved as .xlsx)
#channels = number of channels (default is three: AU, kV, and A, but could be more)
#filterby = the number of datapoints to take the median over. Default is NA, only use if there are many noise spikes
#name = what the run is named (used in the legend for overlays. Default is file name)
create_sciex_table <- function(file, channels = 3, filterby = NA, name = NA){
  
  #because the first column is not a channel, add 1
  channel_adj <- channels + 1 
  
  #convert the number of channels to a letter for excel (columns are lettered, not numbered in excel)
  channel_import_letter <- chartr("123456789", "ABCDEFGHI", channel_adj)
  
  #create the variable for importing the channel names. First is always B:11, last is the letter corresponding to the channel number
  channel_import_var <- paste0('B11:', channel_import_letter, '11')
  
  
  #import channel names into dataframe based on above variable.
  channels_df <- read_excel(file, range = channel_import_var,
                            col_names = FALSE,
                            col_types = 'text',
                            .name_repair = "minimal")
  
  #create blank list to hold the channel names
  channel_list <- rep(NA, channels)
  
  #populate the list. If there is a repeat (like for multiple RFU channels) add * to any repeats. Multiple * allowed
  for (i in 1:channels){
    temp_channel_name <- channels_df[[1,i]]
    if (temp_channel_name %in% channel_list) {
      temp_channel_name <- paste0(temp_channel_name, '*')
    }
    channel_list[i] <- temp_channel_name
  }
  
  #get rid of weird conversion symbol that happens for current. Current is just 'A'
  channel_list <- str_replace(channel_list, 'ｵ', '')

  
  #get the freqency (in hz) from the data, store as df. Always in B8
  hz_df <- read_excel(file, range = "B8:B8",
                                col_names = FALSE,
                                col_types = "numeric",
                                .name_repair = "minimal") 
 
  #pull frequency out of dataframe
  hz <- hz_df[[1]]
  
  #create empty dataframe for the data with the channels as column names
  df_plot <- data.frame(matrix(ncol = channels, nrow = 0))
  colnames(df_plot) <- channel_list
  
  #pull out the number of datapoints (time_points), always in B9
  n_timepoints_df <- read_excel(file, range = "B9:B9",
                                col_names = FALSE,
                                col_types = "numeric",
                                .name_repair = "minimal") 
  #extract from the dataframe
  n_timepoints <- n_timepoints_df[[1]]

  #datapoints start 13 rows down, obtain the excel row index of the last value
  #(1 datapoint per timepoint per channel)
  n_datapoints <- (channels*n_timepoints) + 13

  
  #pull all of the datapoints into one dataframe. all in one column
  data_points_df <- read_excel(file, range = paste0('A14:A',n_datapoints),
                              col_names = FALSE,
                              col_types = "numeric",
                              .name_repair = "minimal")
  

  # for each channel (in order) pull the datapoints corresponding to that channel from the dataframe
  # put them in the empty plot_df made earlier
  # z is the 'first' value for the next channel (starts at 1)
  for (i in 1:channels){
    z <- ((i-1)*n_timepoints)+1
    df_plot[1:n_timepoints,i] <- data_points_df[z:(z+n_timepoints-1),1]
  }
  
  #import the correction factors for each channel
  correction_import_var <- paste0('B13:', channel_import_letter, '13')
  corrections_df <- read_excel(file, range = correction_import_var,
                               col_names = FALSE,
                               col_types = 'numeric',
                               .name_repair = "minimal")
  
  #create empty list for correction factors
  correction_list <- rep(NA, channels)
  
  #populate the list with correction factors
  for (i in 1:channels){
    temp_correction_value <- corrections_df[[1,i]]
    correction_list[i] <- temp_correction_value
  }
  
  #multiply each value by the corresponding correction factor
  for (i in 1:channels){
    df_plot[[i]] <- df_plot[[i]]*correction_list[i]
  }
  
  #check if filtering is present (only if value is supplied)
  #if it is covert to an odd value, and add a column that is each column with 'filtered_' added
  if (!is.na(filterby)){
    
    if((filterby %% 2) == 0) {
      filterby <- filterby - 1
    }
    
    for (i in 1:channels){
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
  
  #return the dataframe
  return(df_plot)
}

#plots a single CE trace from the sciex table function above
#df = dataframe
#x_ and y_axis are the names of the variables. for x, options are 'minutes' or 'seconds', for y options are channel names
#xlab and ylab are what the graph will display as labels
#x and y min and max are for zooming in on the plot, can supply limits.
#interactive is whether to plot it with plotly (default is FALSE, which will return a static ggplot object)
plot_sciex_CE <- function(df, x_axis = 'minutes', y_axis = 'AU', filtered = FALSE,
                          xlab = 'Time (min)', ylab = 'Response', xmin = NA, xmax = NA,
                          ymax = NA, ymin = NA, interactive = FALSE){

  #covert the axis inputs to column names
  if (x_axis == 'seconds') {
    x_var <- 'time_s'
  } else if (x_axis == 'points'){
    x_var <- 'points'
  } else {
    x_var <- 'time_m'
  }

  #just for standard variable naming
  y_var <- y_axis
  

  
  #create the plot
  plot <- ggplot(data = df, aes(text1=time_m, text2=y_var)) +
    geom_line(aes(x = eval(as.symbol(x_var)), y = eval(as.symbol(y_var))), size = 1) +
    theme_bw(base_size = 20)+
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  #describe the plot
  print(paste0('plotting ', x_var, ' vs ', y_var))
  
  #plot with plotly if interactive is true
  if (interactive == TRUE){
    plot <- ggplotly(plot, tooltip = c('text1', 'text2', 'y'))
  }
  
  #return the plot
  return(plot)
  
}

#combines a bunch of tables (not used in directory function)
#this is used to make a dataframe that can be used in the overlay functions
combine_CE_traces <- function(...){
  master_df <- bind_rows(...)
  return(master_df)
}

#creates large cE table, used in the directory function. Not used by itself
#same variables as create_sciex_table
create_sciex_master_df <- function(folder, channels = 3, filterby = NA){
  #obtain the working directory, must be where the folder in question is located
  wd <- getwd()
  
  #assembly filepath
  file_directory <- file.path(wd, folder)
  
  #create a list of files in directory
  file_list <- list.files(path=file_directory)
  
  #create a table for the first file in the list
  master_table <- create_sciex_table(file = file.path(folder, file_list[1]), channels = channels,
                                     filterby = filterby)
  #create tables for each of the rest of the files, and bind them together
  for (i in 2:length(file_list)){
    temp_table <- create_sciex_table(file = file.path(folder, file_list[i]), channels = channels,
                                     filterby = filterby)
    master_table <- bind_rows(master_table, temp_table)
  }
  
  #return the dataframe
  return(master_table)
}


#function to plot an overlay of multiple CE runs
#same variables as plot_sciex_CE
plot_sciex_overlay <-  function(df, x_axis = 'minutes', y_axis = 'AU', filtered = FALSE,
                                                xlab = 'Time (min)', ylab = 'AU', xmin = NA, xmax = NA,
                                                ymax = NA, ymin = NA){
  #covert input variables
  if (x_axis == 'seconds') {
    x_var <- 'time_s'
  } else if (x_axis == 'points'){
    x_var <- 'points'
  } else {
    x_var <- 'time_m'
  }
  
  #check if there the plot should be with the filtered values (default is no)
  if (filtered) {
    y_var <- paste0('filtered_',y_axis)
  } else {
    y_var <- y_axis
  }
  
  
  #create the overlay
  overlay <- ggplot(data = df) +
    geom_line(aes(color = trace, x = eval(as.symbol(x_var)), y = eval(as.symbol(y_var))), size = 1) +
    theme_bw(base_size = 20)+
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank(), legend.title = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    scale_color_brewer(palette = "Set1")
  
  #describe the plot and return it
  print(paste0('plotting ', x_var, ' vs ', y_var))
  return(overlay)
  
}

#loops through a directory and creates a master dataframe, then plots using plot_CE_overlay
#same variables as plot_sciex_overlay. Trace names will be file names
overlay_sciex_directory <- function(folder, channels = 3, filterby = NA, x_axis = 'minutes', y_axis = 'AU', 
                                    filtered = FALSE, xlab = 'Time (min)', ylab = 'Response', xmin = NA, xmax = NA,
                                    ymax = NA, ymin = NA, return = "plot"){
  #get working directory
  wd <- getwd()
  
  #get path for the folder
  file_directory <- file.path(wd, folder)
  
  #create list of all files
  file_list <- list.files(path=file_directory)
  
  #create a master dataframe with all files
  master_table <- create_sciex_master_df(folder = folder, channels = channels, filterby = filterby)
  
  #plot the overlay
  overlay_directory <- plot_sciex_overlay(master_table, x_axis = x_axis, y_axis = y_axis, filtered = filtered,
                                          xlab = xlab, ylab = ylab, xmin = xmin, xmax = xmax,
                                          ymax = ymax, ymin = ymin)
  
  #return the plot or the directory depending on input
  if (return == "plot"){
    return(overlay_directory)
  } else {
    return(master_table)
  }
}

#adds 5 vertical lines at specified values. Used for showing where fractions were collected
#plot_obj is a ggplot obj, x values are where verticle lines should be
#color, linetype, and size are ggplot variables for describing the lines
add_fractions <- function(plot_obj, x1 = 1, x2 = 2, x3 = 3, x4 = 4, x5 = 5, color = 'red',
                          linetype = 'solid', size = 1){
  new_plot <- plot_obj + 
    geom_vline(xintercept = x1, color = color, linetype = linetype, size = size) +
    geom_vline(xintercept = x2, color = color, linetype = linetype, size = size) +
    geom_vline(xintercept = x3, color = color, linetype = linetype, size = size) +
    geom_vline(xintercept = x4, color = color, linetype = linetype, size = size) +
    geom_vline(xintercept = x5, color = color, linetype = linetype, size = size)
  
  return(new_plot)
}




#calculate integral plot from df. Not used by itself, but part of the peak_stats function
#CE_df is the dataframe, made with create_sciex_table
#column is the name of the colunn to be integrated
#peak_start and _end are the time (in minutes) of the peak end and beginning
#multiples is for implimentation of multiple peak integration, if true it assumes baseline style is 'peak'
add_integration_df <- function(CE_df, column = "AU",
                               peak_start = NA, peak_end = NA, multiples = FALSE){
  
  #included for multiple peak integration, auto asigns baseline style as peak if TRUE (default FALSE)
  if (multiples == TRUE){
    baseline_type <- 'peak'
  } else {
    print("What type of basline integration? 'value', 'flat','peak', or 'slope'")
    baseline_type <- readline(prompt = 'baseline type: ')
  }
  
  #baseline option of flat. This will take an input range of baseline, and average the y values over that time
  #assigns this average as the baseline value
  if(baseline_type == 'flat'){
    
    #asks for basline start and end
    baseline_start <- as.numeric(readline(prompt = 'baseline start: '))
    baseline_end <- as.numeric(readline(prompt = 'baseline end: '))
    
    #finds row index closest to the given baseline values
    baseline_index_start <- which.min(abs(CE_df$time_m - baseline_start))
    baseline_index_end <- which.min(abs(CE_df$time_m - baseline_end))
    
  
    #subsets dataframe over the given time interval
    df_baseline <- CE_df[baseline_index_start:baseline_index_end,]
    #finds the average
    baseline_value <- mean(df_baseline[[column]])
    #asigns this value to each row
    CE_df$baseline <- baseline_value
    
    #prints the baseline value
    print(paste0('average baseline value: ', baseline_value))
    
  } else if (baseline_type == 'peak') {
    
    #'peak' will take the start and end of the peak, and calculate a linear slope between these values
    #The baseline is the value of this line at each timepoint
    
    #finds the row index of peak start and end based on given values
    peak_index_start <- which.min(abs(CE_df$time_m - peak_start))
    peak_index_end <- which.min(abs(CE_df$time_m - peak_end))
    
    #sets two points in (x,y) form that are the values
    x1 <- CE_df$time_m[peak_index_start]
    x2 <- CE_df$time_m[peak_index_end]
    y1 <- CE_df[[column]][peak_index_start]
    y2 <- CE_df[[column]][peak_index_end]
    
    #finds the slope and intercept between these points
    slope <- (y2-y1)/(x2-x1)
    y_intercept <- y1 - (slope * x1)
    
    #sets baseline for each point based on slope and intercept
    CE_df$baseline <- slope * CE_df$time_m + y_intercept
    
  } else if (baseline_type == 'slope') {
    #'slope' takes a section of baseline, and does a linear best fit over that range, and uses this line to calculate baseline values

    #asks for baseline start and finish, and calculates row indeces for these values
    baseline_start <- as.numeric(readline(prompt = 'baseline start: '))
    baseline_end <- as.numeric(readline(prompt = 'baseline end: '))
    baseline_index_start <- which.min(abs(CE_df$time_m - baseline_start))
    baseline_index_end <- which.min(abs(CE_df$time_m - baseline_end))
    
    #subsets dataframe over this range
    df_baseline <- CE_df[baseline_index_start:baseline_index_end,]
    
    #best fit linear line for the values, pulls out y intercept and slope
    lm <- lm(df_baseline[[column]]~df_baseline$time_m)
    y_intercept <- coef(lm)["(Intercept)"][[1]]
    slope <- coef(lm)["df_baseline$time_m"][[1]]
    
    #calculates baseline value for each point based on slope and intercept
    CE_df$baseline <- slope * CE_df$time_m + y_intercept

  } else if (baseline_type == 'value') {
    
      #simply asks for a y value, and asigns it as a baseline for each point
      baseline_value <- as.numeric(readline(prompt = 'baseline value: '))
      CE_df$baseline <- baseline_value

  } else {
    
    #if a valid answer isn't given. Will throw error later
    print('not a valid type')
  }
  
  #net y is the y value minus the baseline value for each point
  CE_df$net_y <- CE_df[[column]] - CE_df$baseline
  
  #net y tca is this same value divided by the time in minutes (time corrected area)
  CE_df$net_y_tca <- CE_df$net_y / CE_df$time_m
  
  #get min per point, which will always be equal to the time in minutes at the second datapoint
  min_per_point <- CE_df$time_m[2]
  
  #multiply the net_y by the width of a timepoint
  CE_df$net_y_dx <- CE_df$net_y * min_per_point
  
  #set starting sum value to 0.
  #go through each row and add the previous net_y value to the sum, to get a summed list
  #NOTE dx_sum isn't used, as the values are generally very small. Left in just in case, but the values used for area are just net_y_sum
  CE_df$net_y_dx_sum <- 0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_dx_sum[i] <- CE_df$net_y_dx_sum[i-1] + CE_df$net_y_dx[i]
  }
  
  #same as above, but for net_y_sum. This is used going forward
  CE_df$net_y_sum <-0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_sum[i] <- CE_df$net_y_sum[i-1] + CE_df$net_y[i]
  }
  
  #same as above, but for tca_net_y_sum
  CE_df$net_y_sum_tca <- 0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_sum_tca[i] <- CE_df$net_y_sum_tca[i-1] + CE_df$net_y_tca[i]
  }
  
  #return the dataframe with the summed columns  
  return(CE_df)
}

#function to calculate peak statistics from a dataframe from add_integration_df()
#t1 and t2 are starting and ending times of peak
peak_area_height_time <- function(df, t1, t2){
  
  #calculate row indecies for each column of interest (time, net_y_sum, net_y_sum_tca)
  t1_index <- which.min(abs(df$time_m-t1))

  t2_index <- which.min(abs(df$time_m-t2))

  time1_net_y_sum <- df$net_y_sum[t1_index]

  time2_net_y_sum <- df$net_y_sum[t2_index]
 
  time1_net_y_sum_tca <- df$net_y_sum_tca[t1_index]
  
  time2_net_y_sum_tca <- df$net_y_sum_tca[t2_index]
  
  #subset the dataframe for the peak of interest
  df_peak <- df[t1_index:t2_index,]
  
  #find the max y value
  apex_index <- which.max(df_peak$net_y)
  #find the time at this value
  peak_apex_time <- df_peak$time_m[apex_index]
  #find the 'median' time of the peak ((end-start)/2)
  peak_average_time <- mean(c(df$time_m[t1_index], df$time_m[t2_index]))

  #find peak height at apex
  peak_height <- df_peak$net_y[apex_index]
 
  #find area based on net_y_sum difference
  area <- time2_net_y_sum - time1_net_y_sum
  #find tca based on net_y_sum_tca difference. This is the tca AT EACH POINT
  tca_absolute <- time2_net_y_sum_tca - time1_net_y_sum_tca
  #find tca by dividing area by time at apex
  tca_apex <- area/peak_apex_time
  #find tca by dividing area by median time
  tca_average <- area/peak_average_time
  
  #print out all these stats
  print(paste0('Peak Migration time (median): ', round(peak_average_time, 4), ' min'))
  print(paste0('Peak Migration time (apex): ', round(peak_apex_time, 4), ' min'))
  print(paste0('Peak Height (apex): ', peak_height, ' AU'))
  print(paste0('Peak Area: ', round(area, 5), ' total AU'))
  print(paste0('Time-Corrected Peak Area (absolute): ', round(tca_absolute, 5)))
  print(paste0('Time-Corrected Peak Area (median time): ', round(tca_average, 5)))
  print(paste0('Time-Corrected Peak Area (apex time): ', round(tca_apex, 5)))
  
  #create vectors for each value
  migration_time_median <- c(peak_average_time)
  migration_time_apex <- c(peak_apex_time)
  area <- c(area)
  corrected_area_absolute <- c(tca_absolute)
  corrected_area_median <- c(tca_average)
  corrected_area_apex <- c(tca_apex)
  starting_time <- c(df$time_m[t1_index])
  ending_time <- c(df$time_m[t2_index])
  height <- c(peak_height)
  

  #return the vectors as a dataframe
  return_df <- data.frame(migration_time_median,
                          migration_time_apex, 
                          height,
                          area,
                          corrected_area_absolute,
                          corrected_area_median,
                          corrected_area_apex,
                          starting_time,
                          ending_time)
  return(return_df)
}

#calculate peak area, height, TCA, etc.
#'column' is the name of the y axis variable, default is AU (other would be RFU)
#'peak only' shows only the integration for the selected peak. If false, the full integration trace will be shown
#'integration scales to the response y axis, units are arbitrary
sciex_peak_stats <- function(df, column = 'AU', peak_only = TRUE) {
  
  #show the intial plot
  plot1 <- plot_sciex_CE(df, ylab = column, interactive = TRUE)
  show(plot1)
  
  #ask for start and end. The plot can be zoomed in on for accurate values
  peak_start <- as.numeric(readline(prompt = 'peak start: '))
  peak_end <- as.numeric(readline(prompt = 'peak end: '))
  
  
  #create a dataframe with summed columns (for integration)
  integrated_df <- add_integration_df(CE_df = df,
                                      peak_start = peak_start, peak_end = peak_end)
  
  #calculate stats on the peak
  area_return_df <- peak_area_height_time(integrated_df, peak_start, peak_end)
  
  #create a df to normalize integration traces
  normalized_int_df <- integrated_df
  
  #max, minimum, range y values of the trace
  max_response <- max(integrated_df[[column]])
  min_response <- min(integrated_df[[column]])
  range_response <- max_response - min_response
  
  #max, minimum, range y values of the integration trace
  max_int <- max(integrated_df$net_y_sum)
  min_int <- min(integrated_df$net_y_sum)
  range_int <- max_int - min_int
  
  #ratio between the two ranges
  response_int_ratio <- range_response/range_int
  
  #normalize the integration to the range of the trace responses
  normalized_int_df$net_y_sum_norm <- (((normalized_int_df$net_y_sum - min_int)*range_response)/(range_int)) + min_response
  
  #plot the integration for the peak only
  if(peak_only == TRUE) {
    
    #row indeces the peak start and end
    t1_index <- which.min(abs(integrated_df$time_m-peak_start))
    t2_index <- which.min(abs(integrated_df$time_m-peak_end))
    
    #subset the normalized df based on peak indecies... re normalize this using the same process as above
    int_subset_df <- normalized_int_df[t1_index:t2_index,]
    
    #max, min, range of subset integration traces
    max_int_sub <- max(int_subset_df$net_y_sum)
    min_int_sub <- min(int_subset_df$net_y_sum)
    range_int_sub <- max_int_sub - min_int_sub
    
    #response ratio of integration to response y values
    response_int_ratio_sub <- range_response/range_int_sub
    
    #normalize
    int_subset_df$net_y_sum_norm <- (((int_subset_df$net_y_sum - min_int_sub)*range_response)/(range_int_sub)) + min_response
    
    #plot the integration trace (peak only) with the original data
    plot2 <- plot_sciex_CE(normalized_int_df, ylab = column, interactive = FALSE)
    plot3 <- plot2 + geom_line(data = int_subset_df, aes(x = time_m, y = net_y_sum_norm), color = "green") +
      geom_line(aes(x = time_m, y = baseline), color = 'red') +
      geom_vline(xintercept = area_return_df$starting_time, color = 'red') + 
      geom_vline(xintercept = area_return_df$ending_time, color = 'red')
    plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
    show(plot4)
    
  } else {
    #plot the entire integration trace
    plot2 <- plot_sciex_CE(normalized_int_df, interactive = FALSE)
    plot3 <- plot2 + geom_line(aes(x = time_m, y = net_y_sum_norm), color = "green") +
      geom_line(aes(x = time_m, y = baseline), color = 'red') +
      geom_vline(xintercept = area_return_df$starting_time, color = 'red') + 
      geom_vline(xintercept = area_return_df$ending_time, color = 'red')
    plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
    show(plot4)
  }
}





