library(readxl)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(plotly)

  
#creates df for sciex plotting
create_sciex_table <- function(file, channels = 3, filterby = NA, name = NA){
  
  channel_adj <- channels + 1 
  channel_import_letter <- chartr("123456789", "ABCDEFGHI", channel_adj)
  
  channel_import_var <- paste0('B11:', channel_import_letter, '11')
  #print(channel_import_var)
  
  channels_df <- read_excel(file, range = channel_import_var,
                            col_names = FALSE,
                            col_types = 'text',
                            .name_repair = "minimal")
  
  #print(channels_df)
  channel_list <- rep(NA, channels)
  
  for (i in 1:channels){
    temp_channel_name <- channels_df[[1,i]]
    if (temp_channel_name %in% channel_list) {
      temp_channel_name <- paste0(temp_channel_name, '*')
    }
    channel_list[i] <- temp_channel_name
    #print(channel_list)
  }
  
  #print(channel_list)
  channel_list <- str_replace(channel_list, 'ｵ', '')
  #print(channel_list)
  
  hz_df <- read_excel(file, range = "B8:B8",
                                col_names = FALSE,
                                col_types = "numeric",
                                .name_repair = "minimal") 
  hz <- hz_df[[1]]
  
  df_plot <- data.frame(matrix(ncol = channels, nrow = 0))
  colnames(df_plot) <- channel_list
  
  n_timepoints_df <- read_excel(file, range = "B9:B9",
                                col_names = FALSE,
                                col_types = "numeric",
                                .name_repair = "minimal") 
  n_timepoints <- n_timepoints_df[[1]]
  #print(n_timepoints)
  
  n_datapoints <- (channels*n_timepoints) + 13
  #print(n_datapoints)
  
  data_points_df <- read_excel(file, range = paste0('A14:A',n_datapoints),
                              col_names = FALSE,
                              col_types = "numeric",
                              .name_repair = "minimal")
  #print(data_points_df)
  

  for (i in 1:channels){
    z <- ((i-1)*n_timepoints)+1
    #print(paste('z equals', z))
    df_plot[1:n_timepoints,i] <- data_points_df[z:(z+n_timepoints-1),1]
  }
  
  correction_import_var <- paste0('B13:', channel_import_letter, '13')
  corrections_df <- read_excel(file, range = correction_import_var,
                               col_names = FALSE,
                               col_types = 'numeric',
                               .name_repair = "minimal")
  
  correction_list <- rep(NA, channels)
  
  for (i in 1:channels){
    temp_correction_value <- corrections_df[[1,i]]
    correction_list[i] <- temp_correction_value
  }
  
  for (i in 1:channels){
    df_plot[[i]] <- df_plot[[i]]*correction_list[i]
  }
  
  if (!is.na(filterby)){
    
    if((filterby %% 2) == 0) {
      filterby <- filterby - 1
    }
    
    for (i in 1:channels){
      filtered_channel_temp <- paste0('filtered_', channel_list[i])
      df_plot[[filtered_channel_temp]] <- runmed(df_plot[[channel_list[i]]], filterby)
    }
  }
  
  
  
  points <- 1:(n_timepoints)
  
  df_plot$points <- points
  
  df_plot$time_s <- (df_plot$points - 1)/hz
  df_plot$time_m <- df_plot$time_s/60
  
  if (is.na(name)){
    #obtain file name
    trace_name_path <- tools::file_path_sans_ext(file)
    trace_name <- basename(trace_name_path)
  } else {
    trace_name <- name
  }
  
  #add file name to df
  df_plot$trace <- trace_name
  
  return(df_plot)
}

#plots a single CE trace from the sciex table function above
plot_sciex_CE <- function(df, x_axis = 'minutes', y_axis = 'AU', filtered = FALSE,
                          xlab = 'Time (min)', ylab = 'Response', xmin = NA, xmax = NA,
                          ymax = NA, ymin = NA, interactive = FALSE){

  if (x_axis == 'seconds') {
    x_var <- 'time_s'
  } else if (x_axis == 'points'){
    x_var <- 'points'
  } else {
    x_var <- 'time_m'
  }

  y_var <- y_axis
  

  
  
  plot <- ggplot(data = df, aes(text1=time_m, text2=y_var)) +
    geom_line(aes(x = eval(as.symbol(x_var)), y = eval(as.symbol(y_var))), size = 1) +
    theme_bw(base_size = 20)+
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  print(paste0('plotting ', x_var, ' vs ', y_var))
  
  if (interactive == TRUE){
    plot <- ggplotly(plot, tooltip = c('text1', 'text2', 'y'))
  }
  
  return(plot)
  
}

#combines a bunch of tables (not used in directory function)
combine_CE_traces <- function(...){
  master_df <- bind_rows(...)
  return(master_df)
}

#creates large cE table
create_sciex_master_df <- function(folder, channels = 3, filterby = NA){
  wd <- getwd()
  
  file_directory <- file.path(wd, folder)
  
  
  file_list <- list.files(path=file_directory)
  
  master_table <- create_sciex_table(file = file.path(folder, file_list[1]), channels = channels,
                                     filterby = filterby)
  
  for (i in 2:length(file_list)){
    temp_table <- create_sciex_table(file = file.path(folder, file_list[i]), channels = channels,
                                     filterby = filterby)
    master_table <- bind_rows(master_table, temp_table)
  }
  
  return(master_table)
}

plot_sciex_overlay <-  function(df, x_axis = 'minutes', y_axis = 'AU', filtered = FALSE,
                                                xlab = 'Time (min)', ylab = 'Response', xmin = NA, xmax = NA,
                                                ymax = NA, ymin = NA){
  
  if (x_axis == 'seconds') {
    x_var <- 'time_s'
  } else if (x_axis == 'points'){
    x_var <- 'points'
  } else {
    x_var <- 'time_m'
  }
  
  if (filtered) {
    y_var <- paste0('filtered_',y_axis)
  } else {
    y_var <- y_axis
  }
  
  
  
  overlay <- ggplot(data = df) +
    geom_line(aes(color = trace, x = eval(as.symbol(x_var)), y = eval(as.symbol(y_var))), size = 1) +
    theme_bw(base_size = 20)+
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank(), legend.title = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    scale_color_brewer(palette = "Set1")
  
  print(paste0('plotting ', x_var, ' vs ', y_var))
  return(overlay)
  
}

#loops through a directory and creates a master dataframe, then plots using plot_CE_overlay
overlay_sciex_directory <- function(folder, channels = 3, filterby = NA, x_axis = 'minutes', y_axis = 'AU', 
                                    filtered = FALSE, xlab = 'Time (min)', ylab = 'Response', xmin = NA, xmax = NA,
                                    ymax = NA, ymin = NA, return = "plot"){
  wd <- getwd()
  
  file_directory <- file.path(wd, folder)
  
  
  file_list <- list.files(path=file_directory)
  
  master_table <- create_sciex_master_df(folder = folder, channels = channels, filterby = filterby)
  
  
  overlay_directory <- plot_sciex_overlay(master_table, x_axis = x_axis, y_axis = y_axis, filtered = filtered,
                                          xlab = xlab, ylab = ylab, xmin = xmin, xmax = xmax,
                                          ymax = ymax, ymin = ymin)
  if (return == "plot"){
    #show plot
    return(overlay_directory)
  } else {
    return(master_table)
  }
}
#adds 4 vertical lines at specified values
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




#calculate integral plot from df
add_integration_df <- function(CE_df, column = "AU", correction_factor = 1, peak_start = NA, peak_end = NA, multiples = FALSE){
  
  
  if (multiples == TRUE){
    baseline_type <- 'peak'
  } else {
    print("What type of basline integration? 'value', 'flat','peak', or 'slope'")
    baseline_type <- readline(prompt = 'baseline type: ')
  }
  
  if(baseline_type == 'flat'){
    baseline_start <- as.numeric(readline(prompt = 'baseline start: '))
    baseline_end <- as.numeric(readline(prompt = 'baseline end: '))
    baseline_index_start <- which.min(abs(CE_df$time_m - baseline_start))
    baseline_index_end <- which.min(abs(CE_df$time_m - baseline_end))
    
    df_baseline <- CE_df[baseline_index_start:baseline_index_end,]
    baseline_value <- mean(df_baseline[[column]])
    CE_df$baseline <- baseline_value
    
    print(paste0('average baseline value: ', baseline_value))
    
  } else if (baseline_type == 'peak') {
    
    #peak_start <- as.numeric(readline(prompt = 'peak start: '))
    #peak_end <- as.numeric(readline(prompt = 'peak end: '))
    peak_index_start <- which.min(abs(CE_df$time_m - peak_start))
    peak_index_end <- which.min(abs(CE_df$time_m - peak_end))
    
    x1 <- CE_df$time_m[peak_index_start]
    x2 <- CE_df$time_m[peak_index_end]
    y1 <- CE_df[[column]][peak_index_start]
    y2 <- CE_df[[column]][peak_index_end]
    
    
    slope <- (y2-y1)/(x2-x1)
    y_intercept <- y1 - (slope * x1)
    
    CE_df$baseline <- slope * CE_df$time_m + y_intercept
    
  } else if (baseline_type == 'slope') {
    
    baseline_start <- as.numeric(readline(prompt = 'baseline start: '))
    baseline_end <- as.numeric(readline(prompt = 'baseline end: '))
    baseline_index_start <- which.min(abs(CE_df$time_m - baseline_start))
    baseline_index_end <- which.min(abs(CE_df$time_m - baseline_end))
    
    df_baseline <- CE_df[baseline_index_start:baseline_index_end,]
    
    lm <- lm(df_baseline[[column]]~df_baseline$time_m)
    y_intercept <- coef(lm)["(Intercept)"][[1]]
    slope <- coef(lm)["df_baseline$time_m"][[1]]
    
    CE_df$baseline <- slope * CE_df$time_m + y_intercept

  } else if (baseline_type == 'value') {
    
      baseline_value <- as.numeric(readline(prompt = 'baseline value: '))
      CE_df$baseline <- baseline_value

  } else {

    print('not a valid type')
  }
  
  CE_df$net_y <- CE_df[[column]] - CE_df$baseline
  CE_df$net_y_tca <- CE_df$net_y / CE_df$time_m
  
  #get min per point, which will always be equal to the time in minutes at the second datapoint
  min_per_point <- CE_df$time_m[2]
  
  CE_df$net_y_dx <- CE_df$net_y * min_per_point
  
  CE_df$net_y_dx_sum <- 0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_dx_sum[i] <- CE_df$net_y_dx_sum[i-1] + CE_df$net_y_dx[i]
  }
  CE_df$net_y_dx_sum <- CE_df$net_y_dx_sum * correction_factor
  
  
  CE_df$net_y_sum <-0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_sum[i] <- CE_df$net_y_sum[i-1] + CE_df$net_y[i]
  }
  CE_df$net_y_sum <- CE_df$net_y_sum * correction_factor
  
  CE_df$net_y_sum_tca <- 0
  for (i in 2:(nrow(CE_df))) {
    CE_df$net_y_sum_tca[i] <- CE_df$net_y_sum_tca[i-1] + CE_df$net_y_tca[i]
  }
  CE_df$net_y_sum_tca <- CE_df$net_y_sum_tca * correction_factor
    
  return(CE_df)
}

peak_area_height_time <- function(df, t1, t2){
  t1_index <- which.min(abs(df$time_m-t1))

  t2_index <- which.min(abs(df$time_m-t2))

  time1_net_y_sum <- df$net_y_sum[t1_index]

  time2_net_y_sum <- df$net_y_sum[t2_index]
 
  time1_net_y_sum_tca <- df$net_y_sum_tca[t1_index]
  
  time2_net_y_sum_tca <- df$net_y_sum_tca[t2_index]
  
  df_peak <- df[t1_index:t2_index,]
  
  apex_index <- which.max(df_peak$net_y)
  peak_apex_time <- df_peak$time_m[apex_index]
  peak_average_time <- mean(c(df$time_m[t1_index], df$time_m[t2_index]))

  
  peak_height <- df_peak$net_y[apex_index]
 
  
  area <- time2_net_y_sum - time1_net_y_sum
  tca_absolute <- time2_net_y_sum_tca - time1_net_y_sum_tca
  tca_apex <- area/peak_apex_time
  tca_average <- area/peak_average_time
  
  print(paste0('Peak Migration time (median): ', round(peak_average_time, 4), ' min'))
  print(paste0('Peak Migration time (apex): ', round(peak_apex_time, 4), ' min'))
  print(paste0('Peak Height (apex): ', peak_height, ' AU'))
  print(paste0('Peak Area: ', round(area, 5), ' total AU'))
  print(paste0('Time-Corrected Peak Area (absolute): ', round(tca_absolute, 5)))
  print(paste0('Time-Corrected Peak Area (median time): ', round(tca_average, 5)))
  print(paste0('Time-Corrected Peak Area (apex time): ', round(tca_apex, 5)))
  
  migration_time_median <- c(peak_average_time)
  migration_time_apex <- c(peak_apex_time)
  area <- c(area)
  corrected_area_absolute <- c(tca_absolute)
  corrected_area_median <- c(tca_average)
  corrected_area_apex <- c(tca_apex)
  starting_time <- c(df$time_m[t1_index])
  ending_time <- c(df$time_m[t2_index])
  height <- c(peak_height)
  

  
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
sciex_peak_stats <- function(df, correction_factor = 1, column = 'AU', peak_only = TRUE) {
  
  plot1 <- plot_sciex_CE(df, ylab = column, interactive = TRUE)
  show(plot1)
  
  peak_start <- as.numeric(readline(prompt = 'peak start: '))
  peak_end <- as.numeric(readline(prompt = 'peak end: '))
  
  
  integrated_df <- add_integration_df(CE_df = df, correction_factor = correction_factor,
                                      peak_start = peak_start, peak_end = peak_end)
  
  area_return_df <- peak_area_height_time(integrated_df, peak_start, peak_end)
  
  normalized_int_df <- integrated_df
  
  max_response <- max(integrated_df[[column]])
  min_response <- min(integrated_df[[column]])
  range_response <- max_response - min_response
  
  
  max_int <- max(integrated_df$net_y_sum)
  min_int <- min(integrated_df$net_y_sum)
  range_int <- max_int - min_int
  
  
  response_int_ratio <- range_response/range_int
  
  normalized_int_df$net_y_sum_norm <- (((normalized_int_df$net_y_sum - min_int)*range_response)/(range_int)) + min_response
  
  if(peak_only == TRUE) {
    
    
    t1_index <- which.min(abs(integrated_df$time_m-peak_start))
    t2_index <- which.min(abs(integrated_df$time_m-peak_end))
    
    int_subset_df <- normalized_int_df[t1_index:t2_index,]
    max_int_sub <- max(int_subset_df$net_y_sum)
    min_int_sub <- min(int_subset_df$net_y_sum)
    range_int_sub <- max_int_sub - min_int_sub
    
    response_int_ratio_sub <- range_response/range_int_sub
    
    int_subset_df$net_y_sum_norm <- (((int_subset_df$net_y_sum - min_int_sub)*range_response)/(range_int_sub)) + min_response
    
    plot2 <- plot_sciex_CE(normalized_int_df, ylab = column, interactive = FALSE)
    plot3 <- plot2 + geom_line(data = int_subset_df, aes(x = time_m, y = net_y_sum_norm), color = "green") +
      geom_line(aes(x = time_m, y = baseline), color = 'red') +
      geom_vline(xintercept = area_return_df$starting_time, color = 'red') + 
      geom_vline(xintercept = area_return_df$ending_time, color = 'red')
    plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
    show(plot4)
    
  } else {
    plot2 <- plot_sciex_CE(normalized_int_df, interactive = FALSE)
    plot3 <- plot2 + geom_line(aes(x = time_m, y = net_y_sum_norm), color = "green") +
      geom_line(aes(x = time_m, y = baseline), color = 'red') +
      geom_vline(xintercept = area_return_df$starting_time, color = 'red') + 
      geom_vline(xintercept = area_return_df$ending_time, color = 'red')
    plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
    show(plot4)
  }
}





