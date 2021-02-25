library(readxl)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggplot2)

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
                          ymax = NA, ymin = NA){

  if (x_axis == 'seconds') {
    x_var <- 'time_s'
  } else if (x_axis == 'points'){
    x_var <- 'points'
  } else {
    x_var <- 'time_m'
  }

  y_var <- y_axis
  
  
  plot <- ggplot(data = df) +
    geom_line(aes(x = eval(as.symbol(x_var)), y = eval(as.symbol(y_var))), size = 1) +
    theme_bw(base_size = 20)+
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  print(paste0('plotting ', x_var, ' vs ', y_var))
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
add_fractions <- function(plot_obj, x1 = 1, x2 = 2, x3 = 3, x4 = 4, x5 = 5, color = 'red'){
  new_plot <- plot_obj + 
    geom_vline(xintercept = x1, color = color) +
    geom_vline(xintercept = x2, color = color) +
    geom_vline(xintercept = x3, color = color) +
    geom_vline(xintercept = x4, color = color) +
    geom_vline(xintercept = x5, color = color)
  
  return(new_plot)
}


