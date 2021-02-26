sciex_multiple_peaks <- function(df, correction_factor = 1, column = 'AU', peak_only = TRUE, integral_percent = 0.6, integral_overlay = 1, int_std = 1){
  plot1 <- plot_sciex_CE(df, ylab = column, interactive = TRUE)
  show(plot1)
  
  peak_n <- as.numeric(readline(prompt = 'Number of peaks to integrate: '))

  list_integrated_dfs <- list()
  list_peak_area_return <- list()
  list_integrated_df_subset <- list()

  
  input_peak_df <- data.frame(peak = 1:peak_n,
                        peak_start = rep(NA, peak_n),
                        peak_end = rep(NA, peak_n))
  
  int_subset_norm_params <- data.frame(peak_number = 1:peak_n,
                                       max = rep(NA, peak_n),
                                       min = rep(NA, peak_n),
                                       range = rep(NA, peak_n),
                                       range_ratio = rep(NA, peak_n))
  
  for (i in 1:peak_n){
    pi_start <- as.numeric(readline(prompt = paste0('peak ', i, ' start: ')))
    pi_end <- as.numeric(readline(prompt = paste0('peak ', i, ' end: ')))
    integrated_df_i <- add_integration_df(CE_df = df, correction_factor = correction_factor,
                                           peak_start = pi_start, peak_end = pi_end, multiples = peak_only)
    integrated_df_i$peak_number <- paste0('peak_', i)
    
    list_integrated_dfs[[i]] <- integrated_df_i
    
    input_peak_df$peak_start[i] <- pi_start
    input_peak_df$peak_end[i] <- pi_end
    
    print(paste0('Peak ', i, ' results:'))
    area_return_df_i <- peak_area_height_time(integrated_df_i, pi_start, pi_end)
    area_return_df_i$peak_number <- paste0('peak_', i)
    list_peak_area_return[[i]] <- area_return_df_i
    
    t1_index <- which.min(abs(integrated_df_i$time_m-pi_start))
    t2_index <- which.min(abs(integrated_df_i$time_m-pi_end))
    
    integrated_df_subset_i <- integrated_df_i[t1_index:t2_index,]
    list_integrated_df_subset[[i]] <- integrated_df_subset_i
    
    max_int_sub_i <- max(integrated_df_subset_i$net_y_sum)
    min_int_sub_i <- min(integrated_df_subset_i$net_y_sum)
    range_int_sub_i <- max_int_sub_i - min_int_sub_i
    
    int_subset_norm_params$max[i] <- max_int_sub_i
    int_subset_norm_params$min[i] <- min_int_sub_i
    int_subset_norm_params$range[i] <- range_int_sub_i
  }
  
  
  combined_integrated_df_subset <- list_integrated_df_subset[[1]]
  return_df <- list_peak_area_return[[1]]
  for (i in 2:peak_n){
    combined_integrated_df_subset <- rbind(combined_integrated_df_subset, list_integrated_df_subset[[i]])
    return_df <- rbind(return_df, list_peak_area_return[[i]])
  }
  
  
  max_response <- max(df[[column]])
  min_response <- min(df[[column]])
  range_response <- max_response - min_response
  
  
  max_range_index <- which.max(int_subset_norm_params$range)
  
  max_int <- int_subset_norm_params$max[max_range_index]
  min_int <- int_subset_norm_params$min[max_range_index]
  max_range_int <- int_subset_norm_params$range[max_range_index]
  
  int_subset_norm_params$range_ratio <- int_subset_norm_params$range / max_range_int
  
  
  print(int_subset_norm_params)
  
  list_int_subset_df_norm <- list()
  
  for (i in 1:peak_n){
    sub_int_df_norm_i <- list_integrated_df_subset[[i]]
    sub_int_df_norm_i$net_y_sum_norm <- ((((sub_int_df_norm_i$net_y_sum - int_subset_norm_params$min[i]) / (int_subset_norm_params$range[i]))) * int_subset_norm_params$range_ratio[i] * range_response * integral_percent)+(max_response*integral_overlay)
    sub_int_df_norm_i$peak_number <- paste0('peak_', i)
    list_int_subset_df_norm[[i]] <- sub_int_df_norm_i
  }
    
  combined_sub_int_df_norm <- list_int_subset_df_norm[[1]]
  for (i in 2:peak_n){
    combined_sub_int_df_norm <- rbind(combined_sub_int_df_norm, list_int_subset_df_norm[[i]])
  }
  
 # combined_integrated_df_subset_norm <- combined_integrated_df_subset
  #combined_integrated_df_subset_norm$net_y_sub_norm <- (((combined_integrated_df_subset$net_y_sum - min_int) * range_response) / (max_range_int)) + max_response
  
  plot2 <- plot_sciex_CE(df, interactive = FALSE)
  plot3 <- plot2 + geom_line(data = combined_sub_int_df_norm,
                             aes(x = time_m, y = net_y_sum_norm, color = peak_number)) +
    geom_line(data = combined_integrated_df_subset,
              aes(x = time_m, y = baseline, color = peak_number)) +
    theme(legend.position = 'none')#+
    #geom_vline(xintercept = area_return_df$starting_time, color = 'red') + 
    #geom_vline(xintercept = area_return_df$ending_time, color = 'red')
  plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
  show(plot4)
  
  return(percent_peak_areas(return_df, int_std = int_std))
}

percent_peak_areas <- function(df, int_std = 1) {
  total_peak_area <- sum(df$area)
  total_tca_median <- sum(df$corrected_area_median)
  total_tca_apex <- sum(df$corrected_area_apex)
  
  total_df <- data.frame(total_peak_area = total_peak_area,
                         total_tca_median = total_tca_median,
                         total_tca_apex = total_tca_apex)
  
  area_int_std <- df$area[int_std]
  tca_median_std <- df$corrected_area_median[int_std]
  tca_apex_std <- df$corrected_area_apex[int_std]
  
  peak_df <- df
  peak_df$percent_area <- (peak_df$area / total_peak_area) * 100
  peak_df$percent_tca_median <- (peak_df$corrected_area_median / total_tca_median) * 100
  peak_df$percent_tca_apex <- (peak_df$corrected_area_apex / total_tca_apex) * 100
  
  peak_df$percent_std_area <- (peak_df$area / area_int_std) * 100
  peak_df$percent_std_tca_median <- (peak_df$corrected_area_median / tca_median_std) * 100
  peak_df$percent_std_tca_apex <- (peak_df$corrected_area_apex / tca_apex_std) * 100
  
  peak_df$std <- ""
  peak_df$std[int_std] <- "*"
  
  return(list(peak_df, total_df))
}

