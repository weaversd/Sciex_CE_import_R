#Function to calculate areas for multiple peaks, and to return df with peak comparisons like %areas
#df is a single run df from create_sciex_table
#column is the channel name
#peak only sets all the baseline types to 'peak' default is TRUE. change to false for individual baselin calcs
#integral_percent is a value (0-1) that is the relative height of the integration traces to the peak traces. Default is 0.6
#integral_overlay (0-1) is how much the peaks and integration traces overlay (0 is totally, 1 is no overlap)
#int_std is the peak index of the internal standard (if there is one). Default is 1.
#integration display is whether to display absolute_tca or raw area as the integration traces. Default is tca, 'area' results in raw area
#PEAKS MUST BE INPUT IN ORDER OF MIGRATION TIME
sciex_multiple_peaks <- function(df, column = 'AU', peak_only = TRUE,
                                 integral_percent = 0.6, integral_overlay = 1,
                                 int_std = 1, integration_display = 'tca'){
  #show the initial plot
  plot1 <- plot_sciex_CE(df, ylab = column, interactive = TRUE)
  show(plot1)
  
  #ask for how many peaks to integrate
  peak_n <- as.numeric(readline(prompt = 'Number of peaks to integrate: '))
  
  #create lists for the ingetrated dfs, peak area stats, and integrated df subsets per peak
  list_integrated_dfs <- list()
  list_peak_area_return <- list()
  list_integrated_df_subset <- list()

  
  #create empty df, one row per peak, with columns for start and end times
  input_peak_df <- data.frame(peak = 1:peak_n,
                        peak_start = rep(NA, peak_n),
                        peak_end = rep(NA, peak_n))
  
  #for int normalizations, create df with row for each peak with min, max, range values for each integration
  int_subset_norm_params <- data.frame(peak_number = 1:peak_n,
                                       max = rep(NA, peak_n),
                                       min = rep(NA, peak_n),
                                       range = rep(NA, peak_n),
                                       range_ratio = rep(NA, peak_n))
  
  #for each peak
  for (i in 1:peak_n){
    #get peak start and end times
    pi_start <- as.numeric(readline(prompt = paste0('peak ', i, ' start: ')))
    pi_end <- as.numeric(readline(prompt = paste0('peak ', i, ' end: ')))
    
    #get integration df for that peak
    integrated_df_i <- add_integration_df(CE_df = df,
                                           peak_start = pi_start, peak_end = pi_end, multiples = peak_only)
    #add a column for the peak number
    integrated_df_i$peak_number <- paste0('peak_', i)
    
    #add it to the list of all integrated_dfs
    list_integrated_dfs[[i]] <- integrated_df_i
    
    #add peak start and end to peak dataframe
    input_peak_df$peak_start[i] <- pi_start
    input_peak_df$peak_end[i] <- pi_end
    
    #print out results for the peak using peak_area_height_time function, and save peak stats df
    print(paste0('Peak ', i, ' results:'))
    area_return_df_i <- peak_area_height_time(integrated_df_i, pi_start, pi_end)
    area_return_df_i$peak_number <- paste0('peak_', i)
    #add the peak stat df to the list of dfs
    list_peak_area_return[[i]] <- area_return_df_i
    
    #get row indeces for peak start and end
    t1_index <- which.min(abs(integrated_df_i$time_m-pi_start))
    t2_index <- which.min(abs(integrated_df_i$time_m-pi_end))
    
    #subset dataframe based on these indeces, and add it to the list
    integrated_df_subset_i <- integrated_df_i[t1_index:t2_index,]
    list_integrated_df_subset[[i]] <- integrated_df_subset_i
    
    #if the display is area, do calculations with net_y_sum
    if (integration_display == 'area') {
      #get min, max, range for ingetration trace
      max_int_sub_i <- max(integrated_df_subset_i$net_y_sum)
      min_int_sub_i <- min(integrated_df_subset_i$net_y_sum)
      range_int_sub_i <- max_int_sub_i - min_int_sub_i
      #store them in the integration trace df
      int_subset_norm_params$max[i] <- max_int_sub_i
      int_subset_norm_params$min[i] <- min_int_sub_i
      int_subset_norm_params$range[i] <- range_int_sub_i
      
    } else {
      #do calculations with net_y_sum_tca for tca trace showing. same as above
      max_int_sub_i <- max(integrated_df_subset_i$net_y_sum_tca)
      min_int_sub_i <- min(integrated_df_subset_i$net_y_sum_tca)
      range_int_sub_i <- max_int_sub_i - min_int_sub_i
      
      int_subset_norm_params$max[i] <- max_int_sub_i
      int_subset_norm_params$min[i] <- min_int_sub_i
      int_subset_norm_params$range[i] <- range_int_sub_i

    }
    
  }
  
  #add the first integrated df from the list to a new combined df
  combined_integrated_df_subset <- list_integrated_df_subset[[1]]
  #add the first peak stat df from the list to a new combined df
  return_df <- list_peak_area_return[[1]]
  
  #loop through remaining dfs in list and add them to the combined dfs
  for (i in 2:peak_n){
    combined_integrated_df_subset <- rbind(combined_integrated_df_subset, list_integrated_df_subset[[i]])
    return_df <- rbind(return_df, list_peak_area_return[[i]])
  }
  
  #caluclate max, min, range of y values for peak trace
  max_response <- max(df[[column]])
  min_response <- min(df[[column]])
  range_response <- max_response - min_response
  
  #get the index for the max range of integration traces
  max_range_index <- which.max(int_subset_norm_params$range)
  
  #using this index, get the maximum, min, and range of the largest int trace
  max_int <- int_subset_norm_params$max[max_range_index]
  min_int <- int_subset_norm_params$min[max_range_index]
  max_range_int <- int_subset_norm_params$range[max_range_index]
  
  #find ratio of int to peak response
  int_subset_norm_params$range_ratio <- int_subset_norm_params$range / max_range_int

  #create list to hold subsets of normalized integration dfs for each peak
  list_int_subset_df_norm <- list()
  
  #for each peak, take its subset integrated df, and calculate normalized sum_y values, area and tca
  #this is where the integral_overlay and integral_percent come in
  #save the updated df in the list of normalized subset dataframes (each df has a unique peak name)
  for (i in 1:peak_n){
    sub_int_df_norm_i <- list_integrated_df_subset[[i]]
    sub_int_df_norm_i$net_y_sum_norm <- ((((sub_int_df_norm_i$net_y_sum - int_subset_norm_params$min[i]) / (int_subset_norm_params$range[i]))) * int_subset_norm_params$range_ratio[i] * range_response * integral_percent)+(max_response*integral_overlay)
    sub_int_df_norm_i$net_y_sum_tca_norm <- ((((sub_int_df_norm_i$net_y_sum_tca - int_subset_norm_params$min[i]) / (int_subset_norm_params$range[i]))) * int_subset_norm_params$range_ratio[i] * range_response * integral_percent)+(max_response*integral_overlay)
    sub_int_df_norm_i$peak_number <- paste0('peak_', i)
    list_int_subset_df_norm[[i]] <- sub_int_df_norm_i
  }
    
  #from this list, create a combined df in the same manner as above
  combined_sub_int_df_norm <- list_int_subset_df_norm[[1]]
  for (i in 2:peak_n){
    combined_sub_int_df_norm <- rbind(combined_sub_int_df_norm, list_int_subset_df_norm[[i]])
  }
  
  #create a plot of the original data
  plot2 <- plot_sciex_CE(df, ylab = column, interactive = FALSE)
  
  #if we display raw area, add the traces in using net_y_sum_norm
  if (integration_display == 'area') {
    plot3 <- plot2 + geom_line(data = combined_sub_int_df_norm,
                               aes(x = time_m, y = net_y_sum_norm, color = peak_number)) +
      geom_line(data = combined_integrated_df_subset,
                aes(x = time_m, y = baseline, color = peak_number)) +
     theme(legend.position = 'none')
      
  } else {
    #if we display tca, add the traces using net_y_sum_tca_norm
    plot3 <- plot2 + geom_line(data = combined_sub_int_df_norm,
                               aes(x = time_m, y = net_y_sum_tca_norm, color = peak_number)) +
      geom_line(data = combined_integrated_df_subset,
                aes(x = time_m, y = baseline, color = peak_number)) +
      theme(legend.position = 'none')
  }
  #show the plot as a plotlyobject
  plot4 <- ggplotly(plot3, tooltip=c('text1', 'text2', 'y'))
  show(plot4)
  
  #calculate peak comparison stats, and save as a df
  calc_return_df <- percent_peak_areas(return_df, int_std = int_std)
  
  #print which integration is being displayed
  if (integration_display == 'area') {
    print('Integration is raw area')
  } else {
    print('Integration is absolute Time-Corrected Area')
  }
  
  #return the calculation dfs. This is a list where the first df is all peak stats, and the second is total area stats
  return(calc_return_df)
}

#add in percent peak area calcs, and internal standard calcs
#df is a df_from the sciex_multiple_peaks function, int_std is the peak number to use as internal std.
percent_peak_areas <- function(df, int_std = 1) {
 
   #sum up total peak areas and tcas
  total_peak_area <- sum(df$area)
  total_tca_median <- sum(df$corrected_area_median)
  total_tca_apex <- sum(df$corrected_area_apex)
  total_tca_absolute <- sum(df$corrected_area_absolute)
  
  #create df with total values
  total_df <- data.frame(total_peak_area = total_peak_area,
                         total_tca_median = total_tca_median,
                         total_tca_apex = total_tca_apex,
                         total_tca_absolute = total_tca_absolute)
  
  #find area and tcas of internal standard peaks
  area_int_std <- df$area[int_std]
  tca_median_std <- df$corrected_area_median[int_std]
  tca_apex_std <- df$corrected_area_apex[int_std]
  tca_absolute_std <- df$corrected_area_absolute[int_std]
  
  #set peak_df
  peak_df <- df
  
  #calculate percent area and percent tcas for each peak based on total areas and total tcas
  peak_df$percent_area <- (peak_df$area / total_peak_area) * 100
  peak_df$percent_tca_absolute <- (peak_df$corrected_area_absolute / total_tca_absolute) * 100
  peak_df$percent_tca_median <- (peak_df$corrected_area_median / total_tca_median) * 100
  peak_df$percent_tca_apex <- (peak_df$corrected_area_apex / total_tca_apex) * 100
  
  #calculate percent area and percent tcas for each peak based on internal standard area and tca
  peak_df$percent_std_area <- (peak_df$area / area_int_std) * 100
  peak_df$percent_std_tca_absolute <- (peak_df$corrected_area_absolute / tca_absolute_std) * 100
  peak_df$percent_std_tca_median <- (peak_df$corrected_area_median / tca_median_std) * 100
  peak_df$percent_std_tca_apex <- (peak_df$corrected_area_apex / tca_apex_std) * 100
  
  #set a column of empty strings
  peak_df$std <- ""
  
  #place an astrix in the row of the internal standard
  peak_df$std[int_std] <- "*"
  
  #return a list of the peak dataframe and the total dataframe
  return(list(peak_df, total_df))
}

