#! ---------------------------------------------------------------------------------------
#!
#! Description       :
#!
#! Authors           : Cyril Thébault <cyril.thebault@ucalgary.ca>
#!
#! Creation date     : 2024-09-12 09:30:11
#! Modification date :
#!
#! Comments          :
#!
#! ---------------------------------------------------------------------------------------

#! ----------------------------- workbook directory

#! ----------------------------- package loading

######################
# STREAMFLOW
######################


cumulative_frequency_curve_q <- function(dates, q){
  
  streamflow_data = data.frame(Date = dates, discharge = q)
  
  streamflow_data <- fill_df(streamflow_data, MonthStart = 1)
  
  
  # Calculate maximal annual values
  streamflow_maxima <- streamflow_data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), 
           Month = as.numeric(format(Date, "%m")),
           Hydro_Year = ifelse(Month >= 10, Year + 1, Year)) %>%
    group_by(Year) %>%
    summarize(
      AnnualMax = ifelse(mean(is.na(discharge)) < 0.1, 
                         max(discharge, na.rm = TRUE), 
                         NA),
      .groups = "drop"
    ) %>%
    filter(!is.na(AnnualMax))
  
  # Check if we have enough data
  if(nrow(streamflow_maxima) < 5){
    
    # Create an empty plot with a custom message
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Not enough data:\nless than 5 complete\nhydrological year\n(with less than 10% gap)", color = "red", size = 6, hjust = 0.5) +
      theme_bw() +  # Remove all axes and background elements
      labs(title = "Daily streamflow (CDF)",
           x = "Streamflow [mm/d]",
           y = "Cumulative Frequency")+
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
    
  } else {
  
    # Calculate quantiles and min/max values
    quantiles <- quantile(streamflow_data$discharge, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    min_val <- min(streamflow_data$discharge, na.rm = TRUE)
    max_val <- max(streamflow_data$discharge, na.rm = TRUE)
    
    ggplot() +
      geom_line(
        data = streamflow_data %>% filter(!is.na(discharge)) %>% arrange(discharge),
        aes(
          x = discharge,
          y = seq(0, 1, length.out = length(discharge))
        ), 
        color = "darkblue"
      ) +
      scale_x_log10()+
      coord_flip() +              # Invert x and y axes
      labs(
        title = "Daily streamflow (CDF)",
        x = "Streamflow [mm/d] (log scale)",
        y = "Cumulative Frequency"
      ) +
      
      # Highlight min, max, and quantiles
      geom_point(aes(x = min_val, y = 0), color = "darkblue", size = 3, shape = 16) +  # Min value
      geom_point(aes(x = max_val, y = 1), color = "darkblue", size = 3, shape = 16) +  # Max value
      geom_point(aes(x = quantiles[1], y = 0.25), color = "darkblue", size = 3, shape = 16) +  # 25th quantile
      geom_point(aes(x = quantiles[2], y = 0.50), color = "darkblue", size = 3, shape = 16) +  # 50th quantile
      geom_point(aes(x = quantiles[3], y = 0.75), color = "darkblue", size = 3, shape = 16) +  # 75th quantile
      
      # Add labels for the points
      annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "darkblue", hjust = -0.1) +
      annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "darkblue", hjust = -0.1) +
      annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "darkblue", hjust = -0.1) +
      annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "darkblue", hjust = -0.1) +
      annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "darkblue", hjust = -0.1) +
      
      theme_bw()+
      theme(axis.title.x = element_text(color = "darkblue"),  # Color of x-axis title
           axis.title.y = element_text(color = "darkblue"),  # Color of y-axis title
           axis.text.x = element_text(color = "darkblue"),    # Color of x-axis text
           axis.text.y = element_text(color = "darkblue", angle = 90, hjust = 0.5),    # Color of y-axis text
           plot.title = element_text(color = "darkblue"),     # Color of plot title
           legend.title = element_text(color = "darkblue"),   # Color of legend title
           panel.border = element_rect(color = "darkblue"))
  }
} 


annual_q_barplot <- function(dates, q, dateslim) {
  
  streamflow_data = data.frame(Date = dates, discharge = q)
  
  streamflow_data =fill_df(streamflow_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  streamflow_data <- streamflow_data %>%
    mutate(
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(discharge))  %>% # Check for NA values in Annual_Discharge
    summarise(Annual_Discharge = sum(discharge, na.rm = TRUE),
              Na_Flag = any(na_flag))

  
  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  streamflow_data_filtered <- full_years %>%
    left_join(streamflow_data, by = "Year")

  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  ggplot(streamflow_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_Discharge, fill = ifelse(Na_Flag, "Incomplete year", "Complete year")), stat = "identity", position = "dodge") +  # Tmean bars
    scale_fill_manual(values = c("Complete year" = "darkblue",
                                 "Incomplete year" = "firebrick4"), 
                      name = NULL,
                      breaks = "Incomplete year",
                      na.translate = FALSE) +  # Remove legend title
    labs(
      title = "Mean annual streamflow",
      y = "Streamflow [mm/year]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break) +  # Set x-axis limits
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "darkblue"),  # Color of x-axis title
      axis.title.y = element_text(color = "darkblue"),  # Color of y-axis title
      axis.text.x = element_text(color = "darkblue"),    # Color of x-axis text
      axis.text.y = element_text(color = "darkblue", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "darkblue"),     # Color of plot title
      legend.title = element_text(color = "darkblue"),   # Color of legend title
      panel.border = element_rect(color = "darkblue"),
      legend.position = c(0.17, 0.9),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
  
  
}


monthly_q_regime_boxplot <- function(dates, q) {

  streamflow_data = data.frame(Date = dates, discharge = q)
  
  
  streamflow_data <- fill_df(streamflow_data, MonthStart = 1)
  
  
  # Calculate maximal annual values
  streamflow_maxima <- streamflow_data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), 
           Month = as.numeric(format(Date, "%m")),
           Hydro_Year = ifelse(Month >= 10, Year + 1, Year)) %>%
    group_by(Year) %>%
    summarize(
      AnnualMax = ifelse(mean(is.na(discharge)) < 0.1, 
                         max(discharge, na.rm = TRUE), 
                         NA),
      .groups = "drop"
    ) %>%
    filter(!is.na(AnnualMax))
  
  # Check if we have enough data
  if(nrow(streamflow_maxima) < 5){
    
    # Create an empty plot with a custom message
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Not enough data:\nless than 5 complete\nhydrological year\n(with less than 10% gap)", color = "red", size = 6, hjust = 0.5) +
      theme_bw() +  # Remove all axes and background elements
      labs(
        title = "Mean monthly streamflow regime",
        x = "Month",
        y = "Streamflow [mm/month]"
      ) +
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
    
  } else {
  
    # Aggregate by month and year, summing daily discharge
    streamflow_data <- streamflow_data %>%
      mutate(YearMonth = format(Date, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarize(discharge = sum(discharge, na.rm = TRUE)) %>%
      mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
             Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
    
    
    ggplot(streamflow_data, aes(x = Month, y = discharge)) +
      geom_boxplot(fill = "darkblue", color = "darkblue", alpha = 0.3, outliers = FALSE) +
      scale_y_continuous(limits = c(0, NA)) + 
      labs(
        title = "Mean monthly streamflow regime",
        x = "Month",
        y = "Streamflow [mm/month]"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(color = "darkblue"),  # Color of x-axis title
        axis.title.y = element_text(color = "darkblue"),  # Color of y-axis title
        axis.text.x = element_text(color = "darkblue"),    # Color of x-axis text
        axis.text.y = element_text(color = "darkblue", angle = 90, hjust = 0.5),    # Color of y-axis text
        plot.title = element_text(color = "darkblue"),     # Color of plot title
        legend.title = element_text(color = "darkblue"),   # Color of legend title
        panel.border = element_rect(color = "darkblue"),
        legend.position = c(0.1, 0.9),  # Position legend at the top-left corner
        legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
      ) 
  }
}


distribution_q_maxima_plot <- function(dates, q) {
  
  # Create dataframe for streamflow
  streamflow_data = data.frame(Date = dates, discharge = q)
  
  # Fill the data.frame to define complete years
  streamflow_data <- fill_df(streamflow_data, MonthStart = 1)
  
  # Calculate maximal annual values for precipitation
  streamflow_maxima <- streamflow_data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), 
           Month = as.numeric(format(Date, "%m")),
           Hydro_Year = ifelse(Month >= 10, Year + 1, Year)) %>%
    group_by(Year) %>%
    summarize(
      AnnualMax = ifelse(mean(is.na(discharge)) < 0.1, 
                         max(discharge, na.rm = TRUE), 
                         NA),
      .groups = "drop"
    ) %>%
    filter(!is.na(AnnualMax))
  
  if(nrow(streamflow_maxima) < 5){
    
    # Create an empty plot with a custom message
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Not enough data:\nless than 5 complete\nhydrological year\n(with less than 10% gap)", color = "red", size = 6, hjust = 0.5) +
      theme_bw() +  # Remove all axes and background elements
      labs(title = "Annual maximum daily streamflow",
           x = "Return period [years]",
           y = "Streamflow [mm/d]")+
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
    
  } else {
    
    fit_GEV <- fevd(streamflow_maxima$AnnualMax, type = "GEV")
    
    streamflow_maxima <- streamflow_maxima %>%
      mutate(
        Rang = row_number(),
        Freq = pgev(streamflow_maxima$AnnualMax, fit_GEV$results$par[1], fit_GEV$results$par[2], fit_GEV$results$par[3]),
        ReturnPeriod = 1 / (1-Freq) 
      )
    
    
    # Plot distributions of maximum values as a function of return period
    ggplot(streamflow_maxima, aes(x = ReturnPeriod, y = AnnualMax)) +
      geom_point(shape = 4, color = "darkblue") +
      scale_x_log10() +  
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Annual maximum daily streamflow",
        x = "Return period [years] (log-scale)",
        y = "Streamflow [mm/d]"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
    
  }
  
}


distribution_q_mna_plot <- function(dates, q) {
  
  # Create dataframe for streamflow
  streamflow_data = data.frame(Date = dates, discharge = q)
  
  # Fill the data.frame to define complete years
  streamflow_data <- fill_df(streamflow_data, MonthStart = 1)
  
  # Extract year and month from the date and aggregate at the monthly time step
  monthly_streamflow_data <- streamflow_data %>%
    mutate(Year = format(Date, "%Y"),
           Month = format(Date, "%m")) %>%
    group_by(Year, Month) %>%
    summarise(
      discharge = ifelse(mean(is.na(discharge)) < 0.1, sum(discharge, na.rm = TRUE), NA),
      .groups = "drop"
    )
  
  
  # Calculate minimum annual values for monthly streamflow
  streamflow_minima <- monthly_streamflow_data %>%
    group_by(Year) %>%
    summarize(
      AnnualMin = ifelse(mean(is.na(discharge)) < 0.1, 
                         min(discharge, na.rm = TRUE), 
                         NA),
      .groups = "drop"
    ) %>%
    filter(!is.na(AnnualMin))
  
  if(nrow(streamflow_minima) < 5){
    
    # Create an empty plot with a custom message
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Not enough data:\nless than 5 complete\nhydrological year\n(with less than 10% gap)", color = "red", size = 6, hjust = 0.5) +
      theme_bw() +  # Remove all axes and background elements
      labs(title = "Annual minimum monthly streamflow",
           x = "Return period [years]",
           y = "Streamflow [mm/month]")+
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
    
  } else {
      
    # Fit a log-normal distribution to the annual minima
    
    epsilon = 0.0001
    # Transform the data to log scale
    log_discharge <- log(streamflow_minima$AnnualMin + epsilon)
    
    # Estimate parameters for the normal distribution on the log-transformed data
    meanlog <- mean(log_discharge, na.rm = TRUE)
    sdlog <- sd(log_discharge, na.rm = TRUE)
  
    
    # Calculate the return period based on the inverted values
    streamflow_minima <- streamflow_minima %>%
      mutate(
        Freq = plnorm(AnnualMin, meanlog = meanlog, sdlog = sdlog),
        ReturnPeriod = 1 / Freq # In low-flow context, return period increases as flow decreases
      )
    
    
    # CDF
    ggplot(streamflow_minima, aes(x = ReturnPeriod, y = AnnualMin)) +
      geom_point(shape = 4, color = "darkblue") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_log10() +  # Utiliser une échelle logarithmique pour la période de retour
      labs(
        title = "Annual minimum monthly streamflow",
        x = "Return period [years] (log-scale)",
        y = "Streamflow [mm/month]"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(color = "darkblue"),
        axis.title.y = element_text(color = "darkblue"),
        axis.text.x = element_text(color = "darkblue"),
        axis.text.y = element_text(color = "darkblue"),
        plot.title = element_text(color = "darkblue"),
        legend.title = element_text(color = "darkblue"),
        panel.border = element_rect(color = "darkblue")
      )
  }
}


######################
# PRECIPITATION
######################


cumulative_frequency_curve_p <- function(dates, p){
  
  precipitation_data = data.frame(Date = dates, precip = p)
  
  precipitation_data = precipitation_data[precipitation_data$precip >= 0.1,]
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(precipitation_data$precip, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(precipitation_data$precip, na.rm = TRUE)
  max_val <- max(precipitation_data$precip, na.rm = TRUE)
  
  ggplot() +
    geom_line(data = precipitation_data, aes(x = sort(precip), y = seq(0,1,length.out = length(precip))), color = "royalblue") +  # Empirical CDF (cumulative frequency curve)
    scale_x_log10() +           # Log scale for the x-axis (Precipitation)
    coord_flip() +              # Invert x and y axes
    labs(
      title = "Daily precipitation (P > 0.1 mm/d) (CDF)",
      x = "Precipitation [mm/d] (log scale)",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "royalblue", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "royalblue", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "royalblue", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "royalblue", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "royalblue", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "royalblue", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "royalblue", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "royalblue", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "royalblue", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "royalblue", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "royalblue"),  # Color of x-axis title
          axis.title.y = element_text(color = "royalblue"),  # Color of y-axis title
          axis.text.x = element_text(color = "royalblue"),    # Color of x-axis text
          axis.text.y = element_text(color = "royalblue", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "royalblue"),     # Color of plot title
          legend.title = element_text(color = "royalblue"),   # Color of legend title
          panel.border = element_rect(color = "royalblue"))
  
} 


annual_p_barplot <- function(dates, p, tmean, dateslim) {
  
  precipitation_data = data.frame(Date = dates, precip = p, tmean = tmean)
  
  precipitation_data = fill_df(precipitation_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  precipitation_data <- precipitation_data %>%
    mutate(
      snow = ifelse(tmean <= 0, precip, 0),
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(precip))  %>% # Check for NA values in Annual_precip
    summarise(Annual_precip = sum(precip, na.rm = TRUE),
              Annual_snow = sum(snow, na.rm = TRUE),
              Na_Flag = any(na_flag))
  

  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  precipitation_data_filtered <- full_years %>%
    left_join(precipitation_data, by = "Year") 
  
  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  
  # Create the plot with red bars for Na_Flag == TRUE
  ggplot(precipitation_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_precip, fill = ifelse(Na_Flag, "Incomplete year", "Total precipitation")), 
             stat = "identity", position = "dodge") +  # Precipitation bars
    geom_bar(aes(y = Annual_snow, fill = ifelse(Na_Flag, "Incomplete year", "Solid precipitation")), 
             stat = "identity", position = "dodge") +  # Snow bars
    scale_fill_manual(values = c("Total precipitation" = "royalblue", 
                                 "Solid precipitation" = "lightblue3", 
                                 "Incomplete year" = "firebrick4"), 
                      name = NULL, na.translate = FALSE ) +  # Remove legend title
    labs(
      title = "Mean annual precipitation",
      y = "Precipitation [mm/year]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break)+
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "royalblue"),
      axis.title.y = element_text(color = "royalblue"),
      axis.text.x = element_text(color = "royalblue"),
      axis.text.y = element_text(color = "royalblue", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "royalblue"),
      legend.title = element_text(color = "royalblue"),
      panel.border = element_rect(color = "royalblue"),
      legend.position = c(0.19, 0.85),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
  
}


monthly_p_regime_boxplot <- function(dates, p, tmean) {
  
  precipitation_data = data.frame(Date = dates, precip = p, tmean = tmean)
  
  precipitation_data = fill_df(precipitation_data, MonthStart = 1)
  
  # Aggregate by month and year, summing daily precip
  precipitation_data <- precipitation_data %>%
    mutate(
      snow = ifelse(tmean <= 0, precip, 0),
      YearMonth = format(Date, "%Y-%m")
    ) %>%
    group_by(YearMonth) %>%
    mutate(na_flag = is.na(precip))  %>%
    summarize(precip = sum(precip, na.rm = TRUE),
              snow = sum(snow, na.rm = TRUE)) %>%
    mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
           Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
  
  # Reshape data to long format
  precip_long <- precipitation_data %>%
    pivot_longer(cols = c(precip, snow),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(
      type = recode(type,
                    precip = "Total precipitation",
                    snow = "Solid precipitation")  # Rename for legend clarity
    )
  
  
  ggplot(precip_long, aes(x = Month, y = value, fill = type, colour = type)) +
    geom_boxplot(alpha = 0.3, position = position_dodge(width = 0.8), outlier.shape = NA) +
    scale_fill_manual(values = c("Total precipitation" = "royalblue", "Solid precipitation" = "lightblue3"), 
                      name = NULL, na.translate = FALSE ) +
    scale_colour_manual(values = c("Total precipitation" = "royalblue", "Solid precipitation" = "lightblue3"),
                        name = NULL, na.translate = FALSE ) +
    scale_y_continuous(limits = c(0, NA))+
    labs(
      title = "Mean monthly precipitation regime",
      x = "Month",
      y = "Precipitation [mm/month]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "royalblue"),  # Color of x-axis title
      axis.title.y = element_text(color = "royalblue"),  # Color of y-axis title
      axis.text.x = element_text(color = "royalblue"),    # Color of x-axis text
      axis.text.y = element_text(color = "royalblue", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "royalblue"),     # Color of plot title
      legend.title = element_text(color = "royalblue"),   # Color of legend title
      panel.border = element_rect(color = "royalblue"),
      legend.position = c(0.19, 0.85),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
}


distribution_p_maxima_plot <- function(dates, p) {
  
  precipitation_data = data.frame(Date = dates, precip = p)
  
  # Fill the data.frame to define complete years
  precipitation_data <- fill_df(precipitation_data, MonthStart = 1)
  
  # Calculate maximal annual values for precipitation
  precipitation_maxima <- precipitation_data %>%
    mutate(Year = as.numeric(format(Date, "%Y")), 
           Month = as.numeric(format(Date, "%m")),
           Hydro_Year = ifelse(Month >= 10, Year + 1, Year)) %>%
    group_by(Year) %>%
    summarize(
      AnnualMax = ifelse(mean(is.na(precip)) < 0.1, 
                         max(precip, na.rm = TRUE), 
                         NA),
      .groups = "drop"
    ) %>%
    filter(!is.na(AnnualMax))
  
    
  fit_GEV <- fevd(precipitation_maxima$AnnualMax, type = "GEV")
  
  precipitation_maxima <- precipitation_maxima %>%
    mutate(
      Rang = row_number(),
      Freq = pgev(precipitation_maxima$AnnualMax, fit_GEV$results$par[1], fit_GEV$results$par[2], fit_GEV$results$par[3]),
      ReturnPeriod = 1 / (1-Freq) 
    )
  
  
  # Plot distributions of maximum values as a function of return period
  ggplot(precipitation_maxima, aes(x = ReturnPeriod, y = AnnualMax)) +
    geom_point(shape = 4, color = "royalblue") +
    scale_x_log10() +  
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Annual maximum daily precipitation",
      x = "Return period [years] (log-scale)",
      y = "Precipitation [mm/d]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "royalblue"),
      axis.title.y = element_text(color = "royalblue"),
      axis.text.x = element_text(color = "royalblue"),
      axis.text.y = element_text(color = "royalblue"),
      plot.title = element_text(color = "royalblue"),
      legend.title = element_text(color = "royalblue"),
      panel.border = element_rect(color = "royalblue")
    )
    
}


######################
# PET
######################


cumulative_frequency_curve_pet <- function(dates, pet_ini){
  
  PET_data = data.frame(Date = dates, PETdataset = pet_ini)
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(PET_data$PETdataset, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(PET_data$PETdataset, na.rm = TRUE)
  max_val <- max(PET_data$PETdataset, na.rm = TRUE)
  
  ggplot() +
    # geom_line(data = PET_data, aes(x = sort(PEToudin), y = seq(0,1,length.out = length(PEToudin)), color = "PET Oudin")) +  # Empirical CDF (cumulative frequency curve)
    geom_line(data = PET_data, aes(x = sort(PETdataset), y = seq(0,1,length.out = length(PETdataset)), color = "PET dataset")) +  # Empirical CDF (cumulative frequency curve)
    scale_color_manual(values = c("PET Oudin" = "darkseagreen2", "PET dataset" = "forestgreen"), name = NULL) +  # Remove legend title
    coord_flip() +              
    labs(
      title = "Daily PET (CDF)",
      x = "PET [mm/d] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "forestgreen", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "forestgreen", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "forestgreen", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "forestgreen", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "forestgreen", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "forestgreen", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "forestgreen", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "forestgreen", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "forestgreen", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "forestgreen", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "forestgreen"),  # Color of x-axis title
          axis.title.y = element_text(color = "forestgreen"),  # Color of y-axis title
          axis.text.x = element_text(color = "forestgreen"),    # Color of x-axis text
          axis.text.y = element_text(color = "forestgreen", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "forestgreen"),     # Color of plot title
          legend.title = element_text(color = "forestgreen"),   # Color of legend title
          panel.border = element_rect(color = "forestgreen"),
          legend.position = 'none',  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
          legend.text = element_text(size = 8),  # Reduce legend text size
          legend.key.size = unit(0.8, "lines"))  # Reduce legend key size
  
} 


annual_pet_barplot <- function(dates, pet_ini, dateslim) {
  
  PET_data = data.frame(Date = dates, PETdataset = pet_ini)
  
  PET_data = fill_df(PET_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  PET_data <- PET_data %>%
    mutate(
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(PETdataset))  %>% # Check for NA values in Annual_precip
    summarise(Annual_PETdataset = sum(PETdataset, na.rm = TRUE),
              Na_Flag = any(na_flag))
  

  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  PET_data_filtered <- full_years %>%
    left_join(PET_data, by = "Year") 
  
  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  ggplot(PET_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_PETdataset, fill = ifelse(Na_Flag, "Incomplete year", "PET dataset")), stat = "identity", position = "dodge") + 
    scale_fill_manual(values = c("PET Oudin" = "darkseagreen2", 
                                 "PET dataset" = "forestgreen",
                                 "Incomplete year" = "firebrick4"),
                      name = NULL, 
                      breaks = "Incomplete year",
                      na.translate = FALSE) +  # Remove legend title
    labs(
      title = "Mean annual PET",
      y = "PET [mm/year]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break) +  # Set x-axis limits
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "forestgreen"),
      axis.title.y = element_text(color = "forestgreen"),
      axis.text.x = element_text(color = "forestgreen"),
      axis.text.y = element_text(color = "forestgreen", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "forestgreen"),
      legend.title = element_text(color = "forestgreen"),
      panel.border = element_rect(color = "forestgreen"),
      legend.position = c(0.17, 0.85),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
  
  
}


monthly_pet_regime_boxplot <- function(dates, pet_ini) {
  
  PET_data = data.frame(Date = dates, PETdataset = pet_ini)
  
  # Aggregate by month and year, summing daily PEToudin
  PET_data <- PET_data %>%
    mutate(YearMonth = format(Date, "%Y-%m")) %>%
    group_by(YearMonth) %>%
    summarize(PETdataset = sum(PETdataset, na.rm = TRUE)) %>%
    mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
           Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
  
  # Reshape the data to long format
  PET_data_long <- PET_data %>%
    pivot_longer(cols = c(PETdataset), names_to = "PET_type", values_to = "PET_value")
  
  
  ggplot(PET_data_long, aes(x = Month, y = PET_value, fill = PET_type, color = PET_type)) +
    geom_boxplot(alpha = 0.3, outliers = FALSE) +
    # scale_y_continuous(limits = c(0, NA))+
    scale_fill_manual(values = c("PEToudin" = "darkseagreen2", "PETdataset" = "forestgreen"), 
                      labels = c("PETdataset" = "PET dataset", "PEToudin" = "PET Oudin"),
                      name = NULL) +
    scale_color_manual(values = c("PEToudin" = "darkseagreen2", "PETdataset" = "forestgreen"), 
                      labels = c("PETdataset" = "PET dataset", "PEToudin" = "PET Oudin"),
                      name = NULL) +
    labs(
      title = "Mean monthly PET regime",
      x = "Month",
      y = "PET [mm/month]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "forestgreen"),  # Color of x-axis title
      axis.title.y = element_text(color = "forestgreen"),  # Color of y-axis title
      axis.text.x = element_text(color = "forestgreen"),    # Color of x-axis text
      axis.text.y = element_text(color = "forestgreen", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "forestgreen"),     # Color of plot title
      legend.title = element_text(color = "forestgreen"),   # Color of legend title
      panel.border = element_rect(color = "forestgreen"),
      legend.position = 'none',  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
}


######################
# Temperature
######################


cumulative_frequency_curve_tmean <- function(dates, tmean){
  
  Tmean_data = data.frame(Date = dates, Tmean = tmean)
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(Tmean_data$Tmean, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(Tmean_data$Tmean, na.rm = TRUE)
  max_val <- max(Tmean_data$Tmean, na.rm = TRUE)
  
  ggplot() +
    geom_line(data = Tmean_data, aes(x = sort(Tmean), y = seq(0,1,length.out = length(Tmean))), color = "darkorange") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Daily temperature (CDF)",
      x = "Temperature [°C] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "darkorange", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "darkorange", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "darkorange", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "darkorange", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "darkorange", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "darkorange", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "darkorange", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "darkorange", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "darkorange", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "darkorange", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "darkorange"),  # Color of x-axis title
          axis.title.y = element_text(color = "darkorange"),  # Color of y-axis title
          axis.text.x = element_text(color = "darkorange"),    # Color of x-axis text
          axis.text.y = element_text(color = "darkorange", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "darkorange"),     # Color of plot title
          legend.title = element_text(color = "darkorange"),   # Color of legend title
          panel.border = element_rect(color = "darkorange"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
  
} 


annual_tmean_barplot <- function(dates, tmean, dateslim) {
  
  Tmean_data = data.frame(Date = dates, Tmean=tmean)
  
  Tmean_data = fill_df(Tmean_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  Tmean_data <- Tmean_data %>%
    mutate(
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(Tmean))  %>% # Check for NA values in Annual_precip
    summarise(Annual_Tmean = mean(Tmean, na.rm = TRUE),
              Na_Flag = any(na_flag))
  

  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  Tmean_data_filtered <- full_years %>%
    left_join(Tmean_data, by = "Year") 
  
  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  ggplot(Tmean_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_Tmean, fill = ifelse(Na_Flag, "Incomplete year", "Complete year")), stat = "identity", position = "dodge") +  # Tmean bars
    scale_fill_manual(values = c("Complete year" = "darkorange",
                                 "Incomplete year" = "firebrick4"), 
                      name = NULL, 
                      breaks = "Incomplete year",
                      na.translate = FALSE) +  # Remove legend title
    labs(
      title = "Mean annual temperature",
      y = "Temperature [°C]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break) +  # Set x-axis limits
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "darkorange"),
      axis.title.y = element_text(color = "darkorange"),
      axis.text.x = element_text(color = "darkorange"),
      axis.text.y = element_text(color = "darkorange", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "darkorange"),
      legend.title = element_text(color = "darkorange"),
      panel.border = element_rect(color = "darkorange"),
      legend.position = c(0.17, 0.85),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black"),  # White background with border
      legend.text = element_text(size = 8),  # Reduce legend text size
      legend.key.size = unit(0.8, "lines")  # Reduce legend key size
    )
  
  
}


monthly_tmean_regime_boxplot <- function(dates, tmean) {
  
  Tmean_data = data.frame(Date = dates, Tmean = tmean)
  
  # Aggregate by month and year, summing daily Tmean
  Tmean_data <- Tmean_data %>%
    mutate(YearMonth = format(Date, "%Y-%m")) %>%
    group_by(YearMonth) %>%
    summarize(Tmean = mean(Tmean, na.rm = TRUE)) %>%
    mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
           Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
  
  
  
  ggplot(Tmean_data, aes(x = Month, y = Tmean)) +
    geom_boxplot(alpha = 0.3, outliers = FALSE, fill = "darkorange", color = "darkorange") +
    # scale_y_continuous(limits = c(0, NA))+
    labs(
      title = "Mean monthly temperature regime",
      x = "Month",
      y = "Temperature [°C]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "darkorange"),  # Color of x-axis title
      axis.title.y = element_text(color = "darkorange"),  # Color of y-axis title
      axis.text.x = element_text(color = "darkorange"),    # Color of x-axis text
      axis.text.y = element_text(color = "darkorange", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "darkorange"),     # Color of plot title
      legend.title = element_text(color = "darkorange"),   # Color of legend title
      panel.border = element_rect(color = "darkorange"),
      legend.position = c(0.1, 0.9),  # Position legend at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    )
}


######################
# RADIATION
######################

finger_print_rad <- function(dates, lwr, swr){
  
  wr_data <- data.frame(dates = dates, 
                        lwr = lwr, 
                        swr = swr)
  
  df_contour <- wr_data %>%
    mutate(hour = hour(dates),
           doy = yday(dates),
           total_rad = swr + lwr) %>%
    group_by(doy, hour) %>%
    summarise(mean_total_rad = mean(total_rad, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mean_total_rad = pmax(mean_total_rad, 0))
  
  # Create vector for day of the year corresponding to the first of each month
  first_day_of_month <- yday(ymd(paste0("1950-", 1:12, "-01")))
  
  ggplot(df_contour, aes(x = hour, y = doy)) +
    geom_contour(aes(z = mean_total_rad, color = ..level..)) +
    labs(title = "Total Radiation (short-wave + long-wave)",
         x = "Hour of Day", 
         y = "Day of Year", 
         color = expression("Radiation (W/m"^2*")")) +
    scale_x_continuous(breaks = seq(0, 24, by = 4),  # Set x-axis to 4-hour intervals
                       expand = c(0, 0)) +  # Remove padding around the plot
    scale_y_continuous(breaks = first_day_of_month, 
                       labels = month(1:12, label = TRUE, abbr = TRUE),
                       expand = c(0, 0)) +  # Remove padding on y-axis
    scale_color_viridis_c() +  # Change color scale here
    # scale_color_gradient(low = "lightpink", high = "firebrick4") +  # Gradient from light red to dark red
    theme_bw()  +
    theme(
      axis.title.x = element_text(color = "firebrick3"),  # Color of x-axis title
      axis.title.y = element_text(color = "firebrick3"),  # Color of y-axis title
      axis.text.x = element_text(color = "firebrick3"),    # Color of x-axis text
      axis.text.y = element_text(color = "firebrick3", angle = 0, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "firebrick3"),     # Color of plot title
      legend.title = element_text(color = "firebrick3"),   # Color of legend title
      panel.border = element_rect(color = "firebrick3"),
      legend.key.height = unit(0.4, "cm"),                           # Thinner colorbar
      legend.key.width = unit(0.3, "cm")                             # Narrower colorbar (optional)
      # legend.position = c(0.1, 0.7),  # Position legend at the top-left corner
      # legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    )
  
}

finger_print_rad_fill <- function(dates, lwr, swr){
  
  wr_data <- data.frame(dates = dates, 
                        lwr = lwr, 
                        swr = swr)
  
  df_contour <- wr_data %>%
    mutate(hour = hour(dates),
           doy = yday(dates),
           total_rad = swr + lwr) %>%
    group_by(doy, hour) %>%
    summarise(mean_total_rad = mean(total_rad, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mean_total_rad = pmax(mean_total_rad, 0))
  
  # Create vector for day of the year corresponding to the first of each month
  first_day_of_month <- yday(ymd(paste0("1950-", 1:12, "-01")))
  
  ggplot(df_contour, aes(x = hour, y = doy, z = mean_total_rad)) +
    geom_contour_filled() +
    labs(title = "Total Radiation (short-wave + long-wave)",
         x = "Hour of Day", 
         y = "Day of Year", 
         fill = expression("Radiation (W/m"^2*")")) +
    scale_x_continuous(breaks = seq(0, 24, by = 4),  # Set x-axis to 4-hour intervals
                       expand = c(0, 0)) +  # Remove padding around the plot
    scale_y_continuous(breaks = first_day_of_month, 
                       labels = month(1:12, label = TRUE, abbr = TRUE),
                       expand = c(0, 0)) +  # Remove padding on y-axis
    theme_bw()  +
    theme(
      axis.title.x = element_text(color = "firebrick3"),  # Color of x-axis title
      axis.title.y = element_text(color = "firebrick3"),  # Color of y-axis title
      axis.text.x = element_text(color = "firebrick3"),    # Color of x-axis text
      axis.text.y = element_text(color = "firebrick3", angle = 0, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "firebrick3"),     # Color of plot title
      legend.title = element_text(color = "firebrick3"),   # Color of legend title
      panel.border = element_rect(color = "firebrick3"),
      legend.key.height = unit(0.4, "cm"),                           # Thinner colorbar
      legend.key.width = unit(0.3, "cm")                             # Narrower colorbar (optional)
      # legend.position = c(0.1, 0.7),  # Position legend at the top-left corner
      # legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    )
  
  
}

######################
# HUMIDITY
######################


cumulative_frequency_curve_rh <- function(dates, rh){
  
  if (any(rh>50)){
    rh_per = rh
  } else {
    rh_per = rh*100
  }
  
  rh_data = data.frame(Date = dates, rh = rh_per)
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(rh_data$rh, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(rh_data$rh, na.rm = TRUE)
  max_val <- max(rh_data$rh, na.rm = TRUE)
  
  ggplot() +
    geom_line(data = rh_data, aes(x = sort(rh), y = seq(0,1,length.out = length(rh))), color = "plum3") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Daily relative humidity (CDF)",
      x = "Relative humidity [%] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "plum3", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "plum3", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "plum3", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "plum3", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "plum3", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "plum3", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "plum3", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "plum3", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "plum3", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "plum3", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "plum3"),  # Color of x-axis title
          axis.title.y = element_text(color = "plum3"),  # Color of y-axis title
          axis.text.x = element_text(color = "plum3"),    # Color of x-axis text
          axis.text.y = element_text(color = "plum3", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "plum3"),     # Color of plot title
          legend.title = element_text(color = "plum3"),   # Color of legend title
          panel.border = element_rect(color = "plum3"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
  
} 


annual_rh_barplot <- function(dates, rh, dateslim) {
  
  if (any(rh>50)){
    rh_per = rh
  } else {
    rh_per = rh*100
  }
  
  rh_data = data.frame(Date = dates, rh = rh_per)
  
  rh_data = fill_df(rh_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  rh_data <- rh_data %>%
    mutate(
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(rh))  %>% # Check for NA values in Annual_precip
    summarise(Annual_rh = mean(rh, na.rm = TRUE),
              Na_Flag = any(na_flag))
  

  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  rh_data_filtered <- full_years %>%
    left_join(rh_data, by = "Year") 
  
  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  ggplot(rh_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_rh, fill = ifelse(Na_Flag, "Incomplete year", "Complete year")), stat = "identity", position = "dodge") +  # rh bars
    scale_fill_manual(values = c("Complete year" = "plum3",
                                 "Incomplete year" = "firebrick4"), 
                      name = NULL, 
                      breaks = "Incomplete year",
                      na.translate = FALSE) +  # Remove legend title
    labs(
      title = "Mean annual Relative humidity",
      y = "Relative humidity [%]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break) +  # Set x-axis limits
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "plum3"),
      axis.title.y = element_text(color = "plum3"),
      axis.text.x = element_text(color = "plum3"),
      axis.text.y = element_text(color = "plum3", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "plum3"),
      legend.title = element_text(color = "plum3"),
      panel.border = element_rect(color = "plum3"),
      legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with border
    )
  
  
}


monthly_rh_regime_boxplot <- function(dates, rh) {
  
  if (any(rh>50)){
    rh_per = rh
  } else {
    rh_per = rh*100
  }
  
  rh_data = data.frame(Date = dates, rh = rh_per)
  
  # Aggregate by month and year, summing daily rh
  rh_data <- rh_data %>%
    mutate(YearMonth = format(Date, "%Y-%m")) %>%
    group_by(YearMonth) %>%
    summarize(rh = mean(rh, na.rm = TRUE)) %>%
    mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
           Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
  
  
  
  ggplot(rh_data, aes(x = Month, y = rh)) +
    geom_boxplot(alpha = 0.3, outliers = FALSE, fill = "plum3", color = "plum3") +
    # scale_y_continuous(limits = c(0, NA))+
    labs(
      title = "Mean monthly relative humidity regime",
      x = "Month",
      y = "Relative humidity [%]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "plum3"),  # Color of x-axis title
      axis.title.y = element_text(color = "plum3"),  # Color of y-axis title
      axis.text.x = element_text(color = "plum3"),    # Color of x-axis text
      axis.text.y = element_text(color = "plum3", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "plum3"),     # Color of plot title
      legend.title = element_text(color = "plum3"),   # Color of legend title
      panel.border = element_rect(color = "plum3"),
      legend.position = c(0.1, 0.9),  # Position legend at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    )
}


######################
# WIND
######################

rose_wind <- function(direction, speed){
  
  # Create a data frame
  wind_data <- data.frame(direction = direction, speed = speed)
  
  wind_data$direction <- ifelse(wind_data$direction == 360, 0, wind_data$direction)
  
  # Create bins for directions
  wind_data$direction_bin <- cut(wind_data$direction,
                                 breaks = seq(0, 360, by = 5),
                                 right = FALSE,
                                 labels = seq(0, 355, by = 5))
  wind_data$direction_bin <- as.numeric(as.character(wind_data$direction_bin))
  
  # Create the full range of direction values from 0 to 355
  full_directions <- data.frame(direction_bin = seq(0, 355, by = 5))
  
  total_count <- nrow(wind_data)
  # Summarize the data by direction bin, calculate frequency and Max speed (m/s)
  wind_summary <- wind_data %>%
    group_by(direction_bin) %>%
    summarise(
      `Max speed\n(m/s)` = max(speed, na.rm = TRUE),
      `Frequency\n(%)` = (n() / total_count) * 100
    )
  
  # Merge with the full range of directions
  wind_summary <- full_directions %>%
    left_join(wind_summary, by = "direction_bin")
  
  # Replace NA values with 0 for direction_frequency
  wind_summary <- wind_summary %>%
    replace_na(list(`Frequency\n(%)` = 0, `Max\nspeed (m/s)` = 0))
  
  # Create a data frame for y-axis labels
  y_labels <- data.frame(y = seq(2, max(wind_summary$`Frequency\n(%)`), by = 2))
  
  ggplot(wind_summary, aes(x = direction_bin, y = `Frequency\n(%)`, fill = `Max speed\n(m/s)`)) +
    geom_bar(stat = "identity") +
    
    # Set polar coordinates and start the plot at 0 (North)
    coord_polar(start = 0) +
    
    # Add custom breaks for the direction with cardinal points
    scale_x_continuous(breaks = seq(0, 355, by = 45),
                       labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
    
    # Add custom breaks for frequency on the circular axis
    scale_y_continuous(breaks = seq(0, 100, by = 2),
                       labels = seq(0, 100, by = 2)) +
    
    # Set labels and titles
    labs(title = "Wind Rose", x = "Wind direction", y = "Frequency") +
    
    # Apply theme adjustments for clarity
    theme_bw() +
    theme(axis.title.x = element_text(color = "lightcyan4"),  # Color of x-axis title
          axis.title.y = element_text(color = "lightcyan4"),  # Color of y-axis title
          axis.text.y = element_text(color = "#FFFFFFB3"),    # Color of y-axis text
          axis.ticks.y = element_blank(),
          plot.title = element_text(color = "lightcyan4"),     # Color of plot title
          legend.title = element_text(color = "lightcyan4"),   # Color of legend title
          panel.border = element_rect(color = "lightcyan4"),
          # legend.position = c(0.1, 0.9),  # Position legend at the top-left corner
          # legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    ) +
    
    # Add frequency labels for each grid line
    geom_text(data = y_labels, 
              aes(x = 0, y = y, label = paste(y,"%")), 
              color = "black",size = 2.5,  inherit.aes = FALSE)
  
}


######################
# PRESSURE
######################

cumulative_frequency_curve_sp <- function(dates, sp){

  if (any(sp>5000)){
    sp_kpa = sp/1000
  } else {
    sp_kpa = sp
  }
  
  sp_data = data.frame(Date = dates, sp = sp_kpa)
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(sp_data$sp, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(sp_data$sp, na.rm = TRUE)
  max_val <- max(sp_data$sp, na.rm = TRUE)
  
  ggplot() +
    geom_line(data = sp_data, aes(x = sort(sp), y = seq(0,1,length.out = length(sp))), color = "yellow3") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Daily surface pressure (CDF)",
      x = "Surface pressure [kPa] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "yellow3", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "yellow3", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "yellow3", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "yellow3", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "yellow3", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val, 1), color = "yellow3", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val, 1), color = "yellow3", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1], 1), color = "yellow3", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2], 1), color = "yellow3", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3], 1), color = "yellow3", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "yellow3"),  # Color of x-axis title
          axis.title.y = element_text(color = "yellow3"),  # Color of y-axis title
          axis.text.x = element_text(color = "yellow3"),    # Color of x-axis text
          axis.text.y = element_text(color = "yellow3", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "yellow3"),     # Color of plot title
          legend.title = element_text(color = "yellow3"),   # Color of legend title
          panel.border = element_rect(color = "yellow3"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
  
} 


annual_sp_barplot <- function(dates, sp, dateslim) {
  
  if (any(sp>5000)){
    sp_kpa = sp/1000
  } else {
    sp_kpa = sp
  }
  
  sp_data = data.frame(Date = dates, sp = sp_kpa)
  
  sp_data = fill_df(sp_data, MonthStart = 1)
  
  # Adjust to hydrological year (starting in October)
  sp_data <- sp_data %>%
    mutate(
      Year = as.numeric(format(Date, "%Y")),  # Extract the calendar year
      Month = as.numeric(format(Date, "%m")),
      Hydro_Year = ifelse(Month >= 10, Year + 1, Year)  # If month is October or later, assign next year as hydro year
    ) %>%
    group_by(Year) %>%
    mutate(na_flag = is.na(sp))  %>% # Check for NA values in Annual_precip
    summarise(Annual_sp = mean(sp, na.rm = TRUE),
              Na_Flag = any(na_flag))
  

  full_years <- data.frame(Year = as.numeric(format(seq(from = as.Date(dateslim)[1], to = as.Date(dateslim)[2], by = "year"), "%Y")))
  sp_data_filtered <- full_years %>%
    left_join(sp_data, by = "Year") 
  
  if (as.Date(dateslim[1]) < as.Date("1980-01-01")) {
    years_break <- as.character(seq(1950, 2020, by = 10))
  } else {
    years_break <- as.character(seq(1980, 2020, by = 5))
  }
  
  ggplot(sp_data_filtered, aes(x = factor(Year))) + 
    geom_bar(aes(y = Annual_sp, fill = ifelse(Na_Flag, "Incomplete year", "Complete year")), stat = "identity", position = "dodge") +  # sp bars
    scale_fill_manual(values = c("Complete year" = "yellow3",
                                 "Incomplete year" = "firebrick4"), 
                      name = NULL, breaks = "Incomplete year",na.translate = FALSE) +  # Remove legend title
    labs(
      title = "Mean annual surface pressure",
      y = "Surface pressure [kPa]",
      x = "Year"
    ) +
    scale_x_discrete(breaks = years_break) +  # Set x-axis limits
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "yellow3"),
      axis.title.y = element_text(color = "yellow3"),
      axis.text.x = element_text(color = "yellow3"),
      axis.text.y = element_text(color = "yellow3", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "yellow3"),
      legend.title = element_text(color = "yellow3"),
      panel.border = element_rect(color = "yellow3"),
      legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with border
    )
  
  
}


monthly_sp_regime_boxplot <- function(dates, sp) {
  
  if (any(sp>5000)){
    sp_kpa = sp/1000
  } else {
    sp_kpa = sp
  }
  
  sp_data = data.frame(Date = dates, sp = sp_kpa)
  
  # Aggregate by month and year, summing daily sp
  sp_data <- sp_data %>%
    mutate(YearMonth = format(Date, "%Y-%m")) %>%
    group_by(YearMonth) %>%
    summarize(sp = mean(sp, na.rm = TRUE)) %>%
    mutate(Date = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"),  # Create a Date column for plotting
           Month = factor(format(Date, "%b"), levels = month.abb))  # Short month names
  
  
  
  ggplot(sp_data, aes(x = Month, y = sp)) +
    geom_boxplot(alpha = 0.3, outliers = FALSE, fill = "yellow3", color = "yellow3") +
    # scale_y_continuous(limits = c(0, NA))+
    labs(
      title = "Mean monthly surface pressure regime",
      x = "Month",
      y = "Surface pressure [kPa]"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color = "yellow3"),  # Color of x-axis title
      axis.title.y = element_text(color = "yellow3"),  # Color of y-axis title
      axis.text.x = element_text(color = "yellow3"),    # Color of x-axis text
      axis.text.y = element_text(color = "yellow3", angle = 90, hjust = 0.5),    # Color of y-axis text
      plot.title = element_text(color = "yellow3"),     # Color of plot title
      legend.title = element_text(color = "yellow3"),   # Color of legend title
      panel.border = element_rect(color = "yellow3"),
      legend.position = c(0.1, 0.9),  # Position legend at the top-left corner
      legend.background = element_rect(fill = "#FFFFFFB3", color = "black")  # White background with no border
    )
}

######################
# TOPO
######################


cumulative_frequency_curve_alt <- function(DEM){
  
  # Extract altitude values from the DEM
  altitude_values <- values(DEM)
  altitude_values <- altitude_values[!is.na(altitude_values)]  # Remove NA values
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(altitude_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(altitude_values, na.rm = TRUE)
  max_val <- max(altitude_values, na.rm = TRUE)
  
  # Create a data frame for plotting
  Altitude_data <- data.frame(Altitude = sort(altitude_values))
  
  ggplot() +
    geom_line(data = Altitude_data, aes(x = sort(Altitude), y = seq(0,1,length.out = length(Altitude))), color = "burlywood3") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Altitude (MERIT Hydro)",
      x = "Altitude [m] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "burlywood3", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "burlywood3", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "burlywood3", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "burlywood3", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "burlywood3", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1]), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2]), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3]), color = "burlywood3", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "burlywood3"),  # Color of x-axis title
          axis.title.y = element_text(color = "burlywood3"),  # Color of y-axis title
          axis.text.x = element_text(color = "burlywood3"),    # Color of x-axis text
          axis.text.y = element_text(color = "burlywood3", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "burlywood3"),     # Color of plot title
          legend.title = element_text(color = "burlywood3"),   # Color of legend title
          panel.border = element_rect(color = "burlywood3"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
  
} 


cumulative_frequency_curve_slope <- function(DEM) {
  # Calculate slope from DEM
  slope <- terrain(DEM, opt = "slope", unit = "degrees")
  
  # Extract slope values
  slope_values <- values(slope)
  slope_values <- slope_values[!is.na(slope_values)]  # Remove NA values
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(slope_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(slope_values, na.rm = TRUE)
  max_val <- max(slope_values, na.rm = TRUE)
  
  # Create a data frame for plotting
  Slope_data <- data.frame(Slope = sort(slope_values))
  
  ggplot() +
    geom_line(data = Slope_data, aes(x = sort(Slope), y = seq(0,1,length.out = length(Slope))), color = "burlywood3") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Slope (MERIT Hydro)",
      x = "Slope [°]",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 0), color = "burlywood3", size = 3, shape = 16) +  # Min value
    geom_point(aes(x = max_val, y = 1), color = "burlywood3", size = 3, shape = 16) +  # Max value
    geom_point(aes(x = quantiles[1], y = 0.25), color = "burlywood3", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "burlywood3", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.75), color = "burlywood3", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.03, label = round(min_val), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.89, label = round(max_val), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.3, label = round(quantiles[1]), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2]), color = "burlywood3", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.8, label = round(quantiles[3]), color = "burlywood3", hjust = -0.1) +
    
    theme_bw() +
    theme(axis.title.x = element_text(color = "burlywood3"),  # Color of x-axis title
          axis.title.y = element_text(color = "burlywood3"),  # Color of y-axis title
          axis.text.x = element_text(color = "burlywood3"),    # Color of x-axis text
          axis.text.y = element_text(color = "burlywood3", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "burlywood3"),     # Color of plot title
          legend.title = element_text(color = "burlywood3"),   # Color of legend title
          panel.border = element_rect(color = "burlywood3"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
}



######################
# SOIL
######################

soilclass_barplot <- function(Soilclass, shp_catchment) {

  # Initialize dataframe
  complete_classes <- data.frame(
    class = as.factor(0:12)
  )
  
  # Extract raster values that intersect the catchment polygon
  vals_in_catchment <- raster::extract(Soilclass, shp_catchment)
  
  # `vals_in_catchment` is a list (if multiple polygons), so unlist
  vals <- unlist(vals_in_catchment)
  
  vals[is.na(vals)] = 0
  
  # Count frequency including NAs
  soilclass_freq <- as.data.frame(table(vals))
  colnames(soilclass_freq) = c("class", "count")
  
  # Convert the count to percentages
  total_pixels <- sum(soilclass_freq[, "count"])
  soilclass_freq$percentage = (soilclass_freq[, "count"] / total_pixels) * 100
  
  soilclass_freq_full = merge(complete_classes, soilclass_freq, by = "class", all.x = TRUE)
  
  soilclass_labels <- c(
    "0" = "No Data",
    "1" = "Clay", 
    "2" = "Clay Loam", 
    "3" = "Loam", 
    "4" = "Loamy Sand", 
    "5" = "Sand", 
    "6" = "Sandy Clay", 
    "7" = "Sandy Clay Loam", 
    "8" = "Sandy Loam", 
    "9" = "Silt",
    "10"= "Silty Clay",
    "11"= "Silty Clay Loam",
    "12"= "Silt Loam"
  )
  
  soilclass_colours <- c(
    "0" = "#FFFFFFB3",
    "1" = "khaki1", 
    "2" = "olivedrab2", 
    "3" = "orange4", 
    "4" = "rosybrown1", 
    "5" = "orange3", 
    "6" = "firebrick", 
    "7" = "lightsalmon2", 
    "8" = "plum2", 
    "9" = "palegreen",
    "10"= "powderblue",
    "11"= "mediumaquamarine",
    "12"= "olivedrab4"
  )

  # Make sure class is a factor with all 13 levels (as character strings)
  soilclass_freq_full$class <- factor(
    soilclass_freq_full$class,
    levels = names(soilclass_labels)
  )
  
  
  # Create the bar plot
  ggplot(soilclass_freq_full, aes(x = class, y = percentage, fill = class)) +
    geom_bar(stat = "identity", colour = "black") +
    labs(title = "Soil class distribution (SoilGrids 2.0)", x = "Soil class Type", y = "Percentage [%]") +
    scale_x_discrete(labels = soilclass_labels) +
    scale_fill_manual(values = soilclass_colours) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "chocolate4"),
      axis.text.x = element_text(color = "chocolate4", angle = 45, hjust = 1),
      axis.text.y = element_text(color = "chocolate4", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "chocolate4"),
      legend.position = "none",
      panel.border = element_rect(color = "chocolate4")
    )
  
  
  
}

cumulative_frequency_curve_soildepth <- function(Soildepth){
  
  # Extract soildepth values from the DEM
  soildepth_values <- -values(Soildepth)
  soildepth_values <- soildepth_values[!(is.na(soildepth_values)|soildepth_values == 255|soildepth_values < 0)]  # Remove NA values
  
  # Calculate quantiles and min/max values
  quantiles <- quantile(soildepth_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_val <- min(soildepth_values, na.rm = TRUE)
  max_val <- max(soildepth_values, na.rm = TRUE)
  
  # Create a data frame for plotting
  soildepth_data <- data.frame(soildepth = sort(soildepth_values))
  
  ggplot() +
    geom_line(data = soildepth_data, aes(x = sort(soildepth), y = rev(seq(0,1,length.out = length(soildepth)))), color = "chocolate4") +  # Empirical CDF (cumulative frequency curve)
    coord_flip() +              
    labs(
      title = "Soil depth (Pelletier)",
      x = "Soil depth [m] ",
      y = "Cumulative Frequency"
    ) +
    
    # Highlight min, max, and quantiles
    geom_point(aes(x = min_val, y = 1), color = "chocolate4", size = 3, shape = 16) +  # Min value (most negative)
    geom_point(aes(x = max_val, y = 0), color = "chocolate4", size = 3, shape = 16) +  # Max value (least negative, close to zero)
    geom_point(aes(x = quantiles[1], y = 0.75), color = "chocolate4", size = 3, shape = 16) +  # 25th quantile
    geom_point(aes(x = quantiles[2], y = 0.50), color = "chocolate4", size = 3, shape = 16) +  # 50th quantile
    geom_point(aes(x = quantiles[3], y = 0.25), color = "chocolate4", size = 3, shape = 16) +  # 75th quantile
    
    # Add labels for the points
    annotate("text", x = min_val, y = 0.89, label = round(min_val), color = "chocolate4", hjust = -0.1) +
    annotate("text", x = max_val, y = 0.03, label = round(max_val), color = "chocolate4", hjust = -0.1) +
    annotate("text", x = quantiles[1], y = 0.8, label = round(quantiles[1]), color = "chocolate4", hjust = -0.1) +
    annotate("text", x = quantiles[2], y = 0.55, label = round(quantiles[2]), color = "chocolate4", hjust = -0.1) +
    annotate("text", x = quantiles[3], y = 0.3, label = round(quantiles[3]), color = "chocolate4", hjust = -0.1) +
    
    theme_bw()+
    theme(axis.title.x = element_text(color = "chocolate4"),  # Color of x-axis title
          axis.title.y = element_text(color = "chocolate4"),  # Color of y-axis title
          axis.text.x = element_text(color = "chocolate4"),    # Color of x-axis text
          axis.text.y = element_text(color = "chocolate4", angle = 90, hjust = 0.5),    # Color of y-axis text
          plot.title = element_text(color = "chocolate4"),     # Color of plot title
          legend.title = element_text(color = "chocolate4"),   # Color of legend title
          panel.border = element_rect(color = "chocolate4"),
          legend.position = c(0.1, 0.9),  # Legend position at the top-left corner
          legend.background = element_rect(fill = "#FFFFFFB3", color = "black"))  # White background with border
  
} 


######################
# LANDCOVER
######################

landcover_barplot <- function(Landcover) {
  
  # Initialize dataframe
  complete_classes <- data.frame(
    class = as.factor(1:17)
  )
  
  # Calculate the frequency of each class
  landcover_freq <- freq(Landcover, useNA = "no")
  
  # Convert the count to percentages
  total_pixels <- sum(landcover_freq[, 2], na.rm = TRUE)
  landcover_freq <- data.frame(
    class = landcover_freq[, 1],
    count = landcover_freq[, 2],
    percentage = (landcover_freq[, 2] / total_pixels) * 100
  )
  
  landcover_freq_full = merge(complete_classes, landcover_freq, by = "class", all.x = TRUE)

  landcover_labels <- c(
    "1" = "Evergreen Needleleaf Forest", 
    "2" = "Evergreen Broadleaf Forest", 
    "3" = "Deciduous Needleleaf Forest", 
    "4" = "Deciduous Broadleaf Forest", 
    "5" = "Mixed Forest", 
    "6" = "Closed Shrublands", 
    "7" = "Open Shrublands", 
    "8" = "Woody Savannas", 
    "9" = "Savannas",
    "10"= "Grasslands",
    "11"= "Permanent Wetlands",
    "12"= "Croplands",
    "13"= "Urban and Built-up",
    "14"= "Cropland/Natural Vegetation Mosaic",
    "15"= "Snow and Ice",
    "16"= "Barren or Sparsely Vegetated",
    "17" = "Water"
  )
  
  landcover_colours <- c(
    "1" = "#008000", 
    "2" = "#00FF00", 
    "3" = "#99CC00", 
    "4" = "#99FF99", 
    "5" = "#339966", 
    "6" = "#993366", 
    "7" = "#FFCC99", 
    "8" = "#CCFFCC", 
    "9" = "#FFCC00",
    "10"= "#FF9900",
    "11"= "#006699",
    "12"= "#FFFF00",
    "13"= "#FF0000",
    "14"= "#999966",
    "15"= "#F2F2F2",
    "16"= "#808080",
    "17"= "#000080"
  )
  
  # Make sure class is a factor with all 13 levels (as character strings)
  landcover_freq_full$class <- factor(
    landcover_freq_full$class,
    levels = names(landcover_labels)
  )
  
  # Create the bar plot
  ggplot(landcover_freq_full, aes(x = class, y = percentage, fill = class)) +  # Convert class to numeric for x and factor for fill
    geom_bar(stat = "identity", colour = "black", na.rm = FALSE) +
    labs(title = "Landcover distribution (IGBP classes derived from MODIS)", x = "Landcover Type", y = "Percentage [%]") +
    scale_x_discrete(labels = landcover_labels) +
    scale_fill_manual(values = landcover_colours) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "darkorchid4"),
      axis.text.x = element_text(color = "darkorchid4", angle = 45, hjust = 1),
      axis.text.y = element_text(color = "darkorchid4", angle = 90, hjust = 0.5),
      plot.title = element_text(color = "darkorchid4"),
      legend.position = "none",
      panel.border = element_rect(color = "darkorchid4")
    )
  
}

######################
# TURC-BUDYKO
######################

turc_budyko <- function(attributes, shp_outlets, catchment){
  
  catchment_codes <- shp_outlets$Code
  
  relevant_attributes <- attributes %>%
    filter(Attribute %in% c("aridity1_mean", "runoff_ratio_mean"))
  
  df_long <- relevant_attributes %>%
    pivot_longer(
      cols = all_of(catchment_codes),
      names_to = "Code",
      values_to = "Value"
    ) %>%
    dplyr::select(Attribute, Code, Value)
  
  df <- df_long %>%
    pivot_wider(names_from = Attribute, values_from = Value) %>%
    mutate(
      `Aridity` = 1/as.numeric(aridity1_mean),
      `Runoff Ratio` = as.numeric(runoff_ratio_mean)
    )
  

  myline = data.frame(x=seq(0.5,5.5,0.01))
  myline$y = 1-1/myline$x
  myline$y[nrow(myline)] = 0
  
  outliers_x = df[df$`Aridity` > 5,]
  outliers_y = df[df$`Runoff Ratio`>2,]

  # Create label data frame
  labels_df <- data.frame(
    x = c(2.2, 2.3),  # Adjust X positions (right and left)
    y = c(0.37, 1.95), # Adjust Y positions (bottom and top)
    label = c(paste0("Number of outliers (P/PET > 5):\n", nrow(outliers_x), " catchments"),
              paste0("Number of outliers (Q/P > 2):\n", nrow(outliers_y), " catchments"))
  )
  
  ggplot(data = df) +  # Convert 'year' to numeric
    geom_point(aes(x = `Aridity`, y = `Runoff Ratio`)) +
    annotate("rect", xmin = -0.5, xmax = 5.5, ymin = 1, ymax = 2.2, alpha = 0.5, fill = "darkgrey", col = "royalblue4") +
    geom_polygon(data = myline,
                 aes(x = x, y = y, alpha = 0.5), fill = "darkgrey", col = "royalblue4", show.legend = FALSE) +
    geom_text(x = 4.25, y = 1.05, 
              label = "Q = P", col = "royalblue4",
              hjust = 0, vjust = 0, size = 4) +
    geom_text(x = 3.95, y = 0.6, 
              label = "P-Q = PET", col = "royalblue4",
              hjust = 0, vjust = 0, size = 4) +
    geom_label(data = labels_df, aes(x = x, y = y, label = label),
               hjust = 0, vjust = 1, size = 3.5,
               label.size = 0.3, fill = "white", color = "black", alpha = 0.5)+
    geom_point(data = df[df$Code == catchment,], aes(x = `Aridity`, y = `Runoff Ratio`), color = "firebrick", size = 3) +
    geom_segment(data = df[df$Code == catchment,], 
                   aes(x = `Aridity`, xend = `Aridity`, y = 0, yend = `Runoff Ratio`),
                   linetype = "dashed", color = "firebrick") +
    geom_segment(data = df[df$Code == catchment,], 
                 aes(x = 0, xend = `Aridity`, y = `Runoff Ratio`, yend = `Runoff Ratio`),
                 linetype = "dashed", color = "firebrick") +
    labs(title = "Turc-Budyko graph",
         x = "Aridity index P/PET [-]",
         y = "Runoff ratio Q/P [-]") + 
    coord_cartesian(xlim = c(0, 5 ),
                    ylim = c(0, 2 ),
                    expand = FALSE) + 
    theme_bw() +
    theme(legend.position="none")
  
}

######################
# HEADER
######################

header <- function(metadata, attributes, catchment){
  
  catchment_info <- metadata[metadata$Code == catchment, ]
  name <- catchment_info$Station_name
  name_split <- split_title(name, max_chars = 30)
  lat <- round(catchment_info$Station_lat, 2)
  lon <- round(catchment_info$Station_lon, 2)
  area <- round(catchment_info$Basin_area_km2)
  cat <- catchment_info$subset_category
  
  # Autres variables à extraire de ta table attributaire `attributes`
  precip <- round(as.numeric(attributes %>% filter(Attribute == "PR0_mean") %>% pull(catchment)))
  temp <- round(as.numeric(attributes %>% filter(Attribute == "TT_mean") %>% pull(catchment)))
  pet <- round(as.numeric(attributes %>% filter(Attribute == "pet1_mean") %>% pull(catchment)))
  q <- round(as.numeric(attributes %>% filter(Attribute == "daily_discharge_mean") %>% pull(catchment))*365.25)
  aridity <- round(1/as.numeric(attributes %>% filter(Attribute == "aridity1_mean") %>% pull(catchment)), 2)
  rr <- round(as.numeric(attributes %>% filter(Attribute == "runoff_ratio_mean") %>% pull(catchment)),2)
  
  
  
  qSource = ifelse(catchment_info$Country == "CAN","WSC","USGS")
  meteoSource = "RDRS"
  
  # Text panel
  info_table <- data.frame(
    Label = c("Station Code", "Station Name", "Latitude", "Longitude", "Area [km²]", 
              "Category","Mean streamflow (Q)\n[mm/year]", "Mean precipitation (P)\n[mm/year]", 
              "Mean potential\nevapotranspiration (PET)\n[mm/year]",
              "Aridity index (P/PET)\n[-]","Runoff ratio (Q/P)\n[-]", 
              "Mean temperature (T)\n[°C]", 
              "Hydro Source", "Meteo Source"),
    Value = c(catchment, name_split, lat, lon, area, cat, q, precip, pet, aridity, rr, temp, qSource, meteoSource)
  )
  
  text_panel <- tableGrob(info_table, rows = NULL, theme = ttheme_default(
    core = list(fg_params = list(cex = 2.)),          # font size for body
    colhead = list(fg_params = list(cex = 2.3, fontface = "bold")),  # font size for headers
    padding = unit(c(12, 8), "mm")                      # optional: add spacing
  ))
  
}




######################
# MAPS
######################


map_elevation <- function(DEM, Lakes, shp_river, shp_catchment, shp_outlet){
  
  # Convert raster and vector to terra objects
  r <- rast(DEM)        # Convert to SpatRaster
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_r <- mask(r, v, touches = TRUE)
  
  # Name the raster layer (so the column has the correct name)
  names(masked_r) <- "Elevation (MERIT Hydro) [m]"
  
  # Convert masked raster to data frame with coordinates
  dem_df <- as.data.frame(masked_r, xy = TRUE, na.rm = TRUE)
  
  ggplot() +
    
    # Elevation raster layer
    geom_raster(data = dem_df, aes(x = x, y = y, fill = `Elevation (MERIT Hydro) [m]`)) +
    scale_fill_gradientn(colours = colorRampPalette(c("#006400", "#E6DD09", "#8B4513"))(50), 
                         name = "Elevation\n(MERIT Hydro)\n[m.a.s.l.]") +
    
    # Lakes
    geom_sf(data = Lakes, fill = "royalblue4") +
    
    # (Sub-)Catchment boundaries
    geom_sf(data = shp_catchment, aes(color = "(Sub-)Catchment boundaries"), size = 0.8, fill = NA) +
    
    # River
    geom_sf(data = shp_river, aes(color = "River & Lakes"), size = 1.5) +
    
    # Outlet
    geom_sf(data = shp_outlet, aes(color = "Outlet"), size = 2) +
    
    # Color scale for other layers
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    
    # Theme adjustments
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")
    
}

map_landcover <- function(Landcover, Lakes, shp_river, shp_catchment, shp_outlet){
  
  landcover_labels <- c(
    "1" = "Evergreen Needleleaf Forest", 
    "2" = "Evergreen Broadleaf Forest", 
    "3" = "Deciduous Needleleaf Forest", 
    "4" = "Deciduous Broadleaf Forest", 
    "5" = "Mixed Forest", 
    "6" = "Closed Shrublands", 
    "7" = "Open Shrublands", 
    "8" = "Woody Savannas", 
    "9" = "Savannas",
    "10"= "Grasslands",
    "11"= "Permanent Wetlands",
    "12"= "Croplands",
    "13"= "Urban and Built-up",
    "14"= "Cropland/Natural Vegetation Mosaic",
    "15"= "Snow and Ice",
    "16"= "Barren or Sparsely Vegetated",
    "17"= "Water"
  )
  
  landcover_colours <- c(
    "1" = "#008000", 
    "2" = "#00FF00", 
    "3" = "#99CC00", 
    "4" = "#99FF99", 
    "5" = "#339966", 
    "6" = "#993366", 
    "7" = "#FFCC99", 
    "8" = "#CCFFCC", 
    "9" = "#FFCC00",
    "10"= "#FF9900",
    "11"= "#006699",
    "12"= "#FFFF00",
    "13"= "#FF0000",
    "14"= "#999966",
    "15"= "#F2F2F2",
    "16"= "#808080",
    "17"= "#000080"
  )
  
  # Convert raster and vector to terra objects
  r <- rast(Landcover)        # Convert to SpatRaster
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_r <- mask(r, v, touches = TRUE)
  
  # Name the raster layer (so the column has the correct name)
  names(masked_r) <- "Landcover (MODIS)"
  
  # Convert masked raster to data frame with coordinates
  landcover_df <- as.data.frame(masked_r, xy = TRUE, na.rm = TRUE)
  landcover_df[["Landcover (MODIS)"]] <- factor(
    landcover_df[["Landcover (MODIS)"]],
    levels = as.numeric(names(landcover_labels)),
    labels = landcover_labels
  )
  landcover_df <- landcover_df[!is.na(landcover_df[["Landcover (MODIS)"]]), ]
  
  
  ggplot() +

    # Landcover raster
    geom_raster(data = landcover_df, aes(x = x, y = y, fill = `Landcover (MODIS)`)) +

    # Fill scale for Landcover
    scale_fill_manual(name = "Landcover\n(IGBP classes derived from MODIS)",
                      values = setNames(landcover_colours, landcover_labels),
                      labels = landcover_labels) +
    
    # (Sub-)Catchment boundaries
    geom_sf(data = shp_catchment, aes(color = "(Sub-)Catchment boundaries"), size = 0.8, fill = NA) +
    
    # River
    geom_sf(data = shp_river, aes(color = "River & Lakes"), size = 1.5) +
    
    # Lakes
    geom_sf(data = Lakes, fill = "royalblue4") +
    
    # Outlet
    geom_sf(data = shp_outlet, aes(color = "Outlet"), size = 2) +
    
    # Color scale for other layers
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    
    # Guide positioning
    guides(
      fill = guide_legend(order = 2),   # Landcover (bottom)
      color = guide_legend(order = 1)   # Catchment (top)
    ) +
    
    # Theme adjustments
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")

  
}

map_agriculture <- function(Agri, Lakes, shp_river, shp_catchment, shp_outlet){
  
  agriculture_labels <- c(
    "0" = "Water", 
    "1" = "Non-croplands", 
    "2" = "Irrigated croplands", 
    "3" = "Rainfed croplands"
  )
  
  agriculture_colours <- c(
    "0" = "royalblue2", 
    "1" = "grey90", 
    "2" = "yellowgreen", 
    "3" = "gold"
  )
  
  # Convert raster and vector to terra objects
  r <- rast(Agri)        # Convert to SpatRaster
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_r <- mask(r, v, touches = TRUE)
  
  # Name the raster layer (so the column has the correct name)
  names(masked_r) <- "Agriculture (LGRIP30)"
  
  # Convert masked raster to data frame with coordinates
  lgrip_df <- as.data.frame(masked_r, xy = TRUE, na.rm = TRUE)
  lgrip_df[["Agriculture (LGRIP30)"]] <- factor(
    lgrip_df[["Agriculture (LGRIP30)"]],
    levels = as.numeric(names(agriculture_labels)),
    labels = agriculture_labels
  )
  lgrip_df <- lgrip_df[!is.na(lgrip_df[["Agriculture (LGRIP30)"]]), ] 
  
  ggplot() +
    
    # Agriculture raster
    geom_raster(data = lgrip_df, aes(x = x, y = y, fill = `Agriculture (LGRIP30)`)) +
    
    # Fill scale for Agriculture
    scale_fill_manual(name = "Agriculture\n(LGRIP30)",
                      values = setNames(agriculture_colours, agriculture_labels),
                      labels = agriculture_labels) +
    
    # (Sub-)Catchment boundaries
    geom_sf(data = shp_catchment, aes(color = "(Sub-)Catchment boundaries"), size = 0.8, fill = NA) +
    
    # River
    geom_sf(data = shp_river, aes(color = "River & Lakes"), size = 1.5) +
    
    # Lakes
    geom_sf(data = Lakes, fill = "royalblue4") +
    
    # Outlet
    geom_sf(data = shp_outlet, aes(color = "Outlet"), size = 2) +
    
    # Color scale for other layers
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    
    # Theme adjustments
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")
  
  
}

map_forest <- function(Forest, Lakes, shp_river, shp_catchment, shp_outlet){
  
  # Convert raster and vector to terra objects
  r <- rast(Forest)        # Convert to SpatRaster
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_r <- mask(r, v, touches = TRUE)
  
  # Name the raster layer (so the column has the correct name)
  names(masked_r) <- "Forest height (GLCLUC 2020) [m]"
  
  # Convert masked raster to data frame with coordinates
  forest_df <- as.data.frame(masked_r, xy = TRUE, na.rm = TRUE)
  
  ggplot() +
    # Forest raster layer
    geom_raster(data = forest_df, aes(x = x, y = y, fill = `Forest height (GLCLUC 2020) [m]`)) +
    scale_fill_gradient(low = "papayawhip", high = "forestgreen", name = "Forest height\n(GLCLUC 2020)\n[m]") +
    
    # (Sub-)Catchment boundaries
    geom_sf(data = shp_catchment, aes(color = "(Sub-)Catchment boundaries"), size = 0.8, fill = NA) +
    
    # River
    geom_sf(data = shp_river, aes(color = "River & Lakes"), size = 1.5) +
    
    # Lakes
    geom_sf(data = Lakes, fill = "royalblue4") +
    
    # Outlet
    geom_sf(data = shp_outlet, aes(color = "Outlet"), size = 2) +
    
    # Color scale for other layers
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    
    # Theme adjustments
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")
  
  
}

map_soilclass <- function(Soilclass, Soilcount, Lakes, shp_river, shp_catchment, shp_outlet){
  
  soilclass_labels <- c(
    # "0" = "No Data",
    "1" = "Clay", 
    "2" = "Clay Loam", 
    "3" = "Loam", 
    "4" = "Loamy Sand", 
    "5" = "Sand", 
    "6" = "Sandy Clay", 
    "7" = "Sandy Clay Loam", 
    "8" = "Sandy Loam", 
    "9" = "Silt",
    "10"= "Silty Clay",
    "11"= "Silty Clay Loam",
    "12"= "Silt Loam"
  )
  
  
  soilclass_colours <- c(
    # "0" = "#FFFFFFB3",
    "1" = "khaki1", 
    "2" = "olivedrab2", 
    "3" = "orange4", 
    "4" = "rosybrown1", 
    "5" = "orange3", 
    "6" = "firebrick", 
    "7" = "lightsalmon2", 
    "8" = "plum2", 
    "9" = "palegreen",
    "10"= "powderblue",
    "11"= "mediumaquamarine",
    "12"= "olivedrab4"
  )
  
  # Convert raster and vector to terra objects
  r_class <- rast(Soilclass)        # Convert to SpatRaster
  r_count <- rast(Soilcount)
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_class <- mask(r_class, v, touches = TRUE)
  masked_count <- mask(r_count, v, touches = TRUE) 
  
  # Name the raster layer (so the column has the correct name)
  names(masked_class) <- "SoilClass"
  names(masked_count) <- "LayerCount"
  
  # Convert masked raster to data frame with coordinates
  soil_df <- merge(
    as.data.frame(masked_class, xy = TRUE, na.rm = TRUE),
    as.data.frame(masked_count, xy = TRUE, na.rm = TRUE),
    by = c("x", "y")
  )
  names(soil_df) <- c("x", "y", "SoilClass", "LayerCount")
  
  # dummy data for legend
  min_layers = min(soil_df$LayerCount, na.rm = TRUE)
  dummy_layer <- data.frame(
    x = NA,
    y = NA,
    SoilClass = factor("3", levels = names(soilclass_colours)),  # or use an actual SoilClass
    LayerCount = 6:min_layers)
  
  soil_df <- rbind(soil_df, dummy_layer)
  
  soil_df[["SoilClass"]] <- factor(
    soil_df[["SoilClass"]],
    levels = as.numeric(names(soilclass_labels)),
    labels = soilclass_labels
  )
  
  soil_df <- soil_df[!is.na(soil_df[["SoilClass"]]), ] 
  
  # Alpha mapping
  full_alpha_map <- c(`0` = 0.0, `1` = 0.1, `2` = 0.2, `3` = 0.3,
                      `4` = 0.4, `5` = 0.5, `6` = 1.0)
  
  needed_levels <- 6:min_layers
  layer_alpha_map <- full_alpha_map[as.character(needed_levels)]
  
  soil_df$LayerGroup <- factor(soil_df$LayerCount,
                               levels = needed_levels,
                               labels = paste(needed_levels, "layers"))
  
  
  alpha_values <- setNames(layer_alpha_map, levels(soil_df$LayerGroup))
  
  
  dominant_soil_class <- names(sort(table(soil_df$SoilClass), decreasing = TRUE))[1]
  dominant_color <- soilclass_colours[names(soilclass_labels)[soilclass_labels == dominant_soil_class]]
  
  blend_with_white <- function(color, alpha) {
    # Get RGB values
    rgb_vals <- col2rgb(color) / 255
    
    # Blend with white background (1, 1, 1)
    blended_rgb <- rgb_vals * alpha + c(1, 1, 1) * (1 - alpha)
    
    # Convert back to color
    rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3])
  }
  
  alpha_matched_colors <- sapply(alpha_values, function(a) {
    blend_with_white(dominant_color, a)
  })
  
  
  # Plot
  ggplot() +
    geom_raster(data = soil_df, aes(x = x, y = y, fill = SoilClass, alpha = LayerGroup)) +
    scale_fill_manual(name = "Soil Class\n(SoilGrids 2.0)",
                      values = setNames(soilclass_colours, soilclass_labels),
                      labels = soilclass_labels) +
    scale_alpha_manual(name = "Number of depth\nlayers used",
                       values = alpha_values, 
                       drop = FALSE)+
    geom_sf(data = shp_catchment,
            aes(color = "(Sub-)Catchment boundaries"),
            size = 0.8, fill = NA) +
    geom_sf(data = shp_river,
            aes(color = "River & Lakes"),
            size = 1.5) +
    geom_sf(data = Lakes, fill = "royalblue4") +
    geom_sf(data = shp_outlet,
            aes(color = "Outlet"),
            size = 2) +
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")+
    guides(alpha = guide_legend(
      override.aes = list(fill = dominant_color, alpha = alpha_values),
      title = "Number of depth\nlayers used"
    ))
  
}

map_soildepth <- function(Soildepth, Lakes, shp_river, shp_catchment, shp_outlet){
  
  # Convert raster and vector to terra objects
  r <- rast(Soildepth)        # Convert to SpatRaster
  v <- vect(shp_catchment)    # Convert to SpatVector
  
  # Mask the raster using 'touches = TRUE' to include all intersecting pixels
  masked_r <- mask(r, v, touches = TRUE)
  
  # Name the raster layer (so the column has the correct name)
  names(masked_r) <- "Depth (Pelletier) [m]"
  
  # Convert masked raster to data frame with coordinates
  soildepth_df <- as.data.frame(masked_r, xy = TRUE, na.rm = TRUE)
  
  # Clean invalid values (255 or < 0 → NA)
  soildepth_df$`Depth (Pelletier) [m]`[soildepth_df$`Depth (Pelletier) [m]` == 255 |
                                         soildepth_df$`Depth (Pelletier) [m]` < 0] <- NA
  
  ggplot() +
    
    # Elevation raster layer
    geom_raster(data = soildepth_df, aes(x = x, y = y, fill = `Depth (Pelletier) [m]`)) +
    scale_fill_gradientn(colours = colorRampPalette(c("#006400", "#E6DD09", "#8B4513"))(50), name = "Soil depth\n(Pelletier)\n[m]", na.value = "white") +
    
    # Lakes
    geom_sf(data = Lakes, fill = "royalblue4") +
    
    # (Sub-)Catchment boundaries
    geom_sf(data = shp_catchment, aes(color = "(Sub-)Catchment boundaries"), size = 0.8, fill = NA) +
    
    # River
    geom_sf(data = shp_river, aes(color = "River & Lakes"), size = 1.5) + 
    
    # Outlet
    geom_sf(data = shp_outlet, aes(color = "Outlet"), size = 2) +
    
    # Color scale for other layers
    scale_color_manual(name = "Catchment",
                       values = c("(Sub-)Catchment boundaries" = "black", 
                                  "River & Lakes" = "royalblue4", 
                                  "Outlet" = "black"),
                       breaks = c("(Sub-)Catchment boundaries", "River & Lakes", "Outlet")) +
    
    # Theme adjustments
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.box = "vertical")
  
}

map_NorthAm <- function(shp_outlets, catchment){
  
  # Get all admin-1 regions (states/provinces) worldwide
  all_states <- ne_states(returnclass = "sf")
  
  # Filter for North America manually
  NorthAm <- all_states[all_states$admin %in% c("United States of America", "Canada", "Mexico"), ]
  
  NorthAm_102008 <- st_transform(NorthAm, crs = "ESRI:102008")
  shp_outlets_102008 = st_transform(shp_outlets, crs = "ESRI:102008")
  
  gg <- ggplot() +

    geom_sf(data = NorthAm_102008, fill = "grey90", color = "grey40") +
    
    geom_sf(data = shp_outlets_102008, aes(), size = 1, shape = 21, fill = "black", color = "black") +
    geom_sf(data = shp_outlets_102008[shp_outlets_102008$Code == catchment,], aes(), size = 3, shape = 21, fill = "firebrick") +
    geom_sf(data = shp_outlets_102008[shp_outlets_102008$Code == catchment,], aes(), size = 8, shape = 21, fill = NA, color = "firebrick", stroke = 1.5) +    
    coord_sf(
      xlim = c(-2500000, 3100000),  
      ylim = c(-1600000, 3700000),          
      expand = FALSE
    ) +
    
    # annotation_scale(location = "bl", style = "bar") +
    # annotation_north_arrow(
    #   location = "br",
    #   style = north_arrow_fancy_orienteering,
    #   height = unit(1.1, "cm"),
    #   width = unit(1.1, "cm")
    # ) +
    
    theme_bw()
  
  return(gg)
  
}

######################
# SUMMARY SHEETS
######################



summary_header <- function(attributes, metadata, catchment, shp_outlets){
  
  # --- Extract metadata ---
  catchment_info <- metadata[metadata$Code == catchment, ]
  station_name <- catchment_info$Station_name
  title_text <- split_title(paste0(station_name, " (", catchment, ")"), max_chars = 60)
  
  # --- Title (centered) ---
  title_grob <- textGrob(
    label = title_text,
    gp = gpar(fontsize = 40, fontface = "bold"),
    x = 0.5, just = "center"
  )
  

  # --- Map and Metadata Table ---
  MAP_NA <- map_NorthAm(shp_outlets, catchment)
  HD     <- header(metadata, attributes, catchment)
  
  # --- Assemble bottom row (map + table) ---
  bottom_row <- arrangeGrob(
    HD,
    MAP_NA,
    ncol = 2,
    widths = unit(c(0.5, 0.5), "npc")
  )
  
  # --- Full layout ---
  gg <- arrangeGrob(
    title_grob,
    bottom_row,
    ncol = 1,
    heights = unit(c(1, 6), "null")  # adjust title vs content spacing
  )
  
  return(gg)
}





summary_sheets_graphs <- function(df_observed_daily, df_forcing_hourly, df_forcing_daily, DEM, Landcover, Agri, Forest, Soilclass, Soilcount, Soildepth, Lakes, shp_river, shp_catchment, shp_outlet, attributes, shp_outlets, catchment){
  
  # get dates limits for annual barplots
  dateslim <- c(range(c(df_observed_daily$date, df_forcing_daily$date)))
  
  # Create an empty plot layout
  layout <- matrix(c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14, 15,
    16, 17, 18, 19, 20,
    21, 22, 23, 24, 24
  ), nrow = 5, byrow = TRUE)
  
  
  CDF_Q <- cumulative_frequency_curve_q (df_observed_daily$date, df_observed_daily$qmm)
  BAR_Q <- annual_q_barplot (df_observed_daily$date, df_observed_daily$qmm, dateslim)
  REG_Q <- monthly_q_regime_boxplot (df_observed_daily$date, df_observed_daily$qmm)
  MAX_Q <- distribution_q_maxima_plot (df_observed_daily$date, df_observed_daily$qmm)
  MNA_Q <- distribution_q_mna_plot (df_observed_daily$date, df_observed_daily$qmm)
  CDF_P <- cumulative_frequency_curve_p (df_forcing_daily$date, df_forcing_daily$pmm)
  BAR_P <- annual_p_barplot (df_forcing_daily$date, df_forcing_daily$pmm, df_forcing_daily$tmean, dateslim)
  REG_P <- monthly_p_regime_boxplot (df_forcing_daily$date, df_forcing_daily$pmm, df_forcing_daily$tmean)
  MAX_P <- distribution_p_maxima_plot (df_forcing_daily$date, df_forcing_daily$pmm)
  CDF_PET <- cumulative_frequency_curve_pet (df_forcing_daily$date, df_forcing_daily$pet_ini)
  BAR_PET <- annual_pet_barplot (df_forcing_daily$date, df_forcing_daily$pet_ini, dateslim)
  REG_PET <- monthly_pet_regime_boxplot (df_forcing_daily$date, df_forcing_daily$pet_ini)
  CDF_T<- cumulative_frequency_curve_tmean (df_forcing_daily$date, df_forcing_daily$tmean)
  BAR_T<- annual_tmean_barplot (df_forcing_daily$date, df_forcing_daily$tmean, dateslim)
  REG_T<- monthly_tmean_regime_boxplot (df_forcing_daily$date, df_forcing_daily$tmean)
  FP_RAD <- finger_print_rad(df_forcing_hourly$timeLT, df_forcing_hourly$longwave_radiation, df_forcing_hourly$shortwave_radiation)
  FP_RAD_FILL <- finger_print_rad_fill(df_forcing_hourly$timeLT, df_forcing_hourly$longwave_radiation, df_forcing_hourly$shortwave_radiation)
  CDF_RH<- cumulative_frequency_curve_rh (df_forcing_daily$date, df_forcing_daily$relative_humidity)
  BAR_RH<- annual_rh_barplot (df_forcing_daily$date, df_forcing_daily$relative_humidity, dateslim)
  REG_RH<- monthly_rh_regime_boxplot (df_forcing_daily$date, df_forcing_daily$relative_humidity)
  ROSE_WIND <- rose_wind(df_forcing_daily$wind_dir, df_forcing_daily$wind_speed)
  CDF_SP<- cumulative_frequency_curve_sp (df_forcing_daily$date, df_forcing_daily$surface_pressure)
  BAR_SP<- annual_sp_barplot (df_forcing_daily$date, df_forcing_daily$surface_pressure, dateslim)
  REG_SP<- monthly_sp_regime_boxplot (df_forcing_daily$date, df_forcing_daily$surface_pressure)
  CDF_ALT <- cumulative_frequency_curve_alt (DEM)
  CDF_SLO <- cumulative_frequency_curve_slope (DEM)
  BAR_SC <- soilclass_barplot(Soilclass, shp_catchment)
  TB <- turc_budyko(attributes, shp_outlets, catchment)
  BAR_LAN <- landcover_barplot (Landcover)
  
  gg <- arrangeGrob(
    CDF_Q,
    BAR_Q, 
    REG_Q, 
    MAX_Q,
    MNA_Q,
    CDF_P, 
    BAR_P, 
    REG_P, 
    MAX_P,
    ROSE_WIND, 
    CDF_T,
    BAR_T,
    REG_T,
    CDF_ALT,
    CDF_SLO,
    CDF_PET,
    BAR_PET,
    REG_PET,
    REG_RH,
    REG_SP,
    TB,
    FP_RAD_FILL, 
    BAR_SC,
    BAR_LAN,
    layout_matrix = layout
  )
  
  return(gg)
}



summary_sheets_maps <- function(df_observed_daily, df_forcing_hourly, df_forcing_daily, DEM, Landcover, Agri, Forest, Soilclass, Soilcount, Soildepth, Lakes, shp_river, shp_catchment, shp_outlet){
  
  # Create an empty plot layout
  layout <- matrix(c(
    1, 2, 3,
    4, 5, 6
  ), nrow = 2, byrow = TRUE)
  
  MAP_ALT <- map_elevation(DEM, Lakes, shp_river, shp_catchment, shp_outlet)
  MAP_LAN <- map_landcover(Landcover, Lakes, shp_river, shp_catchment, shp_outlet)
  MAP_AGR <- map_agriculture(Agri, Lakes, shp_river, shp_catchment, shp_outlet)
  MAP_FOR <- map_forest(Forest, Lakes, shp_river, shp_catchment, shp_outlet)
  MAP_SC <- map_soilclass(Soilclass, Soilcount, Lakes, shp_river, shp_catchment, shp_outlet)
  MAP_SD <- map_soildepth(Soildepth, Lakes, shp_river, shp_catchment, shp_outlet)
  
  gg <- arrangeGrob(
    MAP_ALT,
    MAP_LAN, 
    MAP_AGR, 
    MAP_FOR,
    MAP_SC,
    MAP_SD,
    layout_matrix = layout
  )
  
  # Create the text grob
  caption_left <- textGrob(
    "* White pixels represent areas with no data ", 
    x = 0, hjust = 0,
    gp = gpar(fontsize = 9, fontface = "italic")
  )
  
  
  caption_right <- textGrob(
    "* (Sub-)catchment boundaries and rivers are based on MERIT Hydro. Lakes correspond to HydroLAKES water bodies",
    x = 1, hjust = 1,  # right-aligned
    gp = gpar(fontsize = 9, fontface = "italic")
  )
  
  combined_caption <- grobTree(caption_left, caption_right)
  
  # Combine the maps and the caption
  final_plot <- arrangeGrob(
    gg,
    bottom = combined_caption
  )
  
  return(final_plot)
}



generate_full_summary_pdf <- function(catchment,
                                      attributes,
                                      metadata,
                                      shp_outlets,
                                      df_observed_daily,
                                      df_forcing_hourly,
                                      df_forcing_daily,
                                      DEM,
                                      Landcover,
                                      Agri,
                                      Forest,
                                      Soilclass,
                                      Soilcount,
                                      Soildepth,
                                      Lakes,
                                      shp_river,
                                      shp_catchment,
                                      shp_outlet,
                                      output_path,
                                      output_name = NULL) {


  # Generate each page as a grob
  page1 <- summary_header(attributes, metadata, catchment, shp_outlets)
  page2 <- summary_sheets_graphs(df_observed_daily, df_forcing_hourly, df_forcing_daily, DEM, Landcover, Agri, Forest, Soilclass, Soilcount, Soildepth, Lakes, shp_river, shp_catchment, shp_outlet, attributes, shp_outlets, catchment)
  page3 <- summary_sheets_maps(df_observed_daily, df_forcing_hourly, df_forcing_daily, DEM, Landcover, Agri, Forest, Soilclass, Soilcount, Soildepth, Lakes, shp_river, shp_catchment, shp_outlet)
  
  if(is.null(output_name)){
    output_name = paste0("SummarySheet_", catchment,".pdf")
  }
  
  # Output all to a single PDF
  pdf(file.path(output_path, output_name), width = 22, height = 16)
  grid.draw(page1); grid.newpage()
  grid.draw(page2); grid.newpage()
  grid.draw(page3)
  dev.off()
}

