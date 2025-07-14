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

# Function to ensure that time-series do not have any gap in time
fill_df <- function(df, MonthStart = 1){
  
  dates = as.Date(df$Date)
  
  MonthStart = as.numeric(MonthStart)
  MonthEnd = as.numeric(MonthStart) - 1
  
  MonthEnd = ifelse(MonthEnd == 0, 12, MonthEnd)
  
  
  # MonthStart
  DateStart = as.Date(ifelse(min(as.Date(dates)) >= paste0(as.numeric(format(min(as.Date(dates)), "%Y")), "-",MonthStart,"-01"),
                             paste0(as.numeric(format(min(as.Date(dates)), "%Y")), "-",MonthStart,"-01"),
                             paste0(as.numeric(format(min(as.Date(dates)), "%Y"))-1, "-",MonthStart,"-01")
  ))
  
  last_day_of_month <- function(year, month) {
    as.Date(paste0(year, "-", month, "-01")) + months(1) - days(1)
  }
  
  DateEnd <- as.Date(ifelse(
    max(as.Date(dates)) <= last_day_of_month(as.numeric(format(max(as.Date(dates)), "%Y")), MonthEnd),
    last_day_of_month(as.numeric(format(max(as.Date(dates)), "%Y")), MonthEnd),
    last_day_of_month(as.numeric(format(max(as.Date(dates)), "%Y")) + 1, MonthEnd)
  ))
  
  dates_full = seq(DateStart, DateEnd, by = "day")
  
  df_full <- data.frame(matrix(nrow = length(dates_full), ncol = ncol(df)))
  colnames(df_full) = colnames(df)
  
  df_full$Date = dates_full
  
  # Loop through all columns except 'Date' and fill NA values
  df_full[-which(names(df_full) == "Date")] <- lapply(names(df_full)[-which(names(df_full) == "Date")], function(col) {
    ifelse(
      df_full$Date %in% df$Date,
      df[[col]][match(df_full$Date, df$Date)],
      df_full[[col]]
    )
  })
  
  return(df_full)
  
}

# Function for adding a line break after a certain maximum number of characters without breaking the words
split_title <- function(title, max_chars = 40) {
  result <- c()
  
  while (nchar(title) > max_chars) {
    # Find all spaces
    space_pos <- gregexpr(" ", title)[[1]]
    # Find the last space before or at max_chars
    break_pos <- max(space_pos[space_pos <= max_chars])
    
    # If no space found (i.e., one long word), force a cut
    if (!is.finite(break_pos)) {
      break_pos <- max_chars
    }
    
    # Add the line and reduce the string
    result <- c(result, substr(title, 1, break_pos - 1))
    title <- substr(title, break_pos + 1, nchar(title))
  }
  
  # Add the final piece
  result <- c(result, title)
  
  # Join with newlines
  return(paste(result, collapse = "\n"))
}


# Function to process soil classes (from salt, silt, clay at different depth to a single soil class per pixel)
getSoilClass <- function(soil_file_path, return_layer_count = FALSE) {
  
  file_des <- "soil_classes_"
  sl_list <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  files_dir <- list.files(soil_file_path)
  
  # ---------- Helper functions ----------
  
  load_soil_stack <- function(sl, files_dir, soil_file_path) {
    sand_file <- files_dir[grepl(paste0("sand_", sl, "_mean\\.tif$"), files_dir)]
    silt_file <- files_dir[grepl(paste0("silt_", sl, "_mean\\.tif$"), files_dir)]
    clay_file <- files_dir[grepl(paste0("clay_", sl, "_mean\\.tif$"), files_dir)]
    
    sand <- raster(file.path(soil_file_path, sand_file))
    silt <- raster(file.path(soil_file_path, silt_file))
    clay <- raster(file.path(soil_file_path, clay_file))
    
    stack(sand, silt, clay) |> `names<-`(c("sand", "silt", "clay"))
  }
  
  classify_soil <- function(df) {
    sc <- rep(NA, nrow(df))
    
    sc[(df$clay >= 400) & (df$sand <= 450) & (df$silt < 400)] <- 1
    sc[(df$clay >= 270) & (df$clay < 400) & (df$sand > 200) & (df$sand <= 450)] <- 2
    sc[(df$clay >= 70) & (df$clay < 270) & (df$silt >= 280) & (df$silt < 500) & (df$sand < 520)] <- 3
    sc[((df$silt + 15 * df$clay) >= 150) & ((df$silt + 2 * df$clay) < 300)] <- 4
    sc[((df$silt + 15 * df$clay) < 150)] <- 5
    sc[(df$clay >= 350) & (df$sand > 450)] <- 6
    sc[(df$clay >= 200) & (df$clay < 350) & (df$silt < 280) & (df$sand > 450)] <- 7
    sc[((df$clay >= 70) & (df$clay < 200) & (df$sand > 520) & ((df$silt + 2 * df$clay) >= 300)) |
         ((df$clay < 70) & (df$silt < 500) & ((df$silt + 2 * df$clay) >= 300))] <- 8
    sc[(df$silt >= 800) & (df$clay < 120)] <- 9
    sc[(df$clay >= 400) & (df$silt >= 400)] <- 10
    sc[(df$clay >= 270) & (df$clay < 400) & (df$sand <= 200)] <- 11
    sc[((df$silt >= 500) & (df$clay >= 120) & (df$clay < 270)) |
         ((df$silt >= 500) & (df$silt < 800) & (df$clay < 120))] <- 12
    
    return(sc)
  }
  
  # ---------- Load stacks ----------
  
  soil_stacks <- lapply(sl_list, load_soil_stack, files_dir = files_dir, soil_file_path = soil_file_path)
  names(soil_stacks) <- sl_list
  
  template <- soil_stacks[[1]][[1]]
  n_cells <- ncell(template)
  
  # ---------- Per-depth classification ----------
  
  soilclass_matrix <- matrix(NA, nrow = length(sl_list), ncol = n_cells)
  
  for (i in seq_along(soil_stacks)) {
    stk <- soil_stacks[[i]]
    valid_mask <- !is.na(values(stk[[1]])) & !is.na(values(stk[[2]])) & !is.na(values(stk[[3]]))
    
    if (any(valid_mask)) {
      vals <- as.data.frame(values(stk))
      colnames(vals) <- c("sand", "silt", "clay")
      sc <- rep(NA, nrow(vals))
      sc[valid_mask] <- classify_soil(vals[valid_mask, ])
      soilclass_matrix[i, ] <- sc
    }
  }
  
  # ---------- Dominant class and layer count ----------
  
  soil_values <- apply(soilclass_matrix, 2, function(row) {
    valid_vals <- row[!is.na(row)]
    if (length(valid_vals) == 0) return(NA)
    tab <- table(valid_vals)
    as.numeric(names(tab)[which.max(tab)])
  })
  
  layer_counts <- apply(soilclass_matrix, 2, function(row) sum(!is.na(row)))
  
  # ---------- Build spatial data ----------
  
  coords <- xyFromCell(template, 1:n_cells)
  soil_df <- data.frame(x = coords[, 1], y = coords[, 2],
                        type = soil_values,
                        layers = layer_counts)
  
  coordinates(soil_df) <- ~x + y
  proj4string(soil_df) <- crs(template)
  
  # ---------- Rasterize ----------
  r_soil_class <- rasterize(soil_df, template, field = "type", fun = mean)
  
  if (return_layer_count) {
    r_layer_count <- rasterize(soil_df, template, field = "layers", fun = mean)
    return(list(class = r_soil_class, count = r_layer_count))
  } else {
    return(r_soil_class)
  }
}
