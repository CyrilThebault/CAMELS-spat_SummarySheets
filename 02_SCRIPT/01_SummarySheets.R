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

#! ----------------------------- Control file (ONLY THIS PART MUST BE UPDATED)

project_path = "/Users/cyrilthebault/Postdoc_Ucal/02_DATA/git/CAMELS-spat_SummarySheets"
dir_dataset = "/Users/cyrilthebault/Postdoc_Ucal/02_DATA/git/CAMELS-spat_SummarySheets/01_EXAMPLE"
output_path = "/Users/cyrilthebault/Postdoc_Ucal/02_DATA/git/CAMELS-spat_SummarySheets/03_OUTPUTS"

catchments = c("CAN_01AF007") # c("catchment1", "catchment2", ...) for specific catchments or c("all") for all the catchments

#! ----------------------------- package loading

library(dplyr)
library(tidyr)

#-- shapefiles
library(sf)

#-- rasters
library(raster)
library(terra)

#-- dates
library(lubridate)

#-- ncdf
library(ncdf4)
library(ncdf4.helpers)

#-- plot
library(grid)
library(gridExtra)
library(ggplot2)

#-- Other
library(rnaturalearth)
library(extRemes)
library(evd)

setwd(file.path(project_path, "02_SCRIPT"))

source("functions/UtilFunctions.R")
source("functions/Plots.R")


#! ----------------------------- Main

attributes <- read.csv(file.path(dir_dataset,"attributes", "attributes-lumped.csv"))

metadata <- read.csv(file.path(dir_dataset, "camels-spat-metadata.csv"))
metadata$Code <- paste0(metadata$Country, "_", metadata$Station_id)


## catchments outlets
shp_outlets <- sf::st_as_sf(metadata, coords = c('Station_lon','Station_lat'), crs = 4326)

if(catchments == "all"){
  catchments = metadata$Code
}

for(catchment in catchments){

  shp_outlet = subset(shp_outlets, Code %in% catchment)
  
  # area
  catchment_area = as.numeric(shp_outlet$Basin_area_km2) 
  
  # category
  category <- shp_outlet$subset_category
  
  # shp catchment and river from CAMELS-spat
  shp_catchment = read_sf(paste0(dir_dataset,"/shapefiles/",category,"/shapes-distributed/",catchment,"/",catchment,"_distributed_basin.shp"))
  shp_river = read_sf(paste0(dir_dataset,"/shapefiles/",category,"/shapes-distributed/",catchment,"/",catchment,"_distributed_river.shp"))
  
  # Forcing
  inputdata= "rdrs"
  spa = "lumped"
  forcing_path = file.path(dir_dataset, "forcing",category, inputdata, paste0(inputdata,"-",spa))
  forcing_file = list.files(forcing_path, pattern = catchment)
  
  forcing = nc_open(file.path(forcing_path, forcing_file))
  var_names <- c()
  
  for (var_name in names(forcing$var)) {
    dims <- forcing$var[[var_name]]$dim
    # Check if "time" is one of the dimensions
    if (all(sapply(dims, function(x) x$name) == c("hru", "time"))) {
      var_names <- c(var_names, var_name)
    }
  }
  
  # Managing time variable
  time_LT = as.POSIXct(as.character(nc.get.time.series(forcing)), tz = "UTC")
  
  forcing_variables = list()
  
  for(variable in c("timeLT","timeUTC", var_names)){
    
    if(variable == "timeLT"){
      
      value = time_LT
      
    } else if(variable == "timeUTC"){
      
      if(inputdata == "era5"){
        # firstTS = as.POSIXct(as.Date(time_LT[1]), format="%Y-%m-%d %H-%M-%S", tz = 'UTC')
      } else if (inputdata == "rdrs"){
        firstTS = as.POSIXct(paste0(as.Date(time_LT[1]), " 12-00-00"), format="%Y-%m-%d %H-%M-%S", tz = 'UTC')
      }
      
      timediff = firstTS - time_LT[1]
      time_UTC = time_LT + timediff
      value = time_UTC
      
    } else {
      value = ncvar_get(forcing, variable)
    }
    
    
    if(variable %in% names(forcing_variables)){
      forcing_variables[[variable]] = c(forcing_variables[[variable]], value)
    } else {
      forcing_variables[[variable]] = value
    }
    
  }
  
  nc_close(forcing)
  
  
  
  df_forcing_hourly = as.data.frame(forcing_variables)
  
  
  if (inputdata == "rdrs") {
    
    col_labels <- c(
      "e" = "vapor_pressure",
      "hruId" = "hruId", 
      "latitude" = "latitude", 
      "longitude" = "longitude", 
      "nbnds" = "nbnds", 
      "pet" = "pet", 
      "phi" = "wind_dir", 
      "RDRS_v2.1_A_PR0_SFC" = "precip", 
      "RDRS_v2.1_P_FB_SFC" = "shortwave_radiation", 
      "RDRS_v2.1_P_FI_SFC" = "longwave_radiation",
      "RDRS_v2.1_P_GZ_SFC"= "geopotential",
      "RDRS_v2.1_P_HR_1.5m"= "relative_humidity",
      "RDRS_v2.1_P_HU_1.5m"= "specific_humidity",
      "RDRS_v2.1_P_P0_SFC" = "surface_pressure", 
      "RDRS_v2.1_P_TT_1.5m" = "temperature", 
      "RDRS_v2.1_P_UUC_10m" = "wind_u_component",
      "RDRS_v2.1_P_UVC_10m"= "wind_speed",
      "RDRS_v2.1_P_VVC_10m"= "wind_v_component",
      "time_bnds"= "time_bnds Loam"
    )
    
    
  } else if (inputdata == "era5") {
    
    col_labels <- c(
      "e" = "vapor_pressure",
      "hruId" = "hruId", 
      "latitude" = "latitude", 
      "longitude" = "longitude", 
      "mper" = "pet", 
      "msdwlwrf" = "longwave_radiation", 
      "msdwswrf" = "shortwave_radiation",
      "msnlwrf" = "net_longwave_radiation", 
      "msnswrf" = "net_shortwave_radiation",
      "mtpr" = "precip", 
      "nbnds" = "nbnds", 
      "phi" = "wind_dir", 
      "q"= "specific_humidity",
      "rh"= "relative_humidity",
      "sp" = "surface_pressure", 
      "t" = "temperature", 
      "time_bnds"= "time_bnds Loam",
      "u" = "wind_u_component",
      "v"= "wind_v_component",
      "w"= "wind_speed"
    )
    
    
  }
  
  # Rename the columns
  names(df_forcing_hourly)[names(df_forcing_hourly) %in% names(col_labels)] <- col_labels[names(df_forcing_hourly)[names(df_forcing_hourly) %in% names(col_labels)]]
  
  df_forcing_hourly$pmm =  df_forcing_hourly$precip * 60 * 60
  df_forcing_hourly$pet_ini =  df_forcing_hourly$pet * 60 * 60
  df_forcing_hourly$tmean =  df_forcing_hourly$temperature - 273.15
  
  # List of variables to sum
  vars_to_sum <- c("pmm", "pet_ini")
  
  df_forcing_daily <- df_forcing_hourly %>%
    mutate(date = as.Date(timeLT)) %>%
    group_by(date) %>%
    summarise(across(
      where(is.numeric),
      ~ if (cur_column() %in% vars_to_sum) sum(.x, na.rm = TRUE) else mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop")
  
  

  # Observed streamflow
  ts = "daily"
  obs_path = file.path(dir_dataset, "observations", category, paste0("obs-",ts))
  obs_file = list.files(obs_path, pattern = catchment)
  
  obs = nc_open(file.path(obs_path,obs_file))
  
  time_day = as.Date(as.character(nc.get.time.series(obs)))
  
  qobs = ncvar_get(obs, "q_obs")
  
  df_observed_daily = data.frame(date = time_day, qm3s = qobs)
  
  df_observed_daily$qmm = df_observed_daily$qm3s* 60 * 60 * 24/(catchment_area*10^3)
  
  # Lakes
  Lakes = read_sf(paste0(dir_dataset,"/geospatial/",category, "/hydrolakes/",catchment,"/",catchment,"_HydroLAKES_polys_v10_NorthAmerica.shp"))
  
  # Topo
  DEM = raster(paste0(dir_dataset,"/geospatial/",category, "/merit/",catchment,"/",catchment,"_merit_hydro_elv.tif"))
  
  # LandCover
  Landcover = raster(paste0(dir_dataset,"/geospatial/",category, "/modis-land/",catchment,"/",catchment,"_2001_2022_mode_MCD12Q1_LC_Type1.tif"))
  
  # Agriculture
  Agri = raster(paste0(dir_dataset,"/geospatial/",category, "/lgrip30/",catchment,"/",catchment,"_lgrip30_agriculture.tif"))
  
  # Forest
  Forest = raster(paste0(dir_dataset,"/geospatial/",category, "/forest-height/",catchment,"/",catchment,"_forest_height_2000.tif"))
  
  # Soil depth
  Soildepth = raster(paste0(dir_dataset,"/geospatial/",category, "/pelletier/",catchment,"/",catchment,"_average_soil_and_sedimentary-deposit_thickness.tif"))
  
  # Soil class
  Soil_list = getSoilClass(paste0(dir_dataset,"/geospatial/",category, "/soilgrids/",catchment), return_layer_count = TRUE)
  Soilclass = Soil_list$class
  Soilcount = Soil_list$count
  
  
  generate_full_summary_pdf(
    catchment = catchment,
    attributes = attributes,
    metadata = metadata,
    shp_outlets = shp_outlets,
    df_observed_daily = df_observed_daily,
    df_forcing_hourly = df_forcing_hourly,
    df_forcing_daily = df_forcing_daily,
    DEM = DEM,
    Landcover = Landcover,
    Agri = Agri,
    Forest = Forest,
    Soilclass = Soilclass,
    Soilcount = Soilcount,
    Soildepth = Soildepth,
    Lakes = Lakes,
    shp_river = shp_river,
    shp_catchment = shp_catchment,
    shp_outlet = shp_outlet,
    output_path = output_path 
  )
}
