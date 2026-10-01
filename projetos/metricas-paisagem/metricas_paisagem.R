library(tidyverse)
library(sf)

# Loading Database ----
dir_output <- "./Results"
sites <- st_read("./Results/Sites.gpkg")
colnames(sites)

sites <- sites %>% 
  dplyr::select(datasetId, siteId, yearStart) %>% 
  mutate(id_unique = row_number()) # CHANGED: unique key of each site (links the metrics to siteId in Step 5)

## Step 1 - Creating landscapes at different sizes ----

# CRS
Albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs" #102033 - More info: https://epsg.io/102033
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Creating 2000 buffer size for sampling sites

buffers2000 <- sites %>% 
  st_transform(crs = Albers) %>% 
  st_buffer(dist = 2000) %>% 
  st_transform(crs = wgs84)

# # Puting the results in a list
# 
# buffer.list <- list("500"= buffers500, "1k"= buffers1000, "2k" = buffers2000,
#                     "4k" = buffers4000, "8k"= buffers8000)
# 
# # Saving buffer sites ----
# rm(buffers1000, buffers2000, buffers4000, buffers500, buffers8000)
# 
# st_write(buffer.list[["500"]], "./Outputs/site.buffers500_wgs84.gpkg", delete_dsn = T)
# st_write(buffer.list[["1k"]], "./Outputs/site.buffers1k_wgs84.gpkg", delete_dsn = T)
# st_write(buffer.list[["2k"]], "./Outputs/site.buffers2k_wgs84.gpkg", delete_dsn = T)
# st_write(buffer.list[["4k"]], "./Outputs/site.buffers4k_wgs84.gpkg", delete_dsn = T)
# st_write(buffer.list[["8k"]], "./Outputs/site.buffers8k_wgs84.gpkg", delete_dsn = T)

st_write(buffers2000, "./Data/SHP/buffers2000_wgs84.gpkg", delete_dsn = T)

## Step 2 -  Download raster maps ----
library(terra) # version 1.9-27

#source("Codes/Mapbiomas_maps.R")
# Loading and naming mapbiomas rasters
mapbiomas <- list.files("./Data/TIFF/", full.names = T, pattern = "brasil")
mapbiomas.final <- lapply(mapbiomas, rast)

# CHANGED: name each raster by the year written in its file name, not by position
names(mapbiomas.final) <- str_extract(basename(mapbiomas), "\\d{4}(?=\\D*$)")
stopifnot(all(as.character(unique(sites$yearStart)) %in% names(mapbiomas.final))) # stops if a year has no raster

## Step 3 -  Function to extract landscape metrics  ----

# CHANGED: new function. Returns 1 if the patch at the buffer center touches
# the buffer edge (the patch continues outside the buffer), 0 if not
patch_truncated <- function(land, center.xy, buf_dist){
  class.center <- terra::extract(land, center.xy)[1, 1]
  if(is.na(class.center)) return(NA)
  
  patches.cls <- terra::patches(terra::ifel(land == class.center, 1, NA), directions = 8)
  id.center <- terra::extract(patches.cls, center.xy)[1, 1]
  xy <- terra::crds(terra::ifel(patches.cls == id.center, 1, NA), na.rm = TRUE)
  
  dist.max <- max(sqrt((xy[, 1] - center.xy[1])^2 + (xy[, 2] - center.xy[2])^2))
  as.numeric(dist.max >= buf_dist - 2 * max(terra::res(land)))
}

extract_lsm <- function(buflist, rclmatrix, rstack, metrics){
  #list of buffers
  final.2k <- list()
  
  # list of results projected in albers
  proj8 <- list()
 
  # List of metrics
  metrics.2k <- list()
  
  # The body of the function
  
  for(i in 1:nrow(buflist[[1]])){
    #i = 1
    
    message(i)
    
    year.i <- (as.data.frame(buflist[[1]])[i, "yearStart"]) # filter the year of raster to be used
    
    final.2k[[i]] <- terra::crop(x =  mapbiomas.final[[which(names(mapbiomas.final) == year.i)]], y = buflist[[1]][i,]) %>% 
      terra::mask(mask = buflist[[1]][i,]) # crop and mask polygon from the raster
    
    proj8[[i]] <- final.2k[[i]] %>% 
      terra::project(Albers, method = "near") %>% ## project to albers, and reclassify
      terra::classify(rcl = reclass_matrix )
    
    ### compute landscape metrics (for buffer 2k)
    ### save landscape metrics to a table
    
    # CHANGED: class metrics (lsm_c_*) for the whole buffer;
    # patch metrics (lsm_p_*) only for the patch at the buffer center, inside the buffer
    metrics.c <- metrics[!grepl("^lsm_p_", metrics)]
    metrics.p <- metrics[grepl("^lsm_p_", metrics)]
    
    center.xy <- buflist[[1]][i,] %>% 
      st_geometry() %>% 
      st_transform(crs = Albers) %>% 
      st_centroid() %>% 
      st_coordinates()
    
    lsm.c <- calculate_lsm(landscape = proj8[[i]], what = metrics.c)
    
    # landscapemetrics:: is needed because our function has the same name
    lsm.p <- landscapemetrics::extract_lsm(landscape = proj8[[i]], y = center.xy, what = metrics.p) %>% 
      mutate(truncated = patch_truncated(proj8[[i]], center.xy, buf_dist = 2000))
    
    metrics.2k[[i]] <- bind_rows(lsm.c, lsm.p)
    metrics.2k[[i]] <- metrics.2k[[i]] %>% 
      mutate(id_unique = buflist[[1]][i,]$id_unique)
    
    gc() # to clean our memory
  }
  
  names(metrics.2k) <- buflist[[1]]$id_unique # CHANGED: moved out of the loop
  
  metrics.list <- list(metrics.2k)
  names(metrics.list) <- names(buflist)
  
  
  return(metrics.list)
}

## Step 4 - Run the function ----

#Loading polygons at different buffer sizes

buffer.2k <- st_read("./Data/SHP/buffers2000_wgs84.gpkg")
buffers.list <- list("2k" = buffer.2k)
rm(buffer.2k)
gc()

# matrix for reclassification
is <- c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 22, 23, 24, 30, 75, 25, 26, 33, 31, 27)
becomes <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  3, 3, 3, 3, 3,  3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 0)

reclass_matrix <- matrix(c(is, becomes), ncol = 2)

# Metrics
metrics <- c("lsm_c_ed","lsm_c_np", "lsm_c_pland", "lsm_p_area", "lsm_p_enn")

library(landscapemetrics) # install.packages("landscapemetrics", dependencies = T)

sites_lsm <- extract_lsm(buflist = buffers.list, rclmatrix = reclass_matrix, rstack = mapbiomas.final, metrics = metrics)
sites_lsm[[1]][[2]]

library(mapview)

# # Removing Mapbioma Rasters
# unlink("./TIFF", recursive = T, force = T)

## Step 5 - Organizing the final table ----
### fix tables --------------

lsm2k <- sites_lsm[["2k"]]
# CHANGED: removed the loop that replaced id_unique by the list position

## join all data in a single table
library(data.table)
lsm2k <- rbindlist(lsm2k, fill = TRUE)
glimpse(lsm2k)
lsm2k

# CHANGED: class and patch metrics are made wide separately, then joined
class_wide <- lsm2k %>% 
  filter(level == "class") %>% 
  dplyr::select(id_unique, class, metric, value) %>% 
  pivot_wider(names_from = metric, values_from = value)

patch_wide <- lsm2k %>% 
  filter(level == "patch") %>% 
  dplyr::select(id_unique, class, truncated, metric, value) %>% 
  pivot_wider(names_from = metric, values_from = value)

# CHANGED: datasetId and siteId of each id_unique
sites_info <- buffers.list[["2k"]] %>% 
  st_drop_geometry() %>% 
  dplyr::select(id_unique, datasetId, siteId, yearStart)

# area, enn and truncated appear only in the row of the center patch class
final_table <- class_wide %>% 
  left_join(patch_wide, by = c("id_unique", "class")) %>% 
  left_join(sites_info, by = "id_unique") %>% 
  dplyr::select(id_unique, datasetId, siteId, yearStart, class, ed, np, pland, area, enn, truncated)
  
# Saving Final table ----

write_csv(lsm2k, "./Results/lsm.csv")
