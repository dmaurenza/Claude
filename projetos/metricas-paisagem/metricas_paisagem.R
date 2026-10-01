library(tidyverse)
library(sf)

# Loading Database ----
dir_output <- "./Results"
sites <- st_read("./Results/Sites.gpkg")
colnames(sites)

sites <- sites %>%
  dplyr::select(datasetId, siteId, yearStart) %>%
  mutate(id_unique = row_number()) # unique key of each site; links the metrics back to datasetId and siteId (Step 5)

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

# Year taken from each file name (last group of 4 digits), not from the file order
mapbiomas.years <- str_extract(basename(mapbiomas), "(?<!\\d)\\d{4}(?=\\D*$)")

if(any(is.na(mapbiomas.years))) stop("Year not found in file name: ", paste(basename(mapbiomas)[is.na(mapbiomas.years)], collapse = ", "))
if(any(duplicated(mapbiomas.years))) stop("More than one raster for year: ", paste(unique(mapbiomas.years[duplicated(mapbiomas.years)]), collapse = ", "))
missing.years <- setdiff(as.character(unique(sites$yearStart)), mapbiomas.years)
if(length(missing.years) > 0) stop("No raster for year: ", paste(missing.years, collapse = ", "))

mapbiomas.final <- lapply(mapbiomas, rast)
names(mapbiomas.final) <- mapbiomas.years

## Step 3 -  Function to extract landscape metrics  ----

# Area (ha) and enn (m) of the patch located at the buffer center.
# The patch is taken complete, including the part outside the buffer.
# enn: shortest distance (cell center to cell center) between the complete
# center patch and the nearest patch of the same class, inside or outside
# the buffer.
# The search window around the center doubles until (1) the center patch no
# longer touches its edge and (2) no closer patch could lie outside it, up to
# max_window (m). If a condition still fails at max_window, the matching flag
# (area_truncated or enn_truncated) is 1.
center_patch_metrics <- function(rast_year, center, rclmatrix, buf_dist, max_window){
  
  window <- 2 * buf_dist
  c.xy <- st_coordinates(center)
  
  repeat{
    circle <- st_buffer(center, dist = window)
    
    land <- terra::crop(rast_year, terra::vect(st_transform(circle, crs = wgs84))) %>% 
      terra::project(Albers, method = "near") %>% 
      terra::classify(rcl = rclmatrix) %>% 
      terra::mask(terra::vect(circle))
    
    class.center <- terra::extract(land, terra::vect(center))[1, 2]
    if(is.na(class.center)) return(NULL)
    
    # patches of the center class (8 neighbours, as in landscapemetrics)
    patches.cls <- terra::ifel(land == class.center, 1, NA) %>% 
      terra::patches(directions = 8)
    id.center <- terra::extract(patches.cls, terra::vect(center))[1, 2]
    patch.center <- terra::ifel(patches.cls == id.center, 1, NA)
    
    # cells beyond this distance from the center may be cut by the window edge
    limit <- window - 2 * max(terra::res(land))
    
    # does the patch reach the edge of the window?
    xy <- terra::crds(patch.center, na.rm = TRUE)
    dist.max <- max(sqrt((xy[, 1] - c.xy[1])^2 + (xy[, 2] - c.xy[2])^2))
    area.truncated <- dist.max >= limit
    
    enn <- NA_real_
    enn.truncated <- TRUE
    
    if(!area.truncated){
      # cells of the same class that are not part of the center patch (whole window)
      nb <- terra::ifel(is.na(patch.center) & land == class.center, 1, NA) %>% 
        terra::values()
      nb <- nb[, 1]
      
      if(any(!is.na(nb))){
        dist.patch <- terra::distance(patch.center) # distance of each cell to the center patch
        enn <- min(terra::values(dist.patch)[!is.na(nb), 1])
        
        # a closer patch would be at most dist.max + enn from the center;
        # if that is inside the window, it would have been found
        enn.truncated <- dist.max + enn >= limit
      }
    }
    
    if((!area.truncated && !enn.truncated) || window >= max_window) break
    window <- min(window * 2, max_window)
  }
  
  # area of the complete patch (ha)
  area <- nrow(xy) * prod(terra::res(land)) / 10000
  
  tibble(level = "patch", class = class.center, id = id.center,
         metric = c("area", "enn", "area_truncated", "enn_truncated"),
         value = c(area, enn, as.numeric(area.truncated), as.numeric(enn.truncated)))
}

extract_lsm <- function(buflist, rclmatrix, rstack, metrics, buf_dist = 2000, max_window = 32000){
  #list of buffers
  final.2k <- list()
  
  # list of results projected in albers
  proj8 <- list()
 
  # List of metrics
  metrics.2k <- list()
  
  # class metrics (lsm_c_*): whole buffer
  # patch metrics (lsm_p_*): only the complete patch at the buffer center
  metrics.c <- metrics[!grepl("^lsm_p_", metrics)]
  metrics.p <- metrics[grepl("^lsm_p_", metrics)]
  
  # The body of the function
  
  for(i in 1:nrow(buflist[[1]])){
    #i = 1
    
    message(i)
    
    buffer.i <- buflist[[1]][i,]
    year.i <- as.character(buffer.i$yearStart) # filter the year of raster to be used
    rast.i <- mapbiomas.final[[which(names(mapbiomas.final) == year.i)]]
    
    final.2k[[i]] <- terra::crop(x = rast.i, y = buffer.i) %>% 
      terra::mask(mask = buffer.i) # crop and mask polygon from the raster
    
    proj8[[i]] <- final.2k[[i]] %>% 
      terra::project(Albers, method = "near") %>% ## project to albers, and reclassify
      terra::classify(rcl = reclass_matrix )
    
    ### compute landscape metrics (for buffer 2k)
    
    lsm.c <- NULL
    lsm.p <- NULL
    
    if(length(metrics.c) > 0){
      lsm.c <- calculate_lsm(landscape = proj8[[i]], what = metrics.c)
    }
    
    if(length(metrics.p) > 0){
      center.i <- buffer.i %>% st_geometry() %>% st_transform(crs = Albers) %>% st_sf() %>% 
        st_centroid() # buffer center = sampling site
      
      lsm.p <- center_patch_metrics(rast_year = rast.i, center = center.i, rclmatrix = reclass_matrix,
                                    buf_dist = buf_dist, max_window = max_window)
      
      if(is.null(lsm.p)){
        message("Buffer ", i, ": no patch at the center (NA cell)")
      } else {
        m.p <- sub("^lsm_p_", "", metrics.p)
        lsm.p <- lsm.p %>% filter(metric %in% c(m.p, paste0(m.p, "_truncated")))
      }
    }
    
    ### save landscape metrics to a table
    
    metrics.2k[[i]] <- bind_rows(lsm.c, lsm.p) %>% 
      mutate(id_unique = buffer.i$id_unique)
    
    gc() # to clean our memory
  }
  
  names(metrics.2k) <- buflist[[1]]$id_unique
  
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

## join all data in a single table (id_unique was set inside extract_lsm)
lsm2k <- bind_rows(sites_lsm[["2k"]])
glimpse(lsm2k)
lsm2k

# site attributes, to link each id_unique to datasetId and siteId
sites_info <- buffers.list[["2k"]] %>% 
  st_drop_geometry() %>% 
  dplyr::select(id_unique, datasetId, siteId, yearStart)

# class metrics: one row per site and class
lsm2k_class <- lsm2k %>% 
  filter(level == "class") %>% 
  dplyr::select(id_unique, class, metric, value) %>% 
  pivot_wider(names_from = metric, values_from = value)

# patch metrics: one row per site (class of the center patch)
lsm2k_patch <- lsm2k %>% 
  filter(level == "patch") %>% 
  dplyr::select(id_unique, class, metric, value) %>% 
  pivot_wider(names_from = metric, values_from = value)

# area, enn and the truncated flags appear only in the row of the center patch class
final_table <- lsm2k_class %>% 
  full_join(lsm2k_patch, by = c("id_unique", "class")) %>% 
  left_join(sites_info, by = "id_unique") %>% 
  dplyr::select(id_unique, datasetId, siteId, yearStart, class,
                any_of(c("ed", "np", "pland", "area", "enn", "area_truncated", "enn_truncated"))) %>% 
  arrange(id_unique, class)
  
# Saving Final table ----

write_csv(lsm2k, "./Results/lsm.csv")
