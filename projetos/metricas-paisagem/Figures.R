library(sf)
library(tidyverse)
library(rnaturalearth)
library(ggspatial) # install.packages("ggspatial")
library(geobr)
library(terra)
# terraOptions(tempdir = "D:/temp_terra") # optional: temporary files on a disk with free space
library(patchwork) # install.packages("patchwork")

# Input files ----

# Atlantic Forest limit (original extent)
af <- sf::read_sf("Data/SHP/limites_integradores_wgs84_v1_2_0/ma_limite_integrador_muylaert_et_al_2018_wgs84_v1_1_0.shp")

# CHANGED: current forest remnants (your layer)
# - raster (.tif): set forest_values to the pixel values that mean forest
#   (e.g. 1 for a forest / non-forest map; c(3, 4, 5, 6, 49) for MapBiomas collection codes)
# - vector (.shp or .gpkg): every polygon is drawn as forest
forest_now_path <- "Data/TIFF/Atual/brazil_coverage-col11_2025.tif" # MapBiomas collection 11, Brazil only
forest_values <- c(1, 3, 6, 4, 7, 5, 49) # check codes 1 and 7 in the collection 11 legend
forest_now_year <- 2025 # only used in the legend

# Sampling sites
database_gpkg <- read_sf("Results/full_database.gpkg")

# Countries and Brazilian states
sa <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")
sa[sa$name_en == "Brazil", "name_en"] <- ""
sa_label <- st_point_on_surface(sa)

states <- geobr::read_state(code_state = "all", year = 2010, simplified = T) %>%
  st_transform(crs = st_crs(sa))

# CHANGED: other biomes removed

af <- st_transform(af, crs = st_crs(sa))
af_v <- vect(af)

# Map extent and shared theme ----

map_xlim <- c(-60, -32)
map_ylim <- c(-35, 0)

map_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "gray85", linewidth = 0.25), # CHANGED: geographic grid (graticule)
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_line(color = "black", linewidth = 0.4), # CHANGED: outward ticks at the grid lines
  axis.ticks.length = unit(0.15, "cm"),
  axis.text = element_text(size = 9, color = "black"),
  panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10, face = "bold"),
  strip.background = element_rect(fill = "gray80", color = "black", linewidth = 0.6),
  strip.text = element_text(size = 10)
)

# CHANGED: boundaries are lines, so their legend keys are line segments (key_glyph = "path")
boundary_colors <- c("Atlantic Forest limit" = "gray20",
                     "Country boundaries" = "gray50",
                     "State boundaries" = "gray75")

boundary_layers <- function(af_fill = NA){
  list(
    geom_sf(data = sa, aes(color = "Country boundaries"), fill = NA,
            linewidth = 0.3, key_glyph = "path"),
    geom_sf(data = states, aes(color = "State boundaries"), fill = NA,
            linewidth = 0.25, key_glyph = "path"),
    geom_sf(data = af, aes(color = "Atlantic Forest limit"), fill = af_fill,
            linewidth = 0.35, key_glyph = "path"),
    geom_sf_text(data = states, aes(label = abbrev_state), size = 2.5,
                 color = "black", check_overlap = TRUE),
    geom_sf_text(data = sa_label, aes(geometry = geometry, label = name_en),
                 size = 2.5, inherit.aes = FALSE),
    scale_color_manual(name = NULL, values = boundary_colors, breaks = names(boundary_colors)),
    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE)
  )
}

# Sampling sites ----

# one point per site and taxonomic group
sites_group <- database_gpkg %>%
  distinct(taxonGroup, datasetId, siteId, .keep_all = TRUE) %>%
  select(taxonGroup, datasetId, siteId)

# all groups: a site sampled for several groups counts once
sites_all <- sites_group %>%
  distinct(datasetId, siteId, .keep_all = TRUE)

# panel titles with the number of sites: all groups first, then each group
groups <- sort(unique(sites_group$taxonGroup))
panel_titles <- c(paste0("All groups (n = ", nrow(sites_all), ")"),
                  paste0(groups, " (n = ", table(sites_group$taxonGroup)[groups], ")"))

# sites of each panel, in the same order as panel_titles
panel_sites <- c(list(sites_all),
                 map(groups, function(g) filter(sites_group, taxonGroup == g)))

# check: number of sites in each panel (should differ between groups)
print(tibble(panel = panel_titles, n_points = map_int(panel_sites, nrow)))

# CHANGED: panels a-g plus one space for the legend shared by all panels
# (the legend fills the space after the last map)
combine_panels <- function(panels, ncol = 4){
  wrap_plots(c(panels, list(guide_area())), ncol = ncol) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(plot.tag = element_text(face = "bold", size = 11),
          plot.title = element_text(size = 10),
          legend.box = "vertical",
          legend.box.just = "left")
}

# north arrow and scale bar (Figure 1 and panel a of Figure 2)
map_annotations <- function(){
  list(
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           height = unit(0.8, "cm"), width = unit(0.8, "cm")),
    annotation_scale(location = "br", width_hint = 0.3, height = unit(0.15, "cm"))
  )
}

# Figure 1 - Original and current Atlantic Forest, with sampling sites ----

# reads the current forest layer (raster or vector)
read_forest_now <- function(path, af_v, forest_values, max_cells = 1e6){

  if(grepl("\\.tiff?$", path, ignore.case = TRUE)){
    r <- rast(path)

    # CHANGED: read only the Atlantic Forest extent, without copying the 30 m raster
    # (crop() and %in% wrote full-resolution temporary files of several GB)
    window(r) <- ext(project(af_v, crs(r)))

    # coarser grid for mapping (a 30 m raster is too large to draw):
    # share of forest pixels in each coarse cell, computed in a single pass
    fact <- ceiling(sqrt(ncell(r) / max_cells))
    forest_share <- function(x, ...) mean(x %in% forest_values)
    if(fact > 1){
      r <- aggregate(r, fact = fact, fun = forest_share)
    } else {
      r <- r %in% forest_values
    }

    r <- project(r, crs(af_v), method = "near") %>%
      mask(af_v)

    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "forest"
    df <- df %>% filter(forest >= 0.5) # forest when most of the cell was forest

    list(type = "raster", data = df)

  } else {
    v <- read_sf(path) %>%
      st_transform(crs = st_crs(af_v)) %>%
      st_intersection(st_union(st_as_sf(af_v)))

    list(type = "vector", data = v)
  }
}

forest_now <- read_forest_now(forest_now_path, af_v, forest_values)

forest_labels <- c("Original extent", paste0("Current forest remnants (", forest_now_year, ")"))
forest_colors <- setNames(c("#d9ead3", "#1b7837"), forest_labels)

if(forest_now$type == "raster"){
  forest_layer <- geom_tile(data = forest_now$data,
                            aes(x = x, y = y, fill = forest_labels[2]))
} else {
  forest_layer <- geom_sf(data = forest_now$data,
                          aes(fill = forest_labels[2]), color = NA)
}

# map: original extent, current remnants and sampling sites
sites_map <- function(points, title){
  ggplot() +
    geom_sf(data = af, aes(fill = forest_labels[1]), color = NA) +
    forest_layer +
    geom_sf(data = points, aes(shape = "Sampling sites"), size = 0.5, color = "black") +
    boundary_layers() +
    scale_fill_manual(name = "Atlantic Forest", values = forest_colors, breaks = forest_labels) +
    scale_shape_manual(name = NULL, values = c("Sampling sites" = 19)) +
    scale_x_continuous(breaks = seq(-60, -30, by = 10)) +
    scale_y_continuous(breaks = seq(-30, 0, by = 10)) +
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2, override.aes = list(size = 2)),
           color = guide_legend(order = 3)) +
    labs(title = title) +
    map_theme +
    theme(panel.grid.major = element_blank()) # CHANGED: no grid in Figure 1, only the ticks
}

# CHANGED: Figure 1 is a single map with all sampling sites (not split by taxonomic group)
fig1 <- sites_map(sites_all, title = NULL) +
  map_annotations() +
  theme(legend.position = "right")
fig1

ggsave("Fig/Fig1_Sampling_sites.png", fig1, width = 9, height = 8, dpi = 600)

# Figure 2 - Kernel density of sampling sites (a: all groups, b-g: each group) ----

# Kernel density inside the Atlantic Forest limit
# res: 0.045 degrees (~5 km); sigma: 0.225 degrees (~25 km)
kernel_density <- function(points, af_v, res = 0.045, sigma = 0.225){
  r <- rast(af_v, resolution = res)

  p_raster <- rasterize(vect(points), r, fun = "count", background = 0) %>%
    mask(af_v)

  w <- focalMat(p_raster, sigma, type = "Gauss")

  kde <- focal(p_raster, w = w, fun = "sum", na.rm = TRUE) %>%
    mask(af_v)

  df <- as.data.frame(kde, xy = TRUE)
  names(df)[3] <- "Density"
  df
}

kde_list <- map(panel_sites, kernel_density, af_v = af_v)

# same color scale in every panel (needed for one shared legend)
kde_max <- max(map_dbl(kde_list, function(d) max(d$Density, na.rm = TRUE)))

# one-hue sequential palette that starts light (no white, no grey overlay);
# cube-root scale to show low densities, legend in original units
cube_root <- scales::trans_new("cube_root", function(x) x^(1/3), function(x) x^3)

kde_map <- function(kde, title){
  ggplot() +
    geom_tile(data = kde, aes(x = x, y = y, fill = Density)) +
    boundary_layers() + # Atlantic Forest limit drawn as an outline only
    scale_fill_gradientn(name = "Kernel density of\nsampling sites\n(cube-root scale)",
                         colors = c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
                         trans = cube_root,
                         limits = c(0, kde_max),
                         breaks = signif(kde_max * c(0, 1/27, 8/27, 1), 1), # evenly spaced on the cube-root scale
                         na.value = "transparent") +
    scale_x_continuous(breaks = seq(-60, -30, by = 10)) +
    scale_y_continuous(breaks = seq(-30, 0, by = 10)) +
    guides(fill = guide_colorbar(order = 1), color = guide_legend(order = 2)) +
    labs(title = title) +
    map_theme
}

fig2_panels <- map2(kde_list, panel_titles, kde_map)
fig2_panels[[1]] <- fig2_panels[[1]] + map_annotations()

fig2 <- combine_panels(fig2_panels, ncol = 4) # CHANGED: 2 rows x 4 columns
fig2

ggsave("Fig/Fig2_Kernel_density.png", fig2, width = 13, height = 9, dpi = 600)


rm(list = ls())
