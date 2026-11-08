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

# current forest remnants (your layer)
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

af <- st_transform(af, crs = st_crs(sa))
af_v <- vect(af)

# Map extent and shared theme ----

map_xlim <- c(-60, -32)
map_ylim <- c(-35, 0)

map_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_line(color = "black", linewidth = 0.4), # outward ticks
  axis.ticks.length = unit(0.15, "cm"),
  axis.text = element_text(size = 9, color = "black"),
  panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10, face = "bold"),
  strip.background = element_rect(fill = "gray80", color = "black", linewidth = 0.6),
  strip.text = element_text(size = 10)
)

# boundaries differ by color, width and line type
boundary_styles <- tibble(
  name      = c("Atlantic Forest limit", "Country boundaries", "State boundaries"),
  color     = c("#424242",               "#616161",              "#616161"),
  linetype  = c("solid",                 "solid",              "longdash"),
  linewidth = c(0.7,                     0.4,                  0.3)
)

# named vector (one value per boundary) for the manual scales
boundary_values <- function(column) setNames(boundary_styles[[column]], boundary_styles$name)

boundary_layers <- function(af_fill = NA){
  list(
    # drawn from the least to the most important line, so the Atlantic Forest limit stays on top
    geom_sf(data = states, aes(color = "State boundaries", linetype = "State boundaries",
                               linewidth = "State boundaries"),
            fill = NA, key_glyph = "path"),
    geom_sf(data = sa, aes(color = "Country boundaries", linetype = "Country boundaries",
                           linewidth = "Country boundaries"),
            fill = NA, key_glyph = "path"),
    geom_sf(data = af, aes(color = "Atlantic Forest limit", linetype = "Atlantic Forest limit",
                           linewidth = "Atlantic Forest limit"),
            fill = af_fill, key_glyph = "path"),
    geom_sf_text(data = states, aes(label = abbrev_state), size = 2.5,
                 color = "black", check_overlap = TRUE),
    geom_sf_text(data = sa_label, aes(geometry = geometry, label = name_en),
                 size = 2.5, inherit.aes = FALSE),
    # same name and breaks in the three scales, so they merge into one legend
    scale_color_manual(name = NULL, values = boundary_values("color"), breaks = boundary_styles$name),
    scale_linetype_manual(name = NULL, values = boundary_values("linetype"), breaks = boundary_styles$name),
    scale_linewidth_manual(name = NULL, values = boundary_values("linewidth"), breaks = boundary_styles$name),
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

# panels A-G plus one space for the legend shared by all panels
# (the legend fills the space after the last map)
combine_panels <- function(panels, ncol = 4){
  wrap_plots(c(panels, list(guide_area())), ncol = ncol) +
    plot_layout(guides = "collect") &
    theme(plot.title = element_text(size = 10, face = "bold", margin = margin(0, 0, 2, 0)),
          plot.margin = margin(2, 4, 2, 2), # small margins around each map
          legend.box = "vertical",
          legend.box.just = "left")
}

# north arrow and scale bar (panel A only)
map_annotations <- function(){
  list(
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           height = unit(0.8, "cm"), width = unit(0.8, "cm")),
    annotation_scale(location = "br", width_hint = 0.3, height = unit(0.15, "cm"))
  )
}

# Current forest remnants ----

# reads the current forest layer (raster or vector)
read_forest_now <- function(path, af_v, forest_values, max_cells = 1e6){

  if(grepl("\\.tiff?$", path, ignore.case = TRUE)){
    r <- rast(path)

    # read only the Atlantic Forest extent, without copying the 30 m raster
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

# Figure - Sampling sites: A) all groups, B-G) each taxonomic group ----

# point size in each map (smaller than in a single large map, so points do not merge)
point_size <- 0.8

# map: original extent, current remnants and sampling sites
sites_map <- function(points, title){
  ggplot() +
    geom_sf(data = af, aes(fill = forest_labels[1]), color = NA) +
    forest_layer +
    geom_sf(data = points, aes(shape = "Sampling sites"), size = point_size, color = "black") +
    boundary_layers() +
    scale_fill_manual(name = "Atlantic Forest", values = forest_colors, breaks = forest_labels) +
    scale_shape_manual(name = NULL, values = c("Sampling sites" = 19)) +
    scale_x_continuous(breaks = seq(-60, -30, by = 10)) +
    scale_y_continuous(breaks = seq(-30, 0, by = 10)) +
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2, override.aes = list(size = 2)),
           color = guide_legend(order = 3),
           linetype = guide_legend(order = 3),   # same order as color: one legend for the boundaries
           linewidth = guide_legend(order = 3)) +
    labs(title = title) +
    map_theme
}

# CHANGED: one figure with 7 maps; letter (A-G) in the title line
fig_titles <- paste0(LETTERS[seq_along(panel_titles)], ") ", panel_titles)

fig_panels <- map2(panel_sites, fig_titles, sites_map)
fig_panels[[1]] <- fig_panels[[1]] + map_annotations()

# coordinates only on the outer maps (left column and maps with nothing below);
# inner maps keep the ticks, without the numbers
fig_ncol <- 4
for(i in seq_along(fig_panels)){
  first_col <- (i - 1) %% fig_ncol == 0
  no_map_below <- i > length(fig_panels) - fig_ncol
  if(!first_col) fig_panels[[i]] <- fig_panels[[i]] + theme(axis.text.y = element_blank())
  if(!no_map_below) fig_panels[[i]] <- fig_panels[[i]] + theme(axis.text.x = element_blank())
}

fig_sites <- combine_panels(fig_panels, ncol = fig_ncol) # 2 rows x 4 columns, legend in the 8th space
fig_sites

ggsave("Fig/Fig1_Sampling_sites.png", fig_sites, width = 12, height = 8, dpi = 600)


#rm(list = ls())
