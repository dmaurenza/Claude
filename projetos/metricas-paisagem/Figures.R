library(sf)
library(tidyverse)
library(rnaturalearth)
library(ggspatial) # install.packages("ggspatial")
library(geobr)

#af <- sf::read_sf("Data/SHP/limites_wgs94/limite_ma_wwf_200ecprregions_wgs84.shp")
# af <- sf::read_sf("Data/SHP/limites_wgs94/limite_ma_lei_mata_atlantica_wgs84.shp")
# 
# af <- sf::read_sf("Data/SHP/limites_integradores_wgs84_v1_2_0/ma_limite_consensual_muylaert_et_al_2018_wgs84.shp")

af <- sf::read_sf("Data/SHP/limites_integradores_wgs84_v1_2_0/ma_limite_integrador_muylaert_et_al_2018_wgs84_v1_1_0.shp")


plot(af$geometry)

sa <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")

sa[sa$name_en == "Brazil","name_en"] <- ""

biomes <- geobr::read_biomes()

biomes[biomes$name_biome == "Amazônia","name_biome"] <- ""

biomes <- biomes %>% 
  filter(!name_biome %in% c("Sistema Costeiro", "Mata Atlântica"))

biomes <- st_transform(biomes, crs = st_crs(sa))

database_gpkg <- read_sf("Results/full_database.gpkg")

result <- database_gpkg %>% 
  group_by(taxonGroup, datasetId, siteId) %>% 
  distinct(scientificName, .keep_all = TRUE) %>% 
  summarise(richness = n(), .groups = "drop")

# # Bounding box da Mata Atlântica
# bbox_af <- st_bbox(af)
# 
# # Expandir 1 grau em todas as direções
# bbox_expandido <- st_bbox(c( xmin = as.numeric(bbox_af["xmin"]) - 25,
#                              xmax = as.numeric(bbox_af["xmax"]) + 10,
#                              ymin = as.numeric(bbox_af["ymin"]) - 5,
#                              ymax = as.numeric(bbox_af["ymax"]) + 10),
#                           crs = st_crs(af))
# 
# # Converter bbox para objeto sf
# bbox_sf <- st_as_sfc(bbox_expandido)
# 
# # Recortar América do Sul
# sa_crop <- st_crop(sa, bbox_sf)

# Adjustis before map creation ----
database_gpkg <- st_jitter(database_gpkg, amount = 0.2)

database_gpkg$taxonGroup <- as.factor(database_gpkg$taxonGroup)

biomes_label <- st_point_on_surface(biomes)

biomes_label <- biomes_label |>
  tidyr::crossing(taxonGroup = unique(result$taxonGroup))

sa_label <- st_point_on_surface(sa)

sa_label <- sa_label %>% 
  tidyr::crossing(taxonGroup = unique(result$taxonGroup))

last_panel <- data.frame(
  taxonGroup = tail(unique(result$taxonGroup), 1)
)

richness_breaks <- quantile(result$richness, probs = seq(0, 1, length.out = 7))


# Sampling Sites map ----
map <- ggplot() +
  geom_sf(data = sa,
          aes(color = "Regional boundaries"),
          fill = NA,
          linewidth = 0.2) +
  geom_sf_text(data = sa_label,
               aes(geometry = geometry, label = name_en),
               size = 3,
               inherit.aes = FALSE)+
    geom_sf(data = biomes,
          aes(color = "Regional boundaries"),
          fill = NA,
          linewidth = 0.2) +
  geom_sf_text(data = biomes_label,
               aes(geometry = geom, label = name_biome),
               size = 3,
               inherit.aes = FALSE)+
  geom_sf(data = af,
          fill = "forestgreen",
          aes(color = "Atlantic Forest limits"),
          alpha = 0.4,
          linewidth = 0.3) +
  geom_sf(data = result,
          aes(color = "Sampled sites"),
          shape = 19, 
          size = 1)+
  annotation_north_arrow(
    data = last_panel,
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    pad_x = unit(4, "cm"),
    pad_y = unit(0.7, "cm"),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )+
  annotation_scale(
    data = last_panel,
    location = "bl",
    width_hint = 0.2,
    pad_x = unit(2.7, "cm"),
    pad_y = unit(0.3, "cm"),
    height = unit(0.15, "cm"),
    width = unit(0.5, "cm")
  ) +
  coord_sf(xlim = c(-60, -32),
           ylim = c(-35, 0),
           expand = FALSE)+
  scale_x_continuous(breaks = seq(-60, -32, by = 15)) +
  scale_y_continuous(breaks = seq(-30, 0, by = 10))+
    scale_color_manual(
      name = "",
      breaks = c("Atlantic Forest limits",
                 "Regional boundaries",
                 "Sampled sites"),
      values = c(
        "Atlantic Forest limits" = "gray50",
        "Regional boundaries" = "gray50",
        "Sampled sites" = "black"
      )
    ) +
  # scale_size_binned(
  #   name = "Riqueza",
  #   breaks = richness_breaks,
  #   range = c(1, 6),
  #   labels = round(richness_breaks, 1)
  # ) +
  
  theme( 
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),          # remove grid
    axis.title = element_blank(),
    axis.ticks.length = unit(0.15, "cm"),  # comprimento dos ticks
    axis.text = element_text(size = 9),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 5, b = 5),
    plot.margin = margin(10, 10, 30, 10),
    
    strip.background = element_rect(
      fill = "gray80",
      color = "black",
      linewidth = 0.6
    ),
    strip.text = element_text(size = 10),
    
    )

map <- map +
  facet_wrap(vars(taxonGroup))+
  theme(legend.position = "right",
        legend.box = "vertical")
    
map

ggsave("Fig/map.png", map)


# Kernel map ----
database_gpkg <- st_read("Results/full_database.gpkg")

result <- database_gpkg %>% 
  group_by(taxonGroup, datasetId, siteId) %>% 
  distinct(scientificName, .keep_all = TRUE) %>% 
  summarise(richness = n(), .groups = "drop")
poly <- af
points <- result

# Usando Terra
library(terra)
points_v <- vect(points)
poly_v <- vect(poly)

# Criar raster com resolução adequada (maior resolução para evitar problemas)
# 5000 m em graus (aproximadamente 0.045 graus)
r <- rast(poly_v, resolution = 0.045, crs = crs(poly_v))

# Rasterizar pontos
p_raster <- rasterize(points_v, r, fun = "count", background = 0)

# Aplicar máscara
p_raster <- mask(p_raster, poly_v)

# Verificar dimensões do raster
print(dim(p_raster))

kernel <- focalMat(p_raster, 0.225, type = "Gauss")

# Verificar tamanho do kernel
print(dim(kernel))

# Aplicar focal
kde <- focal(p_raster, w = kernel, fun = "sum", na.rm = TRUE)

# Continuar com o processamento
kde <- project(kde, crs(poly_v))
kde <- mask(kde, poly_v)

# Aumentar destaque  
kde_transformed <- kde^(1/3)

kde_df <- as.data.frame(kde_transformed, xy = TRUE)
names(kde_df)[3] <- "Density"

poly_sf <- st_as_sf(poly_v)

library(geobr)
states <- geobr::read_state(code_state = "all", year = 2010, simplified = T)

states_sf <- st_transform(states, 4326)

# Kernel Density Map ----

p <- ggplot() +
  # Regional boundaries
  geom_sf(data = sa,
          aes(color = "Regional boundaries"),
          fill = NA,
          linewidth = 0.3) +
  geom_sf_text(data = sa_label,
               aes(geometry = geometry, label = name_en),
               size = 2.5,
               inherit.aes = FALSE) +
  
  # Raster KDE
  geom_tile(data = kde_df,
            aes(x = x, y = y, fill = Density)) +
  
  # Estados
  geom_sf(data = states_sf,
          fill = NA,
          color = "gray80",
          linewidth = 0.3) +
  
  # POLÍGONO DA MATA ATLÂNTICA
  geom_sf(data = poly_sf,
          aes(color = "Atlantic Forest limits"),
          fill = "gray30",
          linewidth = 0.3,
          alpha = 0.3) +
  
  # Texto dos estados
  geom_sf_text(data = states_sf,
               aes(label = abbrev_state),
               size = 3,
               color = "black",
               check_overlap = TRUE) + 
  
  # ESCALA PARA O RASTER
  scale_fill_gradientn(
    name = "Kernel Density",
    colors = c("white", "yellow", "orange", "red", "darkred"),
    na.value = "transparent"
  ) +
  
  # ESCALA PARA AS LINHAS - com breaks definindo a ordem
  scale_color_manual(
    name = "",
    values = c(
      "Regional boundaries" = "gray50",
      "Atlantic Forest limits" = "gray30"
    ),
    breaks = c("Regional boundaries", "Atlantic Forest limits")  # Ordem explícita
  ) +
  annotation_north_arrow(
    data = last_panel,
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    pad_x = unit(1.5, "cm"),
    pad_y = unit(0.7, "cm"),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )+
  annotation_scale(
    data = last_panel,
    location = "bl",
    width_hint = 0.2,
    pad_x = unit(3.1, "cm"),
    pad_y = unit(0.3, "cm"),
    height = unit(0.15, "cm"),
    width = unit(0.5, "cm")
  ) +
  
  # AJUSTE DA LEGENDA
  guides(
    color = guide_legend(
      override.aes = list(
        fill = c(NA, "gray30"),
        alpha = c(NA, 0.3),
        linewidth = c(0.2, 0.4)
      )
    )
  ) +
  
  coord_sf(xlim = c(-60, -32),
           ylim = c(-35, 0),
           expand = FALSE) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = c(0.77, 0.17),
    legend.key.size = unit(0.4, "cm"),         # tamanho dos símbolos
    legend.spacing.y = unit(0.1, "cm"), 
    #legend.position = "right",
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  ) +
  
  labs(x = "", y = "")

print(p)

# Salvar com ggsave
ggsave("./Fig/Kernel.png", p, dpi = 600)




rm(list = ls())
