library(tidyverse)

# Leitura dos dados PREDICTS

biodiversity <- readRDS("Data/6fa1dedf-c546-41e0-a470-17c4863686b8.rds")

# De Palma processes ----
# Recebe os dados brutos do PREDICTS, filtra por realm e biomas de interesse
# e aplica as regras de reclassificacao de LandUse (De Palma et al.)
process_diversity <- function(data, realm, biome) {

  realm_data <- data %>%
    dplyr::filter(Realm == realm)

  br_neo <- realm_data %>%
    dplyr::filter(Biome %in% biome)

  diversity <- br_neo |>
    # make a level of Primary minimal. Everything else gets the coarse land use
    dplyr::mutate(
      LandUse = ifelse(Predominant_land_use == "Primary vegetation" & Use_intensity == "Minimal use",
                       "Primary minimal",
                       paste(Predominant_land_use)),

      # collapse the secondary vegetation classes together
      LandUse = ifelse(grepl("secondary", tolower(LandUse)),
                       "Secondary vegetation",
                       paste(LandUse)),

      # change cannot decide into NA
      LandUse = ifelse(Predominant_land_use == "Cannot decide",
                       NA,
                       paste(LandUse)),

      # relevel the factor so that Primary minimal is the first level (so that it is the intercept term in models)
      LandUse = factor(LandUse),
      LandUse = relevel(LandUse, ref = "Primary minimal")
    )

  diversity <- diversity %>%
    mutate(
      LandUse = if_else(LandUse == "Primary vegetation" & Biome %in% c("Tropical & Subtropical Grasslands, Savannas & Shrublands", "Tropical & Subtropical Dry Broadleaf Forests", "Temperate Grasslands, Savannas & Shrublands"),
                        "Savanna",
                        paste(LandUse)),
      LandUse = if_else(LandUse == "Primary vegetation" & Biome == "Tropical & Subtropical Moist Broadleaf Forests",
                        "Forest",
                        paste(LandUse)),
      LandUse = if_else(LandUse == "Secondary vegetation",
                        "Restoration",
                        paste(LandUse)),
      # relevel the factor so that Primary minimal is the first level (so that it is the intercept term in models)
      LandUse = factor(LandUse),
      LandUse = relevel(LandUse, ref = "Primary minimal")
    )

  diversity
}

# Exemplo de uso: regiao neotropical, biomas contidos no Brasil

biome_br <- c(
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Deserts & Xeric Shrublands",
  "Temperate Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Flooded Grasslands & Savannas"
)

diversity <- process_diversity(biodiversity, realm = "Neotropic", biome = biome_br)
