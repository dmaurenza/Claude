library(tidyverse)

# Leitura dos dados PREDICTS

biodiversity <- readRDS("Data/6fa1dedf-c546-41e0-a470-17c4863686b8.rds")

# Biomas por regiao ----
# Cada vetor lista os biomas (nomenclatura WWF, como aparecem em `Biome`)
# considerados equivalentes a cada regiao do Brasil. Sao usados tanto para
# recortar o Realm "Neotropic" quanto para montar as versoes "globais"
# (todos os realms, restritos aos mesmos biomas da regiao).
# Obs.: os nomes precisam usar "&" (ex.: "Tropical & Subtropical..."), que e
# a grafia usada na coluna Biome do PREDICTS -- usar "and" no lugar de "&"
# faz o filtro nao casar com nada.

Bioma_NE <- c(
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Deserts & Xeric Shrublands",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands"
)

Bioma_CO <- c(
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Flooded Grasslands & Savannas",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Tropical & Subtropical Moist Broadleaf Forests"
)

Bioma_SE <- c(
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Deserts & Xeric Shrublands"
)

Bioma_N <- c(
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests"
)

Bioma_S <- c(
  "Temperate Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Flooded Grasslands & Savannas",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands"
)

# uniao de todos os biomas das regioes brasileiras (equivalente ao antigo biome_br)
biome_br <- unique(c(Bioma_NE, Bioma_CO, Bioma_SE, Bioma_N, Bioma_S))

# De Palma processes ----
# Recebe os dados brutos do PREDICTS, filtra por realm e biomas de interesse
# e aplica as regras de reclassificacao de LandUse (De Palma et al.).
# realm = NULL mantem todos os realms (versao "global"); um vetor filtra um
# ou mais realms especificos (ex.: "Neotropic", ou c("Neotropic", "Afrotropic")).
process_diversity <- function(data, realm = NULL, biome) {

  if (!is.null(realm)) {
    data <- data %>%
      dplyr::filter(Realm %in% realm)
  }

  data <- data %>%
    dplyr::filter(Biome %in% biome)

  diversity <- data |>
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

# Exemplo de uso: regiao Nordeste, so dados do Neotropico
# diversity <- process_diversity(biodiversity, realm = "Neotropic", biome = Bioma_NE)
#
# Exemplo de uso: regiao Nordeste, dados globais (todos os realms com os
# mesmos biomas do Nordeste)
# diversity <- process_diversity(biodiversity, realm = NULL, biome = Bioma_NE)
#
# Para rodar todas as combinacoes de uma vez, ver R/run_all_combinations.R
