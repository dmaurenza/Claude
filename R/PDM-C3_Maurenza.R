###### Este script foi elaborado por Daniel Maurenza ######
# Este código foi elaborado para subsidiar  as análises descritas em outro script chamado "Run_Models_Maurenza.R"
# Aqui são preparados as diferentes combinações de Bioma e Realms, bem como as definições de classes de uso da terra.
# Ambos os scripts são uma adaptação do tutorial elaborado por De Palma [https://adrianadepalma.github.io/BII_tutorial/], para calcular o indice BII - Biodiversity Intactness Index

# Remover todos os elementos
rm(list = ls(all.names = TRUE))

# Leitura dos dados PREDICTS
library(tidyverse)
biodiversity <- readRDS("Data/6fa1dedf-c546-41e0-a470-17c4863686b8.rds")
bio <- readRDS("Data/5b91276b-9051-4f48-9a5b-b3106730e4ae_release_2022.rds")
biodiversity <- rbind(biodiversity, bio)


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
Biome_BR <- unique(c(Bioma_NE, Bioma_CO, Bioma_SE, Bioma_N, Bioma_S))

# De Palma processes ----
# Recebe os dados brutos do PREDICTS, filtra por realm e biomas de interesse
# e aplica as regras de reclassificacao de LandUse (De Palma et al.).
# realm = NULL mantem todos os realms (versao "global"); um vetor filtra um
# ou mais realms especificos (ex.: "Neotropic", ou c("Neotropic", "Afrotropic")).
#
# custom_landuse (opcional): lista com land_use, intensity (vetor) e label,
# para isolar uma combinacao especifica de Predominant_land_use + Use_intensity
# como uma categoria propria, alem das categorias padrao do De Palma. As demais
# linhas (que nao casarem com o filtro) mantem a classificacao padrao normalmente.
# Ex.: list(land_use = "Cropland", intensity = c("Light use", "Intense use"),
#           label = "Cropland_A")
process_diversity <- function(data, realm = NULL, biome, custom_landuse = NULL) {

  if (!is.null(realm)) {
    data <- data |>
      dplyr::filter(Realm %in% realm)
  }

  data <- data |>
    dplyr::filter(Biome %in% biome)

  diversity <- data |>
    # make a level of Primary minimal. Everything else gets the coarse land use
    dplyr::mutate(
      LandUse = ifelse(Predominant_land_use == "Primary vegetation" & Use_intensity == "Minimal use",
                       "Primary minimal",
                       paste(Predominant_land_use)),

      # collapse the secondary vegetation classes together = Qualquer classe que tenha a palavra "secundary"
      LandUse = ifelse(grepl("secondary", tolower(LandUse)),
                       "Secondary vegetation",
                       paste(LandUse)),

      # change cannot decide into NA
      LandUse = ifelse(Predominant_land_use == "Cannot decide",
                       NA,
                       paste(LandUse)),

      LandUse = if_else(LandUse == "Primary vegetation" ,
                        "Natural vegetation",
                        paste(LandUse)),
      LandUse = if_else(LandUse == "Secondary vegetation",
                        "Restoration",
                        paste(LandUse))
    )

  # aplica o filtro customizado, se houver, sobrescrevendo so as linhas que
  # casarem com o land_use + intensidade pedidos (as demais linhas mantem a
  # classificacao padrao De Palma feita acima)
  if (!is.null(custom_landuse)) {
    diversity <- diversity |>
      dplyr::mutate(
        LandUse = dplyr::if_else(
          Predominant_land_use == custom_landuse$land_use &
            Use_intensity %in% custom_landuse$intensity,
          custom_landuse$label,
          LandUse
        )
      )
  }

  diversity <- diversity |>
    # relevel the factor so that Primary minimal is the first level (so that it is the intercept term in models)
    dplyr::mutate(
      LandUse = factor(LandUse),
      LandUse = relevel(LandUse, ref = "Primary minimal")
    )

  diversity
}

# Filtros customizados solicitados ----
# Cada item isola uma combinacao especifica de land use + intensidade como
# sua propria categoria (ver process_diversity() acima). Use UM de cada vez
# em process_diversity() -- rodar mais de um ao mesmo tempo na MESMA base
# geraria sobreposicao entre Cropland_A/Cropland_B e entre
# Plantation_A/Plantation_C (ambos incluem "Light use"): a ultima linha do
# mutate() venceria e sobrescreveria a anterior silenciosamente. Por isso
# cada filtro deve ser tratado como sua propria rodada/modelo.
#
# natural_vegetation_A e o unico filtro que usa land_use = "Primary vegetation".
# De proposito ele usa APENAS "Light use" (sem "Minimal use"): "Primary
# minimal" (Primary vegetation + Minimal use) e a categoria de referencia do
# modelo (relevel(ref = "Primary minimal")) e tambem a "base" usada pelo
# modelo composicional (cd_m) para comparar todo o resto -- se o filtro
# incluisse "Minimal use" junto, ele apagaria essa base na propria rodada em
# que ela seria usada (relevel() falharia por o nivel nao existir mais, e o
# cd_m ficaria sem nenhum site para comparar). Com so "Light use", Primary
# minimal fica intocado e a rodada funciona como qualquer outro filtro.
custom_landuse_list <- list(
  cropland_A = list(
    land_use = "Cropland",
    intensity = c("Light use", "Intense use"),
    label = "Cropland_A"      # Cropland - Light use & Intense use
  ),
  cropland_B = list(
    land_use = "Cropland",
    intensity = "Light use",
    label = "Cropland_B"      # Cropland - Light use
  ),
  plantation_A = list(
    land_use = "Plantation forest",
    intensity = c("Light use", "Intense use"),
    label = "Plantation_A"    # Plantation forest - Light use & Intense use
  ),
  plantation_B = list(
    land_use = "Plantation forest",
    intensity = "Minimal use",
    label = "Plantation_B"    # Plantation forest - Minimal
  ),
  plantation_C = list(
    land_use = "Plantation forest",
    intensity = "Light use",
    label = "Plantation_C"    # Plantation forest - Light
  ),
  pasture_A = list(
    land_use = "Pasture",
    intensity = c("Minimal use", "Light use"),
    label = "Pasture_A"       # Pasture - Minimal use & Light use
  ),
  natural_vegetation_A = list(
    land_use = "Primary vegetation",
    intensity = "Light use",
    label = "Natural_vegetation_A"  # Primary vegetation - Light use (Minimal fica isolado em Primary minimal)
  )
)

# Exemplo de uso: regiao Nordeste, so dados do Neotropico, classificacao padrao
# diversity <- process_diversity(dbbiodtotal, realm = "Neotropic", biome = Bioma_NE)
#
# Exemplo de uso: regiao Nordeste, dados globais (todos os realms com os
# mesmos biomas do Nordeste)
# diversity <- process_diversity(dbbiodtotal, realm = NULL, biome = Bioma_NE)
#
# Exemplo de uso: regiao Nordeste, isolando Cropland_A como categoria propria
# diversity_cropland_A <- process_diversity(dbbiodtotal, realm = "Neotropic",
#   biome = Bioma_NE, custom_landuse = custom_landuse_list$cropland_A)
#
# Para rodar todas as combinacoes de uma vez, ver R/run_all_combinations.R
