###### Este script foi elaborado por Daniel Maurenza ######
# Este código foi elaborado para subsidiar  as análises descritas em outro script chamado "Run_Models_Maurenza.R"
# Aqui são preparados as diferentes combinações de Realm, bem como as definições de classes de uso da terra.
# Ambos os scripts são uma adaptação do tutorial elaborado por De Palma [https://adrianadepalma.github.io/BII_tutorial/], para calcular o indice BII - Biodiversity Intactness Index

# Remover todos os elementos
rm(list = ls(all.names = TRUE))

# Leitura dos dados PREDICTS
library(tidyverse)
biodiversity <- readRDS("Data/6fa1dedf-c546-41e0-a470-17c4863686b8.rds")
bio <- readRDS("Data/5b91276b-9051-4f48-9a5b-b3106730e4ae_release_2022.rds")
biodiversity <- rbind(biodiversity, bio)
colnames(biodiversity)
unique(biodiversity$Biome)

# Biomas tropicais do Brasil ----
# Lista fixa/curada, restrita aos 3 biomas tropicais (exclui os biomas
# desertico/temperado/de inundacao que faziam parte de uma versao anterior
# mais ampla -- ver historico do script): Cerrado/Caatinga (savanas e
# florestas secas tropicais/subtropicais) e Mata Atlantica/Amazonia
# (florestas umidas tropicais/subtropicais). Usada em all_tropics e
# Neotropics abaixo -- Global fica sem filtro de bioma, como referencia
# totalmente irrestrita.
biomas_brasil_custom <- c(
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests"
)

# Filtros de regiao ----
# Os 3 "modelos"/regioes usados nas analises. Cada item tem um filtro de
# `realm` e/ou de `biome` (NULL = sem filtro naquele campo) a passar para
# process_diversity() abaixo. all_tropics e Neotropics testam a MESMA
# pergunta (efeito do uso da terra nos 3 biomas tropicais de
# biomas_brasil_custom) em duas escalas geograficas diferentes; Global e a
# referencia sem nenhum filtro, nem de bioma nem de realm.
#   Global      : todos os realms, todos os biomas (sem filtro nenhum --
#                 referencia irrestrita)
#   all_tropics : biomas_brasil_custom, restrito aos realms tropicais --
#                 Neotropic (America Central e America do Sul, incluindo o
#                 Brasil inteiro), Afrotropic (Africa subsaariana e
#                 Madagascar), Indo-Malay (Sul e Sudeste Asiatico tropical)
#                 e Australasia (Australia, Nova Guine e Nova Zelandia)
#   Neotropics  : biomas_brasil_custom, restrito ao realm Neotropic apenas
regiao_filtros <- list(
  Global = list(realm = NULL, biome = NULL),
  all_tropics = list(realm = c("Neotropic", "Afrotropic", "Indo-Malay", "Australasia"), biome = biomas_brasil_custom),
  Neotropics = list(realm = "Neotropic", biome = biomas_brasil_custom)
)

# De Palma processes ----
# Recebe os dados brutos do PREDICTS, filtra por realm e/ou bioma de
# interesse e aplica as regras de reclassificacao de LandUse (De Palma et
# al.). realm/biome = NULL mantem todos os realms/biomas -- ver
# regiao_filtros acima para os 3 filtros usados nas analises.
#
# custom_landuse (opcional): lista com land_use, intensity (vetor) e label,
# para isolar uma combinacao especifica de Predominant_land_use + Use_intensity
# como uma categoria propria, alem das categorias padrao do De Palma. As demais
# linhas (que nao casarem com o filtro) mantem a classificacao padrao normalmente.
# Ex.: list(land_use = "Cropland", intensity = c("Light use", "Intense use"),
#           label = "Cropland_A")
process_diversity <- function(data, realm = NULL, biome = NULL, custom_landuse = NULL) {

  if (!is.null(realm)) {
    data <- data |>
      dplyr::filter(Realm %in% realm)
  }

  if (!is.null(biome)) {
    data <- data |>
      dplyr::filter(Biome %in% biome)
  }

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

# Exemplo de uso: realms tropicais, restrito aos 3 biomas tropicais do Brasil
# diversity <- process_diversity(biodiversity, realm = regiao_filtros$all_tropics$realm,
#   biome = regiao_filtros$all_tropics$biome)
#
# Exemplo de uso: apenas o Neotropico, restrito aos mesmos 3 biomas
# diversity <- process_diversity(biodiversity, realm = regiao_filtros$Neotropics$realm,
#   biome = regiao_filtros$Neotropics$biome)
#
# Exemplo de uso: todos os realms e biomas (Global), sem filtro nenhum
# diversity <- process_diversity(biodiversity, realm = regiao_filtros$Global$realm,
#   biome = regiao_filtros$Global$biome)
#
# Exemplo de uso: Neotropico, isolando Cropland_A como categoria propria
# diversity_cropland_A <- process_diversity(biodiversity, realm = regiao_filtros$Neotropics$realm,
#   biome = regiao_filtros$Neotropics$biome, custom_landuse = custom_landuse_list$cropland_A)
#
# Para rodar todas as combinacoes de uma vez, ver R/RMM-C3.R
