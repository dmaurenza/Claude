source("R/process_diversity.R")
source("R/run_models.R")

# Combinacoes de Realm/Biome a estimar ----
# "_neotropic": so dados do Realm "Neotropic" (America Latina), biomas da regiao.
# "_global": mesmos biomas da regiao, mas sem restringir o Realm -- agrega
# dados de todos os continentes que tenham biomas equivalentes aos brasileiros,
# dando mais poder estatistico ao modelo de cada regiao.
combinations <- list(
  list(label = "BR_neotropic", realm = "Neotropic", biome = biome_br),
  list(label = "NE_neotropic", realm = "Neotropic", biome = Bioma_NE),
  list(label = "CO_neotropic", realm = "Neotropic", biome = Bioma_CO),
  list(label = "SE_neotropic", realm = "Neotropic", biome = Bioma_SE),
  list(label = "N_neotropic",  realm = "Neotropic", biome = Bioma_N),
  list(label = "S_neotropic",  realm = "Neotropic", biome = Bioma_S),

  list(label = "NE_global", realm = NULL, biome = Bioma_NE),
  list(label = "CO_global", realm = NULL, biome = Bioma_CO),
  list(label = "SE_global", realm = NULL, biome = Bioma_SE),
  list(label = "N_global",  realm = NULL, biome = Bioma_N),
  list(label = "S_global",  realm = NULL, biome = Bioma_S)
)

# Para rodar so um subconjunto, filtre a lista acima antes do loop, ex.:
# combinations <- combinations[c("NE_neotropic", "NE_global")]

results <- purrr::map(combinations, function(combo) {
  message("Processando combinacao: ", combo$label)

  diversity <- process_diversity(biodiversity, realm = combo$realm, biome = combo$biome)

  run_diversity_models(diversity, label = combo$label)
})

names(results) <- purrr::map_chr(combinations, "label")
