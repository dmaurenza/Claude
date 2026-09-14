###### Este script foi elaborado por Daniel Maurenza, com adaptacao para rodar
###### automaticamente todos os filtros customizados de uma vez ######
# Este código roda os modelos de abundancia e composição conforme o tutorial elaborado por De Palma
# [https://adrianadepalma.github.io/BII_tutorial/], para calcular o indice BII - Biodiversity Intactness Index
#
# DIFERENCA em relacao ao Run_Models_Maurenza.R original: em vez de rodar
# process_diversity() uma unica vez (o que so ativa, no maximo, UM filtro de
# custom_landuse por rodada, ja que Cropland_A/Cropland_B e
# Plantation_A/Plantation_C se sobrepoem em "Light use" e nao podem coexistir
# na mesma base), este script:
#   1) roda o pipeline uma vez SEM custom_landuse (categorias padrao De Palma)
#   2) roda o pipeline mais uma vez PARA CADA filtro de custom_landuse_list
#   3) de cada rodada com filtro, guarda so a linha da categoria nova
#   4) junta tudo (categorias padrao + as categorias customizadas de
#      custom_landuse_list) numa unica tabela de resultados
#   5) salva os modelos (ab_m e cd_m) de cada rodada em disco
#   6) recarrega todos os modelos salvos e roda uma selecao de modelos (AIC)
#      comparando todas as combinacoes de LandUse rodadas para essa regiao
#
# Pre-requisito: rodar o PDM-C3.R inteiro antes deste script, na mesma
# sessao do R (ele define dbbiodtotal, Bioma_*, Biome_BR, process_diversity()
# e custom_landuse_list).

# ===== CONFIGURACAO DA REGIAO ==============================================
# Para rodar para uma regiao/bioma diferente, so mude as 3 linhas abaixo --
# o resto do script usa essas 3 variaveis em vez de valores fixos, entao nao
# tem mais nada pra editar em outro lugar.
#   biome_regiao : um dos vetores definidos no PDM-C3.R
#                  (Bioma_NE, Bioma_CO, Bioma_SE, Bioma_N, Bioma_S), ou
#                  Biome_BR para todas as regioes brasileiras de uma vez
#   realm_regiao : "Neotropic" para restringir ao Neotropico, ou NULL para
#                  a versao "global" (todos os realms, mesmos biomas)
#   combination  : nome usado no arquivo de saida (Output/<combination>.csv)
#                  -- troque junto com biome_regiao para nao sobrescrever
#                  o resultado de uma regiao anterior
biome_regiao <- Biome_BR
realm_regiao <- c("Neotropic", "Afrotropic", "Indo-Malay", "Australasia")
combination  <- "Alltropic_BR"

# pasta onde os modelos (ab_m/cd_m) de cada rodada sao salvos (Etapa 3b) e
# depois recarregados para a selecao de modelos por AIC (Etapa 6)
model_dir <- "./Output/Models/"
# ============================================================================

# get_bray() fica FORA/ANTES de run_bii_models() de proposito. Se ela (ou
# qualquer funcao usada dentro do future_map2_dbl) for definida DENTRO de
# run_bii_models(), o ambiente local dela passa a incluir o dbbiodtotal
# inteiro (que e um parametro da funcao) -- e toda vez que essa funcao
# precisar ser exportada para os workers paralelos do future/furrr, o
# dbbiodtotal inteiro e serializado e enviado junto, mesmo sem nenhuma
# necessidade. Isso causa exatamente o erro
# "FutureError... failed to launch... worker no longer alive" com o tamanho
# dos globals na casa dos GiB. Definindo aqui fora (ambiente global), get_bray
# fica leve para exportar.
get_bray <- function(s1, s2, data) {
  sp_data <- data |>
    dplyr::filter(SSBS %in% c(s1, s2)) |>
    dplyr::select(SSBS, Taxon_name_entered, Measurement) |>
    tidyr::pivot_wider(names_from = Taxon_name_entered, values_from = Measurement) |>
    tibble::column_to_rownames("SSBS")

  if (sum(rowSums(sp_data) == 0, na.rm = TRUE) == 1) {
    bray <- 0
  } else if (sum(rowSums(sp_data) == 0, na.rm = TRUE) == 2) {
    bray <- NA
  } else {
    bray <- 1 -
      betapart::bray.part(sp_data) |>
      purrr::pluck("bray.bal") |>
      purrr::pluck(1)
  }
  bray
}

list_abundance <- list()
list_composition <- list()
# Etapa 1/2/3/4 encapsuladas numa funcao, para poder rodar o mesmo pipeline
# repetidas vezes (uma por filtro) sem duplicar codigo ----
#
# run_label identifica a rodada (ex.: "baseline", ou o nome do filtro em
# custom_landuse_list, como "cropland_A") e e usado, junto com `combination`
# (definido no bloco de configuracao acima), para nomear os arquivos de
# modelo salvos na Etapa 3b -- e depois para saber, na Etapa 6, de qual
# rodada/combinacao de LandUse cada modelo recarregado veio.

run_bii_models <- function(dbbiodtotal, biome, realm, custom_landuse = NULL, run_label) {

  # Etapa 1 - Builting database ----
  diversity <- process_diversity(
    data = dbbiodtotal, biome = biome, realm = realm,
    custom_landuse = custom_landuse
  )

  # Etapa 2 - Calculate diversity indices ----
  # Total Abundance #####
  abundance_data <- diversity |>
    dplyr::filter(Diversity_metric_type == "Abundance") |>
    dplyr::group_by(SSBS) |>
    dplyr::mutate(TotalAbundance = sum(Effort_corrected_measurement)) |>
    dplyr::ungroup() |>
    dplyr::distinct(SSBS, .keep_all = TRUE) |>
    dplyr::group_by(SS) |>
    dplyr::mutate(MaxAbundance = max(TotalAbundance)) |>
    dplyr::ungroup() |>
    dplyr::mutate(RescaledAbundance = TotalAbundance / MaxAbundance) |>
    # o LandUse so tem os niveis que sobreviveram a esse filtro/agregacao
    dplyr::mutate(LandUse = droplevels(LandUse))

  # Compositional Similarity #####
  cd_data_input <- diversity |>
    dplyr::filter(!is.na(LandUse)) |>
    dplyr::filter(Diversity_metric_type == "Abundance") |>
    dplyr::group_by(SS) |>
    dplyr::mutate(n_sample_effort = dplyr::n_distinct(Sampling_effort)) |>
    dplyr::mutate(n_species = dplyr::n_distinct(Taxon_name_entered)) |>
    dplyr::mutate(n_primin_records = sum(LandUse == "Primary minimal")) |>
    dplyr::ungroup() |>
    dplyr::filter(n_sample_effort == 1) |>
    dplyr::filter(n_species > 1) |>
    dplyr::filter(n_primin_records > 0) |>
    droplevels()

  studies <- cd_data_input |>
    dplyr::distinct(SS) |>
    dplyr::pull()

  site_comparisons <- purrr::map_dfr(
    .x = studies,
    .f = function(x) {
      site_data <- dplyr::filter(cd_data_input, SS == x) |>
        dplyr::select(SSBS, LandUse) |>
        dplyr::distinct(SSBS, .keep_all = TRUE)

      baseline_sites <- site_data |>
        dplyr::filter(LandUse == "Primary minimal") |>
        dplyr::pull(SSBS)

      site_list <- site_data |>
        dplyr::pull(SSBS)

      # stringsAsFactors = FALSE evita o erro "level sets of factors are
      # different" no filter(s1 != s2) logo abaixo (baseline_sites e um
      # subconjunto de site_list, entao os dois viravam factor com niveis
      # diferentes se deixados no default do expand.grid)
      site_comparisons <- expand.grid(baseline_sites, site_list, stringsAsFactors = FALSE) |>
        dplyr::rename(s1 = Var1, s2 = Var2) |>
        dplyr::filter(s1 != s2) |>
        dplyr::mutate(
          s1 = as.character(s1),
          s2 = as.character(s2),
          contrast = paste(s1, "vs", s2, sep = "_"),
          SS = as.character(x)
        )

      return(site_comparisons)
    }
  )

  future::plan("multisession", workers = parallel::detectCores() - 1)

  # .f = get_bray direto (a funcao global, sem envolver numa formula ~...)
  # e data = cd_data_input passado via ... -- assim nenhuma funcao/wrapper
  # nova e criada dentro do frame de run_bii_models(), e o unico "global"
  # grande exportado para os workers e o cd_data_input em si (que ja e
  # necessario mesmo), nao o dbbiodtotal inteiro.
  bray <- furrr::future_map2_dbl(
    .x = site_comparisons$s1,
    .y = site_comparisons$s2,
    .f = get_bray,
    data = cd_data_input,
    .options = furrr::furrr_options(seed = TRUE)
  )

  future::plan("sequential")

  latlongs <- cd_data_input |>
    dplyr::group_by(SSBS) |>
    dplyr::summarise(Lat = unique(Latitude), Long = unique(Longitude))

  lus <- cd_data_input |>
    dplyr::group_by(SSBS) |>
    dplyr::summarise(lu = unique(LandUse))

  cd_data <- site_comparisons |>
    dplyr::mutate(bray = bray) |>
    dplyr::left_join(latlongs, by = c("s1" = "SSBS")) |>
    dplyr::rename(s1_lat = Lat, s1_long = Long) |>
    dplyr::left_join(latlongs, by = c("s2" = "SSBS")) |>
    dplyr::rename(s2_lat = Lat, s2_long = Long) |>
    dplyr::mutate(
      geog_dist = geosphere::distHaversine(cbind(s1_long, s1_lat), cbind(s2_long, s2_lat))
    ) |>
    dplyr::left_join(lus, by = c("s1" = "SSBS")) |>
    dplyr::rename(s1_lu = lu) |>
    dplyr::left_join(lus, by = c("s2" = "SSBS")) |>
    dplyr::rename(s2_lu = lu) |>
    dplyr::mutate(lu_contrast = paste(s1_lu, s2_lu, sep = "_vs_"))

  # Etapa 3 - Run the statistical analysis ----
  ab_m <- lme4::lmer(
    sqrt(RescaledAbundance) ~ LandUse + (1 | SS) + (1 | SSB),
    data = abundance_data
  )

  cd_data <- dplyr::mutate(
    cd_data,
    logitCS = car::logit(bray, adjust = 0.001, percents = FALSE),
    log10geo = log10(geog_dist + 1),
    lu_contrast = factor(lu_contrast),
    lu_contrast = relevel(lu_contrast, ref = "Primary minimal_vs_Primary minimal")
  )

  cd_m <- lme4::lmer(
    logitCS ~ lu_contrast + log10geo + (1 | SS) + (1 | s2),
    data = cd_data
  )

  # Etapa 3b - Saving models ----
  # Salva ab_m e cd_m desta rodada (run_label) em disco, nomeados por
  # combination + run_label + tipo de modelo, para poderem ser recarregados
  # na Etapa 6 e comparados por AIC contra as demais rodadas (baseline e os
  # outros filtros de custom_landuse_list) dessa mesma regiao/combination.
  if (!dir.exists(model_dir)) {dir.create(model_dir, recursive = TRUE)}
  saveRDS(ab_m, file.path(model_dir, paste0(combination, "_", run_label, "_ab_m.rds")))
  saveRDS(cd_m, file.path(model_dir, paste0(combination, "_", run_label, "_cd_m.rds")))

  # R2 dos modelos (Nakagawa & Schielzeth) ----
  # R2 marginal = variancia explicada so pelos efeitos fixos (LandUse / lu_contrast)
  # R2 condicional = variancia explicada por efeitos fixos + aleatorios (SS, SSB, s2)
  # E um R2 por MODELO (nao por categoria) -- por isso um por rodada (baseline
  # ou cada filtro customizado), nao um por linha de LandUse.
  r2_ab <- performance::r2_nakagawa(ab_m)
  r2_cd <- performance::r2_nakagawa(cd_m)
  r2_table <- data.frame(
    modelo = c("abundancia (ab_m)", "composicional (cd_m)"),
    R2_marginal = c(r2_ab$R2_marginal, r2_cd$R2_marginal),
    R2_condicional = c(r2_ab$R2_conditional, r2_cd$R2_conditional)
  )

  # Etapa 4 - Projecting the model ----

  # numero de sitios usados no modelo de abundancia, por categoria de LandUse
  # (mesma ideia da coluna "Number of sites in abundance model" da tabela do
  # De Palma) -- abundance_data ja tem uma linha por site (SSBS), entao e so
  # contar quantas linhas cada nivel de LandUse tem.
  site_counts <- abundance_data |>
    dplyr::count(LandUse, name = "n_sitios_abundancia")

  newdata_ab <- data.frame(LandUse = levels(abundance_data$LandUse)) |>
    dplyr::mutate(ab_m_preds = predict(ab_m, dplyr::across(dplyr::everything()), re.form = NA) ^ 2) |>
    dplyr::left_join(site_counts, by = "LandUse")

  inv_logit <- function(f, a) {
    a <- (1 - 2 * a)
    (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
  }

  # numero de comparacoes par-a-par usadas no modelo composicional, por
  # contraste (mesma ideia da coluna "Number of pairwise comparisons for
  # compositional similarity" da tabela do De Palma) -- cd_data ja tem uma
  # linha por par de sites comparado, entao e so contar por lu_contrast.
  pair_counts <- cd_data |>
    dplyr::count(lu_contrast, name = "n_comparacoes_composicional")

  newdata_cd <- data.frame(
    lu_contrast = levels(cd_data$lu_contrast),
    log10geo = 0
  ) |>
    dplyr::mutate(
      cd_m_preds = predict(cd_m, dplyr::across(dplyr::everything()), re.form = NA) |>
        inv_logit(a = 0.001),
      # extrai o nome da categoria a partir de "Primary minimal_vs_<categoria>",
      # para poder casar com newdata_ab por LandUse em vez de por posicao de
      # linha (cbind por posicao so funciona por coincidencia, e quebra ou
      # embaralha os valores se newdata_ab e newdata_cd tiverem numeros de
      # linha diferentes -- o que acontece quando uma categoria customizada
      # sobrevive na abundancia mas e descartada na composicional, ou vice-versa)
      LandUse = sub("^Primary minimal_vs_", "", lu_contrast)
    ) |>
    dplyr::left_join(pair_counts, by = "lu_contrast")

  # junta os dois por LandUse (chave), em vez de cbind posicional
  results <- dplyr::full_join(newdata_ab, newdata_cd, by = "LandUse")

  # retorna a tabela de coeficientes E a tabela de R2 dessa rodada
  list(results = results, r2 = r2_table)

}

# Roda o pipeline base (sem filtro customizado) + um por um dos filtros de
# custom_landuse_list, guardando so a linha da categoria nova de cada rodada ----
baseline_run <- run_bii_models(dbbiodtotal, biome = biome_regiao, realm = realm_regiao, run_label = "baseline")

baseline_results <- baseline_run$results
baseline_r2 <- dplyr::mutate(baseline_run$r2, filtro = "baseline (categorias padrao)", .before = 1)

# purrr::imap() em vez de purrr::map(): precisamos do nome de cada filtro
# (cropland_A, cropland_B, ...) dentro do loop, para passar como run_label
# (usado para nomear os arquivos de modelo salvos na Etapa 3b)
custom_runs <- purrr::imap(custom_landuse_list, function(spec, nm) {
  run_bii_models(dbbiodtotal, biome = biome_regiao, realm = realm_regiao, custom_landuse = spec, run_label = nm)
})

custom_results <- purrr::map2_dfr(custom_runs, custom_landuse_list, function(run, spec) {
  dplyr::filter(run$results, LandUse == spec$label)
})

custom_r2 <- purrr::map2_dfr(custom_runs, names(custom_landuse_list), function(run, nm) {
  dplyr::mutate(run$r2, filtro = nm, .before = 1)
})

results <- dplyr::bind_rows(baseline_results, custom_results)
r2_all <- dplyr::bind_rows(baseline_r2, custom_r2)

# Etapa 5 - Saving results ----
output_dir <- "./Output/"
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
output_path <- paste0(output_dir, combination, ".csv")
output_path_r2 <- paste0(output_dir, combination, "_R2.csv")

# trava de seguranca: nao sobrescreve um resultado de outra regiao so porque
# o "combination" no topo do script nao foi atualizado junto com o
# biome_regiao/realm_regiao. Se isso acontecer, para aqui com um aviso claro
# em vez de substituir o arquivo silenciosamente.
if (file.exists(output_path) || file.exists(output_path_r2)) {
  stop(
    "Um dos arquivos ('", output_path, "' ou '", output_path_r2, "') ja existe e NAO sera sobrescrito automaticamente.\n",
    "Va no bloco 'CONFIGURACAO DA REGIAO' no topo do script e mude o valor de 'combination' ",
    "(por exemplo, para refletir a regiao/bioma que voce esta rodando agora), depois rode de novo."
  )
}

readr::write_csv(results, output_path)
readr::write_csv(r2_all, output_path_r2)
cat("Resultado (coeficientes) salvo em:", output_path, "\n")
cat("Resultado (R2 dos modelos) salvo em:", output_path_r2, "\n")

# Etapa 6 - Model selection (AIC) ----
# Recarrega TODOS os modelos salvos na Etapa 3b para esta `combination`
# (baseline + cada filtro de custom_landuse_list) e compara o ajuste deles
# por AIC. A comparacao e feita separadamente por tipo de modelo (abundancia
# x composicional), porque cada tipo tem variavel resposta e estrutura de
# efeitos aleatorios diferentes -- AIC so e comparavel entre modelos com a
# MESMA variavel resposta.
model_files <- list.files(
  model_dir,
  pattern = paste0("^", combination, "_.*_(ab_m|cd_m)\\.rds$"),
  full.names = TRUE
)

if (length(model_files) == 0) {
  stop(
    "Nenhum modelo salvo encontrado em '", model_dir, "' para combination = '", combination, "'.\n",
    "Rode a Etapa 3b (dentro de run_bii_models) antes desta etapa."
  )
}

model_selection <- purrr::map_dfr(model_files, function(f) {
  m <- readRDS(f)

  # nome do arquivo: <combination>_<run_label>_<tipo>.rds (tipo = ab_m ou cd_m)
  nm <- tools::file_path_sans_ext(basename(f))
  nm <- sub(paste0("^", combination, "_"), "", nm)
  tipo_modelo <- sub(".*_(ab_m|cd_m)$", "\\1", nm)
  run_label <- sub("_(ab_m|cd_m)$", "", nm)

  ll <- logLik(m)

  data.frame(
    combination = combination,
    run_label = run_label,
    tipo_modelo = tipo_modelo,
    npar = attr(ll, "df"),
    logLik = as.numeric(ll),
    AIC = AIC(m)
  )
})

# dentro de cada tipo de modelo, ordena do menor pro maior AIC e calcula o
# deltaAIC em relacao ao melhor modelo daquele tipo
model_selection <- model_selection |>
  dplyr::group_by(tipo_modelo) |>
  dplyr::arrange(AIC, .by_group = TRUE) |>
  dplyr::mutate(deltaAIC = AIC - min(AIC)) |>
  dplyr::ungroup() |>
  dplyr::arrange(tipo_modelo, AIC)

output_path_model_selection <- paste0(output_dir, combination, "_ModelSelection.csv")
readr::write_csv(model_selection, output_path_model_selection)
cat("Selecao de modelos (AIC) salva em:", output_path_model_selection, "\n")
print(model_selection)
