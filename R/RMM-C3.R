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
#      (7 rodadas)
#   3) de cada rodada com filtro, guarda so a linha da categoria nova
#   4) junta tudo (categorias padrao + as categorias customizadas de
#      custom_landuse_list) numa unica tabela de resultados
#   5) salva os resultados (coeficientes + R2) em disco
#   6) salva os modelos (ab_m e cd_m) de cada rodada em disco
#
# ... e repete os passos 1-6 automaticamente para CADA filtro de regiao
# listado em `regiao_filtros` (definido no PDM-C3_Realms.R): Global,
# all_tropics e Neotropics.
#
# Pre-requisito: rodar o PDM-C3_Realms.R inteiro antes deste script, na
# mesma sessao do R (ele define biodiversity, regiao_filtros,
# process_diversity() e custom_landuse_list).

# ===== CONFIGURACAO DA REGIAO ==============================================
# Este script roda uma vez para cada filtro de `regiao_filtros` (ver
# PDM-C3_Realms.R) -- Global, all_tropics e Neotropics. Cada filtro ja
# carrega seu proprio `realm` e/ou `biome`, entao nao ha nada para
# configurar aqui: para adicionar/remover uma regiao, edite
# `regiao_filtros` no PDM-C3_Realms.R.
#
# `combination` (usado para nomear os arquivos de saida em Output/ e os
# modelos em Output/Models/) e o proprio nome do filtro em regiao_filtros
# (ex.: "Neotropics", "all_tropics") -- montado automaticamente dentro de
# run_region(), nunca digitado a mao.
output_dir <- "./Output/"
# pasta onde os modelos (ab_m/cd_m) de cada rodada sao salvos (Etapa 3b)
model_dir <- "./Output/Models/"
# ============================================================================

# get_bray() fica FORA/ANTES de run_bii_models() de proposito. Se ela (ou
# qualquer funcao usada dentro do future_map2_dbl) for definida DENTRO de
# run_bii_models(), o ambiente local dela passa a incluir o biodiv inteiro
# (que e um parametro da funcao) -- e toda vez que essa funcao precisar ser
# exportada para os workers paralelos do future/furrr, o biodiv inteiro e
# serializado e enviado junto, mesmo sem nenhuma necessidade. Isso causa
# exatamente o erro "FutureError... failed to launch... worker no longer
# alive" com o tamanho dos globals na casa dos GiB. Definindo aqui fora
# (ambiente global), get_bray fica leve para exportar.
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

# Etapa 1/2/3/4 encapsuladas numa funcao, para poder rodar o mesmo pipeline
# repetidas vezes (uma por filtro) sem duplicar codigo ----
#
# combination identifica a regiao (ex.: "Neotropics", "all_tropics") e
# run_label identifica a rodada dentro dela (ex.: "baseline", ou o nome do
# filtro em custom_landuse_list, como "cropland_A") -- os dois juntos
# nomeiam os arquivos de modelo salvos na Etapa 3b. Sao recebidos
# explicitamente como parametros, em vez de lidos de uma variavel global,
# para nao correr o risco de salvar o modelo de uma rodada com o
# combination/run_label de outra.
run_bii_models <- function(biodiv, realm = NULL, biome = NULL, custom_landuse = NULL,
                            combination, run_label) {

  # Etapa 1 - Builting database ----
  diversity <- process_diversity(
    data = biodiv, realm = realm, biome = biome,
    custom_landuse = custom_landuse
  )

  # Etapa 2 - Calculate diversity indices ----
  # Total Abundance #####
  abundance_data <- diversity |>
    # LandUse == NA sao os registros "Cannot decide" (ver process_diversity())
    # -- precisam ser descartados aqui, do contrario entram no ab_m com
    # LandUse faltante e o lme4::lmer() falha em model.frame() com
    # "missing values in object" (na.fail). cd_data_input, logo abaixo, ja
    # faz esse mesmo filtro.
    dplyr::filter(!is.na(LandUse)) |>
    dplyr::filter(Diversity_metric_type == "Abundance") |>
    dplyr::group_by(SSBS) |>
    dplyr::mutate(TotalAbundance = sum(Effort_corrected_measurement)) |>
    dplyr::ungroup() |>
    dplyr::distinct(SSBS, .keep_all = TRUE) |>
    dplyr::group_by(SS) |>
    dplyr::mutate(MaxAbundance = max(TotalAbundance)) |>
    dplyr::ungroup() |>
    dplyr::mutate(RescaledAbundance = TotalAbundance / MaxAbundance) |>
    # descarta linhas com RescaledAbundance faltante -- acontece quando
    # Effort_corrected_measurement e NA em algum registro (sum() sem na.rm
    # propaga o NA para TotalAbundance) ou quando MaxAbundance e 0 (0/0 =
    # NaN, is.na(NaN) tambem e TRUE). Sem esse filtro, essas linhas chegam
    # ate o lme4::lmer() e derrubam o ajuste do ab_m com "missing values in
    # object" (na.fail).
    dplyr::filter(!is.na(RescaledAbundance)) |>
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
  # necessario mesmo), nao o biodiv inteiro.
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

  # get_bray() retorna NA quando os dois sites comparados tem abundancia
  # total zero (nenhuma especie registrada em nenhum dos dois -- composicao
  # indeterminada). Sem descartar essas linhas, o NA se propaga por
  # car::logit() ate logitCS e derruba o ajuste do cd_m com "missing values
  # in object" (na.fail).
  cd_data <- cd_data |>
    dplyr::filter(!is.na(bray))

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
  # combination + run_label + tipo de modelo (ex.:
  # "Neotropics_cropland_A_ab_m.rds").
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

# Roda o pipeline completo (baseline + custom_landuse_list + salvar
# resultados/R2/modelos) para um unico filtro de regiao (nome + realm/biome,
# vindos de regiao_filtros) ----
run_region <- function(nome_regiao, realm, biome) {

  combination <- nome_regiao
  cat("\n===== Rodando combination =", combination, "=====\n")

  baseline_run <- run_bii_models(
    biodiversity, realm = realm, biome = biome,
    combination = combination, run_label = "baseline"
  )

  baseline_results <- baseline_run$results
  baseline_r2 <- dplyr::mutate(baseline_run$r2, filtro = "baseline (categorias padrao)", .before = 1)

  # purrr::imap() em vez de purrr::map(): precisamos do nome de cada filtro
  # (cropland_A, cropland_B, ...) dentro do loop, para identificar a linha
  # de cada rodada na tabela de resultados/R2 (coluna `filtro`) e para
  # passar como run_label (usado para nomear os arquivos de modelo salvos
  # na Etapa 3b)
  custom_runs <- purrr::imap(custom_landuse_list, function(spec, nm) {
    run_bii_models(
      biodiversity, realm = realm, biome = biome, custom_landuse = spec,
      combination = combination, run_label = nm
    )
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
  if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
  output_path <- paste0(output_dir, combination, ".csv")
  output_path_r2 <- paste0(output_dir, combination, "_R2.csv")

  # trava de seguranca: nao sobrescreve o resultado de uma rodada anterior
  # dessa mesma combination. Se isso acontecer, para aqui com um aviso claro
  # em vez de substituir o arquivo silenciosamente -- apague (ou mova) os
  # arquivos antigos se quiser rodar essa combination de novo.
  if (file.exists(output_path) || file.exists(output_path_r2)) {
    stop(
      "Um dos arquivos ('", output_path, "' ou '", output_path_r2, "') ja existe e NAO sera sobrescrito automaticamente.\n",
      "Apague (ou mova) os arquivos antigos dessa combination se quiser roda-la de novo."
    )
  }

  readr::write_csv(results, output_path)
  readr::write_csv(r2_all, output_path_r2)
  cat("Resultado (coeficientes) salvo em:", output_path, "\n")
  cat("Resultado (R2 dos modelos) salvo em:", output_path_r2, "\n")
  cat("Modelos (ab_m/cd_m) salvos em:", model_dir, "\n")

  list(results = results, r2 = r2_all)
}

# Roda o pipeline completo para cada um dos filtros de regiao definidos em
# regiao_filtros (PDM-C3_Realms.R): Global, all_tropics e Neotropics ----
regioes_results <- purrr::imap(regiao_filtros, function(filtro, nome_regiao) {
  run_region(nome_regiao = nome_regiao, realm = filtro$realm, biome = filtro$biome)
})
