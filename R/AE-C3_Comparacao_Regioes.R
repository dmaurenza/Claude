###### Este script foi elaborado para a etapa de ANALISE ESTATISTICA ######
# Fica separado de proposito do pre-processamento (PDM-C3_Realms.R) e do
# ajuste dos modelos (RMM-C3.R): so precisa dos modelos ja salvos em
# Output/Models/, entao pode ser rodado (e re-rodado) sozinho, sem precisar
# reler os dados brutos nem reajustar os 80 modelos.
#
# OBJETIVO: decidir se vale a pena restringir a analise a um escopo de
# regiao mais local (Neotropics, Brazil, Brazil_custom) ou se o escopo
# Global/all_tropics ja representa bem o padrao.
#
# Por que NAO usar R2 para essa decisao: os 5 escopos (Global, all_tropics,
# Neotropics, Brazil, Brazil_custom) sao ajustados sobre bases de dados
# DIFERENTES (N e heterogeneidade diferentes) -- R2 mais alto num
# subconjunto menor nao significa modelo melhor, so que sobrou menos
# variancia residual naquele recorte. Em vez disso, este script compara a
# ESTIMATIVA do efeito de cada categoria de LandUse (na escala da variavel
# resposta: abundancia relativa reescalada para ab_m, similaridade
# composicional para cd_m) e seu INTERVALO DE CONFIANCA entre os 5 escopos.
# Se as estimativas de Neotropics/Brazil forem parecidas entre si mas
# diferentes de Global, isso e evidencia de que restringir o escopo importa;
# se forem parecidas com Global, restringir so custa poder estatistico
# (amostras menores, IC mais largos).
#
# Pre-requisito: rodar RMM-C3.R antes (ele salva os modelos ab_m/cd_m em
# Output/Models/ na Etapa 3b, e os CSVs de resultado/N em Output/).

library(tidyverse)
library(lme4)

# ===== CONFIGURACAO =========================================================
model_dir  <- "./Output/Models/"
output_dir <- "./Output/"

# nivel de confianca do intervalo (Wald, a partir de fixef()/vcov() do
# modelo -- ver predict_fixed_ci() abaixo)
conf_level <- 0.95

# nomes precisam bater com regiao_filtros e custom_landuse_list definidos
# no PDM-C3_Realms.R -- se voce mudar os nomes la, atualize aqui tambem.
# Usados so para interpretar os nomes dos arquivos de modelo salvos
# (<combination>_<run_label>_{ab_m,cd_m}.rds) de forma robusta, sem
# precisar re-executar o PDM-C3_Realms.R so para isso.
combinations <- c("Global", "all_tropics", "Neotropics", "Brazil", "Brazil_custom")
run_labels <- c(
  "baseline", "cropland_A", "cropland_B", "plantation_A", "plantation_B",
  "plantation_C", "pasture_A", "natural_vegetation_A"
)
# ============================================================================

# inv_logit() e uma copia do helper definido dentro de run_bii_models() no
# RMM-C3.R -- repetida aqui porque este script roda sozinho, sem sourcear o
# RMM-C3.R (que reajustaria os 80 modelos so para pegar essa funcao).
inv_logit <- function(f, a = 0.001) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}

# Etapa 1 - Localizar e identificar os modelos salvos ----------------------
model_files <- list.files(model_dir, pattern = "_(ab_m|cd_m)\\.rds$", full.names = TRUE)

if (length(model_files) == 0) {
  stop(
    "Nenhum modelo encontrado em '", model_dir, "'.\n",
    "Rode o RMM-C3.R antes deste script (ele salva os modelos na Etapa 3b)."
  )
}

# extrai combination/run_label/tipo do nome do arquivo
# (<combination>_<run_label>_<tipo>.rds). Faz por PERTENCIMENTO aos vetores
# `combinations`/`run_labels` acima, em vez de so cortar por "_" -- varios
# desses nomes (all_tropics, Brazil_custom, natural_vegetation_A, ...) tem
# underscore dentro do proprio nome, entao cortar ingenuamente quebraria.
# Usa o prefixo de combination mais LONGO que casar, para nao confundir
# "Brazil" com "Brazil_custom" (um e prefixo do outro).
parse_model_filename <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  tipo_modelo <- sub(".*_(ab_m|cd_m)$", "\\1", nm)
  resto <- sub("_(ab_m|cd_m)$", "", nm)

  candidatos <- combinations[purrr::map_lgl(combinations, ~ startsWith(resto, paste0(.x, "_")))]
  if (length(candidatos) == 0) {
    stop("Nao consegui identificar a 'combination' no nome do arquivo: ", basename(f))
  }
  combination <- candidatos[[which.max(nchar(candidatos))]]

  run_label <- sub(paste0("^", combination, "_"), "", resto)
  if (!run_label %in% run_labels) {
    stop("run_label '", run_label, "' (do arquivo ", basename(f), ") nao esta em `run_labels`.")
  }

  tibble::tibble(combination = combination, run_label = run_label, tipo_modelo = tipo_modelo)
}

model_meta <- purrr::map_dfr(model_files, parse_model_filename) |>
  dplyr::mutate(file = model_files, .before = 1)

# Etapa 2 - IC (Wald) das predicoes de cada modelo --------------------------
# predict.merMod NAO calcula erro-padrao/IC nativamente. Em vez de depender
# de um pacote extra so pra isso (ex.: merTools, que faz bootstrap), usamos
# a formula fechada padrao para combinacao linear dos efeitos fixos:
#   fit = X %*% beta
#   SE  = sqrt(diag(X %*% vcov(modelo) %*% t(X)))
# onde X e a matriz de design (so dos termos FIXOS, sem os efeitos
# aleatorios) para as linhas de `newdata`. E exatamente o que
# predict(modelo, newdata, re.form = NA) faz por baixo dos panos para o
# ponto estimado -- aqui so adicionamos o erro-padrao/IC dessa mesma
# predicao. Nao propaga incerteza da variancia dos efeitos aleatorios (e
# uma aproximacao padrao, a mesma limitacao de predict(re.form = NA)).
predict_fixed_ci <- function(model, newdata, level = conf_level) {
  # nobars() tira os termos "(1 | SS)" da formula, deixando so os fixos;
  # o [-2] tira a variavel resposta (formula fica so "~ termos_fixos")
  formula_fixa <- lme4::nobars(formula(model))[-2]
  mm <- model.matrix(formula_fixa, newdata)

  beta <- lme4::fixef(model)
  vc <- as.matrix(vcov(model))
  mm <- mm[, names(beta), drop = FALSE]

  fit <- as.numeric(mm %*% beta)
  se <- sqrt(rowSums((mm %*% vc) * mm))
  z <- qnorm(1 - (1 - level) / 2)

  tibble::tibble(fit = fit, se = se, lwr = fit - z * se, upr = fit + z * se)
}

# Roda predict_fixed_ci() para um modelo ab_m (todas as categorias de
# LandUse que sobreviveram naquela rodada) e devolve na escala da variavel
# resposta (RescaledAbundance, desfazendo o sqrt() do modelo)
predict_ab_m <- function(model) {
  niveis <- levels(model@frame$LandUse)
  newdata <- data.frame(LandUse = factor(niveis, levels = niveis))

  predict_fixed_ci(model, newdata) |>
    dplyr::mutate(
      LandUse = niveis,
      # a media +/- IC esta na escala sqrt(RescaledAbundance) -- so faz
      # sentido para valores >= 0 antes de elevar ao quadrado (o limite
      # inferior do IC pode, em tese, cair abaixo de 0 se a incerteza for
      # grande; travamos em 0 antes do quadrado por coerencia biologica)
      pred = fit^2,
      lwr_resp = pmax(lwr, 0)^2,
      upr_resp = upr^2,
      .keep = "none"
    ) |>
    dplyr::select(LandUse, pred, lwr_resp, upr_resp)
}

# Mesma ideia para um modelo cd_m (categorias de lu_contrast), na escala de
# similaridade composicional (desfazendo logitCS via inv_logit()).
# log10geo fixado em 0, igual a Etapa 4 do RMM-C3.R (distancia geografica de
# referencia).
predict_cd_m <- function(model) {
  niveis <- levels(model@frame$lu_contrast)
  newdata <- data.frame(
    lu_contrast = factor(niveis, levels = niveis),
    log10geo = 0
  )

  predict_fixed_ci(model, newdata) |>
    dplyr::mutate(
      # extrai o nome da categoria a partir de "Primary minimal_vs_<categoria>",
      # igual a Etapa 4 do RMM-C3.R -- assim a coluna LandUse fica no mesmo
      # formato usada la, e da pra cruzar com os CSVs de resultado (N de
      # sites/comparacoes) por essa chave
      LandUse = sub("^Primary minimal_vs_", "", niveis),
      pred = inv_logit(fit),
      lwr_resp = inv_logit(lwr),
      upr_resp = inv_logit(upr),
      .keep = "none"
    ) |>
    dplyr::select(LandUse, pred, lwr_resp, upr_resp)
}

# roda a funcao certa (ab_m ou cd_m) para cada modelo salvo, marcando quais
# arquivos falharam em vez de derrubar o script inteiro no meio do loop
comparacao <- purrr::pmap_dfr(model_meta, function(file, combination, run_label, tipo_modelo) {
  resultado <- tryCatch(
    {
      modelo <- readRDS(file)
      preds <- if (tipo_modelo == "ab_m") predict_ab_m(modelo) else predict_cd_m(modelo)
      dplyr::mutate(preds, erro = NA_character_)
    },
    error = function(e) {
      message("Falhou em ", basename(file), ": ", conditionMessage(e))
      tibble::tibble(LandUse = NA_character_, pred = NA_real_, lwr_resp = NA_real_,
                      upr_resp = NA_real_, erro = conditionMessage(e))
    }
  )
  dplyr::mutate(resultado, combination = combination, run_label = run_label,
                tipo_modelo = tipo_modelo, .before = 1)
})

# Etapa 3 - Juntar com N de sites/comparacoes (ja calculado pelo RMM-C3.R) --
# Output/<combination>.csv ja tem, por LandUse, n_sitios_abundancia e
# n_comparacoes_composicional (Etapa 4 do RMM-C3.R) -- util para sinalizar
# categorias com amostra pequena, onde o IC e largo so por falta de dado.
n_por_combination <- purrr::map_dfr(unique(comparacao$combination), function(comb) {
  caminho <- paste0(output_dir, comb, ".csv")
  if (!file.exists(caminho)) {
    warning("Nao achei '", caminho, "' -- comparacao ficara sem n_sitios/n_comparacoes para essa combination.")
    return(tibble::tibble(combination = character(), LandUse = character(),
                           n_sitios_abundancia = integer(), n_comparacoes_composicional = integer()))
  }
  readr::read_csv(caminho, show_col_types = FALSE) |>
    dplyr::transmute(combination = comb, LandUse, n_sitios_abundancia, n_comparacoes_composicional)
})

comparacao <- comparacao |>
  dplyr::left_join(n_por_combination, by = c("combination", "LandUse"))

# Etapa 4 - Salvar a tabela comparativa completa ----------------------------
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
output_path_comparacao <- paste0(output_dir, "Comparacao_Regioes.csv")
readr::write_csv(comparacao, output_path_comparacao)
cat("Tabela comparativa (todas as regioes/rodadas) salva em:", output_path_comparacao, "\n")

if (any(!is.na(comparacao$erro))) {
  cat(
    "\nAtencao:", sum(!is.na(comparacao$erro)), "modelo(s) falharam ao gerar predicao -- ",
    "veja a coluna 'erro' em", output_path_comparacao, "ou as mensagens acima.\n"
  )
}

# Etapa 5 - Grafico comparativo (so o baseline, entre as 5 regioes) --------
# Restrito ao baseline (categorias padrao De Palma) porque essa e a
# pergunta central: "o efeito de cada LandUse muda dependendo do escopo de
# regiao?". Para comparar as rodadas de custom_landuse entre regioes, filtre
# `comparacao` (o CSV salvo acima) por run_label.
plot_data <- comparacao |>
  dplyr::filter(run_label == "baseline", !is.na(pred)) |>
  dplyr::mutate(combination = factor(combination, levels = combinations))

grafico <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = LandUse, y = pred, ymin = lwr_resp, ymax = upr_resp, color = combination)
) +
  ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), size = 0.4) +
  ggplot2::facet_wrap(
    ~tipo_modelo, scales = "free_y",
    labeller = ggplot2::as_labeller(c(ab_m = "Abundancia (ab_m)", cd_m = "Composicional (cd_m)"))
  ) +
  ggplot2::labs(
    x = "LandUse", y = "Predicao (IC 95%)", color = "Regiao",
    title = "Comparacao entre regioes -- categorias padrao (baseline)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))

output_path_plot <- paste0(output_dir, "Comparacao_Regioes_baseline.png")
ggplot2::ggsave(output_path_plot, grafico, width = 10, height = 6, dpi = 150)
cat("Grafico comparativo (baseline) salvo em:", output_path_plot, "\n")

cat(
  "\nComo interpretar:\n",
  "- Compare, para cada LandUse, se o ponto/IC de Neotropics/Brazil/Brazil_custom\n",
  "  fica proximo do de Global/all_tropics ou se desloca de forma consistente.\n",
  "- IC muito largo costuma coincidir com n_sitios_abundancia/n_comparacoes_composicional\n",
  "  baixos (colunas na tabela salva) -- desconfie da estimativa nesses casos,\n",
  "  independente do escopo.\n",
  "- R2 NAO entra nessa decisao (ver comentario no topo do script).\n"
)
