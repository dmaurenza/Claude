###### Calculo de perdas e ganhos de biodiversidade (DM) ######
# Este script reproduz os calculos da aba "Aplicacao DM" da planilha
# Gabarito_DM.xlsx. Para cada atributo de biodiversidade calcula:
#   - VBI  : valor de biodiversidade no local do impacto      (coluna I)
#   - VBC  : valor de biodiversidade no local da compensacao  (coluna U)
#   - VPLBi: valor do atributo = VBC - (-VBI)                  (coluna AJ)
#   - VPLBc: media dos VPLBi de um mesmo componente            (coluna AK)
#
# Correspondencia com as colunas do Gabarito (aba "Aplicacao DM"):
#   M_pre_imp = K   M_pos_imp = O   Bi_imp = N   a_imp = Q
#   M_pre_off = X   M_pos_off = AA  Bi_off = Z   a_off = AB
#   d = AD          y = AE          t = AF
#
# Formulas (iguais as da planilha):
#   C_pre_imp = M_pre_imp / Bi_imp          (J)
#   C_pos_imp = M_pos_imp / Bi_imp          (M)
#   VBI       = (C_pos_imp - C_pre_imp) * a_imp            (I)
#   C_pre_off = M_pre_off / Bi_off          (V)
#   C_pos_off = M_pos_off / Bi_off          (Y)
#   VBC       = (C_pos_off - C_pre_off) * a_off            (U)
#   VPLBi     = VBC - (-VBI)                               (AJ)
#
# Atencao: a coluna U da planilha NAO aplica os fatores d, y e t, embora o
# texto da coluna T descreva [(C_pos_off - C_pre_off) x y]/[(1 + d)^t] x a.
# A versao com os fatores so aparece na celula AS2. Por isso o script
# calcula as duas versoes: VBC / VPLBi (identicos a planilha) e
# VBC_ajustado / VPLBi_ajustado (com y, d e t).
#
# Uso:
#   1. Rode o script uma vez. Se Data/Entrada_DM.xlsx nao existir, ele e
#      criado a partir dos dados embutidos abaixo (dados do Gabarito).
#   2. Edite Data/Entrada_DM.xlsx (aba "Entrada") com os seus atributos.
#   3. Rode de novo. O resultado sai em Output/Resultado_DM.xlsx.

# Remover todos os elementos
rm(list = ls(all.names = TRUE))

library(readxl)
library(openxlsx)

arquivo_entrada <- "Data/Entrada_DM.xlsx"
arquivo_saida   <- "Output/Resultado_DM.xlsx"

# Valores aceitos para a taxa de incerteza (y): confiabilidade baixa, media e alta
valores_y <- c(0.62, 0.825, 0.955)

# Planilha de entrada embutida ----
# Dados da aba "Aplicacao DM" (linhas 3 a 15) do Gabarito_DM.xlsx.
# grupo_vplbc/incluir_vplbc reproduzem a coluna AK: a linha 8 nao tem AK e
# a linha 12 fica fora da media AK11 = MEDIA(AJ11; AJ13).
# A area do impacto estava como texto ("0,03174871062980965") no Gabarito.
area_impacto <- 0.03174871062980965

entrada_exemplo <- data.frame(
  id = 1:13,
  tipo = c("Palmito Jussara", "Espécies ameaçadas", "Espécies endêmicas",
           "Espécies endêmicas",
           rep("Floresta Ombrófila Densa de Mata Atlântica Terras baixas", 9)),
  componente = c("Tamanho da população de Palmito Juçara",
                 "Diversidade de espécies ameaçadas",
                 "Tamanho da população de Guapuruvu",
                 "Diversidade de espécies endêmicas",
                 "Indicadora de estágio maduro",
                 "Indicadora de estágio inicial",
                 "Forma de vida arbórea", "Forma de vida arbórea",
                 "Polinização Zoofílica", "Polinização Zoofílica",
                 "Polinização Zoofílica", "Zoocoria", "Zoocoria"),
  atributo = c("Número de indivíduos", "Riqueza", "Número de indivíduos",
               "Riqueza", "Número de indivíduos", "Abundância", "Riqueza",
               "Número de indivíduos", "Riqueza",
               "Número de indivíduos de Guapuruvu", "Número de indivíduos",
               "Número de indivíduos", "Riqueza"),
  especie = c("Euterpe edulis", "Espécies ameaçadas", "Schizolobium parahyba",
              "Espécies endêmicas", "Cupania oblongifolia",
              "Schizolobium parahyba", "Espécies arbóreas",
              "Espécies arbóreas", "Espécies zoófilas",
              "Schizolobium parahyba", "Espécies Zoófilas",
              "Espécies Zoocóricas", "Espécies Zoocóricas"),
  referencia = c("Portal CNCFlora", NA, "EMBRAPA",
                 "Tabarelli & Mantovani 1999", "Baitello et al. 1992", NA,
                 "Campos et al. 2011", "Campos et al. 2011",
                 "Colonetti et al. 2009", "EMBRAPA",
                 "Iamara-Nogueira et al. 2021; Boscolo et al. 2023",
                 "Miranda et al. 2019", "Piña-Rodrigues 1997"),
  grupo_vplbc = c("Palmito", "Ameaçadas", "Guapuruvu", "Endêmicas",
                  "Estágio maduro", "Estágio inicial", "Arbórea", "Arbórea",
                  "Polinização", "Polinização", "Polinização",
                  "Zoocoria", "Zoocoria"),
  incluir_vplbc = c(rep("Sim", 5), "Não", "Sim", "Sim", "Sim", "Não",
                    "Sim", "Sim", "Sim"),
  M_pre_imp = c(0, 0, 0, 1, 0, 6, 3, 15, 1, 6, 12, 13, 2),
  M_pos_imp = c(0, 0, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0),
  Bi_imp    = c(50, 20, 40, 40, 13, 10, 142, 1274, 100, 45, 1715, 1600, 80),
  a_imp     = area_impacto,
  M_pre_off = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0),
  M_pos_off = c(6, 1, 26, 4, 2, 26, 31, 821, 7, 26, 342, 565, 18),
  Bi_off    = c(50, 20, 40, 40, 13, 10, 142, 1274, 100, 45, 1715, 1600, 80),
  a_off     = 10.7439,
  d = 0.03,
  y = 0.955,
  t = 10,
  stringsAsFactors = FALSE
)

# Descricao de cada coluna da aba "Entrada" (vai para a aba LEIAME)
dicionario <- data.frame(
  coluna = names(entrada_exemplo),
  obrigatoria = c("Sim", "Sim", "Sim", "Sim", "Não", "Não", "Sim", "Sim",
                  rep("Sim", 8), "Não", "Não", "Não"),
  descricao = c(
    "Identificador único da linha",
    "Tipo (ecossistema, grupo de espécies etc.)",
    "Componente de biodiversidade avaliado",
    "Atributo medido (riqueza, número de indivíduos, abundância...)",
    "Espécie ou grupo de espécies",
    "Referência bibliográfica do valor de referência (Bi)",
    "Grupo usado na média do VPLBc (linhas com o mesmo nome são agregadas)",
    "Sim = entra na média do VPLBc do grupo; Não = fica de fora",
    "Medida do atributo antes do impacto (M_pre)",
    "Medida do atributo depois do impacto (M_pos)",
    "Valor de referência do atributo no local do impacto (Bi), > 0",
    "Área do impacto (a), em hectares",
    "Medida do atributo antes da compensação (M_pre)",
    "Medida do atributo depois da compensação (M_pos)",
    "Valor de referência do atributo no local da compensação (Bi), > 0",
    "Área da compensação (a), em hectares",
    "Taxa de desconto no tempo (d). Vazio = valor da aba Parametros",
    "Taxa de incerteza da compensação (y): 0,62 / 0,825 / 0,955. Vazio = aba Parametros",
    "Tempo (t), em anos. Vazio = valor da aba Parametros"
  ),
  coluna_gabarito = c("", "A", "B", "C", "D", "E", "", "", "K", "O", "N",
                      "Q", "X", "AA", "Z", "AB", "AD", "AE", "AF"),
  stringsAsFactors = FALSE
)

parametros_padrao <- data.frame(
  parametro = c("d", "y", "t"),
  valor = c(0.03, 0.955, 10),
  descricao = c("Taxa de desconto no tempo (ex.: 0,03 = 3% ao ano)",
                "Taxa de incerteza: 0,62 (baixa), 0,825 (média), 0,955 (alta)",
                "Tempo, em anos, até a compensação atingir nenhuma perda líquida"),
  stringsAsFactors = FALSE
)

colunas_numericas <- c("M_pre_imp", "M_pos_imp", "Bi_imp", "a_imp",
                       "M_pre_off", "M_pos_off", "Bi_off", "a_off",
                       "d", "y", "t")

# Criar a planilha modelo de entrada ----
criar_modelo_entrada <- function(caminho, dados = entrada_exemplo) {
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

  est_cab   <- createStyle(textDecoration = "bold", fgFill = "#D9D9D9",
                           border = "Bottom", wrapText = TRUE,
                           valign = "center")
  est_input <- createStyle(fgFill = "#FFF2CC", fontColour = "#0000FF")
  est_texto <- createStyle(wrapText = TRUE, valign = "top")

  # Aba LEIAME
  addWorksheet(wb, "LEIAME")
  leiame <- c(
    "Planilha de entrada para R/DM-Calculo_Perdas_Ganhos.R",
    "",
    "Preencha a aba 'Entrada': uma linha por atributo. Células amarelas com texto azul são entradas.",
    "Não renomeie as colunas da linha 1 da aba 'Entrada' (o script lê os nomes).",
    "A aba 'Parametros' guarda os valores padrão de d, y e t, usados quando a linha deixa d, y ou t vazios.",
    "Use ponto ou vírgula como separador decimal; números digitados como texto também são aceitos.",
    "As linhas de exemplo são os dados da aba 'Aplicação DM' do Gabarito_DM.xlsx."
  )
  writeData(wb, "LEIAME", leiame, startCol = 1, startRow = 1)
  addStyle(wb, "LEIAME", createStyle(textDecoration = "bold", fontSize = 12),
           rows = 1, cols = 1)
  writeData(wb, "LEIAME", dicionario, startRow = 9, headerStyle = est_cab)
  setColWidths(wb, "LEIAME", cols = 1:4, widths = c(18, 12, 80, 16))

  # Aba Entrada
  addWorksheet(wb, "Entrada")
  writeData(wb, "Entrada", dados, headerStyle = est_cab)
  n <- max(nrow(dados), 1) + 200  # espaco para novas linhas
  addStyle(wb, "Entrada", est_input, rows = 2:(n + 1),
           cols = seq_along(dados), gridExpand = TRUE)
  addStyle(wb, "Entrada", createStyle(fgFill = "#FFF2CC",
                                      fontColour = "#0000FF",
                                      numFmt = "0.0000"),
           rows = 2:(n + 1), cols = match(colunas_numericas, names(dados)),
           gridExpand = TRUE)
  for (j in seq_len(nrow(dicionario))) {
    writeComment(wb, "Entrada", col = j, row = 1,
                 comment = createComment(dicionario$descricao[j],
                                         author = "DM", visible = FALSE))
  }
  # suppressWarnings: aviso interno do openxlsx em listas, sem efeito no arquivo
  suppressWarnings({
    dataValidation(wb, "Entrada", col = match("incluir_vplbc", names(dados)),
                   rows = 2:(n + 1), type = "list",
                   value = "'Listas'!$A$2:$A$3")
    dataValidation(wb, "Entrada", col = match("y", names(dados)),
                   rows = 2:(n + 1), type = "list",
                   value = "'Listas'!$B$2:$B$4")
  })
  for (col in c("Bi_imp", "Bi_off")) {
    dataValidation(wb, "Entrada", col = match(col, names(dados)),
                   rows = 2:(n + 1), type = "decimal", operator = "greaterThan",
                   value = 0)
  }
  for (col in c("M_pre_imp", "M_pos_imp", "a_imp", "M_pre_off", "M_pos_off",
                "a_off", "d", "t")) {
    dataValidation(wb, "Entrada", col = match(col, names(dados)),
                   rows = 2:(n + 1), type = "decimal",
                   operator = "greaterThanOrEqual", value = 0)
  }
  freezePane(wb, "Entrada", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "Entrada", cols = seq_along(dados),
               widths = c(5, 30, 34, 28, 26, 30, 16, 13, rep(11, 11)))
  setRowHeights(wb, "Entrada", rows = 1, heights = 30)

  # Aba Parametros
  addWorksheet(wb, "Parametros")
  writeData(wb, "Parametros", parametros_padrao, headerStyle = est_cab)
  addStyle(wb, "Parametros", est_input, rows = 2:4, cols = 2)
  suppressWarnings(
    dataValidation(wb, "Parametros", col = 2, rows = 3, type = "list",
                   value = "'Listas'!$B$2:$B$4")
  )
  setColWidths(wb, "Parametros", cols = 1:3, widths = c(12, 10, 70))

  # Aba Listas (opcoes das listas suspensas)
  addWorksheet(wb, "Listas")
  writeData(wb, "Listas", data.frame(incluir_vplbc = c("Sim", "Não")),
            startCol = 1)
  writeData(wb, "Listas", data.frame(y = valores_y), startCol = 2)
  writeData(wb, "Listas", data.frame(confiabilidade = c("baixa", "média", "alta")),
            startCol = 3)

  saveWorkbook(wb, caminho, overwrite = TRUE)
  invisible(caminho)
}

# Leitura da planilha de entrada ----
# Converte numeros digitados como texto, com virgula decimal (ex.: "0,0317")
como_numero <- function(x) {
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- ifelse(grepl(",", x), gsub(",", ".", gsub(".", "", x, fixed = TRUE),
                                  fixed = TRUE), x)
  suppressWarnings(as.numeric(x))
}

ler_entrada <- function(caminho) {
  dados <- as.data.frame(read_excel(caminho, sheet = "Entrada"))
  faltando <- setdiff(names(entrada_exemplo), names(dados))
  if (length(faltando) > 0) {
    stop("Colunas ausentes na aba 'Entrada': ", paste(faltando, collapse = ", "))
  }
  # Remove linhas totalmente vazias
  dados <- dados[rowSums(!is.na(dados[, colunas_numericas])) > 0, ]
  for (col in colunas_numericas) dados[[col]] <- como_numero(dados[[col]])

  par <- as.data.frame(read_excel(caminho, sheet = "Parametros"))
  par <- setNames(como_numero(par$valor), par$parametro)
  list(dados = dados, parametros = par)
}

# Calculos ----
calcular_dm <- function(dados, parametros) {
  # d, y e t vazios na linha recebem o valor padrao da aba Parametros
  for (p in c("d", "y", "t")) {
    dados[[p]][is.na(dados[[p]])] <- parametros[[p]]
  }

  obrig <- c("M_pre_imp", "M_pos_imp", "Bi_imp", "a_imp",
             "M_pre_off", "M_pos_off", "Bi_off", "a_off")
  vazias <- which(rowSums(is.na(dados[, obrig])) > 0)
  if (length(vazias) > 0) {
    stop("Valores ausentes ou não numéricos nas linhas id: ",
         paste(dados$id[vazias], collapse = ", "))
  }
  bi_inval <- which(dados$Bi_imp <= 0 | dados$Bi_off <= 0)
  if (length(bi_inval) > 0) {
    stop("Bi deve ser maior que zero (linhas id: ",
         paste(dados$id[bi_inval], collapse = ", "), ")")
  }
  y_fora <- which(!sapply(dados$y, function(v) any(abs(v - valores_y) < 1e-9)))
  if (length(y_fora) > 0) {
    warning("y fora das classes 0,62 / 0,825 / 0,955 nas linhas id: ",
            paste(dados$id[y_fora], collapse = ", "))
  }

  within(dados, {
    C_pre_imp      <- M_pre_imp / Bi_imp
    C_pos_imp      <- M_pos_imp / Bi_imp
    VBI            <- (C_pos_imp - C_pre_imp) * a_imp
    C_pre_off      <- M_pre_off / Bi_off
    C_pos_off      <- M_pos_off / Bi_off
    VBC            <- (C_pos_off - C_pre_off) * a_off
    VBC_ajustado   <- ((C_pos_off - C_pre_off) * y) / ((1 + d)^t) * a_off
    VPLBi          <- VBC - (-VBI)
    VPLBi_ajustado <- VBC_ajustado - (-VBI)
  })
}

# VPLBc: media dos VPLBi de cada grupo (apenas linhas com incluir_vplbc = "Sim")
resumir_vplbc <- function(res) {
  sel <- res[toupper(substr(res$incluir_vplbc, 1, 1)) == "S", ]
  grupos <- unique(sel$grupo_vplbc)
  data.frame(
    grupo_vplbc    = grupos,
    n_atributos    = sapply(grupos, function(g) sum(sel$grupo_vplbc == g)),
    ids            = sapply(grupos, function(g)
                       paste(sel$id[sel$grupo_vplbc == g], collapse = ", ")),
    VPLBc          = sapply(grupos, function(g)
                       mean(sel$VPLBi[sel$grupo_vplbc == g])),
    VPLBc_ajustado = sapply(grupos, function(g)
                       mean(sel$VPLBi_ajustado[sel$grupo_vplbc == g])),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Conferencia com o Gabarito ----
# Valores calculados pelo Excel nas colunas AJ (VPLBi) e AK (VPLBc), linhas 3 a 15
conferir_gabarito <- function(tol = 1e-9) {
  aj_gabarito <- c(1.289268, 0.537195, 6.983535, 1.07359628223425,
                   1.65290769230769, 27.93414, 2.34482854836698,
                   6.92328545474141, 0.751755512893702, 4.05880666666667,
                   2.1422931868644, 3.79368172922613, 2.41658378223426)
  ak_gabarito <- c(Palmito = 1.289268, `Ameaçadas` = 0.537195,
                   Guapuruvu = 6.983535, `Endêmicas` = 1.07359628223425,
                   `Estágio maduro` = 1.65290769230769,
                   `Arbórea` = 4.63405700155419,
                   `Polinização` = 1.44702434987905,
                   Zoocoria = 3.10513275573019)
  res <- calcular_dm(entrada_exemplo, setNames(parametros_padrao$valor,
                                               parametros_padrao$parametro))
  vplbc <- resumir_vplbc(res)
  ok_aj <- all(abs(res$VPLBi - aj_gabarito) < tol)
  ok_ak <- all(abs(vplbc$VPLBc - ak_gabarito[vplbc$grupo_vplbc]) < tol) &&
    setequal(vplbc$grupo_vplbc, names(ak_gabarito))
  cat("Conferência com o Gabarito: VPLBi (AJ)", ifelse(ok_aj, "OK", "DIFERENTE"),
      "| VPLBc (AK)", ifelse(ok_ak, "OK", "DIFERENTE"), "\n")
  invisible(ok_aj && ok_ak)
}

# Exportar resultados ----
salvar_resultado <- function(res, vplbc, parametros, caminho) {
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  est_cab <- createStyle(textDecoration = "bold", fgFill = "#D9D9D9",
                         border = "Bottom", wrapText = TRUE)
  est_num <- createStyle(numFmt = "0.000000")

  ordem <- c("id", "tipo", "componente", "atributo", "especie", "referencia",
             "grupo_vplbc", "incluir_vplbc",
             "M_pre_imp", "M_pos_imp", "Bi_imp", "a_imp",
             "C_pre_imp", "C_pos_imp", "VBI",
             "M_pre_off", "M_pos_off", "Bi_off", "a_off",
             "C_pre_off", "C_pos_off", "VBC",
             "d", "y", "t", "VBC_ajustado", "VPLBi", "VPLBi_ajustado")
  res <- res[, ordem]

  addWorksheet(wb, "Resultados")
  writeData(wb, "Resultados", res, headerStyle = est_cab)
  calc <- match(c("C_pre_imp", "C_pos_imp", "VBI", "C_pre_off", "C_pos_off",
                  "VBC", "VBC_ajustado", "VPLBi", "VPLBi_ajustado"), ordem)
  addStyle(wb, "Resultados", est_num, rows = 2:(nrow(res) + 1), cols = calc,
           gridExpand = TRUE)
  freezePane(wb, "Resultados", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "Resultados", cols = seq_along(ordem), widths = "auto")

  addWorksheet(wb, "VPLBc")
  writeData(wb, "VPLBc", vplbc, headerStyle = est_cab)
  addStyle(wb, "VPLBc", est_num, rows = 2:(nrow(vplbc) + 1), cols = 4:5,
           gridExpand = TRUE)
  setColWidths(wb, "VPLBc", cols = 1:5, widths = c(20, 12, 12, 14, 16))

  addWorksheet(wb, "Parametros_usados")
  writeData(wb, "Parametros_usados",
            data.frame(parametro = names(parametros), valor = parametros),
            headerStyle = est_cab)

  saveWorkbook(wb, caminho, overwrite = TRUE)
  invisible(caminho)
}

# Execucao ----
conferir_gabarito()

if (!file.exists(arquivo_entrada)) {
  criar_modelo_entrada(arquivo_entrada)
  cat("Modelo de entrada criado em", arquivo_entrada,
      "(com os dados do Gabarito)\n")
}

entrada    <- ler_entrada(arquivo_entrada)
resultados <- calcular_dm(entrada$dados, entrada$parametros)
vplbc      <- resumir_vplbc(resultados)

print(resultados[, c("id", "componente", "VBI", "VBC", "VPLBi",
                     "VPLBi_ajustado")])
print(vplbc)

salvar_resultado(resultados, vplbc, entrada$parametros, arquivo_saida)
cat("Resultados salvos em", arquivo_saida, "\n")
