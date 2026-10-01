###### Conversao de coordenadas de ocorrencia para WGS84 (graus decimais) ######
# Le a planilha de ocorrencias (estrutura Darwin Core: verbatimLatitude,
# verbatimLongitude, verbatimSRS, decimalLatitude, decimalLongitude,
# geodeticDatum), identifica o formato de cada coordenada e devolve
# latitude/longitude em graus decimais no datum WGS84 (EPSG:4326).
#
# Formatos reconhecidos (em cada celula, de forma independente):
#   1. Grau decimal ........ -22.881988  |  -22,881988  |  22.881988 S
#   2. Grau-min-seg (GMS) .. 22°56'18.88''S  |  42°40'11.64''O  |  22°56’18”S
#   3. Grau-min decimal .... 22°56.3147'S
#   4. UTM (metros) ........ 7488776 (northing) + 723445 (easting), "UTM K23"
#
# Ordem de leitura por linha: primeiro as colunas verbatim (valor como veio
# da fonte); se estiverem vazias ou invalidas, as colunas decimal*.
#
# UTM: northing sozinho NAO define um ponto. E preciso o easting na mesma
# linha, a ZONA (lida de verbatimSRS, ex.: "UTM K23" -> 23) e o DATUM.
# O datum e procurado em verbatimSRS/geodeticDatum (SAD69, SIRGAS, WGS84,
# Corrego Alegre). Quando nao esta informado, usa datum_utm_padrao e grava
# em incerteza_datum_m a distancia entre a conversao feita como SAD69 e
# como SIRGAS2000 -- ou seja, o tamanho do erro possivel se o datum assumido
# estiver errado (no ponto UTM K23 do arquivo de teste, ~64 m).
#
# Coordenadas em grau (decimal ou GMS) sao tratadas como WGS84: o numero
# sozinho nao revela o datum. SIRGAS2000 e WGS84 diferem < 1 m.
#
# Checagem de plausibilidade: o par final precisa cair no retangulo do
# Brasil (bbox_* abaixo). Se cair fora mas o par invertido (lat <-> lon)
# cair dentro, inverte e avisa. Se nem assim, a linha fica sem coordenada
# (ex.: placeholders "1, 1").
#
# Saida: a planilha original + colunas lat_wgs84, lon_wgs84, fonte_coord,
# formato_coord, datum_origem, incerteza_datum_m e obs_coord. Gravada em
# .xlsx (7 casas decimais) e .csv no padrao brasileiro (";" e ",").

library(readxl)
library(openxlsx)
library(sf)

# ===== CONFIGURACAO =========================================================
arquivo_entrada <- "./Data/test.csv"            # .xlsx, .xls ou .csv
aba             <- 1                            # aba (so para Excel)

# nomes das colunas; use NA para a que nao existir na planilha
col_lat_verbatim <- "verbatimLatitude"
col_lon_verbatim <- "verbatimLongitude"
col_srs          <- "verbatimSRS"      # sistema/zona, ex.: "UTM K23", "WGS84"
col_lat_decimal  <- "decimalLatitude"
col_lon_decimal  <- "decimalLongitude"
col_datum        <- "geodeticDatum"

# textos tratados como celula vazia
valores_vazios <- c("", "NA", "N/A", "NULL", "-", "?")

# UTM sem datum informado: qual assumir?
#   "SIRGAS2000" -> padrao oficial do Brasil desde 2015 (IBGE)
#   "SAD69"      -> comum em levantamentos e cartas mais antigos
datum_utm_padrao <- "SIRGAS2000"
zona_utm_padrao  <- 23   # usada se a zona nao aparecer em col_srs

# GMS sem sinal e sem letra de hemisferio (Brasil: lat S, lon W)
hemisferio_lat_padrao <- "S"
hemisferio_lon_padrao <- "W"

# retangulo do Brasil (inclui ilhas oceanicas); NA desliga a checagem
bbox_lat <- c(-34.0, 5.5)
bbox_lon <- c(-74.0, -28.5)

casas_decimais <- 7
arquivo_saida  <- "./Output/ocorrencias_WGS84"  # sem extensao; gera .xlsx e .csv

# ===== FUNCOES ==============================================================

# Codigo EPSG da projecao UTM (hemisferio sul) para cada datum.
# Ex.: zona 23 -> 31983 SIRGAS 2000 / UTM 23S; 29193 SAD69 / UTM 23S;
#      32723 WGS 84 / UTM 23S; 22523 Corrego Alegre 1970-72 / UTM 23S
epsg_utm_sul <- function(zona, datum) {
  base <- switch(datum,
                 SIRGAS2000    = 31960L,
                 WGS84         = 32700L,
                 SAD69         = 29170L,
                 CorregoAlegre = 22500L,
                 stop("datum desconhecido: ", datum))
  base + as.integer(zona)
}

# Procura o nome de um datum num texto livre ("SAD 69", "Sirgas 2000"...)
detecta_datum <- function(txt) {
  txt <- toupper(ifelse(is.na(txt), "", txt))
  out <- rep(NA_character_, length(txt))
  out[grepl("C[OÓ]RREGO", txt)]  <- "CorregoAlegre"
  out[grepl("WGS", txt)]         <- "WGS84"
  out[grepl("SAD\\s*-?\\s*69", txt)] <- "SAD69"
  out[grepl("SIRGAS", txt)]      <- "SIRGAS2000"
  out
}

# Interpreta UMA celula de texto. Devolve lista com:
#   valor   - graus decimais (ou metros, se UTM)
#   formato - "decimal", "GMS", "GM", "UTM" ou NA
#   aviso   - texto de aviso ou ""
parse_coord <- function(x, eixo = c("lat", "lon")) {
  eixo <- match.arg(eixo)
  vazio <- list(valor = NA_real_, formato = NA_character_, aviso = "")
  if (is.na(x)) return(vazio)
  s <- toupper(trimws(as.character(x)))
  if (s %in% toupper(valores_vazios)) return(vazio)

  # hemisferio por letra: N/S, E/W e tambem L (leste) / O (oeste)
  letra <- regmatches(s, regexpr("[NSEWLO]", s))
  hemi  <- if (length(letra)) chartr("LO", "EW", letra) else NA_character_
  negativo <- grepl("^\\s*-", s)

  # todos os numeros da celula (virgula ou ponto como decimal)
  nums <- regmatches(s, gregexpr("[0-9]+([.,][0-9]+)?", s))[[1]]
  nums <- as.numeric(sub(",", ".", nums, fixed = TRUE))
  if (length(nums) == 0 || length(nums) > 3)
    return(list(valor = NA_real_, formato = NA_character_,
                aviso = paste0("formato nao reconhecido: '", x, "'")))

  aviso <- ""
  if (length(nums) == 1) {
    v <- nums[1]
    if (v > 180) return(list(valor = v, formato = "UTM", aviso = ""))
    formato <- "decimal"
  } else {
    g <- nums[1]; m <- nums[2]; sec <- if (length(nums) == 3) nums[3] else 0
    if (m >= 60 || sec >= 60)
      aviso <- paste0("minutos/segundos >= 60 em '", x, "'")
    v <- g + m / 60 + sec / 3600
    formato <- if (length(nums) == 3) "GMS" else "GM"
  }

  if (!is.na(hemi)) {
    ok <- if (eixo == "lat") hemi %in% c("N", "S") else hemi %in% c("E", "W")
    if (!ok) aviso <- paste0("letra '", hemi, "' incompativel com ", eixo)
    if (hemi %in% c("S", "W")) negativo <- TRUE
  } else if (!negativo && formato != "decimal") {
    padrao <- if (eixo == "lat") hemisferio_lat_padrao else hemisferio_lon_padrao
    negativo <- padrao %in% c("S", "W")
    aviso <- paste0(eixo, " sem hemisferio; assumido ", padrao)
  }
  if (negativo) v <- -v

  limite <- if (eixo == "lat") 90 else 180
  if (abs(v) > limite) {
    aviso <- paste0(eixo, " fora do intervalo valido: ", v)
    v <- NA_real_
  }
  list(valor = v, formato = formato, aviso = aviso)
}

# Aplica parse_coord a um vetor, interpretando cada valor distinto uma vez
parse_vetor <- function(x, eixo) {
  if (is.null(x)) x <- rep(NA_character_, n)
  u <- unique(x)
  p <- lapply(u, parse_coord, eixo = eixo)
  i <- match(x, u)
  list(valor   = vapply(p, `[[`, numeric(1), "valor")[i],
       formato = vapply(p, `[[`, character(1), "formato")[i],
       aviso   = vapply(p, `[[`, character(1), "aviso")[i])
}

# UTM (vetores) -> WGS84; uma chamada de st_transform por combinacao zona+datum
utm_para_wgs84 <- function(este, norte, zona, datum) {
  out <- matrix(NA_real_, nrow = length(este), ncol = 2,
                dimnames = list(NULL, c("lon", "lat")))
  grupo <- paste(zona, datum)
  for (g in unique(grupo)) {
    i <- which(grupo == g)
    pts <- st_as_sf(data.frame(x = este[i], y = norte[i]), coords = c("x", "y"),
                    crs = epsg_utm_sul(zona[i[1]], datum[i[1]]))
    out[i, ] <- st_coordinates(st_transform(pts, 4326))
  }
  out
}

# distancia em metros entre dois pontos (haversine)
dist_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(sqrt(a))
}

dentro_bbox <- function(lat, lon) {
  if (anyNA(c(bbox_lat, bbox_lon))) return(!is.na(lat) & !is.na(lon))
  !is.na(lat) & !is.na(lon) &
    lat >= bbox_lat[1] & lat <= bbox_lat[2] &
    lon >= bbox_lon[1] & lon <= bbox_lon[2]
}

junta_obs <- function(...) {
  o <- paste(..., sep = "; ")
  gsub("^(; )+|(; )+$", "", gsub("(; ){2,}", "; ", o))
}

coluna <- function(nome) if (is.na(nome)) NULL else dados[[nome]]

# ===== LEITURA ==============================================================
# Tudo como texto, para nao perder os simbolos ° ' " nem a virgula decimal
if (grepl("\\.csv$", arquivo_entrada, ignore.case = TRUE)) {
  # detecta o separador pela primeira linha (";" padrao BR ou ",")
  cab <- readLines(arquivo_entrada, n = 1, encoding = "UTF-8", warn = FALSE)
  sep <- if (lengths(gregexpr(";", cab)) > lengths(gregexpr(",", cab))) ";" else ","
  dados <- read.csv(arquivo_entrada, sep = sep, colClasses = "character",
                    check.names = FALSE, encoding = "UTF-8", na.strings = NULL)
} else {
  dados <- as.data.frame(read_excel(arquivo_entrada, sheet = aba,
                                    col_types = "text"))
}
cols <- c(col_lat_verbatim, col_lon_verbatim, col_srs,
          col_lat_decimal, col_lon_decimal, col_datum)
faltando <- setdiff(cols[!is.na(cols)], names(dados))
if (length(faltando)) stop("colunas nao encontradas: ", paste(faltando, collapse = ", "))
n <- nrow(dados)

# ===== INTERPRETACAO ========================================================
# Cada fonte (verbatim / decimal) e convertida separadamente; depois a
# linha usa a primeira que der um par valido dentro do Brasil.
srs_txt   <- paste(coluna(col_srs), coluna(col_datum))
datum_txt <- detecta_datum(srs_txt)
# zona: numero 18-25 isolado de outros digitos (pega "UTM K23", "23S", "zona 23")
m_zona   <- regmatches(srs_txt, regexec("(?<![0-9])(1[89]|2[0-5])(?![0-9])", srs_txt, perl = TRUE))
zona_txt <- as.integer(vapply(m_zona, function(v) if (length(v)) v[2] else NA_character_, ""))
zona <- ifelse(is.na(zona_txt), zona_utm_padrao, zona_txt)

converte_fonte <- function(col_lat, col_lon) {
  pl <- parse_vetor(coluna(col_lat), "lat")
  po <- parse_vetor(coluna(col_lon), "lon")
  lat <- pl$valor; lon <- po$valor
  obs <- junta_obs(pl$aviso, po$aviso)
  formato <- ifelse(is.na(pl$formato) | is.na(po$formato), NA_character_,
                    ifelse(pl$formato == po$formato, pl$formato,
                           paste(pl$formato, po$formato, sep = "/")))
  datum <- rep(NA_character_, n)
  incerteza <- rep(NA_real_, n)

  utm   <- pl$formato %in% "UTM" & po$formato %in% "UTM"
  misto <- xor(pl$formato %in% "UTM", po$formato %in% "UTM")
  obs[misto] <- junta_obs(obs[misto], "UTM incompleto ou misturado com graus")
  lat[misto] <- NA; lon[misto] <- NA

  if (any(utm)) {
    norte <- lat[utm]; este <- lon[utm]
    # easting (~160 000-840 000 m) e sempre menor que o northing no Brasil
    trocado <- este > norte
    tmp <- norte[trocado]; norte[trocado] <- este[trocado]; este[trocado] <- tmp
    idx <- which(utm)
    obs[idx[trocado]] <- junta_obs(obs[idx[trocado]], "easting/northing trocados")
    fora <- este < 100000 | este > 900000 | norte < 0 | norte > 10000000
    obs[idx[fora]] <- junta_obs(obs[idx[fora]], "UTM fora da faixa esperada")

    z  <- zona[utm]
    dt <- datum_txt[utm]
    sem_datum <- is.na(dt)
    dt[sem_datum] <- datum_utm_padrao
    ll <- utm_para_wgs84(este, norte, z, dt)
    lat[utm] <- ll[, "lat"]; lon[utm] <- ll[, "lon"]
    datum[utm] <- dt
    obs[idx[is.na(zona_txt[utm])]] <- junta_obs(obs[idx[is.na(zona_txt[utm])]],
                                                paste0("zona UTM nao informada; assumida ", zona_utm_padrao))

    # sem datum: quanto o ponto muda entre SAD69 e SIRGAS2000?
    if (any(sem_datum)) {
      k  <- idx[sem_datum]
      a  <- utm_para_wgs84(este[sem_datum], norte[sem_datum], z[sem_datum], rep("SAD69", sum(sem_datum)))
      b  <- utm_para_wgs84(este[sem_datum], norte[sem_datum], z[sem_datum], rep("SIRGAS2000", sum(sem_datum)))
      incerteza[k] <- round(dist_m(a[, "lat"], a[, "lon"], b[, "lat"], b[, "lon"]), 1)
      obs[k] <- junta_obs(obs[k], paste0("datum UTM nao informado; assumido ", datum_utm_padrao))
    }
  }

  graus <- !is.na(formato) & !utm & !misto
  datum[graus] <- ifelse(is.na(datum_txt[graus]), "WGS84 (assumido)", datum_txt[graus])

  # plausibilidade: dentro do Brasil? se nao, tenta o par invertido
  ok <- dentro_bbox(lat, lon)
  inv <- !ok & dentro_bbox(lon, lat)
  tmp <- lat[inv]; lat[inv] <- lon[inv]; lon[inv] <- tmp
  obs[inv] <- junta_obs(obs[inv], "latitude/longitude estavam invertidas")
  ruim <- !ok & !inv & !is.na(lat) & !is.na(lon)
  obs[ruim] <- junta_obs(obs[ruim], paste0("fora do Brasil (", lat[ruim], ", ", lon[ruim], ")"))
  lat[ruim] <- NA; lon[ruim] <- NA

  # linha sem uma das duas coordenadas fica sem as duas
  inc <- is.na(lat) | is.na(lon)
  lat[inc] <- NA; lon[inc] <- NA
  list(lat = lat, lon = lon, formato = formato, datum = datum,
       incerteza = incerteza, obs = obs)
}

f_verb <- converte_fonte(col_lat_verbatim, col_lon_verbatim)
f_dec  <- converte_fonte(col_lat_decimal,  col_lon_decimal)

usa_verb <- !is.na(f_verb$lat)
usa_dec  <- !usa_verb & !is.na(f_dec$lat)
escolhe  <- function(campo) ifelse(usa_verb, f_verb[[campo]],
                                   ifelse(usa_dec, f_dec[[campo]], NA))

dados$lat_wgs84         <- round(escolhe("lat"), casas_decimais)
dados$lon_wgs84         <- round(escolhe("lon"), casas_decimais)
dados$fonte_coord       <- ifelse(usa_verb, "verbatim", ifelse(usa_dec, "decimal", NA))
dados$formato_coord     <- escolhe("formato")
dados$datum_origem      <- escolhe("datum")
dados$incerteza_datum_m <- escolhe("incerteza")
# avisos: os da fonte usada; se nenhuma serviu, os de ambas
dados$obs_coord <- ifelse(usa_verb, f_verb$obs,
                   ifelse(usa_dec, f_dec$obs,
                          junta_obs(ifelse(f_verb$obs == "", "", paste("verbatim:", f_verb$obs)),
                                    ifelse(f_dec$obs  == "", "", paste("decimal:",  f_dec$obs)))))

# ===== RESUMO ===============================================================
cat("Linhas lidas:", n, "\n")
cat("Linhas com coordenada final:", sum(!is.na(dados$lat_wgs84)), "\n")
cat("Fonte x formato:\n")
print(table(dados$fonte_coord, dados$formato_coord, useNA = "ifany"))
cat("Datum de origem:\n"); print(table(dados$datum_origem, useNA = "ifany"))
cat("Avisos mais frequentes:\n")
print(head(sort(table(dados$obs_coord[dados$obs_coord != ""]), decreasing = TRUE), 15))

# ===== GRAVACAO =============================================================
dir.create(dirname(arquivo_saida), showWarnings = FALSE, recursive = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "ocorrencias")
writeData(wb, "ocorrencias", dados)
cols_coord <- match(c("lat_wgs84", "lon_wgs84"), names(dados))
addStyle(wb, "ocorrencias",
         createStyle(numFmt = paste0("0.", strrep("0", casas_decimais))),
         rows = 2:(n + 1), cols = cols_coord, gridExpand = TRUE)
saveWorkbook(wb, paste0(arquivo_saida, ".xlsx"), overwrite = TRUE)

# CSV padrao brasileiro: -22,8819880
write.csv2(dados, paste0(arquivo_saida, ".csv"), row.names = FALSE,
           fileEncoding = "UTF-8", na = "")

cat("Arquivos gravados em", paste0(arquivo_saida, c(".xlsx", ".csv")), sep = "\n  ")
