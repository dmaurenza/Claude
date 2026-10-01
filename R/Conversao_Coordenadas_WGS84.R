###### Conversao de coordenadas de ocorrencia para WGS84 (graus decimais) ######
# Le a planilha de ocorrencias, identifica o formato de cada coordenada e
# devolve latitude/longitude em graus decimais no datum WGS84 (EPSG:4326).
#
# Formatos reconhecidos (em cada celula, de forma independente):
#   1. Grau decimal ........ -22.881988  |  -22,881988  |  22.881988 S
#   2. Grau-min-seg (GMS) .. 22°56'18.88''S  |  22 56 18,88 S  |  -22°56'18.88"
#   3. Grau-min decimal .... 22°56.3147'S
#   4. UTM (metros) ........ 7461074 (northing) + 680123 (easting)
#
# UTM: um numero como 7461074 sozinho NAO define um ponto. Ele e o northing
# (distancia em metros ate o equador, no hemisferio sul contado a partir de
# 10.000.000 m). Para converter e preciso tambem o easting (que deve estar
# na coluna de longitude da mesma linha), a ZONA UTM e o DATUM de origem.
# Configure isso abaixo. Na duvida sobre o datum dos dados antigos, confira
# a ficha de campo / metadado: SAD69 e Corrego Alegre deslocam o ponto em
# dezenas de metros em relacao ao WGS84.
#
# Coordenadas em grau decimal e GMS sao tratadas como se ja estivessem em
# WGS84 (nao ha como saber o datum so pelo numero). SIRGAS2000 e WGS84 sao
# praticamente identicos (diferenca < 1 m).
#
# Saida: a planilha original + colunas lat_wgs84, lon_wgs84, formato_lat,
# formato_lon e obs_coord (avisos por linha). Gravada em .xlsx (numero com
# 7 casas decimais) e .csv no padrao brasileiro (separador ";", decimal ",").

library(readxl)
library(openxlsx)
library(sf)

# ===== CONFIGURACAO =========================================================
arquivo_entrada <- "./Data/ocorrencias.xlsx"   # .xlsx, .xls ou .csv
aba             <- 1                            # aba da planilha (nome ou numero)
col_lat         <- "latitude"                   # coluna com latitude / northing
col_lon         <- "longitude"                  # coluna com longitude / easting

# UTM
col_zona_utm    <- NA     # nome da coluna com a zona (ex.: "23" ou "23K"); NA = usar padrao
zona_utm_padrao <- 23     # zona usada quando nao ha coluna de zona (23 = maior parte de SP/RJ/MG)
datum_utm       <- "SIRGAS2000"  # "SIRGAS2000", "WGS84", "SAD69" ou "CorregoAlegre"

# GMS sem sinal e sem letra de hemisferio: qual hemisferio assumir?
# (no Brasil quase toda latitude e S e toda longitude e W). A linha recebe aviso.
hemisferio_lat_padrao <- "S"
hemisferio_lon_padrao <- "W"

casas_decimais <- 7
arquivo_saida  <- "./Output/ocorrencias_WGS84"  # sem extensao; gera .xlsx e .csv

# ===== FUNCOES ==============================================================

# Codigo EPSG da projecao UTM (hemisferio sul) para cada datum.
# Conferir com sf::st_crs(codigo)$Name antes de usar em outro contexto.
epsg_utm_sul <- function(zona, datum) {
  base <- switch(datum,
                 SIRGAS2000    = 31960L,  # 31983 = SIRGAS 2000 / UTM zone 23S
                 WGS84         = 32700L,  # 32723 = WGS 84 / UTM zone 23S
                 SAD69         = 29170L,  # 29193 = SAD69 / UTM zone 23S
                 CorregoAlegre = 22500L,  # 22523 = Corrego Alegre 1970-72 / UTM zone 23S
                 stop("datum_utm desconhecido: ", datum))
  base + as.integer(zona)
}

# Interpreta UMA celula de texto. Devolve lista com:
#   valor   - numero em graus decimais (ou metros, se UTM)
#   formato - "decimal", "GMS", "GM", "UTM" ou NA
#   aviso   - texto de aviso ou ""
parse_coord <- function(x, eixo = c("lat", "lon")) {
  eixo <- match.arg(eixo)
  vazio <- list(valor = NA_real_, formato = NA_character_, aviso = "")
  if (is.na(x)) return(vazio)
  s <- toupper(trimws(as.character(x)))
  if (s == "") return(vazio)

  # hemisferio por letra: N/S, E/W e tambem L (leste) / O (oeste)
  letra <- regmatches(s, regexpr("[NSEWLO]", s))
  hemi  <- if (length(letra)) chartr("LO", "EW", letra) else NA_character_
  negativo <- grepl("^\\s*-", s)

  # todos os numeros da celula (aceita virgula ou ponto como decimal)
  nums <- regmatches(s, gregexpr("[0-9]+([.,][0-9]+)?", s))[[1]]
  nums <- as.numeric(sub(",", ".", nums, fixed = TRUE))
  if (length(nums) == 0 || length(nums) > 3)
    return(list(valor = NA_real_, formato = NA_character_,
                aviso = paste0("formato nao reconhecido: '", x, "'")))

  aviso <- ""
  if (length(nums) == 1) {
    v <- nums[1]
    if (v > 180) {
      # metros: UTM (sinal e letras nao se aplicam)
      return(list(valor = v, formato = "UTM", aviso = ""))
    }
    formato <- "decimal"
  } else {
    g <- nums[1]; m <- nums[2]; sec <- if (length(nums) == 3) nums[3] else 0
    if (m >= 60 || sec >= 60)
      aviso <- paste0("minutos/segundos >= 60 em '", x, "'")
    v <- g + m / 60 + sec / 3600
    formato <- if (length(nums) == 3) "GMS" else "GM"
  }

  # sinal: "-" na frente ou letra S/W tornam negativo
  if (!is.na(hemi)) {
    ok <- if (eixo == "lat") hemi %in% c("N", "S") else hemi %in% c("E", "W")
    if (!ok) aviso <- paste0("letra de hemisferio '", hemi, "' incompativel com ", eixo)
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

# Converte pares UTM (vetores) para WGS84. Agrupa por zona para chamar
# st_transform uma vez por zona.
utm_para_wgs84 <- function(este, norte, zona, datum) {
  out <- matrix(NA_real_, nrow = length(este), ncol = 2,
                dimnames = list(NULL, c("lon", "lat")))
  for (z in unique(zona)) {
    i <- which(zona == z)
    pts <- st_as_sf(data.frame(x = este[i], y = norte[i]),
                    coords = c("x", "y"), crs = epsg_utm_sul(z, datum))
    out[i, ] <- st_coordinates(st_transform(pts, 4326))
  }
  out
}

# ===== LEITURA ==============================================================
# Tudo como texto, para nao perder os formatos GMS nem a virgula decimal
if (grepl("\\.csv$", arquivo_entrada, ignore.case = TRUE)) {
  dados <- read.csv2(arquivo_entrada, colClasses = "character",
                     check.names = FALSE, encoding = "UTF-8")
} else {
  dados <- as.data.frame(read_excel(arquivo_entrada, sheet = aba,
                                    col_types = "text"))
}
stopifnot(col_lat %in% names(dados), col_lon %in% names(dados))

n <- nrow(dados)
p_lat <- lapply(dados[[col_lat]], parse_coord, eixo = "lat")
p_lon <- lapply(dados[[col_lon]], parse_coord, eixo = "lon")

lat     <- vapply(p_lat, `[[`, numeric(1), "valor")
lon     <- vapply(p_lon, `[[`, numeric(1), "valor")
fmt_lat <- vapply(p_lat, `[[`, character(1), "formato")
fmt_lon <- vapply(p_lon, `[[`, character(1), "formato")
obs     <- paste(vapply(p_lat, `[[`, character(1), "aviso"),
                 vapply(p_lon, `[[`, character(1), "aviso"), sep = "; ")

# ===== UTM ==================================================================
eh_utm  <- fmt_lat %in% "UTM" | fmt_lon %in% "UTM"
par_utm <- fmt_lat %in% "UTM" & fmt_lon %in% "UTM"

# so um dos dois em metros: nao ha como converter
so_um <- eh_utm & !par_utm
obs[so_um] <- paste0(obs[so_um], "; UTM incompleto (falta easting ou northing)")
lat[so_um] <- NA; lon[so_um] <- NA

if (any(par_utm)) {
  norte <- lat[par_utm]
  este  <- lon[par_utm]
  # easting fica entre ~160.000 e 840.000 m; northing no Brasil passa de 1.000.000.
  # Se vieram trocados (easting na coluna de latitude), desfaz a troca.
  trocado <- este > norte
  tmp <- norte[trocado]; norte[trocado] <- este[trocado]; este[trocado] <- tmp
  idx <- which(par_utm)
  obs[idx[trocado]] <- paste0(obs[idx[trocado]], "; easting/northing estavam trocados")

  if (!is.na(col_zona_utm)) {
    zona <- as.integer(gsub("[^0-9]", "", dados[[col_zona_utm]][par_utm]))
    zona[is.na(zona)] <- zona_utm_padrao
  } else {
    zona <- rep(zona_utm_padrao, sum(par_utm))
  }

  fora <- este < 100000 | este > 900000 | norte < 0 | norte > 10000000
  obs[idx[fora]] <- paste0(obs[idx[fora]], "; valor UTM fora da faixa esperada")

  ll <- utm_para_wgs84(este, norte, zona, datum_utm)
  lat[par_utm] <- ll[, "lat"]
  lon[par_utm] <- ll[, "lon"]
  obs[par_utm] <- paste0(obs[par_utm], "; UTM zona ", zona, "S ", datum_utm)
}

# formatos diferentes entre lat e lon da mesma linha (ex.: GMS + decimal) e
# aceito, mas UTM + grau nao
misto <- !is.na(fmt_lat) & !is.na(fmt_lon) & xor(fmt_lat == "UTM", fmt_lon == "UTM")
obs[misto] <- paste0(obs[misto], "; mistura de UTM com graus")

# linha sem uma das duas coordenadas fica sem as duas
incompleta <- is.na(lat) | is.na(lon)
lat[incompleta] <- NA; lon[incompleta] <- NA

# limpa os separadores vazios do campo de observacao
obs <- gsub("^(; )+|(; )+$", "", gsub("(; ){2,}", "; ", obs))

dados$lat_wgs84   <- round(lat, casas_decimais)
dados$lon_wgs84   <- round(lon, casas_decimais)
dados$formato_lat <- fmt_lat
dados$formato_lon <- fmt_lon
dados$obs_coord   <- obs

# ===== RESUMO ===============================================================
cat("Linhas lidas:", n, "\n")
cat("Formato da latitude:\n");  print(table(fmt_lat, useNA = "ifany"))
cat("Formato da longitude:\n"); print(table(fmt_lon, useNA = "ifany"))
cat("Linhas sem coordenada final:", sum(is.na(dados$lat_wgs84) | is.na(dados$lon_wgs84)), "\n")
cat("Linhas com aviso em obs_coord:", sum(obs != ""), "\n")

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
