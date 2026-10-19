###### Padronizacao de dados vetoriais para SIRGAS 2000 / UTM 23S (EPSG:31983) ######
# Le todos os shapefiles (.shp) e GeoPackages (.gpkg) de uma pasta, descobre
# em que sistema de coordenadas cada um esta e reprojeta tudo para
# SIRGAS 2000 / UTM zona 23S (EPSG:31983).
#
# Por que nao basta confiar no CRS declarado: arquivos vindos de fontes
# diferentes muitas vezes chegam sem .prj (CRS desconhecido) ou com o CRS
# ERRADO. Ex.: o Alagado_Brejo.gpkg declara WGS 84 (EPSG:4326, graus), mas as
# coordenadas sao da ordem de -4.758.000 / -2.620.000 -- isso e Web Mercator
# (EPSG:3857, metros), o padrao do Google/OSM. Reprojetar confiando no CRS
# declarado geraria lixo. Por isso o script confere se os valores do bbox
# combinam com o CRS declarado e, quando nao combinam (ou nao ha CRS),
# deduz o CRS pela faixa dos valores (ver inferir_crs()).
#
# A deducao e uma heuristica: ela distingue graus, UTM e Web Mercator, mas
# NAO distingue datums com o mesmo tipo de coordenada (ex.: UTM 23S em
# SIRGAS 2000 vs SAD69 vs Corrego Alegre; graus em SIRGAS 2000 vs WGS 84 vs
# SAD69). Se souber o CRS de origem de algum arquivo, informe em crs_manual
# -- isso tem prioridade sobre tudo.
#
# Saida: cada arquivo de entrada vira um shapefile separado em pasta_saida
# (mesmo nome do original), mais uma tabela-resumo (CSV) do que foi feito com
# cada um. Um .gpkg com varias camadas vira um shapefile por camada.

library(sf)

# ===== CONFIGURACAO =========================================================
pasta_entrada <- "./Data/Shapes/"
pasta_saida   <- "./Output/Shapes_31983/"

epsg_destino <- 31983  # SIRGAS 2000 / UTM zone 23S

# CRS de origem informado a mao (nome do arquivo = EPSG). Tem prioridade sobre
# o CRS declarado e sobre a deducao automatica. Deixe vazio para nao usar.
# Ex.: c("Per_metro_-_Canteiro_HDD.shp" = 4674, "Alagado_Brejo.shp" = 3857)
crs_manual <- c()

# CRS assumido quando a deducao acha coordenadas em graus / em UTM 23S sem
# saber o datum. SIRGAS 2000 e o padrao oficial no Brasil; troque se os dados
# forem antigos (ex.: 4618 = SAD69 graus, 29193 = SAD69 / UTM 23S).
epsg_graus_padrao <- 4674   # SIRGAS 2000 geografico
epsg_utm_padrao   <- 31983  # SIRGAS 2000 / UTM 23S

# permite abrir .shp mesmo sem o .shx ao lado (o GDAL reconstroi o indice)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

# ===== FUNCOES ==============================================================

# Classifica um bbox pela faixa dos valores:
#   "graus"    -> lon/lat (|x| <= 180, |y| <= 90)
#   "utm_sul"  -> UTM hemisferio sul (E entre 100 e 900 km, N entre 1.000 e
#                 10.000 km)
#   "mercator" -> Web Mercator (metros, ate +-20.037.508 m, fora das faixas
#                 acima)
tipo_coordenada <- function(bb) {
  x <- bb[c("xmin", "xmax")]
  y <- bb[c("ymin", "ymax")]
  if (all(abs(x) <= 180) && all(abs(y) <= 90)) return("graus")
  if (all(x > 100000 & x < 900000) && all(y > 1e6 & y < 1e7)) return("utm_sul")
  if (all(abs(c(x, y)) <= 20037509)) return("mercator")
  "desconhecido"
}

inferir_crs <- function(bb) {
  switch(tipo_coordenada(bb),
         graus        = epsg_graus_padrao,
         utm_sul      = epsg_utm_padrao,
         mercator     = 3857,
         desconhecido = NA)
}

# O CRS declarado e coerente com os valores? Geografico precisa estar em
# graus; projetado nao pode estar em graus.
crs_coerente <- function(crs, bb) {
  tipo <- tipo_coordenada(bb)
  if (isTRUE(st_is_longlat(crs))) tipo == "graus" else tipo != "graus"
}

# Decide o CRS de origem de uma camada. Devolve lista com o crs e a origem da
# decisao (vai para a tabela-resumo).
definir_crs_origem <- function(camada, nome_arquivo) {
  bb <- st_bbox(camada)
  crs_decl <- st_crs(camada)

  if (nome_arquivo %in% names(crs_manual)) {
    return(list(crs = st_crs(crs_manual[[nome_arquivo]]), origem = "manual"))
  }
  if (!is.na(crs_decl) && crs_coerente(crs_decl, bb)) {
    return(list(crs = crs_decl, origem = "declarado"))
  }

  epsg <- inferir_crs(bb)
  if (is.na(epsg)) {
    stop("Nao foi possivel deduzir o CRS de '", nome_arquivo,
         "'. Informe o EPSG de origem em crs_manual.")
  }
  motivo <- if (is.na(crs_decl)) "sem CRS" else
    paste0("CRS declarado (", crs_decl$input, ") incoerente com as coordenadas")
  warning(nome_arquivo, ": ", motivo, " -> assumindo EPSG:", epsg,
          ". Confira se esta correto.", call. = FALSE)
  list(crs = st_crs(epsg), origem = paste0("deduzido (", motivo, ")"))
}

# ===== PROCESSAMENTO ========================================================
arquivos <- list.files(pasta_entrada, pattern = "\\.(shp|gpkg)$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(arquivos) == 0) stop("Nenhum .shp/.gpkg em ", pasta_entrada)

resumo <- list()

for (arq in arquivos) {
  nome_arquivo <- basename(arq)
  ext  <- tolower(tools::file_ext(arq))
  base <- tools::file_path_sans_ext(nome_arquivo)

  # sem o .dbf ao lado o GDAL abre so a geometria, sem erro -- e as colunas
  # de atributos somem da saida em silencio. Avisa para nao passar batido.
  tem_dbf <- tolower(paste0(base, ".dbf")) %in% tolower(list.files(dirname(arq)))
  if (ext == "shp" && !tem_dbf) {
    warning(nome_arquivo, ": .dbf nao encontrado na pasta de entrada -> ",
            "a saida tera so a geometria, sem as colunas de atributos.",
            call. = FALSE)
  }

  # um .gpkg pode ter varias camadas; um .shp tem uma so
  camadas <- st_layers(arq)$name

  for (lyr in camadas) {
    camada <- st_read(arq, layer = lyr, quiet = TRUE)
    origem <- definir_crs_origem(camada, nome_arquivo)

    # st_set_crs so ROTULA (nao mexe nas coordenadas); st_transform reprojeta
    camada <- st_set_crs(camada, NA)
    camada <- st_set_crs(camada, origem$crs)
    camada_utm <- st_transform(camada, epsg_destino)

    # descarta Z/M: alguns arquivos vem como PolygonZ (Z = 0, herdado do
    # software de origem) e o driver de shapefile recusa gravar "3D Polygon"
    # misturado com 2D
    camada_utm <- st_zm(camada_utm, drop = TRUE, what = "ZM")

    # sufixo evita colisao entre Alagado_Brejo.shp e Alagado_Brejo.gpkg
    nome_saida <- if (ext == "gpkg") paste0(base, "_gpkg") else base
    if (length(camadas) > 1) nome_saida <- paste0(nome_saida, "_", lyr)

    shp_saida <- file.path(pasta_saida, paste0(nome_saida, ".shp"))
    st_write(camada_utm, shp_saida, layer_options = "ENCODING=UTF-8",
             delete_dsn = file.exists(shp_saida), quiet = TRUE)

    bb <- st_bbox(camada_utm)
    resumo[[length(resumo) + 1]] <- data.frame(
      arquivo     = nome_arquivo,
      camada      = lyr,
      saida       = nome_saida,
      n_feicoes   = nrow(camada_utm),
      n_colunas   = ncol(st_drop_geometry(camada_utm)),
      crs_origem  = origem$crs$input,
      decisao_crs = origem$origem,
      xmin = round(bb[["xmin"]], 2), ymin = round(bb[["ymin"]], 2),
      xmax = round(bb[["xmax"]], 2), ymax = round(bb[["ymax"]], 2)
    )
  }
}

resumo <- do.call(rbind, resumo)
write.csv(resumo, file.path(pasta_saida, "resumo_reprojecao.csv"),
          row.names = FALSE)
print(resumo[, c("saida", "crs_origem", "decisao_crs")])

# Conferencia: tudo em UTM 23S no Brasil deve cair em E ~ 160.000-840.000 m e
# N ~ 6.500.000-10.000.000 m. Fora disso, algum CRS de origem esta errado.
fora <- with(resumo, xmin < 1e5 | xmax > 9e5 | ymin < 6e6 | ymax > 1e7)
if (any(fora)) {
  warning("Camadas fora da faixa esperada para UTM 23S: ",
          paste(resumo$saida[fora], collapse = ", "), call. = FALSE)
}
