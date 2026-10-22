###### Calculo de DM (perdas e ganhos de biodiversidade) ######
# Planilha de entrada: Modelos/Entrada_DM.xlsx
#   aba "Entrada"    -> uma linha por atributo
#   aba "Parametros" -> valores de d, y e t

library(readxl)

arquivo <- "Modelos/Entrada_DM.xlsx"

# 1. Importar os dados ----
dados <- read_excel(arquivo, sheet = "Entrada")

# 2. Importar as variaveis auxiliares (d, y, t) ----
parametros <- read_excel(arquivo, sheet = "Parametros")

d <- parametros$valor[parametros$parametro == "d"]  # taxa de desconto no tempo
y <- parametros$valor[parametros$parametro == "y"]  # taxa de incerteza
t <- parametros$valor[parametros$parametro == "t"]  # tempo (anos)

# 3. Calcular o DM de cada linha ----
# Local do impacto
dados$C_pre_imp <- dados$M_pre_imp / dados$Bi_imp
dados$C_pos_imp <- dados$M_pos_imp / dados$Bi_imp
dados$VBI       <- (dados$C_pos_imp - dados$C_pre_imp) * dados$a_imp

# Local da compensacao
dados$C_pre_off <- dados$M_pre_off / dados$Bi_off
dados$C_pos_off <- dados$M_pos_off / dados$Bi_off
dados$VBC       <- ((dados$C_pos_off - dados$C_pre_off) * y) / (1 + d)^t * dados$a_off

# DM = VBC - (-VBI)
dados$DM <- dados$VBC - (-dados$VBI)

dados[, c("componente", "VBI", "VBC", "DM")]

# 4. Calcular o DM medio por componente ----
# Usa apenas as linhas marcadas com "Sim" na coluna incluir_vplbc
dados_media <- dados[dados$incluir_vplbc == "Sim", ]

dm_medio <- aggregate(DM ~ componente, data = dados_media, FUN = mean)

dm_medio

# Salvar os resultados
write.csv(dados, "Output/DM_por_linha.csv", row.names = FALSE)
write.csv(dm_medio, "Output/DM_medio_por_componente.csv", row.names = FALSE)
