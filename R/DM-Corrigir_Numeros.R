###### Corrigir numeros digitados como texto ######
# Converte textos como "0,0317" ou "1.234,5" em numero.
# Celulas que ja sao numero ficam como estao.

corrigir_numero <- function(x) {
  if (is.numeric(x)) return(x)              # ja e numero: nao faz nada
  x <- trimws(as.character(x))              # tira espacos no inicio e no fim
  x[x == ""] <- NA                          # celula vazia vira NA
  com_virgula <- grepl(",", x)              # linhas com virgula decimal
  x[com_virgula] <- gsub(".", "", x[com_virgula], fixed = TRUE)   # tira o ponto de milhar
  x[com_virgula] <- gsub(",", ".", x[com_virgula], fixed = TRUE)  # troca virgula por ponto
  as.numeric(x)
}

# Exemplo de uso, logo apos importar a aba "Entrada":
# colunas_numericas <- c("M_pre_imp", "M_pos_imp", "Bi_imp", "a_imp",
#                        "M_pre_off", "M_pos_off", "Bi_off", "a_off")
# for (col in colunas_numericas) {
#   dados[[col]] <- corrigir_numero(dados[[col]])
# }
