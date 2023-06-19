
source("R/leitura.r")
source("R/visualizacao.r")

arq <- "data/nwlistop_CR_HIB12.rel"
nome <- strsplit(arq, "/")[[1]][2]
nome <- sub(".rel", "", nome)

cat("\nLendo arquivo nwlistop... ")

ordem <- fread("data/OrdemImpressao.csv")
dados <- le_nwlistop(arq)
dados <- merge(dados, ordem, by = c("IUSI", "NOME DA USINA"))
setorder(dados, ORDEM)
dados[, `NOME DA USINA` := factor(`NOME DA USINA`, ordem[[2]], ordered = TRUE)]

cat("Ok!\n")

# ------------------------------------------------------------------------------

cat("Aplicando modificacoes de dado... \n")

# como criar nova coluna
# dados[, NOME_COL_NOVA := EXPRESSAO]
dados[, QDEF := (VTUR + VERT) / 730.5 / 3600 * 1e6]
cat("\t QDEF -- Ok!\n")

# coluna violacao total
dados[, VIOLTOTAL := DEPMIN + DTBMIN]
cat("\t VIOLTOTAL -- Ok!\n")

# ------------------------------------------------------------------------------

cat("Gerando plots... ")

if(!dir.exists("out")) dir.create("out")

gg <- plota_series(QDEF)
ggsave(file.path("out", paste0(nome, "_QDEF", ".jpeg")), gg, width = 16, height = 9)

gg <- plota_series(`VARMF%`, eixo_y_livre = FALSE)
ggsave(file.path("out", paste0(nome, "_VARMFP", ".jpeg")), gg, width = 16, height = 9)

gg <- plota_series(VIOLTOTAL)
ggsave(file.path("out", paste0(nome, "_VIOLTOTAL", ".jpeg")), gg, width = 16, height = 9)

cat("Ok!\n")