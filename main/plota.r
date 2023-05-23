library(ggplot2)

source("R/leitura.r")

arq <- "data/nwlistop_CR_HIB12.rel"
nome <- strsplit(arq, "/")[[1]][2]
nome <- sub(".rel", "", nome)

ordem <- fread("data/OrdemImpressao.csv")
dados <- le_nwlistop(arq)
dados <- merge(dados, ordem, by = c("IUSI", "NOME DA USINA"))
setorder(dados, ORDEM)
dados[, `NOME DA USINA` := factor(`NOME DA USINA`, ordem[[2]], ordered = TRUE)]

# ------------------------------------------------------------------------------

# como criar nova coluna
# dados[, NOME_COL_NOVA := EXPRESSAO]
dados[, QDEF := (VTUR + VERT) / 730.5 / 3600 * 1e6]

# ------------------------------------------------------------------------------

if(!dir.exists("out")) dir.create("out")

gg <- ggplot(dados[IMPRIME == 1, .("QDEF" = sum(QDEF)), by = c("IPER", "IUSI", "NOME DA USINA")]) +
    geom_line(aes(IPER, QDEF)) + facet_wrap(~ `NOME DA USINA`, scales = "free_y", dir = "v") +
    theme_bw()
ggsave(file.path("out", paste0(nome, "_QDEF", ".jpeg")), gg, width = 16, height = 9)

gg <- ggplot(dados[(IMPRIME == 1) & (IPAT == 1)]) +
    geom_line(aes(x = IPER, y = `VARMF%`)) + facet_wrap(~`NOME DA USINA`, dir = "v") +
    theme_bw()
ggsave(file.path("out", paste0(nome, "_VARMFP", ".jpeg")), gg, width = 16, height = 9)
