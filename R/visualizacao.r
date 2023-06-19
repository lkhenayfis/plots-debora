library(ggplot2)

plota_series <- function(variavel, patamar = NULL, only_imprime = TRUE, eixo_y_livre = TRUE,
    dat = dados, facet = `NOME DA USINA`) {

    if (only_imprime) dat <- dat[IMPRIME == 1]
    if (is.null(patamar)) {
        dat <- dat[, lapply(.SD, sum, na.rm = TRUE),  by = c("IPER", "IUSI", "NOME DA USINA")]
        patamar <- 6
    }

    gg <- ggplot(dat[IPAT == patamar]) +
        geom_line(aes(x = IPER, y = {{variavel}})) +
        facet_wrap(~`NOME DA USINA`, scales = ifelse(eixo_y_livre, "free_y", "fixed"), dir = "v") +
        theme_bw()

    return(gg)
}