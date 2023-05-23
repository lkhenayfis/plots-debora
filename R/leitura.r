library(data.table)

le_nwlistop <- function(arq) {
    nwlistop <- readLines(arq)

    ini <- grep("DADOS DAS USINAS INDIVIDUALIZADAS PARA", nwlistop)
    fim <- grep("OPERACAO DO PERIODO:", nwlistop)
    fim <- ini + (fim[2] - ini[1])
    fim[fim > length(nwlistop)] <- length(nwlistop)

    # identificacao dos tamanhos de coluna
    tabstr <- nwlistop[ini[1]:fim[1]]
    icol <- gregexpr("X", tabstr[5])[[1]]
    icol <- icol[-length(icol)] # ultimo X so fecha o negocio todo
    fcol <- strsplit(tabstr[5], "X")[[1]][-1] # antes do primeiro X e vazio
    fcol <- sapply(fcol, nchar, USE.NAMES = FALSE)
    fcol <- icol + fcol

    titulos <- substring(tabstr[3], icol, fcol)
    titulos <- trimws(titulos)

    tabelas <- mapply(ini, fim, FUN = function(i, f) {
        tabstr <- nwlistop[i:f]
        tabstr <- tabstr[-c(1, 2, 5)]
        tabstr <- tabstr[-(length(tabstr) - 0:3)]
        tabstr <- lapply(tabstr, substring, icol, fcol)
        tabstr <- lapply(tabstr, trimws)

        tab <- do.call(rbind, tabstr[-seq(2)])
        tab <- as.data.table(tab)
        tab[, (names(tab)[-4]) := lapply(.SD, as.numeric), .SDcols = -4]
        colnames(tab) <- titulos
        tab[, QDEF := (VTUR + VERT) / 730.5 / 3600 * 1e6]
    }, SIMPLIFY = FALSE)

    out <- rbindlist(tabelas)

    return(out)
}
