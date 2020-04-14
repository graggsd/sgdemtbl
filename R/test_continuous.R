#' @export
test_continuous <- function(data,
                            predictor,
                            response,
                            strata = NULL) {
    rank_sum_p <- vector(mode = "list", length = length(predictor))
    names(rank_sum_p) <- predictor
    for (i in 1:length(predictor)) {
        eq <- as.formula(paste0(predictor[i], " ~ ", response))
        # return(eq)
        rank_sum_p[[i]] <- wilcox.test(eq, data = data)$p.value
    }
    out <- data.frame(name = names(rank_sum_p),
                      rank_sum_p = unlist(rank_sum_p),
                      stringsAsFactors = FALSE)
    rownames(out) <- 1:nrow(out)
    return(out)
}
