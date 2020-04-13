format_OR <- function(data,
                      OR = "OR",
                      CI_lower = "CI_lower",
                      CI_upper = "CI_upper") {
    ffun <- function(x) sprintf("%.2f", x)

    cols <- c(OR, CI_lower, CI_upper)
    for (col in cols) {
        data[, col] <- ffun(data[, col])
    }

    data$`OR(95%CI)` <- paste0(data[, OR],
                         "(",
                         data[, CI_lower],
                         "-",
                         data[, CI_upper],
                         ")")
    data[, c(OR, CI_upper, CI_lower)] <- NULL


    return(data)
}

format_p <- function(data) {
    p <- data$p
    new_p <- sprintf("%.2f", p)
    idx <- p < 0.001
    new_p[idx] <- "p<0.001"
    idx <- (p >= 0.001) & (p < 0.01)
    new_p[idx] <- "p<0.01"
    idx <- p < 0.05
    new_p[idx] <- paste0(new_p, "*")
    data$p <- new_p
    return(data)
}

select_test <- function(data) {
    data$p <- data$pearson_p
    fisher_idx <- data$pref_test == "Fisher"
    data$CI_lower[fisher_idx] <- data$fisher_CI_lower
    data$CI_upper[fisher_idx] <- data$fisher_CI_upper
    data$p[fisher_idx] <- data$fisher_p[fisher_idx]

    data[, c("pearson_p",
             "fisher_p",
             "fisher_CI_upper",
             "fisher_CI_lower")] <- NULL
    return(data)
}

format_nperc <- function(x) {
    paste0(x$n, "(", sprintf("%.1f", x$perc), ")")
}
