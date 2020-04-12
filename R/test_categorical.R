
dim_warn <- function(tab) {
    if(!identical(dim(tab), c(2L, 2L))) {
        warning("Input must be a 2x2 table to compute OR and CI", call = FALSE)
    }
}

OR <- function(tab) {
    dim_warn(tab)
    if (identical(dim(tab), c(2L, 2L))) {
        out <- tryCatch(
            (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1]),
            error = function(e) NA
        )
    } else {
        out <- NA
    }
    out <- c(OR = out)
    return(out)
}

OR_confint <- function(tab) {
    dim_warn(tab)
    odds_ratio <- suppressWarnings(OR(tab))
    log_OR <- log(odds_ratio)
    diff <- tryCatch(
        1.96*sqrt(1/tab[1,1] + 1/tab[1,2] + 1/tab[2,1] + 1/tab[2,2]),
        error = function(e) NA
    )
    out <- c(odds_ratio,
             exp(log_OR - diff),
             exp(log_OR + diff))
    names(out) <- c("OR", "CI_lower", "CI_upper")
    return(out)
}

test_pearson <- function(tab) {
    out <-
        tryCatch(chisq.test(tab)$p.value,
                 error = function(e) NA
        )
    return(c(pearson_p = out))
}

test_fisher <- function(tab) {
    fish_test <- tryCatch(
        fisher.test(tab),
        error = function(e) NA
    )
    if (!all(is.na(fish_test))) {
        p <- fish_test$p.value
        if(is.null(p)) p <- NA
        ci <- fish_test$conf.int
        if(is.null(ci)) ci <- c(NA, NA)
        out <- c(ci, p)
    } else {
        out <- rep(NA, 3)
    }
    names(out) <- c("fisher_CI_lower", "fisher_CI_upper", "fisher_p")
    return(out)
}

test_prop <- function(tab) {
    out <- c(OR_confint(tab),
             test_pearson(tab),
             test_fisher(tab))
    return(out)
}

tab_rf <- function(predictor, response, data) {
    if (is.factor(data[, predictor])) {
        predictor_values <- levels(data[, predictor])
    } else {
        predictor_values <- sort(unique(data[, predictor]))
    }
    base_val <- predictor_values[1]
    predictor_values <- predictor_values[-1]

    data[, predictor] <- as.character(data[, predictor])

    out <- vector(mode = "list", length = length(predictor_values))
    names(out) <- predictor_values
    for (pred_val in predictor_values) {
        row_idx <- which(data[, predictor] %in% c(base_val, pred_val))
        out[[pred_val]] <- table(data[row_idx, predictor],
                                 data[row_idx, response])
    }
    return(out)
}

tab_rn <- function(predictor, response, data) {

    p_vec <- as.character(data[, predictor])
    r_vec <- as.character(data[, response])

    idx <- !is.na(p_vec) & !is.na(r_vec)

    p_vec <- p_vec[idx]
    r_vec <- r_vec[idx]

    pred_vals <- unique(p_vec)

    out <- vector(mode = "list", length = length(pred_vals))
    names(out) <- pred_vals

    for (pr_val in pred_vals) {
        p_vec_tmp <- rep(FALSE, length(p_vec))
        idx <- p_vec == pr_val
        p_vec_tmp[idx] <- TRUE
        out[[pr_val]] <- table(p_vec_tmp, r_vec)
    }
    return(out)
}

tab_a <- function(predictor, response, data) {
    return(table(data[, predictor], data[, response]))
}

test_categorical_engine <- function(data, predictor, response) {

    int_1 <- lapply(predictor, tab_rf, response, data)
    name_col <- lapply(int_1, length)
    for (i in 1:length(name_col)) {
        name_col[[i]] <- rep(predictor[i], name_col[[i]])
    }
    name_col <- unlist(name_col)
    val_col <- unlist(lapply(int_1, names))
    int_1 <- lapply(unlist(int_1, recursive = FALSE), test_prop)
    col_names <- c("name", "value", names(int_1[[1]]))
    out <- do.call(rbind.data.frame, int_1)
    out <- cbind(name_col, val_col, out)
    colnames(out) <- col_names
    return(out)
}

#' @export
test_categorical <- function(data, predictor, response, strata = NULL) {

    if (!is.null(strata)) {
        strata_levels <- get_new_levels(data, strata)
        data[, strata] <- as.character(data[, strata])
        tmp_data <- data
        tmp_data[, strata] <- "Total"
        data <- rbind(tmp_data, data)

        by_strata <- function(strata_val, data, predictor, response, strata) {
            data <- data[which(data[, strata] == strata_val), ]
            out <- test_categorical_engine(data, predictor, response)
            colnames(out)[-(1:2)] <- paste0(strata_val, "_",
                                            colnames(out)[-(1:2)])
            return(out)
        }

        out <- lapply(strata_levels, by_strata, data, predictor, response,
                      strata)
        out <- do.call(cbind, res)
    } else {
        out <- test_categorical_engine(data, predictor, response)
    }

    return(out)

}
