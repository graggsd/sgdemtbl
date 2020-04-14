#' @export
summarise_continuous <- function(data,
                                 predictor,
                                 response = NULL,
                                 strata = NULL) {
    dat <-
        data %>%
        tidyr::pivot_longer(cols = predictor) %>%
        as.data.frame(stringsAsFactors = FALSE)

    if (!is.null(response)) {
        response_levels <- get_new_levels(dat, response)
        dat[, response] <- make_tabulatable(dat[, response])
        tmp <- dat
        tmp[, response] <- "Total"
        dat <- bind_rows(dat, tmp)
    }
    if (!is.null(strata)) {
        strata_levels <- get_new_levels(dat, strata)
        dat[, strata] <- make_tabulatable(dat[, strata])
        tmp <- dat
        tmp[, strata] <- "Total"
        dat <- bind_rows(dat, tmp)
    }

    out <-
        dat %>%
        dplyr::group_by_at(c("name", response, strata)) %>%
        dplyr::summarise(mean = mean(value, na.rm = TRUE),
                  median = median(value, na.rm = TRUE),
                  min = min(value, na.rm = TRUE),
                  max = max(value, na.rm = TRUE)) %>%
        as.data.frame(stringsAsFactors = FALSE)

    vals <- c("mean", "median", "min", "max")
    # TODO Make this optional
    # TODO Add ways to change up formatting
    out <- out %>%
        dplyr::mutate(value = paste0(round(median), "(", round(min),
                                        "-", round(max), ")"))
    out[, vals] <- NULL
    vals <- "value"

    if (!is.null(response)) {
        out[, response] <- factor(out[, response], levels = response_levels)
    }
    if (!is.null(strata)) {
        out[, strata] <- factor(out[, strata], levels = strata_levels)
    }
    out <- out  %>%
        dplyr::arrange_at(c("name", strata, response)) %>%
        tidyr::pivot_wider(names_from = c(strata, response),
                           values_from = vals)
    return(out)
}


