# Preserve factor order and ensure "Total" columns appear first ---------
get_new_levels <- function(data, col) {
    if (is.factor(data[, col])) {
        out <- c("Total", setdiff(levels(data[, col]), "Total"))
    } else {
        out <- c("Total", setdiff(sort(unique(data[, col])), "Total"))
    }
    return(out)
}

#' @export
count_categorical <- function(data,
                              predictor,
                              response = NULL,
                              strata = NULL,
                              exclude_missing = TRUE) {
    # IDEA This could perhaps be organized even more elegantly by using
    # some tricks from purrr and nested data.frames. For instance,
    # I could think about creating a nested table to start containing
    # the different strata, and then iterating on this instead of just
    # stacking the data up long-wise and grouping...not sure there's any
    # specific benefit to either method.

    # TODO Remove tidyverse dependence


    if (!is.null(response)) {
        response_levels <- get_new_levels(data, response)
        data[, response] <- as.character(data[, response])
    }

    if (!is.null(strata)) {
        strata_levels <- get_new_levels(data, strata)
        data[, strata] <- as.character(data[, strata])
    }

    # Create long dataset ---------------------------------------------------
    out <- data %>% tidyr::pivot_longer(cols = predictor)

    # Choose whether to count missing values or not -------------------------
    if (exclude_missing) {
        out <- out %>% tidyr::drop_na(value)
    }

    # Change to data.frame because tibble behaves funny right now -----------
    # TODO check status of changes to tibble
    out <- as.data.frame(out)

    if (!is.null(response)) {
        tmp <- out %>% dplyr::mutate_at(response, function(x) "Total")
        out <- dplyr::bind_rows(out, tmp)
    }

    if (!is.null(strata)) {
        tmp <- out %>% dplyr::mutate_at(strata, function(x) "Total")
        out <- dplyr::bind_rows(out, tmp)
    }

    # Calculate the number and percent for each response and strata -----------
    out <-
        out %>%
        dplyr::group_by_at(c("name", response, strata)) %>%
        dplyr::count(value) %>%
        dplyr::mutate(perc = n / sum(n) * 100) %>%
        tidyr::nest(nperc = c(n, perc))

    # Change levels for appropriate sorting -----------------------------------
    # TODO Change this if tibble gets fixed
    out <- as.data.frame(out)
    if (!is.null(response)) {
        out[, response] <- factor(out[, response],
                                  levels = response_levels)
    }
    if (!is.null(strata)) {
        out[, strata] <- factor(out[, strata],
                                levels = strata_levels)
    }

    # Arrange the table so it prints out as expected --------------------------
    out <- out %>%
        dplyr::arrange_at(c("name", "value", strata, response))
    if (!is.null(c(response, strata))) {
        out <- out %>%
            tidyr::pivot_wider(names_from = c(strata, response),
                               values_from = c("nperc"))
    }
    # TODO Create an option not to format N(%)
    # IDEA Possibly make it so that the function takes an argument that
    # specifies the formatting function

    # to_unnest <- setdiff(colnames(out), c("name", "value"))
    # return(unnest(out, cols = to_unnest, .sep = "_"))

    format_nperc <- function(x) {
        paste0(x$n, "(", sprintf("%.1f", x$perc), ")")
    }
    out <- out %>%
        dplyr::mutate_at(vars(-tidyselect::any_of(c("name", "value"))),
                         map_chr,
                         format_nperc)
    return(out)
}


