# Remake contingency table with missing values
make_miss_tbl <- function(form, data){

    return(as.matrix(xtabs(form, data = data,
                           na.action = na.pass,
                           addNA = TRUE)))

}

#' Round p-values and indicate significance with asterisks
#'
#' @param p.val P-value to be modified
#' @return Formatted p-value, rounded to the second decimal place and with asterisks to indicate significance
format_pval_dem <- function(p.val, format_pval = FALSE, p_val_digits = 4){

    cutoff <- 1*10^(-1*p_val_digits)

    if (format_pval) {

        if (p.val < cutoff) {

            p.val <- paste0("<", cutoff, "**")

        } else if (p.val < 0.05) {

            p.val <- paste0(round(p.val, digits = p_val_digits), "*")

        } else {
            p.val <- round(p.val,digits = p_val_digits)
        }
    } else {
        p.val <- round(p.val, digits = p_val_digits)
    }

    return(p.val)
}

#' Add percentages to a table
#'
#' @param input.tbl Table to be modified
#' @return A table with the original cell-wise totals and percentages by row in parentheses
calc_percent_format <- function(input.tbl){

    perc.table <- prop.table(as.matrix(input.tbl), margin = 2)

    for(i in 1:ncol(input.tbl)){

        input.tbl[,i] <- paste0(input.tbl[,i],
                                " (",
                                round(perc.table[,i] * 100, 1),
                                ")")
    }

    return(input.tbl)
}

#' Count the NA values in a table
#'
#' @param input.tbl Table in which to count NA values
#' @return A vector, the first value of which is the total number of NAs for all
#' outcome categories. The remaining values are the category-specific number of
#' missing values.
count_NAs_in_tbl <- function(input.tbl) {

    if (sum(is.na(rownames(input.tbl))) > 0) {
        rownames(input.tbl)[is.na(rownames(input.tbl))] <- "NA"
        output <- c(sum(input.tbl["NA",]), input.tbl["NA",])
    } else {

        output <- NULL

    }

    return(output)

}

#' Summarize a vector by mean, standard deviation, median, and range
#'
#' @param input.vector Numeric vector to be sumarrized
#' @return Vector of summary statistics
format_summary_stats <- function(input.vector){

    median <- round(median(input.vector, na.rm = T), 1)
    mean <- round(mean(input.vector, na.rm = T),1)
    range <- paste0(round(min(input.vector, na.rm = T), 1), "-",
                    round(max(input.vector, na.rm = T), 1))
    std <- round(sd(input.vector, na.rm = T), 1)

    out.vector <- c(paste0(mean, " (", std, ")"),
                    median,
                    range)

    return(out.vector)
}
