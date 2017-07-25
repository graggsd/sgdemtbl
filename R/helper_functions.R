#' Round p-values and indicate significance with asterisks
#'
#' @param p.val P-value to be modified
#' @return Formatted p-value, rounded to the second decimal place and with asterisks to indicate significance
format_pval_dem <- function(p.val){

    if (p.val < 0.01){

        p.val <- "<0.01**"

    } else if (p.val < 0.05){

        p.val <- paste0(round(p.val, digits = 2), "*")

    } else {

        p.val <- round(p.val,digits = 2)

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
#' @return A vector with the number of NA's per column and the total number of
#' NA's as the first value in the vector
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
