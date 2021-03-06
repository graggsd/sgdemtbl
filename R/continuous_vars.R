# This function takes a set of data, a given outcome variable, and a continuous variable,
# And tests for differences in this variable between the various outcomes using
# Either parametric or non-parametric tests

cont_compare <- function(data,
                         continuous.covariate,
                         outcome,
                         continuous.test = c("wilcox.test",
                                             "students.t.test",
                                             "kruskal.wallis.test",
                                             "omnibus.f.test"),
                         format_pval = FALSE,
                         p_val_digits = 4) {

    if ("wilcox.test" %in% continuous.test &
        length(unique(data[,outcome])) > 2 |
        "students.t.test" %in% continuous.test &
        length(unique(data[,outcome])) > 2) {

        stop("If 'wilcox.test' or 'students.t.test' are selected, ",
             "then there can only be two levels in the outcome variable.")

    }

    # Turn data into a numeric
    data[,continuous.covariate] <- as.numeric(data[,continuous.covariate])

    # Begin a table that includes mean, median, mode, and range
    sum.table <- as.matrix(format_summary_stats(data[,continuous.covariate]))
    colnames(sum.table) <- "Total"

    # Get the names of the outcome classifications
    outcome_categories <- levels(as.factor(data[,outcome]))

    # Loop through each outcome category, reporting summary statistics
    for (outcome_cat in outcome_categories){
        # Make index for a given outcome i
        outcome_idx <- data[,outcome] == outcome_cat
        # As above, but stratified by outcome
        sum.table <- cbind(sum.table,
                           format_summary_stats(data[outcome_idx, continuous.covariate]))
        # Rename to the new column appropriately
        colnames(sum.table)[ncol(sum.table)] <- outcome_cat
    }

    # Rename outcome columns to reflect the variable they came from
    colnames(sum.table)[-1] <- paste0(colnames(sum.table)[-1], ".", outcome)

    # Count the number of missing values

    # Generate formula to be used by xtabs function
    form <- as.formula(paste0("~", "`", continuous.covariate,"`", "+", "`", outcome, "`"))

    # Make a table to count the number of missing values
    miss.tbl <- make_miss_tbl(form = form, data = data)

    # Count number of NA values
    miss.tbl <- count_NAs_in_tbl(miss.tbl)

    if (!is.null(miss.tbl)){

        # Add to output table
        sum.table <- rbind(sum.table, miss.tbl)

    }

    quant <- c("    Mean (SD)", "    Median", "    Range")

    if (!is.null(miss.tbl)) {

        quant <- c(quant, "NA")

    }

    # Add the column indicating what each metric refers to
    sum.table <- cbind(quant = quant, sum.table)

    # Choose which proportionality test to perform
    if (continuous.test == "omnibus.f.test"){

        p_val <- anova(lm(data[,continuous.covariate] ~ data[,outcome]))[["Pr(>F)"]][[1]]

    } else if (continuous.test == "students.t.test"){

        p_val <- t.test(data[,continuous.covariate] ~ data[,outcome])$p.value

    } else if (continuous.test == "wilcox.test"){

        p_val <- wilcox.test(data[,continuous.covariate] ~ data[,outcome])$p.value

    } else if (continuous.test == "kruskal.wallis.test") {

        p_val <- kruskal.test(data[,continuous.covariate] ~ as.factor(data[,outcome]))$p.value
    }

    # Add the p-value as a column in the table
    p_val <- format_pval_dem(p_val,
                             format_pval = format_pval,
                             p_val_digits = p_val_digits)

    sum.table <- cbind(sum.table, P = c(p_val, rep("", nrow(sum.table) - 1)),
                       test = c(continuous.test, rep("", nrow(sum.table) - 1)))

    sum.table <- rbind(c(continuous.covariate, rep("", length(outcome_categories) + 3)),
                       sum.table)

    sum.table <- cbind(sub.tbl = continuous.covariate, sum.table)

    return(sum.table)

}


multi_cont_compare <- function(data,
                               continuous.covariates,
                               outcome,
                               continuous.tests = "wilcox.test",
                               format_pval = FALSE,
                               p_val_digits = 4) {

    if (length(continuous.tests) == 1){

        fact <- 0

    } else if (length(continuous.covariates) == length(continuous.tests)){

        fact <- 1

    } else {

        stop("The specified continuous tests must be of the same
             length as the number of continuous variables or of length 1")

    }

    multi.tbl <- cont_compare(data,
                              continuous.covariates[1],
                              outcome,
                              continuous.test = continuous.tests[1],
                              format_pval = format_pval,
                              p_val_digits = p_val_digits)

    if (length(continuous.covariates) > 1){
        for (i in 2:length(continuous.covariates)){
            multi.tbl <-
                rbind(multi.tbl,
                      cont_compare(data,
                                   continuous.covariates[i],
                                   outcome,
                                   continuous.test = continuous.tests[i^fact],
                                   format_pval = format_pval,
                                   p_val_digits = p_val_digits))
        }
    }

    return(multi.tbl)

}
