# This function takes a set of data, a given outcome variable, and a continuous variable,
# And tests for differences in this variable between the various outcomes using
# Either parametric or non-parametric tests
cont_compare <- function(data, continuous.covariate,
                         outcome, continuous.test = "wilcox.test"){

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
    miss.tbl <- as.matrix(xtabs(form, data = data, na.action = na.pass, exclude = NULL))

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

    # Make sure that the selected test is among those available
    if (!(continuous.test %in% c("omnibus.f.test",
                                 "students.t.test", "kruskall.wallis.test", "wilcox.test"))){

        stop("Stated categorical test must be one of the following:
             'omnibus.f.test', 'students.t.test',
             'kruskall.wallis.test', 'wilcox.test'")

    }

    # Choose which proportionality test to perform
    if (continuous.test == "omnibus.f.test"){

        p_val <- anova(lm(data[,continuous.covariate] ~ data[,outcome]))[["Pr(>F)"]][[1]]

    } else if (continuous.test == "students.t.test"){

        p_val <- t.test(data[,continuous.covariate] ~ data[,outcome])$p.value

        #     } else if (continuous.test == "kruskall.wallis.test"){
        #
        #         # TBD
        #         p.val <- fisher.test(cat.tbl)$p.value
        #
    } else if (continuous.test == "wilcox.test"){

        p_val <- wilcox.test(data[,continuous.covariate] ~ data[,outcome])$p.value

    }

    # Add the p-value as a column in the table
    if (p_val < 0.001){
        p_val <- "<0.001**"
    } else {
        p_val <- round(p_val, 3)
        if (p_val < 0.05){
            p_val <- paste0(p_val, "*")
        }
    }

    sum.table <- cbind(sum.table, P = c(p_val, rep("", nrow(sum.table) - 1)),
                       test = c(continuous.test, rep("", nrow(sum.table) - 1)))

    sum.table <- rbind(c(continuous.covariate, rep("", length(outcome_categories) + 3)),
                       sum.table)

    sum.table <- cbind(sub.tbl = continuous.covariate, sum.table)

    return(sum.table)

}


multi_cont_compare <- function(data, continuous.covariates, outcome,
                               continuous.tests = "wilcox.test"){

    if (length(continuous.tests) == 1){

        fact <- 0

    } else if (length(continuous.covariates) == length(continuous.tests)){

        fact <- 1

    } else {

        stop("The specified continuous tests must be of the same
             length as the number of continuous variables or of length 1")

    }

    multi.tbl <- cont_compare(data, continuous.covariates[1], outcome,
                              continuous.test = continuous.tests[1])

    if (length(continuous.covariates) > 1){
        for (i in 2:length(continuous.covariates)){
            multi.tbl <- rbind(multi.tbl, cont_compare(data, continuous.covariates[i],
                                                       outcome, continuous.test = continuous.tests[i^fact]))
        }
    }

    return(multi.tbl)

}
