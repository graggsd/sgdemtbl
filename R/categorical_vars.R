cat_compare <- function(data, categorical.covariate, outcome,
                        categorical.test = "likelihood.ratio.chi.square"){

    # Generate formula to be used by xtabs function
    form <- as.formula(paste0("~", "`", categorical.covariate, "`",
                              "+", "`", outcome, "`"))

    # Form contingency table
    cat.tbl <- xtabs(form, data = data, drop.unused.levels = T)

    # Change the column names to reflect the outcome variable
    colnames(cat.tbl) <- paste0(colnames(cat.tbl), ".", outcome)

    # Make sure that the selected test is among those available
    if (!(categorical.test %in% c("likelihood.ratio.chi.square",
                                  "pearson.chi.square", "fisher.exact"))){

        stop("Stated categorical test must be one of the following:
             'likelihood.ratio.chi.square', 'pearson.chi.square',
             'fisher.exact'")

    }

    # Choose which proportionality test to perform
    if (categorical.test == "likelihood.ratio.chi.square"){
        p.val <- DescTools::GTest(cat.tbl, correct = "none")$p.value
    } else if (categorical.test == "pearson.chi.square"){
        p.val <- chisq.test(cat.tbl)$p.value
    } else if (categorical.test == "fisher.exact"){
        p.val <- fisher.test(cat.tbl)$p.value
    }

    # Change formatting of the p value
    p.val <- format_pval_dem(p.val)

    # Add totals to left-hand side of table
    cat.tbl <- cbind(Total = rowSums(cat.tbl), cat.tbl)

    # Add percentages to the table
    cat.tbl <- calc_percent_format(cat.tbl)

    # Remake contingency table with missing values
    miss.tbl <- as.matrix(xtabs(form, data = data,
                                na.action = na.pass, exclude = NULL))

    # Count number of NA values
    miss.tbl <- count_NAs_in_tbl(miss.tbl)

    if (!is.null(miss.tbl)){

        # Add to output table
        cat.tbl <- rbind(cat.tbl, miss.tbl)
        rownames(cat.tbl)[nrow(cat.tbl)] <- "NA"

    }

    # Add p-value
    cat.tbl <- cbind(cat.tbl,
                     P = c(p.val, rep("", nrow(cat.tbl) - 1)))

    # Add the test type to the output data
    cat.tbl <- cbind(cat.tbl,
                     test = c(categorical.test, rep("", nrow(cat.tbl) - 1)))

    # Add the names of each of the categories as a column
    cat.tbl <- cbind(quant = paste0("    ", rownames(cat.tbl)),
                     cat.tbl)

    # Add a heading to the top
    cat.tbl <- rbind(
        c(paste0(categorical.covariate, ", N (%)"), rep("", ncol(cat.tbl) - 1)),
        cat.tbl)

    # Add a column that indicates which covariate was being tested
    cat.tbl <- cbind(sub.tbl = categorical.covariate, cat.tbl)

    # Return the contingency table
    return(cat.tbl)

}

multi_cat_compare <- function(data, categorical.covariates,
                              outcome, categorical.tests = "likelihood.ratio.chi.square"){

    if (length(categorical.tests) == 1){

        fact <- 0

    } else if (length(categorical.tests) == length(categorical.tests)){

        fact <- 1

    } else {

        stop("The specified categorical tests must be of the same length as the number of categorical variables or of length 1")

    }

    for (i in 1:length(categorical.covariates)){

        if (i == 1){
            multi.cat.tbl <- cat_compare(data, categorical.covariates[i],
                                         outcome, categorical.test = categorical.tests[i^fact])
        }

        else {
            cat.tbl <- cat_compare(data, categorical.covariates[i],
                                   outcome, categorical.test = categorical.tests[i^fact])
            multi.cat.tbl <- rbind(multi.cat.tbl, cat.tbl)
        }
    }

    return(multi.cat.tbl)

}

