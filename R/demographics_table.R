

# Todo
# Add headers for Black, White, and Total
# Add method to use the demographics function and tidy up output table


# Probs need to set up t-test to account for equal or unequal variances
# You would also want to test the assumption of the t-test
# to decide whether you want a parametric or non-parametric value

# Could remove the -88's and -99's by using the "exclude =" argument
# Probably want to set this scheme up so that it will use the native values all the way to the end

# Create a function that will return continuous values up to a certain cutoff

dem_tbl <- function(data, outcome, continuous.covariates = NULL,
                    continuous.tests = "wilcox.test",
                    categorical.covariates = NULL,
                    categorical.tests = "likelihood.ratio.chi.square"){

    ###### Make a table for total values #######

    # Crosstabulate based on a specified outcome
    totals <- as.matrix(xtabs(as.formula(paste0("~",
                                                "`", outcome, "`")), data=data))
    # Calculate Percentages
    per.tbl <- prop.table(totals)
    # Combine percentages with the raw numbers
    totals <- paste0(totals, " (", round(per.tbl * 100, 1), ")")
    # This will inadvertantly get rid of the names, return them using those in
    # the percentage table
    names(totals) <- paste0(names(per.tbl), ".", outcome)
    # Add a description of the sub-table, a label to explain what is contained in each cell,
    # and a P-value place-holder

    ########################################
    totals <- c(sub.tbl = "Totals", quant = "N (%)", Total = nrow(data), totals, P = "", test="")

    ############# Make table for continuous covariates #########################

    # Compute a table for continuous covariates if they are specified
    if (!is.null(continuous.covariates)){

        ###################################
        # Just specify what is missing and terminate the function
        ###################################

        # Check whether any continuous covariates do not match
        if(sum(!(continuous.covariates %in% colnames(data))) > 0){
            print("An element in 'continuous.covarates' does not match with the column names of 'data'")

            # Subset continuous.covariates to contain only those elements which match with columns in data
            continuous.covariates <- intersect(continuous.covariates, colnames(data))

        }

        # If anything in continuous.covariates matches, then implement
        # the cont.tbl function as appropriate
        if (length(continuous.covariates) > 0){

            cont.tbl <- multi_cont_compare(data = data,
                                           continuous.covariates = continuous.covariates,
                                           outcome = outcome, continuous.tests = continuous.tests)

        } else {

            print("None of 'continuous.covariates' matched with columns of 'data'")
            cont.tbl <- NULL
        }

    }

    ############### Make table for categorical covariates #####################

    # Compute a table for continuous covariates if they are specified
    if (!is.null(categorical.covariates)){

        # Check whether any continuous covariates do not match
        if(sum(!(categorical.covariates %in% colnames(data))) > 0){
            print("An element in 'categorical.covarates' does not match with the column names of 'data'")

            # Subset categorical.covariates to contain only those elements which match with columns in data
            categorical.covariates <- intersect(categorical.covariates %in% colnames(data))

        }

        # If anything in categorical.covariates matches, then implement
        # the cont.tbl function as appropriate
        if (length(categorical.covariates) > 0){

            # Create a categorical table
            cat.tbl <- multi_cat_compare(data = data,
                                         categorical.covariates = categorical.covariates,
                                         outcome = outcome, categorical.tests = categorical.tests)

        } else {
            print("None of 'categorical.covariates' matched with columns of 'data'")
            cat.tbl <- NULL
        }

    }

    ############# Pull everything together #################
    #     if (!is.null(cat.tbl)){
    #         cat.tbl <- cat.tbl[,names(totals)]
    #     }
    #
    #     if (!is.null(cont.tbl)){
    #         cont.tbl <- cont.tbl[,names(totals)]
    #     }

    out.tbl <- rbind(totals, cat.tbl, cont.tbl)

    return(out.tbl)

}


#' Create a stratified demographics table
#'
#' @param data A set of data with all covariates in separate columns and each row representing exactly 1 individual
#' @param outcome Column containing indicating the primary outcome of interest (e.g. case/control or disease subtypes)
#' @param continuous.covariates Columns containing continuous covariates of interest
#' @param continuous.tests A vector of tests to run on the continuous covariates
#' @param categorical.covariates Columns containing the categorical covariates of interest
#' @param categorical.tests A vector of tests to run on categorical covariates
#' @param strata.col Column containing the categories by which the data will be stratified
#' @return A demographics table in the format typically implemented by the iMAGE study.
#' @export
strat_dem_tbl <-  function(data, outcome, continuous.covariates = NULL,
                           continuous.tests = "wilcox.test", categorical.covariates = NULL,
                           categorical.tests = "likelihood.ratio.chi.square", strata.col = NULL){

    # Make base table
    out.data <- dem_tbl(data = data, outcome = outcome,
                        continuous.covariates = continuous.covariates,
                        continuous.tests = continuous.tests,
                        categorical.covariates = categorical.covariates,
                        categorical.tests = categorical.tests)

    # Indicate that this is the unstratified data
    colnames(out.data) <- paste0(colnames(out.data), ".Overall")

    # Create uniquely identifying rownames to help line up the stratified tables
    rownames(out.data) <- paste0(out.data[,1], out.data[,2])

    if (!(is.null(strata.col))){

        # Find each individual stratum
        if (is.factor(data[,strata.col])){
            strata <- levels(data[,strata.col])
        } else {
            strata <- unique(data[,strata.col])
        }

        # Make a demographics table for each individual stratum and add to the base table
        for (stratum in strata){

            # Make a temporary subset of data
            tmp.data <- data[data[,strata.col] == stratum,]

            # Run demographics table function on this temporary set of data
            tmp.dem.data <- dem_tbl(data = tmp.data, outcome = outcome,
                                    continuous.covariates = continuous.covariates,
                                    continuous.tests = continuous.tests,
                                    categorical.covariates = categorical.covariates,
                                    categorical.tests = categorical.tests)

            # Create uniquely identifying rownames to help line up the stratified tables
            rownames(tmp.dem.data) <- paste0(tmp.dem.data[,1], tmp.dem.data[,2])

            # Find the rownames that are not included in the sub table
            idx <- which(!(rownames(out.data) %in% rownames(tmp.dem.data)))

            # Loop through each position where there is a lack of alignment
            # Replace one line at a time with empty space
            for (i in idx){
                tmp.dem.data <- rbind(tmp.dem.data[1:(i-1),], "", tmp.dem.data[i:nrow(tmp.dem.data),])
            }

            # Rename the columns to reflect which stratum we are referring to
            colnames(tmp.dem.data) <- paste0(colnames(tmp.dem.data), ".", strata.col, ".", stratum)

            # Once this is done, bind the two tables together
            out.data <- cbind(out.data, "", tmp.dem.data[,-(1:2)])

        }

    }

    out.data <- out.data[,-1]
    rownames(out.data) <- 1:nrow(out.data)

    return(out.data)

}

