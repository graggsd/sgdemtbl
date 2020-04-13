test_that("output df has appropriate classes", {
    test <- suppressWarnings(
        test_categorical(data = demtbl_dat_2,
                             predictor = colnames(demtbl_dat_2)[2:5],
                             response = "outcome")
    )
    # char_cols <- colnames(test) %in% c("name", "value", "pref_test")
    num_cols <- colnames(test) %in% c("OR", "CI_lower", "CI_upper", "pearson_p",
                  "fisher_CI_lower", "fisher_CI_upper", "fisher_p")
    col_classes <- unlist(lapply(test, class))
    expect_true(all(col_classes[num_cols] == "numeric"))
})


