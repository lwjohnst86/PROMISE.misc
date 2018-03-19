context("Renaming and extracting")

fattyacids <- c("ne160", "tg181n7", "pl205n3", "ce141n7",
                "transfa_ne160", "pct_tg201n9", "TotalTG")

test_that("Renaming of fatty acids with fraction", {
    renamed <- renaming_fa(fattyacids, keep.fraction = TRUE)
    expected <- c("NE 16:0", "TG 18:1 n-7", "PL 20:5 n-3",
                  "CE 14:1 n-7", "transfa_NE 16:0", "pct_TG 20:1 n-9", "TotalTG")
    expect_identical(renamed, expected)
})

test_that("Renaming of fatty acids without fraction", {
    renamed <- renaming_fa(fattyacids, keep.fraction = FALSE)
    expected <- c("16:0", "18:1 n-7", "20:5 n-3", "14:1 n-7",
                  "transfa_16:0", "pct_20:1 n-9", "TotalTG")

    expect_identical(renamed, expected)
})

test_that("Extracting fatty acid fraction", {
    extracted <- extract_fa_fraction(fattyacids)
    expected <- c("ne", "tg", "pl", "ce", "ne", "tg", NA)
    expect_identical(extracted, expected)

    extracted <- extract_fa_fraction(fattyacids, uppercase = TRUE)
    expected <- c("ne", "tg", "pl", "ce", "ne", "tg")
    expect_identical(extracted, c(toupper(expected), NA))
})
