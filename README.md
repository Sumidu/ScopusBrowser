
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ScopusBrowser

<!-- badges: start -->

<!-- badges: end -->

The goal of ScopusBrowser is to make it easier to browse Scopus exports.
This can be used when writing systematic reviews for example.

## Current usage

Clone the repository to your computer and

1.  Put tags to use in the `categories.csv`
2.  Put the exported `csv`-file from Scopus in the `data_exports_scopus`
    folder
3.  *\[optional\]* Run the `setup_db.R` script in the `R` folder.
4.  *\[optional\]* Use the `filesplitter.R` script to split the work
    between colleagues.
5.  Run the `app.r` in the `ScopusScanner` directory.
6.  Collect all finished `rds`-files in the `import` import directory.
7.  Use `GenerateReport.Rmd` to generate a report on the selected data.
