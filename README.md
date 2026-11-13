# Brexit tuition shock replication

This package reproduces every numbered table and figure in the manuscript in manuscript order. The master file is `replication.Rmd`. It sources the numbered R files in `code` and writes one output-only HTML document, machine-readable CSV copies of every table, and separate PNG files for every figure and figure panel.

Run the package from its root directory with

```r
source("run_replication.R")
```

The default run uses the frozen audited World Bank workbook in `data/reference`. This prevents later revisions to historical World Development Indicators from moving the reproduced estimates again. If that workbook is absent, the code downloads the required indicators with `WDI::WDI`. Set `REFRESH_WDI=true` only when an intentional live World Bank refresh is required.

The data flow is sequential. `01_construct_control_group.R` reads the first UCAS workbook and writes `control_group_output.xlsx`. `02_prepare_panels.R` reads that workbook and the 2016 through 2025 UCAS workbook. `03_prepare_wdi_panel.R` adds the frozen 2016 through 2024 controls. `04_estimate_models.R` writes `betas_saturated_model.xlsx`. `06_loan_calibrations.R` reads those coefficients together with the 2021 and 2024 wage workbooks. `08_validate_results.R` compares the reproduced estimates with independently generated benchmark files and stops the run if they differ beyond numerical tolerance. `09_estimate_figure_models.R` estimates the event-study and robustness models. `10_prepare_figure_data.R` writes `figure_data.xlsx`. `11_build_figures.R` reads that workbook and produces Figures 1 through 10 and Figures A1 through A2.

The required R packages are listed in `DEPENDENCIES.csv`. The audited synthetic difference-in-differences implementation used `synthdid` version 0.0.9.

The headline sample contains 520 observations from 52 countries over 2016 through 2025. The World Bank models contain 468 observations over 2016 through 2024. The control group is constructed only from the separate first UCAS workbook over the pre-treatment years 2014 through 2020.

Generated spreadsheets are written to `data/derived`. Reproduced tables, figures, and audit files are written to `output`. The combined document is `output/replication_tables_and_figures.html`.

`OUTPUT_ORDER.csv` records the exact manuscript sequence. `TABLE_MAP.csv` and `FIGURE_MAP.csv` map each output to its generating script, input object, and saved file.

`expected_table_3_current_wdi.csv` records the small reconciliation between the values printed in manuscript Table 3 and the later frozen World Bank vintage used by the supplied controlled-panel workbook and Appendix Table A1. The R pipeline always prints model estimates generated from data. It does not hardcode the older displayed coefficients.
