# Reading Children in the Dark

R scripts for the analysis pipeline used to generate the study tables and figures.

## Repository structure

- `run_all.R`: runs the full pipeline in sequence
- `R/00_setup.R`: package loading, directories, and shared utilities
- `R/01_helpers.R`: helper functions
- `R/02_data_prep.R`: data loading and analytic data preparation
- `R/03_table1.R`: Table 1
- `R/04_multilevel_models_table2.R`: multilevel models and Table 2
- `R/05_meta_regression_table3_fig1.R`: meta-regression, Table 3, and Figure 1
- `R/06_fig2_interaction.R`: Figure 2
- `R/07_fig3_forest.R`: Figure 3

## Data availability

The data used in this project are not included in this repository. PIRLS data are available from the [IEA PIRLS Data Repository](https://www.iea.nl/data-tools/repository/pirls), subject to the IEA’s terms and conditions of access and use.
