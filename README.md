# Predicting UTI in the ED using machine learning

This repo contains the analysis code accompanying the manuscript [Can the application of machine learning to electronic health records guide antibiotic prescribing decisions for suspected urinary tract infection in the Emergency Department?](https://www.medrxiv.org/content/10.1101/2022.09.23.22268727v1)


## Repo structure

```
.
├── analysis/          # code used to fit prediction models
├── data/              # preprocessed data
├── results/           # analysis results, including tables, plots, etc.
├── src/               # function definitions, etc.
├── .gitignore
├── README.md
└── requirements.txt
```

## Note on reproducibility

This code was created for sensitive health data in a restrictive data safe haven environment. As a result, this repo contains several pecularities and does not run out-of-the-box. It's aim is to provide additional detail on the methods used to obtain the published models and to allow readers to inspect and re-use parts of the code.

Notably, by request from Queen Elizabeth Hospital Birmingham (QEHB) no code related to the cleaning of raw patient data was included in this repo for concerns that the structure of the exported data may allow to infer QEHB's internal data structure. Consequently, only code related to the main analysis is provided. File paths have been amended to account for this omission but the resulting code was never tested outside the safe haven and may therefore contain errors.

## Note on packages

Unfortunately, the package manager [packrat](https://cran.r-project.org/web/packages/packrat/index.html) did not work properly in the safe haven environment and no package snapshot could be provided with this repo. Instead, a list of package versions used in this repo was provided manually in `requirements.txt`.