# Stratification in Bayesian Adaptive Survey Design

This repository serves as the research archive for the Master's thesis “Optimal Stratification in Bayesian Adaptive Survey Designs” submitted on 10/05/2021 by Yongchao Ma. It contains computer code files to reproduce the case study on the [Dutch Health Survey](https://www.cbs.nl/en-gb/onze-diensten/methods/surveys/korte-onderzoeksbeschrijvingen/health-survey-as-of-2014) in [R](https://www.r-project.org).
The published version will appear in *Sociological Methods & Research*.

## Content

| Files/Folders         | Description |
| ----                  | ----        |
| `0.Execute.R`         | **Instruction to reproduce the study step-by-step** |
| `1.ReadRecodeRawData.R`   | Script to read the raw micro data and recode into analysis data |
| `2.BayesModelSvyVar.R`     | Script to predict key survey variables |
| `3.1 StratificationResponseY.R`  | Script to perform ResponseY stratification method |
| `3.2 StratificationVisitsY.R`  | Script to perform VisitY stratification method |
| `3.3 StratificationResponseX.R`  | Script to perform ResponseX stratification method |
| `3.4 StratificationVisitsX.R`  | Script to perform VisitX stratification method |
| `3.5 StratificationCostX.R`  | Script to perform CostX stratification method |
| `4.DesignPara.R`      | Script to perform Bayesian analysis of survey design parameters |
| `5.Optimization.R`    | Script to perform mathematical optimization of adaptive survey designs |
| `6.Sensitivity.R`    | Script to perform sensitivity analysis |
| `Tables.R`         | Script to reproduce Tables |
| `Figures.R`         | Script to reproduce Figures |
| `Functions`          | Folders containing the functions used by `0.Execute.R` |

## Privacy

The micro data collected between 04/2017 and 03/2018 by Statistics Netherlands (CBS) are secure use files and are only available via on-site access.
This research archive is also permanently stored on the CBS server with the internal path `//cbsp.nl/Productie/Primair/PDCA-SEC/Beheer/StratificationBASD`.

## Contact

For any help with the files in this archive, please contact [Yongchao Ma](mailto:ytma@umich.edu). For help in accessing the micro data, please contact [CBS micro data service](https://www.cbs.nl/en-gb/onze-diensten/customised-services-microdata/microdata-conducting-your-own-research/contact).
