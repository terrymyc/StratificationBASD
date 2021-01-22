# Stratification in Bayesian Adaptive Survey Design

This repository serves as the research archive for the Master's thesis “Optimal Stratification in Bayesian Adaptive Survey Design” by Yongchao Ma. It contains all necessary files to replicate this study in [R](https://www.r-project.org). The micro data provided by Statistics Netherlands are secure use files and are only available via on-site or remote access. 

For any help with the files in this archive, please contact Yongchao Ma (y.ma1@uu.nl). For help in accessing the micro data and reproducing this study, please contact Barry Schouten (jg.schouten@cbs.nl).

| Files/Folders         | Description |
| ----                  | ----        |
| `0.Execute.R`         | Script to install dependencies and reproduce the study step-by-step |
| `1.ReadValidData.R`   | Script to read the micro data and recode key survey variables |
| `2.ModelSvyVar.R`     | Script to model key survey variables |
| `3.Stratification.R`  | Script to stratify the target population into subgroups |
| `4.DesignPara.R`      | Script to perform Bayesian analysis of survey design parameters |
| `5.Optimization.R`    | Script to perform mathematical optimization of adaptive survey designs |
| `EthicalApproval.pdf` | Proof of ethical clearance by the FETC (Utrecht University) |
| `/Functions`          | Folders containing the functions used by `0.Execute.R` |
| `/Manuscript`         | Folders containing the manuscript written in LaTeX |
