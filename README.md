# Stratification in Bayesian Adaptive Survey Designs

This repository serves as the research archive for the Master's thesis “Optimal Stratification in Bayesian Adaptive Survey Designs” submitted on 10/05/2021 by Yongchao Ma. It contains computer code files to reproduce the case study on the [Dutch Health Survey](https://www.cbs.nl/en-gb/onze-diensten/methods/surveys/korte-onderzoeksbeschrijvingen/health-survey-as-of-2014) in [R](https://www.r-project.org).

## Abstract

In an increasing number of survey designs, adaptive data collection strategies for different members of the population are adopted to balance the data quality and cost.
Stratifying the target population into subgroups in an effective manner plays a decisive role in identifying the optimal adaptive survey design.
This paper presents a stratification method on the basis of which the optimal adaptive survey designs can be constructed under the Bayesian analysis to minimize nonresponse bias.
The utility of this method compared to two other response- and cost-oriented stratification methods is assessed through a case study based on the Dutch Health Survey.
The optimal adaptive survey designs based on the proposed method outperform in minimizing nonresponse bias, which indicates that the underlying stratification is the optimal stratification.


## Content
| Files/Folders         | Description |
| ----                  | ----        |
| `0.Execute.R`         | **Instruction to reproduce the study step-by-step** |
| `1.ReadRecodeRawData.R`   | Script to read the raw micro data and recode into analysis data |
| `2.BayesModelSvyVar.R`     | Script to predict key survey variables |
| `3.1 StratificationResponseY.R`  | Script to perform ResponseY stratification method |
| `3.2 StratificationResponseX.R`  | Script to perform ResponseX stratification method |
| `3.3 StratificationCostX.R`  | Script to perform CostX stratification method |
| `4.DesignPara.R`      | Script to perform Bayesian analysis of survey design parameters |
| `5.Optimization.R`    | Script to perform mathematical optimization of adaptive survey designs |
| `6.OptimalStratification.R`    | Script to determine optimal stratification |
| `EthicalApproval.pdf` | Proof of ethical clearance by the FETC (Utrecht University) |
| `Functions`          | Folders containing the functions used by `0.Execute.R` |
| `Manuscript`         | Folders containing the manuscript written in LaTeX |
| `Tables`         | Folders containing the generated Table data in the manuscript |
| `Figures`         | Folders containing the generated Figures in the manuscript |
| `Output`         | Folders containing the aggregated data used by `6.OptimalStratification.R` |


## Privacy

The micro data collected between 04/2017 and 03/2018 by Statistics Netherlands are secure use files and are only available via on-site or remote access.

The study has been approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences (FETC) of Utrecht University, filed as an amendment under study 20-0104.

## Permission and Access

This research archive is permanently stored on the CBS server with the internal path `//cbsp.nl/Productie/Primair/PDCA-SEC/Beheer/StratificationBASD`. This research archive is also privately stored on [GitHub](https://github.com/terrymyc/StratificationBASD) for a minimum of 10 years and can be accessed by the author Yongchao Ma and the coordinator of the Master's programme, Rens van de Schoot.

## Contact
For any help with the files in this archive, please contact [Yongchao Ma](https://yongchaoma.com). For help in accessing the micro data, please contact Barry Schouten (jg.schouten@cbs.nl) or [CBS micro data service](https://www.cbs.nl/en-gb/onze-diensten/customised-services-microdata/microdata-conducting-your-own-research/contact).
