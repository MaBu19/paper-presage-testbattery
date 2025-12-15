# PRESAGE cross-domain test battery -- analysis code for control group

## Manuscript: "A cross-domain test battery for comprehensive hearing loss characterisation using functional, physiological, and vestibular measures"
- Code version: 1.0 (December, 2025)

## Authors: 
- Shiran Koifman (shiran.koifman@uol.de)
- Mareike Buhl (mareike.buhl@pasteur.fr)

## Code: 
- Loads a harmonised dataset (a single table integrating French and German data).
- Performs pre-processing used in the manuscript (e.g., better-ear PTA definition, missing data checks, variables calculations, & harmonisation of data across centres).
- Runs statistical models and produces manuscript figures/tables. The code is divided into subparts for convenient browsing using the outline option in R. Figures and table numbers are specified in the relevant subparts titles.

## Data availability:
- The analysis dataset used for this manuscript/code is not included in this release.
- The current data includes only participants from the control group (group=1).
- Complete data are planned to be published in an open repository together with the hearing-impaired data at a later stage of the project. 
 
## Inputs: 
- .CSV or .XLSX files located in ./input/
- R functions located in ./functions/

## Outputs: 
- Figures & tables written to ./output/

## Reproducibility: 
- Tested with R version 4.3.2.
