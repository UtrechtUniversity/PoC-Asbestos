# Probability of Causation: Lung cancer due to occupational exposure to asbestos

This repository contains the source code for the manuscript entitled *Probability of Causation: Lung cancer due to occupational exposure to asbestos*, as well as the associated R code and documentation for the statistical analyses and results. The objective of this study was to investigate the process of deciding on compensation claims by lung cancer patients exposed occupationally to asbestos by calculating the probability of causation (PoC) threshold at which lung cancer is *more likely than not* due to asbestos. You may read more details in the associated [manuscript](docs/manuscript).

## How to use 

The suggested use of this repository starts with making sure that R and RStudio are installed in your computer:
1. Install [R and RStudio](https://posit.co/download/rstudio-desktop/) on your computer if you haven't done so. (Note that these analyses were conducted under R version 4.4.0 and RStudio 2024.04.0).
2. [Clone this repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository). If you do not know how to do this, [you can follow these instructions](https://docs.github.com/en/desktop/overview/getting-started-with-github-desktop). Alternatively, you can [download the ZIP file](https://github.com/UtrechtUniversity/lexces-silicosis-predict/archive/refs/heads/main.zip), unpack it, and place it in a folder in your computer.
3. You should now have all these files in your computer with an identical folder structure (described in the following section).
4. In the main directory, open the file named ***PoC-Asbestos.Rproj*** in RStudio.
5. You can navigate through the folders on the right-bottom panel of R Studio. Open the **R** folder. You should now see a series of files ending with ***.qmd***.
6. Open one of the .qmd files. You can run every chunk of code sequentially to reproduce the analyses. Make sure to respect the order and if something fails, I recommend that you start running al chunks of code from the beginning. If you don't know how to run a chunk of code, you can [imitate what this person is doing](https://youtu.be/RPF6gGyeJmg?feature=shared&t=30). If you get a message saying "Access denied", change from *Visual* to *Source* mode which can be done with the Ctrl+Shift+F4 command.
7. Please note that scripts are meant to be sourced into the flow of analyses in the main .qmd files. You may encounter problems if you attempt to run the scripts independently. 
-   [Misclassification error of silicosis diagnostic prediction rule](R/Silicosis_diagnostic_rule.qmd). Link to the report: [PDF](docs/reports/Silicosis_diagnostic_rule.pdf)

If you are not able to follow the prior steps, you may also consider reviewing the [PDF report](docs/reports) documenting the analyses. 

Currently, there is only one relevant .qmd file linked to the R scripts and analyses: 

-   [PoC_Asbestos](R/PoC_Asbestos.qmd). Link to the report: [PDF](docs/reports/PoC_Asbestos.pdf)

Although I have made significant efforts to ensure reproducibility of this project, I encourage you to [contact me](mailto:j.mancillagalindo@uu.nl) or post a request in this repository in case you encounter any issues.   

## Project Structure

The project structure distinguishes three kinds of folders:
- read-only (RO): not edited by either code or researcher
- human-writeable (HW): edited by the researcher only.
- project-generated (PG): folders generated when running the code; these folders can be deleted or emptied and will be completely reconstituted as the project is run.

```         
.
├── .gitignore
├── CITATION.cff
├── LICENSE
├── README.md
├── PoC-Asbestos.Rproj
├── data                  <- All project data files, hidden by default. 
│   ├── processed         <- The final, canonical data sets for modeling. (PG)
│   ├── raw               <- The original, immutable data. (RO)
│   └── temp              <- Intermediate data that has been transformed. (PG)
├── docs                  <- Documentation for users (HW)
│   ├── manuscript        <- Manuscript source, docx, html. (HW)
│   ├── presentations     <- Presentations, pptx, pdf. (HW)
│   └── reports           <- Project reports, pdf. (HW)
├── results
│   ├── output_figures    <- Figures for the manuscript or reports (PG)
│   └── output_tables     <- Output tables for the manuscript (PG)
└── R                     <- Source code for this project (HW)
    ├── scripts           <- Scripts sourced in main R markdown documents (PG)
    └── sessions          <- Text files with information of R sessions (PG)

```

## License

This project is licensed under the terms of the [MIT License](/LICENSE) by Utrecht University and Lexces. 

For all uses related to Lexces, please refer to their [copyright notice](https://www.lexces.nl/en/copyright).

This project structure repository is adapted from the [Utrecht University simple R project template](https://github.com/UtrechtUniversity/simple-r-project), which builds upon the [Good Enough Project](https://github.com/bvreede/good-enough-project) Cookiecutter template by Barbara Vreede (2019).
