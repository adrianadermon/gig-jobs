This is the replication package for Adermon, Adrian, and Lena Hensvik (2022), "Gig-Jobs: Stepping Stones or Dead Ends?", [Labour Economics 76: 102171](https://doi.org/10.1016/j.labeco.2022.102171).

The analysis was performed using [R](https://www.r-project.org/) version 4.3.0. To reproduce the full set of results, take the following steps:

1. Install [Docker](https://www.docker.com/).
2. Copy the contents of this repository to a local folder on your computer (for example, by running the command `git clone https://github.com/adrianadermon/gig-jobs` in a terminal in your local folder).
3. Open a terminal (on Windows, you need to use PowerShell) and navigate to the folder where you saved the replication files.
4. Run the command `docker build -t gigjobs .` in the terminal. This will build a Docker image containing the required R version and packages.
5. Run the command `docker run -v ${pwd}/results:/results/ gigjobs` in the terminal. This will run a Docker container which performs the full analysis and copies the results to the subfolder `results` in your local folder.

The tables will be exported as LaTeX files, and the figures as PDF files.

If you prefer to perform the replication manually, first create the subfolder `results` in your local folder. Then run the R script `analysis.R` in the same folder as the data file `data.csv`. 

The table below lists which R packages were used in the analysis. The Docker image will automatically install the correct versions. If you perform the replication manually, you might have to make sure you have the correct versions of the packages (using, e.g., the [posit Package Manager](https://packagemanager.posit.co/).

| Package        | Version |
| -------------- | ------- |                     
| `data.table`   | 1.14.8  |                     
| `ggplot2`      | 3.4.2   | 
| `scales`       | 1.2.1   |
| `fixest`       | 0.11.1  |
| `modelsummary` | 1.4.1   |
| `car`          | 3.1-2   |
| `kableExtra`   | 1.3.4   |
