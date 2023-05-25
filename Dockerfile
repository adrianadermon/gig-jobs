FROM rocker/r-ver:4.3.0
RUN install2.r data.table ggplot2 scales fixest modelsummary car kableExtra
COPY data.csv analysis.R /
RUN mkdir /results/
CMD Rscript analysis.R