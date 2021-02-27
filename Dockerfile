# Base image https://hub.docker.com/u/rocker/
FROM rocker/r-base:latest

## create directories
RUN mkdir -p /01_data
RUN mkdir -p /02_code
RUN mkdir -p /03_output

COPY /01_data /01_data
COPY /02_code /02_code
COPY /03_output /03_output

## install R-packages
RUN Rscript /02_code/install_packages.R
RUN Rscript /02_code/stats.R
RUN Rscript /02_code/segmentation.R

# start our script
CMD Rscript /02_code/recommender.R