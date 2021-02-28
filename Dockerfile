# Base image https://hub.docker.com/u/rocker/
FROM rocker/r-base:latest

## Create directories
RUN mkdir -p /01_data
RUN mkdir -p /02_code
RUN mkdir -p /03_generated
RUN mkdir -p /04_recommender

COPY /01_data /01_data
COPY /02_code /02_code
COPY /03_generated /03_generated
COPY /04_recommender /04_recommender

# Install prerequisites
RUN apt-get update
RUN apt-get install -y curl r-cran-car

## Run R scripts
RUN Rscript /02_code/install_packages.R
RUN Rscript /02_code/stats.R
RUN Rscript /02_code/segmentation.R

# Start our script
CMD Rscript /02_code/recommender.R
