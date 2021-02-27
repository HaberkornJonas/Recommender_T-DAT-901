# T-DAT-901

## How to start

First, download the dataset `KaDo.csv` and put it inside the `01_data` directory.

It's recommanded to run the scripts using RStudio in the following order:
1. `install_packages.R`
2. `stats.R`
3. `segmentation.R`
4. `recommender.R`

You can also use the provided Dockerfile but this will use more RAM ans memory on your computer than using RStudio.  
If you still want to use Docker, run `docker-compose build` for the first time and then `docker-compose up` for each new recommendation.

## How to add a new library

1. If the library needs dependencies outside R, then add them to the `Dockerfile`
2. Add the library install command into `02_code/install_packages.R`
3. Run `docker-compose up --build`

