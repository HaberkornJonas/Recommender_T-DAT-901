# T-DAT-901

## How to start

First, download the dataset `KaDo.csv` and put it inside the `01_data` directory.

It's recommanded to run the scripts using RStudio in the following order:
1. `install_packages.R`
2. `stats.R`
3. `segmentation.R`
4. `recommender.R`

An other solution would be to use the provided Dockerfile.  
> **/!\\** This would use **more RAM and memory on your computer** than using RStudio (it also needs way **more time to install the libraries and build the stats than RStudio**, going for a total of arround **30 minutes**).   
> 
>**/!\\** You will also **not be able to see the generated graphs** as the docker image generates them during the build and mounting a volume on the directory will overwrite the generated data.

If you still want to use Docker, run `docker-compose build` for the first time (while installing libraries, the logs are displayed red as errors, don't worry). Then start it with `docker-compose up`.  
  
If you cannot enter the client ID in the console (this is an issue with interactive process and docker-compose on Windows, has not be tested on Linux) then, while still having *docker-compose up* running, open a second terminal and run `docker exec -it <container name> /bin/bash` then start the recommender with `Rscript /02_code/recommender.R`

## How to add a new library

1. If the library needs dependencies outside R, then add them to the `Dockerfile`
2. Add the library install command into `02_code/install_packages.R`
3. Run `docker-compose build` to update the docker image

