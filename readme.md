# T-DAT-901

## How to start

1. Download the dataset and put it inside the `01_data` directory
2. Run `docker-compose up`
3. Edit the `02_code/script.R` file to change the data manipulation and analyse

## How to add a new library

1. If the library needs dependencies outside R, then add them to the `Dockerfile`
2. Add the library install command into `02_code/install_packages.R`
3. Run `docker-compose up --build`

