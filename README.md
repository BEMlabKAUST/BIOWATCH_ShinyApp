<h1>BIOWATCH</h1>

<h2>1. Introduction</h2>
BIOWATCH is a R shiny app which aims to improve the detection of species of interest in metabarcoding datasets. BIOWATCH enables the user to create custom reference databases from public databases and then investigate metabarcoding datasets against these custom databases to detect species of interest. Users are also able to visualize the results to investigate spatial and temporal patterns in the detection of species of interest. BIOWATCH offers a versatile platform that empowers users to interpret metabarcoding results with greater confidence, contributing to improved ecosystem monitoring, conservation planning, and species management in an era of rapid environmental change.
<h2>2. Installation</h2>
<h3>2.1 Via Conda</h3>

  To install BIOWATCH via conda you must first install conda. Follow this [link](https://docs.conda.io/projects/conda/en/latest/user-guide/install/) for instructions. If conda is already installed, please make sure it is sufficiently up to date prior to installation.
  
  For GBIF downloads to work registration (free) is required. Please visit their [website](https://www.gbif.org) to register.
  
  Once conda is install please download the BIOWATCH_ShinyApp repository and navigate into the folder. Once there enter the commands in the order they appear below. Please enter in user details for the NCBI email and GBIF requirements.

       conda env create -f conda/environment.yml

       conda activate BIOWATCH

       mkdir -p $CONDA_PREFIX/etc/conda/activate.d
       cat << EOF > $CONDA_PREFIX/etc/conda/activate.d/env_vars.sh
       # NCBI
       export NCBI_EMAIL="your@email.com"
       # GBIF credentials
       export GBIF_EMAIL="your.gbif@email.com"
       export GBIF_USER="your_gbif_username"
       export GBIF_PWD="your_gbif_password"
       EOF

       mkdir -p $CONDA_PREFIX/etc/conda/deactivate.d
       cat << EOF > $CONDA_PREFIX/etc/conda/deactivate.d/env_vars.sh
       unset NCBI_EMAIL
       unset GBIF_EMAIL
       unset GBIF_USER
       unset GBIF_PWD
       EOF
       
       chmod +x launch.sh

To make sure the NCBI and GBIF requirements are correctly activated please restart the environment. Once the environment has restarted the command

      ./launch.sh

can be used to launch the BIOWATCH app in the browser. 

The installation via conda has been tested on Mac and Linux operating systems. 

  <h2>3. Workflow</h2>

Example files are available for download in the relevant tabs (or are found in the data folder) for demonstration purposes. 
  
  <h3>3.1 Database Creation</h3>

Users can create custom reference databases through BIOWATCH. BIOWATCH takes a user defined species list and has the ability to download sequences from public databases [NCBI](https://doi.org/10.1093/nar/gkac1012), [BOLD](https://doi.org/10.1111/1755-0998.13741) and [SILVA](https://doi.org/10.1093/nar/gkaf1247). For NCBI user defined search criteria are required (e.g. coi[gene]). The retrieval of sequences is undertaken using the [CRABS](https://github.com/gjeunen/reference_database_creator/tree/main?tab=readme-ov-file) package.

To enable detection of the species of interest from closely related species, sequences of these closely related species will also be downloaded. These are determined in a two step process: 1) belonging to the same genus as the species of interest and 2) belonging to the same family but also in the same region geographically as the species of interest. This region is determined by the use. Note that the polygon determination on the leaflet must be made anti-clockwise.
    
As a species list may be used to create multiple databases (e.g. different genes/primers) a user defined suffix is required to differentiate between databases. 

Primer sequences are required so that the database can be specific for the primer region being studied. This 

The downloaded database will be stored in the database folder and results showing the completion of the database will be visualised in BIOWATCH to give the user a idea of the species of interest which are present/absent in the database. 

  <h3>3.2 Species of Interest Detection</h3>

Species of interst detection from a metabarcoding fasta file is undertaken using blastn against the custom database created previously. 

Users can define the percentage identity to use as a threshold. Species of interest are classified as either *Likely* or *Putative*. 

*Likely* detections are when only a single species has the top bitscore for a sequence. 

*Putative* detections are when the top bitscore for a sequence is shared amongst multiple detections and further investigation is required.

If the user included the options of adding in a ASV table and coordinate file then further visualisations are available to show the spatial distribution of the data. 

The newly run data can also be added to previous data that has already been run and stored (in the historic.csv file found in the data folder). This can then be visualised in the Long Term Data tab. 

  <h3>3.3 Long Term Data</h3>

This tab users the ability to investigate spatial and temporal patterns in the dataset. Spatial patterns can be visualised on the map while clicking on a point will show detections of the species of interest over time for that site. 
