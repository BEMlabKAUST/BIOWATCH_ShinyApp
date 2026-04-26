<h1>BIOWATCH</h1>

<h2>1. Introduction</h2>
BIOWATCH is a R shiny app which aims to improve the detection of species of interest in metabarcoding datasets. BIOWATCH enables the user to create custom reference databases from public databases and then investigate metabarcoding datasets against these custom databases to detect species of interest. Users are also able to visualize the results to investigate spatial and temporal patterns in the detection of species of interest. BIOWATCH offers a versatile platform that empowers users to interpret metabarcoding results with greater confidence, contributing to improved ecosystem monitoring, conservation planning, and species management in an era of rapid environmental change.
<h2>2. Installation</h2>
<h3>2.1 Via Conda</h3>

  To install BIOWATCH via conda you must first install conda. Follow this [link](https://docs.conda.io/projects/conda/en/latest/user-guide/install/) for instructions. If conda is already installed, please make sure it is sufficiently up to date prior to installation.
  
  For GBIF downloads to work registration (free) is required. Please visit their [website](https://www.gbif.org) to register.
  
  Once conda is install please download the BIOWATCH_ShinyApp repository and navigate into the folder. Once there enter the commands in the order they appear below. Please enter in user details for the NCBI email and GBIF requirements.
       
       git clone https://github.com/BEMlabKAUST/BIOWATCH_ShinyApp/
       
       cd BIOWATCH_ShinyApp

       conda env create -f conda/environment.yml

       conda activate BIOWATCH
       
       chmod +x launch.sh

To make sure the NCBI and GBIF requirements are correctly activated please restart the environment. Once the environment has restarted the command

      ./launch.sh

can be used to launch the BIOWATCH app in the browser. 

The installation via conda has been tested on Mac and Linux operating systems. 

<h3>via Docker</h3>

Docker is a container application that allows the deployment of software applications that are isolated from your computer and run through a Docker Engine. The isolation of the container from your computer means that Docker containers can be run on most operating systems including Mac, Windows and Linux.

Here is a introduction to the use of [Docker](https://docs.docker.com/get-started/) and a free account is required to use teh Docker software. To run BIOWATCH with Docker the user will need to install and run Docker Desktop. Here are instructions for installation using [Mac](https://docs.docker.com/desktop/setup/install/mac-install/), [Windows](https://docs.docker.com/desktop/setup/install/windows-install/) and [Linux](https://docs.docker.com/desktop/setup/install/linux/). Once Docker Desktop is running then the user can obtain the Docker image for BIOWATCH by running the command:

    docker pull jkpearmanbioinf/biowatch:latest

The easiest way to run the docker image is to navigate into the downloaded git folder which contains a file called docker-compose.yml. If the user runs the below command in the terminal then BIOWATCH will start up.

    docker compose up

The user can then view BIOWATCH in a web browser by typing:

    http://localhost:3838

For further information on commands for the use of docker please view the Docker [documents](https://docs.docker.com/get-started/).

<h2>3. Workflow and examples</h2>

Example files are available for download in the relevant tabs (or are found in the data folder) for demonstration purposes. 
  
  <h3>3.1 Database Creation</h3>
  <h4>3.1.1 Database Construction</h4>

Users can create custom reference databases through BIOWATCH. BIOWATCH takes a user defined species list and has the ability to download sequences from public databases [NCBI](https://doi.org/10.1093/nar/gkac1012), [BOLD](https://doi.org/10.1111/1755-0998.13741) and [SILVA](https://doi.org/10.1093/nar/gkaf1247). For NCBI user defined search criteria are required (e.g. coi[gene]). The retrieval of sequences is undertaken using the [CRABS](https://github.com/gjeunen/reference_database_creator/tree/main?tab=readme-ov-file) package.

There are several required inputs for a database to be constructed (Figure 1). 

<img width="2966" height="1620" alt="Figure1" src="https://github.com/user-attachments/assets/e67a3ecf-9776-47b7-9ead-d31b30134de1" />

<em>Figure 1: The database construction input page</em><br></br>

Firstly, a input species list is required. This is a text file containing a list of species of interest. An example file can be found in the data folder (example_specieslist.txt).

Secondly a user defined suffix is required to differentiate between databases. As a species list can be used to create multiple databases  (e.g. different genes/primers) this suffix combined with the basename of the species list will be used in combination to create the database name. 

If the user has sequences from the species of interest (or related species) that are not publicly available then they can be added with the upload custom fasta file option. An example fasta file (example.custom.fasta) is included in the data folder. 

The user has an option to download sequences from the NCBI, BOLD or SILVA databases depending on the gene of interest. For example, for the mitochondrial cytochrome oxidase I gene the user may decide to download sequences from the NCBI and BOLD databases while for the 18S rRNA gene the NCBI and SILVA database would be more appropriate. If the NCBI database is selected then a user defined search criteria are required (e.g. coi[gene] or 18S[All Fields]). Multiple choices can be added (e.g. coi[gene], co1[gene], mtcoi[gene]) to increase the sensitivity of the search or for more complicated search requirements the user can put in a custom query. 

To make the database specific to the primers used and not just general to the gene of interest primer sequences are required from the user (see figure 1 for example input). This will enable the database to be trimmed to the correct primer region. This will enable the database completion results to be specific for the primers being used in metabarcoding studies that the user wants to investigate. 

To enable detection of the species of interest from closely related species, sequences of these closely related species will also be downloaded. The closely related species to be downloaded is determined in a two-step process: 1) species belonging to the same genus as the species of interest. These species can be present from any geographic location; and 2) species that are in the same family as the species of interest but have been observed in the region of interest. The region of interest is determined by the user either by plotting a polygon on the map (see Figure 1 for an example) or by directly inputting WKT coordinates for a polygon (e.g. POLYGON ((33.925781 30.297018, 30.849609 30.221102, 36.738281 17.978733, 42.539063 11.867351, 46.054688 9.362353000000001, 53.613281 12.297068, 64.6875 21.616579, 66.269531 25.403585, 65.302734 29.993002, 50.976563 32.398516, 33.925781 30.297018)).) The geographic coordinates and species list combination can be cached for faster downloads subsequently. 

<h4>3.1.2	Database Completion</h4>

The presence/absence of species of interest in the database is depicted in a heatmap with a downloadable table (Figure 2).

<img width="2992" height="1254" alt="Figure2" src="https://github.com/user-attachments/assets/699d8e7e-9eab-429e-bd62-82e622645e43" />
<em>Figure 2: Example of the database completion tab showing the presence/absence heatmap</em>

<h4>3.1.3	Species Differentiation</h4>

For those species of interest detected in the database a blast comparison is undertaken against the other sequences in the database. Sequences with a 100% similarity are noted and displayed in a table (Figure 3). The number of sequences found for each species of interest is listed (SoI_Sequences) and then the number of different species that have a 100% hit (Num_Matching_Species) and what species they are (Matchin_Species). Species of interest with matches to other species with 100% similarity indicate the primer used for the database creation may not differentiate between these species.  

<img width="1507" height="701" alt="Figure3" src="https://github.com/user-attachments/assets/b892d24a-5cd2-4b5e-887d-fe7f7764dd99" />
<em>Figure 3: Example of the species differentiation tab depicting the species of interest in the database and how many other species have 100% similarity hits against those species</em>

<h4>3.1.4	Database Comparison</h4>

This tab enables comparison of previously downloaded databases from the same species list and outputs the comparison as a heatmap with a downloadable table (Figure 4). This heatmap shows the presence/absence of each species in the databases and enables the users to assess the suitability of different primers/genes to detect the species of interest. 

<img width="2770" height="1328" alt="Figure4" src="https://github.com/user-attachments/assets/dbe893e1-6518-43e3-bb46-1d555739f859" />

<em>Figure 4: Example of the Database Comparison tab show the presence/absence heatmap across different databases</em>

<h3>Species Detection</h3>
<h4>3.2.1 Sequence Processing</h4>

The input for the BLAST analysis is a fasta file of ASVs. The processing of sequencing reads into a fasta file is not included in the shiny app. The robust processing of the sequence data is vital and the decisions made during this process can affect the results. Thus, the ability to process sequence data, either through the ability to run bioinformatic pipelines or outsourcing to third party sequencing providers, is required prior to running the app. We have however provided an example Rmd file (example_sequence_processing.Rmd) for the processing of sequences using [DADA2](https://www.nature.com/articles/nmeth.3869) within R. 

<h4 >3.2.2 BLAST input</h4>

The blast input tab allows the user to input a fasta file containing sequences for investigation (Figure 5). The user can then select the appropriate database for comparison against (made via the database construction steps above). The user also has the option to include a ASV table and a coordinates file to allow more in depth analysis of the dataset. 

<img width="2984" height="1602" alt="Figure5" src="https://github.com/user-attachments/assets/d568803b-44af-4080-b219-354174cf1157" />
<em>Figure 5: Example of the file inputs for the BlAST input tab.</em><br></br>

The user can define the maximum target sequences for the blast query.

<h4>3.2.3 Species Detection</h4>

Prior to the results being shown a pop-up disclaimer appears for the user to accept acknowledging the limitations of using the app. 

The Species detection tab outputs species cards that show the ASVs that were detected for each species of interest and the similarity of those ASVs to the species of interest. Species of interest are classified as either Likely or Putative (Figure 6).

<img width="2996" height="1396" alt="Figure6" src="https://github.com/user-attachments/assets/aae3235c-8973-4419-9fc0-3605cbdec36f" />
<em>Figure 6: Example for the species detection showing the species cards with the ASVs assigned to the species and the similarity</em>


Likely detections are when only a single species has the top bitscore for a sequence.

Putative detections are when the top bitscore for a sequence is shared amongst multiple detections and further investigation is required.

The user has the ability to alter the default thresholds for both percentage identity and sequence coverage and the species cards will update dependent on changes to these settings. 

The full blast results and the filtered blast results (based on the percentage identity and coverage thresholds) can be downloaded for further investigation of the hits.

If the user included an ASV table and coordinate file then the subsequent tabs will provide further information on the detection of the species of interest.

<h4>Detection in Controls</h4>

Detection of species of interest in negative controls may indicate issues with the processing of samples and should elicit investigation of the methodological procedures. BIOWATCH shows a presence/absence heatmap for each ASV assigned to a species of interest to show whether the ASV was detected in a control or not as shown in Figure 7. 

<img width="1446" height="930" alt="Figure7" src="https://github.com/user-attachments/assets/53d4efb3-cdda-44c0-8e8e-112653fc10da" />
<em>Figure 7: Example for the detection of species of interest in the controls</em>


<h4>Detection in Replicates</h4>

Confidence in the detection of a species is not necessarily down to just the similarity of a taxonomic assignment. The frequency and abundance of detections across replicates can also give an indication of the reliability of a detection. The Replication tab of BIOWATCH shows the frequency of detection in replicates for each ASV using a colour coded heatmap (Figure 8). The user can define the minimum abundance threshold for a detection with the default being any level of detection. Higher confidence of detection would be given to those ASVs that were found in a high proportion of replicates although it should be noted that low levels of detection should not be ignored as they could be from very rare species or new arrivals at low abundance. 

<img width="2056" height="1178" alt="Figure8" src="https://github.com/user-attachments/assets/82432ffe-e6db-4617-b223-2348b6c06f63" />
<em>Figure 8: Example for the frequency of detectin of the species of interest ASVs in replicates</em>


<h4>Diversity</h4>

The diversity tab shows the number of ASVs attributed to species of interest at a site or region (Figure 9). This gives the user some idea of the prevalence of the species of interest at the sites/region. 

<img width="2986" height="1054" alt="Figure9" src="https://github.com/user-attachments/assets/c325b593-7acf-4f34-a141-cb56de360ee0" />
<em>Figure 9: Example of the diversity map showing the number of ASVs (scaled bubbles) belonging to species of interest at each site.</em>


<h4>Distribution</h4>

In the distribution tab, users can select a species of interest and see the distribution and abundance in a dataset (Figure 10). The bubbles are scaled by the proportion of the species in the dataset. 

<img width="2984" height="1354" alt="Figure10" src="https://github.com/user-attachments/assets/7e9270c8-84c0-4797-b337-d35a900a5213" />
<em>Figure 10: Example of the distribution of specific species of interest across the dataset.</em>


<h4>Update Long-term data</h4>

The user has the ability to add data obtained from the BLAST analysis to the Long-term data (Figure 11). The user can select which species they want to add (in the below case <em>Amphibalanus amphitrite</em> and <em>Pennaria disticha</em>. They can then fill in the year, gene and sampling methodology used in the dataset. By clicking Generate Editable Table this generates a table where the user can alter the information contained in the table before clicking the Add to Long Term Data button which adds the data to the Long-term data set. If there is no historic.csv file in the data folder this new dataset will be used to form it. 

<img width="2992" height="1572" alt="Figure11" src="https://github.com/user-attachments/assets/c22447b7-af3f-4307-8669-067fdbe6bdc8" />
<em>Figure 11: Example tab for the update of the long-term data showing the editable dataframe.</em>


<h3>3.3 Long-term data</h3>

This tab users the ability to investigate spatial and temporal patterns in the dataset. Spatial patterns can be visualised on the map while clicking on a point will show detections of the species of interest over time for that site. The information is contained in the historic.csv file in the data folder. Selecting a site will show the years in which the selected species was detected at the site.

<img width="2978" height="1532" alt="Figure12" src="https://github.com/user-attachments/assets/21835780-d5e0-4f26-8eaf-9e5428c6c5db" />
<em>Figure 12: Example of the Long Term Data tab showing the distribution and detection of Amphibalanus amphitrite using the COI gene in Autonomous Reef Monitoring Structures (ARMS)</em>


<h2>Benchmarking</h2>

The download of the database can take a while depending on how many species are present in the list. To benchmark the time that the database download took we undertook downloads of two datasets. Firstly the example database which consisted of 15 species and secondly a New Zealand non indigenous species list containing 453 species. These downloads were undertaken on A MacBook Pro with a M1 Pro chip and 32 GB of memory. We investigated two genes (the 18S rRNA gene and the mitochondrial cytochrome oxidase I gene using two public datasets for both to give an example of the most time consuming procedure.

The download of the example data set using the NCBI and SILVA dataset with the NCBI query 18S[All Fields] with no cached gbif data took 22 mins and 35 seconds. For the NZ NIS dataset it took 1 hour 03 mins and 42 seconds using the same commands. 

Against the NCBI and BOLD databases using the NCBI query coi[gene], co1[gene], mtcoi[gene] with no cached gbif data took 34 mins and 29 seconds. For the NZ NIS dataset it took 3 hours 04 mins and 06 seconds using the same commands. 











