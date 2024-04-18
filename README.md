# Undergraduate Thesis: Assessing the Impact of Public Transit-Rideshare Partnership 

This project analyzes the effect of public transit-ridershare partnerships on transit ridership. It utilizes data from the National Transit Databse (NTD) to determine agency ridership numbers and the American Community Survey (ACS) to account for the demographics of the urban areas that the agencies are based in. I find a nearly 5% increase in ridership as a result of these partnerships.

To replicate the project, an internet connection and access to R are required. Running the code files in order will generate the cleaned data, as well as the regressions, tables, and figures used in the paper. Please read all of the steps for replicating results below before beginning.

# Replicating results
### Before running any code (IMPORTANT)
1. Before running any code, you will also need a Census API key, which can be obtained at [this link](https://api.census.gov/data/key_signup.html).
2. Check that the file `transit_data.xlsx` is in the folder `data/raw`. If not, it can be obtained from the NTD at [this link](https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release). Save this file in `data/raw` under the name `transit_data.xlsx`.
### To replicate results
1. Simply run `code/run_script.r`.
2. `run_script.r` will install all necessary packages, as well as clean the NTD data previously saved and download/clean the ACS dataset (you will be prompted for your API key). This will result in a merged dataset that will be used in all analysis and figures.
3. `run_script.r` will then run the analysis and figure scripts, saving the output in the `tables` and `figures` folders, respectively. Output will be saved as a combination of .csv, .txt, .tex, .pdf, .png. Sorry about that.

### Critical to replication:
- code folder: build for scripts to build the data and analysis for scripts to analyze the data
- data folder: raw for raw data, temp for various saves throughout building, clean for cleaned data, and final for final data
  - raw: raw data
  - clean: cleaned data
- figures folder: figures produced in analysis
- tables folder: tables produced in analysis
### Other folders
- presentations folder: for any presentations given on the project
- writing folder: contains thesis pdf, thesis .tex file, references.bib.
- stop level folder: contains some stop level data obtained via FOIA request; if interested in using this data for analysis, raise an issue and I will happily send it along

# Relevant files (in order)
- `code/run_script.R`: runs all relevant scripts in correct order
- `code/housekeeping.R`: downloads all relevant packages
- `code/upt_data.R`: downloads the unlinked passenger trips for each agency for each month from the NTD .xlsx (`transit_data.xlsx`); saves this dataset as `data/clean/upt_data.csv`
- `code/vrh_data.R`: downloads the vehicle revenue hours for each agency for each month from the NTD .xlsx (`transit_data.xlsx`); saves this dataset as `data/clean/vrh_data.csv`
- `code/merge_vrh_upt.R`: merges the previous two datasets into one dataset containing VRH and UPT for each agency for each month; saves this dataset as `data/clean/ntd_data.csv`
- `code/treating_ntd.R`: assigns treated status, along with relative months to treatment based on the list from [this paper](https://las.depaul.edu/centers-and-institutes/chaddick-institute-for-metropolitan-development/research-and-publications/Documents/Partners%20in%20Transit_Live1.pdf) from the Chaddick Institute
- `code/census_data.R`: downloads the ACS dataset at the urbanized area level (will prompt user for their API key); saves this dataset as `data/clean/census.csv`
- `code/merge_data.R`: merges the ACS dataset and the completely clean NTD dataset created previously into the master dataset used for all analysis; saves this dataset as `data/clean/ntd_census.csv`
- `code/summary_stats.R`: creates summary statistics for all variables used in the regression: saves this output in `figures`
- `code/analysis_tables.R`: runs regression equation and saves the regression output as `tables/reg_output.csv`
- `code/figures.R`: creates all figures used in the paper: saves this output in `figures`
- `code/data_appendix.R`: creates summary statistics for all variable, as well as basic distribution plots

# Data sources
Here are the main data sources. 
- This project uses the National Transit Database (NTD) for a list of all agencies, where they are based, and their monthly ridership numbers. More information can be found [here](https://www.transit.dot.gov/ntd).
- This project also uses the American Community Survey (ACS) from the US Census to determine demographic variables on the urban areas the agency is based in. More information can be found [here](https://www.census.gov/programs-surveys/acs).

