# Data paper for the forest plots of Irstea Grenoble in revision in Ecology

Code and data for a data paper describing the forest plots data (protocol and meta-data) and R code to format the data.

This code reads the data, generates the tables and plots of the manuscript.

The following files are provided in the folder `output`:

- `data_c.csv`: Tree coordinates file
- `data_m.csv`: Tree measurements file
- `data_p.csv`: Plots description file
- `metadata_data_c.csv`: Metadata for tree coordinates file
- `metadata_data_m.csv`: Metadata for tree measurements file
- `metadata_data_p.csv`: Metadata for plots description file
- `species_code.csv`: Correspondence between species code and Latin name
- `status_code.csv`: Definition of status code


In addition the code computes growth and do several quality check and generate the html of the metadata description.


## Installation


The following code work with `remake` which can be installed directly in R

- remake: `devtools::install_github("richfitz/remake")`


## Folder structure

- `output` formatted data and table generated for the paper.
- `ms` mardown file for metadata
- `R` R functions used.

