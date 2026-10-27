Main Author: Juliana Balluffi-Fry
Principal Investigator: Ben Dantzer
Additional code sources: Erin Siracusa and Andrew McAdam

Working exploratory project for a study testing how a common reproductive trade-off in red squirrels, litter size and growth rate, is influenced by the mother's local density, food conditions, and perceived local density.


TO RUN CODE:

Download and load all packages in "R/packages.R". Some additional Bayesian packages plus Rtools are required in the Bayesian scripts. Data is mostly sourced from the Kluane Red Squirrel Project (KRSP) database. To run any code linked to the database requires access to the KRSP database.

For a breakdown of the analysis steps and a map showing script associations/paths, see the 'Guide to Project.xlsx' spreadsheet. It has two sheets within it, one for steps done to complete all data collation and one for a script map.

If everything works (packages are installed and access to the KRSP database is a-okay) code can be run using the "Master" scripts. To do this, first source "Scripts/Master_Data_Prep.R" to collate and clean all data. This runs scripts 01 - 08. Next source the 9th script, "Scripts/09-bayesian-models.R", separately to run the bayesian models. This might take a while depending on computing power. It also requires the installation of Rtools. This sometimes is tricky to trouble shoot. For both of these reasons, I did not include this script in the master scripts. The third step is to source the "Scripts/Master_Bayesian_Results.R" to create all tables and figures from bayesian models. If you'd like to create the example figure for how we calculate local density, run the 15th script, "Scripts/15-density_method_figures.R". Finally, to update methods and results, knit the markdown files.


SYNTAX:

Code is primarily written using the 'data.table' package (an alternative to dplyr)

Data frames must be classified as data tables to be used with data.table

Syntax for data.table is DT[i, j, by] which stands for with i, do j, by group. 

So, using the syntax it's saying this: DT[with i rows, do j function, by factor(s)]

e.g., litters[grid == "AG", mean(weight), by = bucket]


FOLDER ORGANIZATION:

Scripts/ All scripts used in full analysis. For current analysis, start to finish, follow along the numbered 
scripts in their order. Anything not numbered is currently a side quest/test.

Input/ Data that is not in the KRSP database

Output/ Products generated from scripts. 
		Output/Data/ Cleaned and manipulated datasets.
		Output/Tables/ Tables used to help understand the data

R/ System requirements. This is loaded at the start of each script and includes any packages, variables, or custom functions etc. (see first line of each script for how to source this folder)

Examples scripts/ Test code or sourced code from other authors.

Extra scripts/ Retired scripts.








