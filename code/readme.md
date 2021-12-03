Hello. Welcome to my project github.

The present analysis will attempt to estimate the percent of the population in each US region that may be heistant to get a vaccine. The analysis also tries to determine the relationship between ethnicity and social vulnerability index

The final draft my project is located in the products tab under the manuscript folder. It includes a more polished draft of the paper with plots and tables and full analysis trying several different models. We use a test/train split and Cross-Validation for decision tree models. If you knit that to HTML, it should reproduce what I did.

HERE ARE THE STEPS TO REPRODUCE MY ANALYSIS

Run the processingscript.R located in the processing_code folder. This loads in all the datasets, merges, and cleans them.

Run the analysisscript.R located in the analysis_code folder. This performs descriptive analysis on the data, producing several tables and plots to be used in the manuscript. This runs the full analysis trying several different models (simple, decision tree) and saves different plots and tables needed for the manuscript.

 The supplementtarymaterial.rmd contains additional results which are in the form of  plots and graphs.
 
Run the manuscript.Rmd located in the products-> manuscript folder. This document will produce my submission for this part of the project. The reference file is located in the manuscript tab and named bibliography.bibtex. Supplemental_Material.Rmd contains plots and tables mentioned in the main text, but not included.