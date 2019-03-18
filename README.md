
# Framework ALFAA

This repository contains data and codes for the prototype based on the framework ALFAA (Active Learning Fingerprint-based Anti Aliasing) to correct developer identity errors in version control data. The details of this framework can be found at https://arxiv.org/abs/1901.03363

### Data
crossRaterRelibility.clean.txt - Data provided to raters to create labels. Data contains user ID and company affiliations of developers - external information that helped in manual classification. These labels are used to compute agreement between raters.

### Code  
rf.r - Script to build random forest models, predict and cross-validate. 
       Experiments to understand effects of augmenting fingerprint metrics to string similarity.

.pbs, test_pmclust.R, test_pkmeans.r - codes for running on distributed environment (TITAN)

### Models

#To be uploaded
rfmodelsFullP7.c.RData - random forest model 
HOMONYM_RFMODELS - Contains all random forest models built using fingerprint (files, projects, text) metrics used for cross validation.
 


