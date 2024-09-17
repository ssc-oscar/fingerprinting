
# Framework ALFAA

This repository contains data and codes for the prototype based on the framework ALFAA (Active Learning Fingerprint-based Anti Aliasing) to correct developer identity errors in version control data. The details of this framework can be found at https://arxiv.org/abs/1901.03363

### Data
crossRaterRelibility.clean.txt - Data provided to raters to create labels. Data contains user ID and company affiliations of developers - external information that helped in manual classification. These labels are used to compute agreement between raters.

### Code  

rf_model.r - Script to build random forest models, predict and cross-validate. 
       Experiments to understand effects of augmenting fingerprint metrics to string similarity.

rf.r - Various debugging/exploratory code used to build random forest models, predict and 
       cross-validate. Also icludes experiments to understand effects of augmenting fingerprint 
       metrics to string similarity. It is primarily for the record keeping.

.pbs, test_pmclust.R, test_pkmeans.r - codes for running on distributed environment (TITAN)

### Models


Models can be found at https://drive.google.com/drive/folders/170vwBToJDdBZ6li_ag2vKeqxr2ni03eM?usp=sharing

match2345_pairs.RDS - all pairs feature dataframe  

rfmodels.Rdata -

rfmodelsC.Rdata -

rfmodelsFullP7.c.RData - 

HOMONYM_RFMODELS - Contains all random forest models built using fingerprint (files, projects, text) metrics used for cross validation.
 


