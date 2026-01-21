## **LESig(LUAD-ECM.Sig Shiny App)**

A user-friendly Shiny application for Lung Adenocarcinoma (LUAD) Extracellular Matrix (ECM) Signature Analysis, integrating risk score calculation, survival analysis, and unsupervised clustering visualization.(The manuscript is currently under review.)

**Core Functions**

1. Risk Score Analysis Module
Support multiple built-in LUAD datasets (TCGA-LUAD, GSE series) and custom user-uploaded data
Automatically calculate ECM signature risk scores and classify samples into High/Low risk groups
Generate survival curves (K-M plots) and time-dependent ROC curves for prognostic evaluation
Export analysis results (risk score table, plots) in multiple formats

2. Unsupervised Clustering Module
Automatic file recognition: No need to worry about upload order - the system automatically identifies training sets (with cluster column) and test sets
Gene expression normalization aligned with training set to avoid data leakage
PCA visualization of cluster assignments with interactive labels for new samples
Export cluster prediction results and PCA plots

3. Common Features
Responsive UI with theme adaptation
Detailed error prompts and operation notifications
One-click download of analysis results (Excel tables, high-resolution PNG/PDF plots)
Data validation and format checking to ensure analysis robustness

## Installation & Execution
1. Dependencies
Install required R packages first
```
required_packages <- c("shiny", "shinythemes", "ggplot2", "survival", "survminer", 
                       "timeROC", "ggsci", "DT", "openxlsx")

for(pkg in required_packages){ 
  if(!require(pkg, character.only = TRUE)) install.packages(pkg) 
  library(pkg, character.only = TRUE) 
```
## Run the App
Clone this repository
```
git clone https://github.com/txwellj/LESig.git  
cd ./LESig
```
Open app.R in RStudio and click the Run App button, or run in R console
```
shiny::runApp("app.R")
```
