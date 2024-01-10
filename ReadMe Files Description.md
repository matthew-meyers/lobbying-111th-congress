File Description:

Open Secrets Raw Data.txt - Data Downloaded from OpenSecrets (was too large to put on GradeScope so sent via email)

ObtainData.R - Process Open Secrets Data and Use API to get Congress.gov data. Merge Open Secrets and Congress.gov data. NOTE: I can't share my API key so this file will not run past the first half unless you sign up for your own. Instructions are included in the comments of the file

Rawdata.rds - output of ObtainData.R

PrepData.R - cleans raw data slightly to make it useable for analysis

data.rds - output of PrepData.R

Analysis.R - Linear, Negative Binomial, and Ordinal Logistic Modelling

Report.Rmd - RMD of written report, contains code used to make data analysis and tables

Report.pdf - Knit version of Report.Rmd

ExecutiveSummary.Rmd - RMD of executive summary: identical to executive summary in written report

ExecutiveSummary.pdf - Knit version of ExecutiveSummary.Rmd

Fit vs resid.jpeg - image of fits vs residuals plot of log linear model created in the Analysis.R file

Any csv file - various tabular output of the Analysis.R file that is displayed in the Report