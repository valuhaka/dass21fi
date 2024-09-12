## Aims

The scripts in this repository were used to produce an evaluation of the Finnish translation of the Depression, Anxiety, Stress Scales (DASS-21) quesionnaire. They investigate, among other things, the structure and invariance of the response data from two cohorts of Finnish adults.

## Contents.

#### Data

1. cfaModels.Rdata
	- contains the model specification used for the factor analyses in Lavaan format

2. scaleLocs.Rdata
	- contains information on which of the 21 questions belongs in which subscale in a data.frame

Unfortunately the participant data may not be published due to privacy concerns.

#### Scripts

1. AttritionAnalyses.R

2. cfa.R
	- this script takes in the preprocessed data and provides the main analyses on structural validity

3. createDASS21Data.R
	- takes lightly preprocessed data and extracts the most relevant items, imputing for missing values

4. defineModels.R
	- create cfaMoldels.Rdata

5. invariance.R
	- study the invariance by age and gender in both cohorts

6. makeNorms.R
	- generate the percentile norms

7. preprocessData.R
	- select, rename, and convert data from SAS format to R data.frames

8. prevalence.R

9. psychometrics.R
	- investigate the samples' distribution and reliability

10. sampleFormation.R
	- plot the formation of the final samples in the two cohorts

11. table1.R
	- generate Table 1 of the participants SES variables

###### Subscripts

1. attritionSub

2. psychometricsSub