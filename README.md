<h1 align="center"> Depression Anxiety Stress Scales (DASS-21) for Finnish-speaking adults: validation and normative data </h1> <br>
<p align="center">
  <a href="https://helsinki.fi/hhs/">
    <img alt="The DASS-21 Finnish Validation" title="The DASS-21 Finnish Validation" src="https://www.helsinki.fi/assets/drupal/styles/hero_image/s3/media-image/HHS_metro.jpg.webp?itok=wOMorWTB" width="600">
  </a>
</p>
  
## Project description

This folder will contain the code used to analyse the DASS-21 questionnaire for an upcoming article by Vähäsarja et al. The data were collected as a part of the [Helsinki Health Study](helsinki.fi/hhs), an epidemiological study started in 2000. 

The scripts in this repository were used to produce an evaluation of the Finnish translation of the Depression, Anxiety, Stress Scales (DASS-21) quesionnaire. They investigate, among other things, the structure and invariance of the response data from two cohorts of Finnish adults.

## Table of Contents

- [What is DASS?](#what-is-dass)
- [Data](#data)
- [Scripts](#scripts)

## What is DASS?

The Depression, Anxiety and Stress Scales (DASS-42) is a public domain questionnaire intended to measure and differentiate affective symptoms. The shortened DASS-21 has later become popular, particularly for research. It has appeared in thousands of original research articles and been translated to more than forty languages.

Questionnaire data is tricky. We investigated whether or not the new Finnish translation works among Finns as it is expected to. If the factor structure or reported symptoms significantly deviated from English-speaking populations, the DASS-21 could not have been employed.

# Scripts

<div style="text-align: left;">

## Data

1. cfaModels.Rdata
	- contains the model specification used for the factor analyses in Lavaan format

2. scaleLocs.Rdata
	- contains information on which of the 21 questions belongs in which subscale in a data.frame

Unfortunately the participant data may not be published due to privacy concerns.

## Scripts

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

### Subscripts

1. attritionSub

2. psychometricsSub
