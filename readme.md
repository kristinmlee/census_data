# Census Data Analysis

This repository contains census data and an analysis modeling income level. The prediction task is to determine the income level, binned at the $50K level, to present a binary classification problem.

### Analysis
A markdown file for the corresponding R code is found [here](https://github.com/kristinmlee/census_data/blob/master/censusData.md).

The goal of this exercise is to model the information contained in the last column (42nd), i.e., which people make more or less than $50,000 / year, from the information contained in the other columns. The exercise here consists of modeling a binary variable.
This is done through 5 steps:
1. Importing the learning and text files
2. Based on the learning file, making a quick statistic based and univariate audit of the different columns’ content and produce the results in visual / graphic format. 
3. Creating a model using these variables to model wining more or less than $50,000 / year.
4. Choosing the model that appears to have the highest performance based on a comparison between reality (the 42nd variable) and the model’s prediction. 
5. Applying the model to the test file and measure it’s real performance on it (same method as above).

### Data
This US Census dataset contains detailed but anonymized information for approximately 300,000 people. The archive contains 3 files: 
1. [A large learning .csv file](https://github.com/kristinmlee/census_data/blob/master/census_income_learn.csv)
2. [Another test .csv file](https://github.com/kristinmlee/census_data/blob/master/census_income_test.csv)
3. [A metadata file describing the columns of the two above mentioned files (identical for both)](https://github.com/kristinmlee/census_data/blob/master/census_income_metadata.txt)