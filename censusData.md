###### Load necessary packages.

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Warning: package 'lmtest' was built under R version 3.4.4

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Warning: package 'pROC' was built under R version 3.4.4

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

###### Step 1 : Import the learning and text files

    #read learning csv file
    census = read.csv("census_income_learn.csv", header = FALSE)

    #read txt file containing information about census data
    #comments (lines starting with "|") are ignored
    metaData = read.delim("/Users/kristinlee/Documents/census/census_income_metadata.txt", stringsAsFactors = FALSE, sep = "\n", fill = TRUE, comment = "|")

    #get column names for dataset from metadata file
    #add last column: income level (+/- 50000)
    colNames = c(apply(metaData, 1, function(i) {
      gsub(":.*", "", i)
    }), "income level")

    #collapse names: change spaces to _
    colNames_noSpace = gsub(" ", "_", colNames)

    #rename columns of dataset
    colnames(census) = colNames_noSpace

    #get each variable type - in the metadata file, numeric are listed as "continuous" while the rest ("nominal") have the possible levels and will be coded here as factors
    varDescriptions = apply(metaData, 1, function(i){
      gsub(".*:", "", i)
    })

    #get continuous variables by which ones have continuous in description
    contVar = which(grepl("continuous", varDescriptions))

    #what are the continuous variables?
    colNames_noSpace[contVar]

    ## [1] "age"                             "wage_per_hour"                  
    ## [3] "capital_gains"                   "capital_losses"                 
    ## [5] "dividends_from_stocks"           "instance_weight"                
    ## [7] "num_persons_worked_for_employer" "weeks_worked_in_year"

    #Note: metadata file says to ignore instance_weight in classifiers

    #get rest as non-continuous variables
    notContVar = c(1:length(varDescriptions))[-contVar]

    #treat all non-continuous variables as factors
    for(i in notContVar) {
      census[ ,i] = as.factor(census[ ,i])  
    }

    #double check this is correct
    #get type of continuous variables
    sapply(contVar, function(i) class(census[ , i]))

    ## [1] "integer" "integer" "integer" "integer" "integer" "numeric" "integer"
    ## [8] "integer"

    #get type of non-continuous variables
    sapply(notContVar, function(i) class(census[ , i]))

    ##  [1] "factor" "factor" "factor" "factor" "factor" "factor" "factor"
    ##  [8] "factor" "factor" "factor" "factor" "factor" "factor" "factor"
    ## [15] "factor" "factor" "factor" "factor" "factor" "factor" "factor"
    ## [22] "factor" "factor" "factor" "factor" "factor" "factor" "factor"
    ## [29] "factor" "factor" "factor" "factor" "factor"

###### Step 2: Based on the learning file, make a quick statistic based and univariate audit of the different columns’ content and produce the results in visual / graphic format. This audit should describe the variable distribution, the % of missing values, the extreme values, and so on.

    #get structure of data frame
    str(census)

    ## 'data.frame':    199523 obs. of  42 variables:
    ##  $ age                                       : int  73 58 18 9 10 48 42 28 47 34 ...
    ##  $ class_of_worker                           : Factor w/ 9 levels " Federal government",..: 4 7 4 4 4 5 5 5 2 5 ...
    ##  $ detailed_industry_recode                  : Factor w/ 52 levels "0","1","2","3",..: 1 5 1 1 1 41 35 5 44 5 ...
    ##  $ detailed_occupation_recode                : Factor w/ 47 levels "0","1","2","3",..: 1 35 1 1 1 11 4 41 27 38 ...
    ##  $ education                                 : Factor w/ 17 levels " 10th grade",..: 13 17 1 11 11 17 10 13 17 17 ...
    ##  $ wage_per_hour                             : int  0 0 0 0 0 1200 0 0 876 0 ...
    ##  $ enroll_in_edu_inst_last_wk                : Factor w/ 3 levels " College or university",..: 3 3 2 3 3 3 3 3 3 3 ...
    ##  $ marital_stat                              : Factor w/ 7 levels " Divorced"," Married-A F spouse present",..: 7 1 5 5 5 3 3 5 3 3 ...
    ##  $ major_industry_code                       : Factor w/ 24 levels " Agriculture",..: 15 5 15 15 15 7 8 5 6 5 ...
    ##  $ major_occupation_code                     : Factor w/ 15 levels " Adm support including clerical",..: 7 9 7 7 7 11 3 5 1 6 ...
    ##  $ race                                      : Factor w/ 5 levels " Amer Indian Aleut or Eskimo",..: 5 5 2 5 5 1 5 5 5 5 ...
    ##  $ hispanic_origin                           : Factor w/ 10 levels " All other"," Central or South American",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sex                                       : Factor w/ 2 levels " Female"," Male": 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ member_of_a_labor_union                   : Factor w/ 3 levels " No"," Not in universe",..: 2 2 2 2 2 1 2 2 1 2 ...
    ##  $ reason_for_unemployment                   : Factor w/ 6 levels " Job leaver",..: 4 4 4 4 4 4 4 2 4 4 ...
    ##  $ full_or_part_time_employment_stat         : Factor w/ 8 levels " Children or Armed Forces",..: 3 1 3 1 1 2 1 7 2 1 ...
    ##  $ capital_gains                             : int  0 0 0 0 0 0 5178 0 0 0 ...
    ##  $ capital_losses                            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dividends_from_stocks                     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ tax_filer_stat                            : Factor w/ 6 levels " Head of household",..: 5 1 5 5 5 3 3 6 3 3 ...
    ##  $ region_of_previous_residence              : Factor w/ 6 levels " Abroad"," Midwest",..: 4 5 4 4 4 4 4 4 4 4 ...
    ##  $ state_of_previous_residence               : Factor w/ 51 levels " ?"," Abroad",..: 37 6 37 37 37 37 37 37 37 37 ...
    ##  $ detailed_household_and_family_stat        : Factor w/ 38 levels " Child <18 ever marr not in subfamily",..: 30 21 8 3 3 37 21 36 37 21 ...
    ##  $ detailed_household_summary_in_household   : Factor w/ 8 levels " Child 18 or older",..: 7 5 1 3 3 8 5 6 8 5 ...
    ##  $ instance_weight                           : num  1700 1054 992 1758 1069 ...
    ##  $ migration_code-change_in_msa              : Factor w/ 10 levels " ?"," Abroad to MSA",..: 1 4 1 6 6 1 6 1 1 6 ...
    ##  $ migration_code-change_in_reg              : Factor w/ 9 levels " ?"," Abroad",..: 1 9 1 7 7 1 7 1 1 7 ...
    ##  $ migration_code-move_within_reg            : Factor w/ 10 levels " ?"," Abroad",..: 1 10 1 8 8 1 8 1 1 8 ...
    ##  $ live_in_this_house_1_year_ago             : Factor w/ 3 levels " No"," Not in universe under 1 year old",..: 2 1 2 3 3 2 3 2 2 3 ...
    ##  $ migration_prev_res_in_sunbelt             : Factor w/ 4 levels " ?"," No"," Not in universe",..: 1 4 1 3 3 1 3 1 1 3 ...
    ##  $ num_persons_worked_for_employer           : int  0 1 0 0 0 1 6 4 5 6 ...
    ##  $ family_members_under_18                   : Factor w/ 5 levels " Both parents present",..: 5 5 5 1 1 5 5 5 5 5 ...
    ##  $ country_of_birth_father                   : Factor w/ 43 levels " ?"," Cambodia",..: 41 41 42 41 41 32 41 41 41 41 ...
    ##  $ country_of_birth_mother                   : Factor w/ 43 levels " ?"," Cambodia",..: 41 41 42 41 41 41 41 41 41 41 ...
    ##  $ country_of_birth_self                     : Factor w/ 43 levels " ?"," Cambodia",..: 41 41 42 41 41 41 41 41 41 41 ...
    ##  $ citizenship                               : Factor w/ 5 levels " Foreign born- Not a citizen of U S ",..: 5 5 1 5 5 5 5 5 5 5 ...
    ##  $ own_business_or_self_employed             : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 3 1 1 1 1 ...
    ##  $ fill_inc_questionnaire_for_veteran's_admin: Factor w/ 3 levels " No"," Not in universe",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ veterans_benefits                         : Factor w/ 3 levels "0","1","2": 3 3 3 1 1 3 3 3 3 3 ...
    ##  $ weeks_worked_in_year                      : int  0 52 0 0 0 52 52 30 52 52 ...
    ##  $ year                                      : Factor w/ 2 levels "94","95": 2 1 2 1 1 2 1 2 2 1 ...
    ##  $ income_level                              : Factor w/ 2 levels " - 50000."," 50000+.": 1 1 1 1 1 1 1 1 1 1 ...

    ##make plots for each variable to get a sense of distribution

    #set color palette to use
    colPal = brewer.pal(4, "PuOr")
    colPal_all = rep(colPal, ceiling(ncol(census)/4))

    #make density plots for variables of type numeric or integer
    for(i in contVar) {
      plot(density(census[ ,i]), main = paste("Distribution of", colNames[i]), col = colPal_all[i], cex.main = 0.7)
    }

![](censusData_files/figure-markdown_strict/2-1.png)![](censusData_files/figure-markdown_strict/2-2.png)![](censusData_files/figure-markdown_strict/2-3.png)![](censusData_files/figure-markdown_strict/2-4.png)![](censusData_files/figure-markdown_strict/2-5.png)![](censusData_files/figure-markdown_strict/2-6.png)![](censusData_files/figure-markdown_strict/2-7.png)![](censusData_files/figure-markdown_strict/2-8.png)

    #make barplots for all factors
    for(i in notContVar) {
      plot(census[ ,i], main = colNames[i], las = 2, col = colPal_all[i], ylab = "Number of entries", cex.axis = 0.7, cex.names = 0.6 )
    }

![](censusData_files/figure-markdown_strict/2-9.png)![](censusData_files/figure-markdown_strict/2-10.png)![](censusData_files/figure-markdown_strict/2-11.png)![](censusData_files/figure-markdown_strict/2-12.png)![](censusData_files/figure-markdown_strict/2-13.png)![](censusData_files/figure-markdown_strict/2-14.png)![](censusData_files/figure-markdown_strict/2-15.png)![](censusData_files/figure-markdown_strict/2-16.png)![](censusData_files/figure-markdown_strict/2-17.png)![](censusData_files/figure-markdown_strict/2-18.png)![](censusData_files/figure-markdown_strict/2-19.png)![](censusData_files/figure-markdown_strict/2-20.png)![](censusData_files/figure-markdown_strict/2-21.png)![](censusData_files/figure-markdown_strict/2-22.png)![](censusData_files/figure-markdown_strict/2-23.png)![](censusData_files/figure-markdown_strict/2-24.png)![](censusData_files/figure-markdown_strict/2-25.png)![](censusData_files/figure-markdown_strict/2-26.png)![](censusData_files/figure-markdown_strict/2-27.png)![](censusData_files/figure-markdown_strict/2-28.png)![](censusData_files/figure-markdown_strict/2-29.png)![](censusData_files/figure-markdown_strict/2-30.png)![](censusData_files/figure-markdown_strict/2-31.png)![](censusData_files/figure-markdown_strict/2-32.png)![](censusData_files/figure-markdown_strict/2-33.png)![](censusData_files/figure-markdown_strict/2-34.png)![](censusData_files/figure-markdown_strict/2-35.png)![](censusData_files/figure-markdown_strict/2-36.png)![](censusData_files/figure-markdown_strict/2-37.png)![](censusData_files/figure-markdown_strict/2-38.png)![](censusData_files/figure-markdown_strict/2-39.png)![](censusData_files/figure-markdown_strict/2-40.png)![](censusData_files/figure-markdown_strict/2-41.png)

    ##there are variables with "?" and "Not in universe"
    ##we'll treat this as missing data and set to NA to make it easier to work with
    #note: this is a bit more complicated because we're dealing with factors but we can use recode_factor function from dplyr package
    for(i in notContVar) {
      census[ ,i] = recode_factor(census[ ,i], " ?" = NA_character_)
      census[ ,i] = recode_factor(census[ ,i], " Not in universe" = NA_character_)
      census[ ,i] = recode_factor(census[ ,i], " Not in universe under 1 year old" = NA_character_)
      census[ ,i] = recode_factor(census[ ,i], " Not in universe or children" = NA_character_)
    }

    #recheck structure of data
    str(census)

    ## 'data.frame':    199523 obs. of  42 variables:
    ##  $ age                                       : int  73 58 18 9 10 48 42 28 47 34 ...
    ##  $ class_of_worker                           : Factor w/ 8 levels " Federal government",..: NA 6 NA NA NA 4 4 4 2 4 ...
    ##  $ detailed_industry_recode                  : Factor w/ 52 levels "0","1","2","3",..: 1 5 1 1 1 41 35 5 44 5 ...
    ##  $ detailed_occupation_recode                : Factor w/ 47 levels "0","1","2","3",..: 1 35 1 1 1 11 4 41 27 38 ...
    ##  $ education                                 : Factor w/ 17 levels " 10th grade",..: 13 17 1 11 11 17 10 13 17 17 ...
    ##  $ wage_per_hour                             : int  0 0 0 0 0 1200 0 0 876 0 ...
    ##  $ enroll_in_edu_inst_last_wk                : Factor w/ 2 levels " College or university",..: NA NA 2 NA NA NA NA NA NA NA ...
    ##  $ marital_stat                              : Factor w/ 7 levels " Divorced"," Married-A F spouse present",..: 7 1 5 5 5 3 3 5 3 3 ...
    ##  $ major_industry_code                       : Factor w/ 23 levels " Agriculture",..: NA 5 NA NA NA 7 8 5 6 5 ...
    ##  $ major_occupation_code                     : Factor w/ 14 levels " Adm support including clerical",..: NA 8 NA NA NA 10 3 5 1 6 ...
    ##  $ race                                      : Factor w/ 5 levels " Amer Indian Aleut or Eskimo",..: 5 5 2 5 5 1 5 5 5 5 ...
    ##  $ hispanic_origin                           : Factor w/ 10 levels " All other"," Central or South American",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sex                                       : Factor w/ 2 levels " Female"," Male": 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ member_of_a_labor_union                   : Factor w/ 2 levels " No"," Yes": NA NA NA NA NA 1 NA NA 1 NA ...
    ##  $ reason_for_unemployment                   : Factor w/ 5 levels " Job leaver",..: NA NA NA NA NA NA NA 2 NA NA ...
    ##  $ full_or_part_time_employment_stat         : Factor w/ 8 levels " Children or Armed Forces",..: 3 1 3 1 1 2 1 7 2 1 ...
    ##  $ capital_gains                             : int  0 0 0 0 0 0 5178 0 0 0 ...
    ##  $ capital_losses                            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ dividends_from_stocks                     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ tax_filer_stat                            : Factor w/ 6 levels " Head of household",..: 5 1 5 5 5 3 3 6 3 3 ...
    ##  $ region_of_previous_residence              : Factor w/ 5 levels " Abroad"," Midwest",..: NA 4 NA NA NA NA NA NA NA NA ...
    ##  $ state_of_previous_residence               : Factor w/ 49 levels " Abroad"," Alabama",..: NA 5 NA NA NA NA NA NA NA NA ...
    ##  $ detailed_household_and_family_stat        : Factor w/ 38 levels " Child <18 ever marr not in subfamily",..: 30 21 8 3 3 37 21 36 37 21 ...
    ##  $ detailed_household_summary_in_household   : Factor w/ 8 levels " Child 18 or older",..: 7 5 1 3 3 8 5 6 8 5 ...
    ##  $ instance_weight                           : num  1700 1054 992 1758 1069 ...
    ##  $ migration_code-change_in_msa              : Factor w/ 8 levels " Abroad to MSA",..: NA 3 NA 5 5 NA 5 NA NA 5 ...
    ##  $ migration_code-change_in_reg              : Factor w/ 7 levels " Abroad"," Different county same state",..: NA 7 NA 6 6 NA 6 NA NA 6 ...
    ##  $ migration_code-move_within_reg            : Factor w/ 8 levels " Abroad"," Different county same state",..: NA 8 NA 7 7 NA 7 NA NA 7 ...
    ##  $ live_in_this_house_1_year_ago             : Factor w/ 2 levels " No"," Yes": NA 1 NA 2 2 NA 2 NA NA 2 ...
    ##  $ migration_prev_res_in_sunbelt             : Factor w/ 2 levels " No"," Yes": NA 2 NA NA NA NA NA NA NA NA ...
    ##  $ num_persons_worked_for_employer           : int  0 1 0 0 0 1 6 4 5 6 ...
    ##  $ family_members_under_18                   : Factor w/ 4 levels " Both parents present",..: NA NA NA 1 1 NA NA NA NA NA ...
    ##  $ country_of_birth_father                   : Factor w/ 42 levels " Cambodia"," Canada",..: 40 40 41 40 40 31 40 40 40 40 ...
    ##  $ country_of_birth_mother                   : Factor w/ 42 levels " Cambodia"," Canada",..: 40 40 41 40 40 40 40 40 40 40 ...
    ##  $ country_of_birth_self                     : Factor w/ 42 levels " Cambodia"," Canada",..: 40 40 41 40 40 40 40 40 40 40 ...
    ##  $ citizenship                               : Factor w/ 5 levels " Foreign born- Not a citizen of U S ",..: 5 5 1 5 5 5 5 5 5 5 ...
    ##  $ own_business_or_self_employed             : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 3 1 1 1 1 ...
    ##  $ fill_inc_questionnaire_for_veteran's_admin: Factor w/ 2 levels " No"," Yes": NA NA NA NA NA NA NA NA NA NA ...
    ##  $ veterans_benefits                         : Factor w/ 3 levels "0","1","2": 3 3 3 1 1 3 3 3 3 3 ...
    ##  $ weeks_worked_in_year                      : int  0 52 0 0 0 52 52 30 52 52 ...
    ##  $ year                                      : Factor w/ 2 levels "94","95": 2 1 2 1 1 2 1 2 2 1 ...
    ##  $ income_level                              : Factor w/ 2 levels " - 50000."," 50000+.": 1 1 1 1 1 1 1 1 1 1 ...

    #rename income_level levels to under/over because current naming (- 50000/50000+) could be more intuitive and has some weird spacing
    levels(census[ ,"income_level"])

    ## [1] " - 50000." " 50000+."

    levels(census[ ,"income_level"]) = c("under", "over")

###### Step 3: Create a model using these variables (you can use whichever variables you want, or even create you own; for example, you could find the ratio or relationship between different variables, the one-hot encoding of “categorical” variables, etc.) to model wining more or less than $50,000 / year. Here, the idea would be for you to test one or two algorithms, such as logistic regression, or a decision tree. Feel free to choose others if wish.

    ## I want to explore the data further before choosing variables for model building.

    #get how many entries (rows) have missing data
    sum(apply(census, 1, function(i) sum(is.na(i)) > 0))

    ## [1] 199523

    #every row has some amount of missing data, which is not surprising given that many entries were "Not in universe"
    nrow(census)

    ## [1] 199523

    #get number of entries with missing data for each variable
    numMissingPerCol = apply(census, 2, function(i) sum(is.na(i)))

    numMissingPerCol

    ##                                        age 
    ##                                          0 
    ##                            class_of_worker 
    ##                                     100245 
    ##                   detailed_industry_recode 
    ##                                          0 
    ##                 detailed_occupation_recode 
    ##                                          0 
    ##                                  education 
    ##                                          0 
    ##                              wage_per_hour 
    ##                                          0 
    ##                 enroll_in_edu_inst_last_wk 
    ##                                     186943 
    ##                               marital_stat 
    ##                                          0 
    ##                        major_industry_code 
    ##                                     100684 
    ##                      major_occupation_code 
    ##                                     100684 
    ##                                       race 
    ##                                          0 
    ##                            hispanic_origin 
    ##                                          0 
    ##                                        sex 
    ##                                          0 
    ##                    member_of_a_labor_union 
    ##                                     180459 
    ##                    reason_for_unemployment 
    ##                                     193453 
    ##          full_or_part_time_employment_stat 
    ##                                          0 
    ##                              capital_gains 
    ##                                          0 
    ##                             capital_losses 
    ##                                          0 
    ##                      dividends_from_stocks 
    ##                                          0 
    ##                             tax_filer_stat 
    ##                                          0 
    ##               region_of_previous_residence 
    ##                                     183750 
    ##                state_of_previous_residence 
    ##                                     184458 
    ##         detailed_household_and_family_stat 
    ##                                          0 
    ##    detailed_household_summary_in_household 
    ##                                          0 
    ##                            instance_weight 
    ##                                          0 
    ##               migration_code-change_in_msa 
    ##                                     101212 
    ##               migration_code-change_in_reg 
    ##                                     101212 
    ##             migration_code-move_within_reg 
    ##                                     101212 
    ##              live_in_this_house_1_year_ago 
    ##                                     101212 
    ##              migration_prev_res_in_sunbelt 
    ##                                     183750 
    ##            num_persons_worked_for_employer 
    ##                                          0 
    ##                    family_members_under_18 
    ##                                     144232 
    ##                    country_of_birth_father 
    ##                                       6713 
    ##                    country_of_birth_mother 
    ##                                       6119 
    ##                      country_of_birth_self 
    ##                                       3393 
    ##                                citizenship 
    ##                                          0 
    ##              own_business_or_self_employed 
    ##                                          0 
    ## fill_inc_questionnaire_for_veteran's_admin 
    ##                                     197539 
    ##                          veterans_benefits 
    ##                                          0 
    ##                       weeks_worked_in_year 
    ##                                          0 
    ##                                       year 
    ##                                          0 
    ##                               income_level 
    ##                                          0

    #some columns are complete (like age, education, wage_per_hour) and these seem like good variables to use to build models

    ##for complete columns, create visualizations to see if there's patterns related to income_level
    #get variables with no missing data
    completeCol = which(numMissingPerCol == 0)

    #remove income_level and instance_weight from this vector because income_level is what we want to predict and we should not include instance_weight as a predictor
    completeCol = completeCol[-which(names(completeCol) == "income_level" | names(completeCol) == "instance_weight")]

    ##visualize distributions of data separated by income_level for each variable

    #function to plot density of continuous variable by income_level
    plotDensityByIncome = function(i) {
      densityplot(census[,i], groups=census[ ,"income_level"], xlab = colNames[i], plot.points=FALSE, auto.key=TRUE, main = list(label = paste(colNames[i], "by Income Level (under/over 50K)"), cex = 0.7))
    }
    #plot density for all continous variables with no missing data
    for(i in intersect(contVar, completeCol)) {
      print(plotDensityByIncome(i)) #need print here because lattice plot is strange in for loops
    }

![](censusData_files/figure-markdown_strict/3-1.png)![](censusData_files/figure-markdown_strict/3-2.png)![](censusData_files/figure-markdown_strict/3-3.png)![](censusData_files/figure-markdown_strict/3-4.png)![](censusData_files/figure-markdown_strict/3-5.png)![](censusData_files/figure-markdown_strict/3-6.png)![](censusData_files/figure-markdown_strict/3-7.png)

    #function to plot histograms of non-continuous variable by income_level
    plotHistByIncome = function(i) {
      histogram(~census[ ,i] | census[ ,42], xlab = colNames[i], las = 1, layout = c(1, 2), scales = list(x = list(rot = 90, cex = 0.5)))
    }
    #plot histograms for all non-continous variables with no missing data
    for(i in intersect(notContVar, completeCol)) {
      print(plotHistByIncome(i))
    }

![](censusData_files/figure-markdown_strict/3-8.png)![](censusData_files/figure-markdown_strict/3-9.png)![](censusData_files/figure-markdown_strict/3-10.png)![](censusData_files/figure-markdown_strict/3-11.png)![](censusData_files/figure-markdown_strict/3-12.png)![](censusData_files/figure-markdown_strict/3-13.png)![](censusData_files/figure-markdown_strict/3-14.png)![](censusData_files/figure-markdown_strict/3-15.png)![](censusData_files/figure-markdown_strict/3-16.png)![](censusData_files/figure-markdown_strict/3-17.png)![](censusData_files/figure-markdown_strict/3-18.png)![](censusData_files/figure-markdown_strict/3-19.png)![](censusData_files/figure-markdown_strict/3-20.png)![](censusData_files/figure-markdown_strict/3-21.png)![](censusData_files/figure-markdown_strict/3-22.png)

    ##based on the plots, I'm going to do two logistic regression with different predictors:

    #model 1- predictors: age, sex, race, marital status, education, detailed industry recode, detailed occupation recode, weeks worked in year, and number persons worked for employer
    model_1 = glm(income_level ~ age + sex + race + marital_stat + education + detailed_industry_recode + detailed_occupation_recode + weeks_worked_in_year + num_persons_worked_for_employer, family = binomial, na.action = na.omit, data = census) 

    #get summary of model 1
    summary(model_1)

    ## 
    ## Call:
    ## glm(formula = income_level ~ age + sex + race + marital_stat + 
    ##     education + detailed_industry_recode + detailed_occupation_recode + 
    ##     weeks_worked_in_year + num_persons_worked_for_employer, family = binomial, 
    ##     data = census, na.action = na.omit)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3979  -0.2366  -0.0944  -0.0001   4.2306  
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                                    Estimate Std. Error
    ## (Intercept)                                      -8.804e+00  2.241e-01
    ## age                                               3.841e-02  1.043e-03
    ## sex Male                                          1.124e+00  2.875e-02
    ## race Asian or Pacific Islander                    2.269e-01  1.731e-01
    ## race Black                                        2.895e-02  1.679e-01
    ## race Other                                        1.745e-01  2.053e-01
    ## race White                                        3.306e-01  1.607e-01
    ## marital_stat Married-A F spouse present          -2.638e-01  2.852e-01
    ## marital_stat Married-civilian spouse present      5.891e-02  3.999e-02
    ## marital_stat Married-spouse absent               -5.449e-02  1.328e-01
    ## marital_stat Never married                       -5.532e-01  5.249e-02
    ## marital_stat Separated                           -5.334e-02  1.010e-01
    ## marital_stat Widowed                              1.882e-01  7.671e-02
    ## education 11th grade                              2.568e-01  1.806e-01
    ## education 12th grade no diploma                   5.400e-01  2.231e-01
    ## education 1st 2nd 3rd or 4th grade               -3.404e-01  3.121e-01
    ## education 5th or 6th grade                       -4.760e-01  2.543e-01
    ## education 7th and 8th grade                      -1.157e-01  1.788e-01
    ## education 9th grade                              -7.974e-02  2.127e-01
    ## education Associates degree-academic program      1.439e+00  1.442e-01
    ## education Associates degree-occup /vocational     1.271e+00  1.435e-01
    ## education Bachelors degree(BA AB BS)              2.132e+00  1.344e-01
    ## education Children                               -1.246e+01  7.989e+01
    ## education Doctorate degree(PhD EdD)               3.359e+00  1.526e-01
    ## education High school graduate                    8.517e-01  1.338e-01
    ## education Less than 1st grade                    -1.734e+00  1.011e+00
    ## education Masters degree(MA MS MEng MEd MSW MBA)  2.738e+00  1.372e-01
    ## education Prof school degree (MD DDS DVM LLB JD)  3.070e+00  1.535e-01
    ## education Some college but no degree              1.261e+00  1.346e-01
    ## detailed_industry_recode1                         7.791e-01  3.833e-01
    ## detailed_industry_recode2                         1.092e+00  4.196e-01
    ## detailed_industry_recode3                         1.985e+00  3.515e-01
    ## detailed_industry_recode4                         1.191e+00  3.379e-01
    ## detailed_industry_recode5                         8.418e-01  3.282e-01
    ## detailed_industry_recode6                         6.985e-01  3.961e-01
    ## detailed_industry_recode7                         1.227e+00  3.797e-01
    ## detailed_industry_recode8                         1.642e+00  3.595e-01
    ## detailed_industry_recode9                         1.385e+00  3.513e-01
    ## detailed_industry_recode10                       -1.490e+01  8.092e+03
    ## detailed_industry_recode11                        1.499e+00  3.404e-01
    ## detailed_industry_recode12                        1.562e+00  3.427e-01
    ## detailed_industry_recode13                        2.240e+00  3.462e-01
    ## detailed_industry_recode14                        1.475e+00  3.715e-01
    ## detailed_industry_recode15                        1.531e+00  3.588e-01
    ## detailed_industry_recode16                        1.474e+00  3.564e-01
    ## detailed_industry_recode17                        8.841e-01  4.969e-01
    ## detailed_industry_recode18                        1.108e+00  3.865e-01
    ## detailed_industry_recode19                        7.952e-01  3.553e-01
    ## detailed_industry_recode20                        3.170e+00  5.400e-01
    ## detailed_industry_recode21                        8.289e-01  4.004e-01
    ## detailed_industry_recode22                        9.396e-01  3.826e-01
    ## detailed_industry_recode23                        1.864e+00  3.593e-01
    ## detailed_industry_recode24                        1.214e+00  3.446e-01
    ## detailed_industry_recode25                        1.862e+00  3.429e-01
    ## detailed_industry_recode26                        2.655e+00  4.009e-01
    ## detailed_industry_recode27                        1.153e+00  3.693e-01
    ## detailed_industry_recode28                        1.127e+00  5.427e-01
    ## detailed_industry_recode29                        1.501e+00  3.379e-01
    ## detailed_industry_recode30                        1.658e+00  3.425e-01
    ## detailed_industry_recode31                        1.721e+00  3.421e-01
    ## detailed_industry_recode32                        1.214e+00  3.383e-01
    ## detailed_industry_recode33                        7.015e-01  3.364e-01
    ## detailed_industry_recode34                        1.500e+00  3.379e-01
    ## detailed_industry_recode35                        1.222e+00  3.377e-01
    ## detailed_industry_recode36                        7.317e-01  7.137e-01
    ## detailed_industry_recode37                        1.344e+00  3.370e-01
    ## detailed_industry_recode38                        9.304e-01  3.519e-01
    ## detailed_industry_recode39                        6.749e-01  3.510e-01
    ## detailed_industry_recode40                        9.864e-01  3.503e-01
    ## detailed_industry_recode41                        7.737e-01  3.401e-01
    ## detailed_industry_recode42                        8.385e-01  3.404e-01
    ## detailed_industry_recode43                        4.458e-01  3.378e-01
    ## detailed_industry_recode44                        1.127e-01  3.539e-01
    ## detailed_industry_recode45                        1.196e+00  3.355e-01
    ## detailed_industry_recode46                        8.023e-01  2.971e-01
    ## detailed_industry_recode47                        1.421e+00  3.455e-01
    ## detailed_industry_recode48                        4.613e-01  3.655e-01
    ## detailed_industry_recode49                        1.579e+00  3.506e-01
    ## detailed_industry_recode50                        1.196e+00  3.415e-01
    ## detailed_industry_recode51                        1.744e+00  4.789e-01
    ## detailed_occupation_recode1                      -7.592e-01  3.507e-01
    ## detailed_occupation_recode2                       3.610e-03  3.285e-01
    ## detailed_occupation_recode3                      -7.720e-01  3.315e-01
    ## detailed_occupation_recode4                      -2.879e-01  3.338e-01
    ## detailed_occupation_recode5                      -2.559e-01  3.380e-01
    ## detailed_occupation_recode6                      -9.352e-01  3.428e-01
    ## detailed_occupation_recode7                       5.723e-01  3.482e-01
    ## detailed_occupation_recode8                      -2.074e-01  3.377e-01
    ## detailed_occupation_recode9                      -7.643e-01  3.485e-01
    ## detailed_occupation_recode10                     -1.041e+00  3.379e-01
    ## detailed_occupation_recode11                      2.210e-01  3.479e-01
    ## detailed_occupation_recode12                     -1.161e+00  3.330e-01
    ## detailed_occupation_recode13                     -1.250e+00  3.567e-01
    ## detailed_occupation_recode14                     -1.277e+00  3.431e-01
    ## detailed_occupation_recode15                     -6.933e-01  3.422e-01
    ## detailed_occupation_recode16                     -2.665e-01  3.336e-01
    ## detailed_occupation_recode17                     -3.904e-01  3.354e-01
    ## detailed_occupation_recode18                     -4.583e-01  3.388e-01
    ## detailed_occupation_recode19                     -1.425e+00  3.429e-01
    ## detailed_occupation_recode20                     -9.973e-01  6.309e-01
    ## detailed_occupation_recode21                     -9.271e-01  3.542e-01
    ## detailed_occupation_recode22                     -1.480e+00  3.890e-01
    ## detailed_occupation_recode23                     -2.087e+00  3.548e-01
    ## detailed_occupation_recode24                     -1.972e+00  3.639e-01
    ## detailed_occupation_recode25                     -1.983e+00  3.670e-01
    ## detailed_occupation_recode26                     -2.234e+00  3.365e-01
    ## detailed_occupation_recode27                     -2.635e+00  1.006e+00
    ## detailed_occupation_recode28                     -9.853e-01  3.409e-01
    ## detailed_occupation_recode29                     -2.458e+00  3.814e-01
    ## detailed_occupation_recode30                     -1.804e+00  3.869e-01
    ## detailed_occupation_recode31                     -2.745e+00  3.801e-01
    ## detailed_occupation_recode32                     -1.665e+00  3.734e-01
    ## detailed_occupation_recode33                     -1.247e+00  3.333e-01
    ## detailed_occupation_recode34                     -1.172e+00  3.357e-01
    ## detailed_occupation_recode35                     -1.350e+00  3.337e-01
    ## detailed_occupation_recode36                     -1.891e+00  3.387e-01
    ## detailed_occupation_recode37                     -2.012e+00  3.474e-01
    ## detailed_occupation_recode38                     -1.727e+00  3.392e-01
    ## detailed_occupation_recode39                     -1.336e+00  3.461e-01
    ## detailed_occupation_recode40                     -1.550e+00  4.093e-01
    ## detailed_occupation_recode41                     -2.292e+00  3.835e-01
    ## detailed_occupation_recode42                     -2.380e+00  3.730e-01
    ## detailed_occupation_recode43                     -1.173e+00  4.238e-01
    ## detailed_occupation_recode44                     -1.938e+00  4.010e-01
    ## detailed_occupation_recode45                             NA         NA
    ## detailed_occupation_recode46                             NA         NA
    ## weeks_worked_in_year                              4.007e-02  1.345e-03
    ## num_persons_worked_for_employer                   1.418e-01  6.691e-03
    ##                                                  z value Pr(>|z|)    
    ## (Intercept)                                      -39.284  < 2e-16 ***
    ## age                                               36.818  < 2e-16 ***
    ## sex Male                                          39.090  < 2e-16 ***
    ## race Asian or Pacific Islander                     1.311 0.189971    
    ## race Black                                         0.172 0.863085    
    ## race Other                                         0.850 0.395281    
    ## race White                                         2.057 0.039679 *  
    ## marital_stat Married-A F spouse present           -0.925 0.355064    
    ## marital_stat Married-civilian spouse present       1.473 0.140667    
    ## marital_stat Married-spouse absent                -0.410 0.681517    
    ## marital_stat Never married                       -10.538  < 2e-16 ***
    ## marital_stat Separated                            -0.528 0.597572    
    ## marital_stat Widowed                               2.453 0.014159 *  
    ## education 11th grade                               1.422 0.155114    
    ## education 12th grade no diploma                    2.420 0.015519 *  
    ## education 1st 2nd 3rd or 4th grade                -1.091 0.275330    
    ## education 5th or 6th grade                        -1.872 0.061203 .  
    ## education 7th and 8th grade                       -0.647 0.517391    
    ## education 9th grade                               -0.375 0.707708    
    ## education Associates degree-academic program       9.977  < 2e-16 ***
    ## education Associates degree-occup /vocational      8.855  < 2e-16 ***
    ## education Bachelors degree(BA AB BS)              15.864  < 2e-16 ***
    ## education Children                                -0.156 0.876088    
    ## education Doctorate degree(PhD EdD)               22.013  < 2e-16 ***
    ## education High school graduate                     6.366 1.94e-10 ***
    ## education Less than 1st grade                     -1.715 0.086297 .  
    ## education Masters degree(MA MS MEng MEd MSW MBA)  19.959  < 2e-16 ***
    ## education Prof school degree (MD DDS DVM LLB JD)  19.994  < 2e-16 ***
    ## education Some college but no degree               9.369  < 2e-16 ***
    ## detailed_industry_recode1                          2.033 0.042101 *  
    ## detailed_industry_recode2                          2.603 0.009229 ** 
    ## detailed_industry_recode3                          5.648 1.62e-08 ***
    ## detailed_industry_recode4                          3.524 0.000424 ***
    ## detailed_industry_recode5                          2.565 0.010321 *  
    ## detailed_industry_recode6                          1.763 0.077835 .  
    ## detailed_industry_recode7                          3.231 0.001232 ** 
    ## detailed_industry_recode8                          4.569 4.90e-06 ***
    ## detailed_industry_recode9                          3.942 8.08e-05 ***
    ## detailed_industry_recode10                        -0.002 0.998531    
    ## detailed_industry_recode11                         4.404 1.06e-05 ***
    ## detailed_industry_recode12                         4.557 5.19e-06 ***
    ## detailed_industry_recode13                         6.468 9.90e-11 ***
    ## detailed_industry_recode14                         3.972 7.13e-05 ***
    ## detailed_industry_recode15                         4.266 1.99e-05 ***
    ## detailed_industry_recode16                         4.136 3.53e-05 ***
    ## detailed_industry_recode17                         1.779 0.075215 .  
    ## detailed_industry_recode18                         2.866 0.004155 ** 
    ## detailed_industry_recode19                         2.238 0.025209 *  
    ## detailed_industry_recode20                         5.869 4.38e-09 ***
    ## detailed_industry_recode21                         2.070 0.038422 *  
    ## detailed_industry_recode22                         2.456 0.014058 *  
    ## detailed_industry_recode23                         5.187 2.14e-07 ***
    ## detailed_industry_recode24                         3.524 0.000425 ***
    ## detailed_industry_recode25                         5.432 5.58e-08 ***
    ## detailed_industry_recode26                         6.622 3.54e-11 ***
    ## detailed_industry_recode27                         3.121 0.001804 ** 
    ## detailed_industry_recode28                         2.076 0.037899 *  
    ## detailed_industry_recode29                         4.442 8.92e-06 ***
    ## detailed_industry_recode30                         4.840 1.30e-06 ***
    ## detailed_industry_recode31                         5.032 4.86e-07 ***
    ## detailed_industry_recode32                         3.588 0.000333 ***
    ## detailed_industry_recode33                         2.085 0.037029 *  
    ## detailed_industry_recode34                         4.439 9.04e-06 ***
    ## detailed_industry_recode35                         3.618 0.000296 ***
    ## detailed_industry_recode36                         1.025 0.305242    
    ## detailed_industry_recode37                         3.989 6.63e-05 ***
    ## detailed_industry_recode38                         2.644 0.008193 ** 
    ## detailed_industry_recode39                         1.923 0.054480 .  
    ## detailed_industry_recode40                         2.815 0.004870 ** 
    ## detailed_industry_recode41                         2.275 0.022923 *  
    ## detailed_industry_recode42                         2.463 0.013764 *  
    ## detailed_industry_recode43                         1.320 0.186978    
    ## detailed_industry_recode44                         0.318 0.750210    
    ## detailed_industry_recode45                         3.566 0.000363 ***
    ## detailed_industry_recode46                         2.700 0.006931 ** 
    ## detailed_industry_recode47                         4.114 3.90e-05 ***
    ## detailed_industry_recode48                         1.262 0.206906    
    ## detailed_industry_recode49                         4.504 6.68e-06 ***
    ## detailed_industry_recode50                         3.502 0.000462 ***
    ## detailed_industry_recode51                         3.642 0.000271 ***
    ## detailed_occupation_recode1                       -2.165 0.030380 *  
    ## detailed_occupation_recode2                        0.011 0.991233    
    ## detailed_occupation_recode3                       -2.329 0.019879 *  
    ## detailed_occupation_recode4                       -0.863 0.388282    
    ## detailed_occupation_recode5                       -0.757 0.448921    
    ## detailed_occupation_recode6                       -2.729 0.006360 ** 
    ## detailed_occupation_recode7                        1.644 0.100241    
    ## detailed_occupation_recode8                       -0.614 0.539217    
    ## detailed_occupation_recode9                       -2.193 0.028312 *  
    ## detailed_occupation_recode10                      -3.080 0.002067 ** 
    ## detailed_occupation_recode11                       0.635 0.525352    
    ## detailed_occupation_recode12                      -3.487 0.000488 ***
    ## detailed_occupation_recode13                      -3.504 0.000458 ***
    ## detailed_occupation_recode14                      -3.721 0.000198 ***
    ## detailed_occupation_recode15                      -2.026 0.042755 *  
    ## detailed_occupation_recode16                      -0.799 0.424416    
    ## detailed_occupation_recode17                      -1.164 0.244466    
    ## detailed_occupation_recode18                      -1.353 0.176194    
    ## detailed_occupation_recode19                      -4.157 3.23e-05 ***
    ## detailed_occupation_recode20                      -1.581 0.113922    
    ## detailed_occupation_recode21                      -2.617 0.008858 ** 
    ## detailed_occupation_recode22                      -3.804 0.000142 ***
    ## detailed_occupation_recode23                      -5.881 4.08e-09 ***
    ## detailed_occupation_recode24                      -5.419 5.98e-08 ***
    ## detailed_occupation_recode25                      -5.402 6.59e-08 ***
    ## detailed_occupation_recode26                      -6.639 3.16e-11 ***
    ## detailed_occupation_recode27                      -2.619 0.008831 ** 
    ## detailed_occupation_recode28                      -2.890 0.003849 ** 
    ## detailed_occupation_recode29                      -6.447 1.14e-10 ***
    ## detailed_occupation_recode30                      -4.663 3.11e-06 ***
    ## detailed_occupation_recode31                      -7.223 5.10e-13 ***
    ## detailed_occupation_recode32                      -4.459 8.24e-06 ***
    ## detailed_occupation_recode33                      -3.743 0.000182 ***
    ## detailed_occupation_recode34                      -3.490 0.000483 ***
    ## detailed_occupation_recode35                      -4.044 5.25e-05 ***
    ## detailed_occupation_recode36                      -5.582 2.37e-08 ***
    ## detailed_occupation_recode37                      -5.792 6.97e-09 ***
    ## detailed_occupation_recode38                      -5.091 3.55e-07 ***
    ## detailed_occupation_recode39                      -3.861 0.000113 ***
    ## detailed_occupation_recode40                      -3.787 0.000153 ***
    ## detailed_occupation_recode41                      -5.976 2.29e-09 ***
    ## detailed_occupation_recode42                      -6.380 1.77e-10 ***
    ## detailed_occupation_recode43                      -2.769 0.005621 ** 
    ## detailed_occupation_recode44                      -4.832 1.35e-06 ***
    ## detailed_occupation_recode45                          NA       NA    
    ## detailed_occupation_recode46                          NA       NA    
    ## weeks_worked_in_year                              29.786  < 2e-16 ***
    ## num_persons_worked_for_employer                   21.200  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 92815  on 199522  degrees of freedom
    ## Residual deviance: 53700  on 199397  degrees of freedom
    ## AIC: 53952
    ## 
    ## Number of Fisher Scoring iterations: 19

    #model 2- predictors: age, sex, race, education, weeks worked in year
    #this a reduced set of the predictors used for model 1 because I wanted to know if we could still do accurate predictions with fewers predictors  
    model_2 = glm(income_level ~ age + sex + race + education + weeks_worked_in_year, data = census, family = binomial, na.action = na.omit) 

    #get summary of model 2
    summary(model_2)

    ## 
    ## Call:
    ## glm(formula = income_level ~ age + sex + race + education + weeks_worked_in_year, 
    ##     family = binomial, data = census, na.action = na.omit)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3940  -0.2937  -0.1000  -0.0001   3.9842  
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error
    ## (Intercept)                                      -9.647e+00  2.137e-01
    ## age                                               4.174e-02  8.692e-04
    ## sex Male                                          1.366e+00  2.450e-02
    ## race Asian or Pacific Islander                    2.626e-01  1.671e-01
    ## race Black                                       -3.995e-03  1.630e-01
    ## race Other                                        8.343e-02  1.974e-01
    ## race White                                        4.252e-01  1.563e-01
    ## education 11th grade                              1.990e-01  1.783e-01
    ## education 12th grade no diploma                   5.463e-01  2.192e-01
    ## education 1st 2nd 3rd or 4th grade               -4.841e-01  3.097e-01
    ## education 5th or 6th grade                       -6.153e-01  2.518e-01
    ## education 7th and 8th grade                      -1.104e-01  1.772e-01
    ## education 9th grade                              -1.181e-01  2.099e-01
    ## education Associates degree-academic program      1.971e+00  1.408e-01
    ## education Associates degree-occup /vocational     1.713e+00  1.404e-01
    ## education Bachelors degree(BA AB BS)              2.782e+00  1.313e-01
    ## education Children                               -1.144e+01  4.803e+01
    ## education Doctorate degree(PhD EdD)               3.949e+00  1.445e-01
    ## education High school graduate                    1.044e+00  1.320e-01
    ## education Less than 1st grade                    -2.037e+00  1.011e+00
    ## education Masters degree(MA MS MEng MEd MSW MBA)  3.305e+00  1.331e-01
    ## education Prof school degree (MD DDS DVM LLB JD)  4.091e+00  1.405e-01
    ## education Some college but no degree              1.626e+00  1.323e-01
    ## weeks_worked_in_year                              5.495e-02  8.876e-04
    ##                                                  z value Pr(>|z|)    
    ## (Intercept)                                      -45.137  < 2e-16 ***
    ## age                                               48.016  < 2e-16 ***
    ## sex Male                                          55.747  < 2e-16 ***
    ## race Asian or Pacific Islander                     1.571  0.11609    
    ## race Black                                        -0.025  0.98045    
    ## race Other                                         0.423  0.67261    
    ## race White                                         2.721  0.00651 ** 
    ## education 11th grade                               1.116  0.26438    
    ## education 12th grade no diploma                    2.492  0.01269 *  
    ## education 1st 2nd 3rd or 4th grade                -1.563  0.11807    
    ## education 5th or 6th grade                        -2.443  0.01456 *  
    ## education 7th and 8th grade                       -0.623  0.53331    
    ## education 9th grade                               -0.563  0.57355    
    ## education Associates degree-academic program      13.994  < 2e-16 ***
    ## education Associates degree-occup /vocational     12.201  < 2e-16 ***
    ## education Bachelors degree(BA AB BS)              21.192  < 2e-16 ***
    ## education Children                                -0.238  0.81172    
    ## education Doctorate degree(PhD EdD)               27.326  < 2e-16 ***
    ## education High school graduate                     7.913 2.52e-15 ***
    ## education Less than 1st grade                     -2.015  0.04387 *  
    ## education Masters degree(MA MS MEng MEd MSW MBA)  24.824  < 2e-16 ***
    ## education Prof school degree (MD DDS DVM LLB JD)  29.115  < 2e-16 ***
    ## education Some college but no degree              12.294  < 2e-16 ***
    ## weeks_worked_in_year                              61.903  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 92815  on 199522  degrees of freedom
    ## Residual deviance: 59148  on 199499  degrees of freedom
    ## AIC: 59196
    ## 
    ## Number of Fisher Scoring iterations: 18

###### Step 4: Choose the model that appears to have the highest performance based on a comparison between reality (the 42nd variable) and the model’s prediction.

    ##using model, predict outcome
    #if prediction > 0.5, label as "over", else label as "under" $50K income level
    probs_model1 = predict(model_1,  type='response')
    pred_model1 = ifelse(probs_model1 > 0.5, "over", "under")

    #calculate accuracy
    accuracy_model1 = mean(pred_model1 == census[ , "income_level"])
    accuracy_model1

    ## [1] 0.9485573

    #same prediction and accuracy calculation for model 2
    probs_model2 = predict(model_2,  type='response')
    pred_model2 = ifelse(probs_model2 > 0.5, "over", "under")
    accuracy_model2 = mean(pred_model2 == census[ , "income_level"])
    accuracy_model2

    ## [1] 0.9426181

    ##model 1 is slightly more accurate but there are many more degrees of freedom so is it actually better?
    #these models are nested so can do likelihood ratio test
    lrtest(model_1, model_2)

    ## Likelihood ratio test
    ## 
    ## Model 1: income_level ~ age + sex + race + marital_stat + education + 
    ##     detailed_industry_recode + detailed_occupation_recode + weeks_worked_in_year + 
    ##     num_persons_worked_for_employer
    ## Model 2: income_level ~ age + sex + race + education + weeks_worked_in_year
    ##   #Df LogLik   Df  Chisq Pr(>Chisq)    
    ## 1 126 -26850                           
    ## 2  24 -29574 -102 5447.6  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    #they are statistically significant despite difference in degrees of freedom so let's proceed with model 1 as best fit of the 2

    ##plot ROC curves to visualize and compare model sensitivity (true positive rate) and specificity (true negative rate) where "over" is positive

    #format data as numeric for ROC curves
    pred_model1_numeric = ifelse(pred_model1 == "under", 1, 2)
    roc_model1 = roc(income_level ~ pred_model1_numeric, data = census)

    pred_model2_numeric = ifelse(pred_model2 == "under", 1, 2)
    roc_model2 = roc(income_level ~ pred_model2_numeric, data = census)

    plot(roc_model1, col = colPal_all[1], main = "ROC curves (over = case, under = control)")
    lines(roc_model2, col = colPal_all[4])
    legend("topleft", c("Model 1", "Model 2"), col = colPal_all[c(1,4)], lty = 1)

![](censusData_files/figure-markdown_strict/4-1.png)

###### Step 5: Apply your model to the test file and measure it’s real performance on it (same method as above).
