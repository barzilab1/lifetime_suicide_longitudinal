# longitudinal-risk-factors

The primary analyses that we will do will involve the lifetime suicide attempt.

longitudinal:
Overall there are:

* 922 participants 
* 49 lifetime suicide attempts (5.3%)
* 33 current suicidal ideation  
* 95 current depression

* 1% missing data

The outcomes of features:

* 109677 = Current suicidal ideation (0=no, 1=yes)
* 109678 = Lifetime suicide attempt (0=no, 1=yes)
* 109679 = Sum PHQ-9 score
* Depression_mod_above_at_phq = Depression in PHQ (0=no, 1=yes)

cross-sectional:
* 322 participate in both 
* 6674 participate only in cross and have data
* 660 suicide attempts (10%)

#### Buckets:

1. Demographics
2. Clinical
3. Cognitive
4. Environmental based on geocode (i.e., SES)
5. Trauma (these are the ptd items from teh GOASSESS)
6. Environmental based on family characteristics (i.e., family history of psychiatric disorders, parents separated/together, parental education)


#### How to Run:
1. Make sure all the data-sets are under the folder "Data"
2. download stir: https://github.com/insilico/stir
3. To run locally, run main.R
4. To run on PMACS, run main.sh. the output will be in main.Rout and Rplots.pdf


  
---
####  Questions:
**Q1.** Do we also include kids with Lifetime suicide attempt before goassess time?  
  - Don't have the information  

**Q2.** Do we have data on trauma after goassess time  
  - No data after the goassess time except for the PHQ  

**Q3.** Do we have data on non-suicidal self-harm  
  - No  
  
**Q4.** what to do with neighborhoodCrime? 542 out of 922 (59%) are missing
  - We add them to the model later and see their impact 
  
**Q5.** Which buckets do not need outliers handle?  
   1. Clinical
   2. Cognitive
   3. Demograohics
   4. Family
  