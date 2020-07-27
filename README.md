# WG1-P1
The official code repository of SEISMIC working group 1, project 1.

## Organization
Our goal is to use the **analysis** and **tools** folders to share code among collaborators from different institutions. As such, the code should adhere to the common data model that is currently housed elsewhere (see the Wiki). Analysis scripts are meant to be specific to tasks for the project, while the tools will hopefully be broadly useful to others outside the project.
Most users will find what they need in the analysis folder. 

Collaborators that want to can form their own institution-specific folders with material to be shared among their local colleagues, or perhaps results. 

## Tool To-Dos
- Code to map ACT to SAT and the reverse. Tables exist in an SAT/ACT subfolder that are a start.
- Convert networks scripts to common data model
- Convert course vectors scripts to common data model

## Examples

### Create a unified grade/grade anomaly for all top 10 STEM (27-July-2020).
```
create_all_stem_layer.R:
This runs as a wrapper, calling grade_penalty_wg1_p1.R

It takes the same two tables as inputs.

The Data Model has been updated to include a flag "is_stem", 
to indicate that a course is considered STEM by the user at
their institution. 

INPUTS:
sr, sc: the student course and student record tables
crse_termcd_limit: The lower limit for TERM_CD of the cohort to include. 
                   It includes this term and all those after.
top10: set this to true to let the code try to identify and run on your top 10 courses by 
       enrollment of these students. WARNING: for reasons unknown the
       join required is super slow. 
```

### Grade Outcomes at the Course Level
### Run the Grade Penalties Script (19-Apr-2020)
```
PURPOSE: Consider one course at a time, one term. This returns various demographic
         breakdowns and returns plots that show differences in outcomes for single
         demographic variables: ethnicity, gender, lowinc, and first gen.
INPUT: see https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021
       sc - table of COURSE LEVEL VARIABLES 
       sr - table of STUDENT LEVEL VARIABLES 
       COURSE - the crs_name for the course to be studied
       TERM   - the crs_term for the course
       aggregate_terms - set to TRUE if you want to disregard term and aggregate over all.
OUTPUT: 1) This prints four plots the plot device (usually the plot view in RStudio)
        2) This returns a list, a variable containing 3 tables
        element 1: demographic statistics for the course by ethnicity, gender, LI, FG.
        element 2: grade penalty statistics by the Molinaro classification
        element 3: grade penalty statistics by the Fiorini classification
DEPENDENCIES: Must have tidyverse installed, must source the 'grade_penalty_functions.R'
              > source(str_c(<PATH_TO_YOUR_CODE>,'/grade_penalty_functions.R'))

EXAMPLE:
> kk <- grade_penalty_wg1_p1(sr,sc,COURSE='PHYSICS 140',TERM='FA 2012')
> print(kk[[1]])
> print(kk[[2]])
> print(kk[[3]])
```
