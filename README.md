# Employee-Absenteeism-Project
> In this project a dataset from a courier company has been provided.
As it is known that human capital plays an important role in collection, transportation and delivery. 
The company is passing through genuine issue of Absenteeism. 
The company has shared it dataset and requested to have an answer on the following areas:
1. What changes company should bring to reduce the number of absenteeism?  
2. How much losses every month can we project in 2011 if same trend of absenteeism continues? 

> Here I have assessed the dataset first.Thereafter I have cleaned them up using

> Missing value analysis:-
Here I have first counte the null values present in any variable including the dependent variable. If the null values are more than
30 percent then it won’t be wise to impute or drop it. Otherwise we can either impute the null values or simply drop that record from
the dataset. Here I have imputed the missing values using the mean and median method. Here we have not used KNN method of imputation
due to system constraints in both R and Python languages.Here we have first replaced a known value with null in the dataset and stored
it in a variable. Next we have imputed the value using both the methods mean and median. After imputing the missing values using both
methods each of the imputed value is compared with the actual value stored originally and the method which imputes the value closest to
actual value is used for imputing missing values of that particular variable. In python this procedure has been performed using user 
defined function and loops, and in R it showing memory error so only user defined function has been used and no loops.
After performing the missing value analysis the data is cross checked so that no null values remain leftover.

> Moving on to EDA I have used pie-charts for every categorical variable and histograms for continuous variables
to see their distribution in the dataset

> Outliers from the data has been removed using flooring and capping method.
In this method we compare each value of a variable with two values

Min = q25-1.5*iqr
Max = q75+1.5*iqr
 
Where q25 = 25th percentile of that variable
             
             q75= 75th percentile of that variable
 
            iqr = inter-quartile range of that variable.

If a value exceeds Max then it is replaced with Max similarly when a value is less than Min it is replaced with min.
Thus outliers are removed from every variable.

> Again exploratory data analysis has been performed on cleaned data here notmonly univariate but mjultivariate plots has also been used,
 both in R and Python.

> After data cleaning the continuous variables have been normalised as every continuous variable has different range which may convey
some wrong information regarding some variable.

> After feature scaling correlation plot has been used to check for multicollinearity in continuous variables and anova has been used for
categorical variables to check if all the variables are significant.Thereafter dummies has been added for categorical variables and one
dummy from each category has been removed to avoid any linear dependency.

> After adding dummies machine learning model has been built using different algorithms such as decision tree,random forest,
support vector machines,linear regression and Xgboost regression in both R and python. 
> RMSE for differet algorithms has been computed on training and testing set along with r-squared. 

> After this Principal component analysis has been performed in both python and R. It was found that around 25 variables can explain
more than 95% of the variance in the dataset. So again models have been built using the same algorithms along computing their 
RMSE on training and testing data with R squared.The highest R squared value achieved was 0.4 using XGboost. It may be due to the reason
that we have very small dataset with 740 records in total.

> The solution to the questions have been provided by analysizing plots of different vairables attached in the report. 



Dataset Details: Dataset Characteristics: Timeseries Multivariant Number of Attributes: 21 Missing Values : Yes 
 
Attribute Information: 
 
1. Individual identification (ID) 
2. Reason for absence (ICD).
Absences attested by the International Code of Diseases (ICD) stratified into 21 categories (I to XXI) as follows:
I Certain infectious and parasitic diseases
II Neoplasms
III Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism
IV Endocrine, nutritional and metabolic diseases
V Mental and behavioural disorders
VI Diseases of the nervous system
VII Diseases of the eye and adnexa 
VIII Diseases of the ear and mastoid process
IX Diseases of the circulatory system
X Diseases of the respiratory system
XI Diseases of the digestive system
XII Diseases of the skin and subcutaneous tissue
XIII Diseases of the musculoskeletal system and connective tissue
XIV Diseases of the genitourinary system
XV Pregnancy, childbirth and the puerperium
XVI Certain conditions originating in the perinatal period
XVII Congenital malformations, deformations and chromosomal abnormalities
XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
XIX Injury, poisoning and certain other consequences of external causes
XX External causes of morbidity and mortality
XXI Factors influencing health status and contact with health services. 
 
And 7 categories without (CID) patient follow-up (22), medical consultation (23), blood donation (24), laboratory examination (25),
unjustified absence (26), physiotherapy (27), dental consultation (28). 
3. Month of absence 
4. Day of the week (Monday (2), Tuesday (3), Wednesday (4), Thursday (5), Friday (6))
5. Seasons (summer (1), autumn (2), winter (3), spring (4))
6. Transportation expense
7. Distance from Residence to Work (kilometers)
8. Service time
9. Age 
10. Work load Average/day
11. Hit target 
12. Disciplinary failure (yes=1; no=0)
13. Education (high school (1), graduate (2), postgraduate (3), master and doctor (4))
14. Son (number of children)
15. Social drinker (yes=1; no=0)
16. Social smoker (yes=1; no=0)
17. Pet (number of pet)
18. Weight
19. Height
20. Body mass index
21. Absenteeism time in hours (target) 
