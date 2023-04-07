# Human-Resources-Project-
The Problem Statement of this Project was to determine the likelihood of an employee to leave an organization. Since this is a classification problem therefore in this project I had used 3 Machine Leaning Algorithms: Logistic Regression, Decision Trees and Random Forest. To know more please click on the Readme File 

Following Factors had to kept in Mind before predicting as to which employee was likely to leave the organisation.
satisfaction level, last evaluation, number of project participated in, average monthly hours worked in organsation,	total years spend in the company
Work accident,  promotion in the last 5 years, salary, to which department the employee belongs to 

Among all the Machine Learning Algorithm it was Random Forest which provided the best AUC score and since only probability scores were required to be sent hence mysubmission file contains only probability score and hence separating the records into 2 different classes has not been done. Often companies have their own Business Criteria for class separation and hence this can be assumed in this case as well.

Among all the features the most Important feature  based on Feature Importance plot was Employee Satisfaction Level. This is not so surprising since job satisfaction is the most important factor for any employee before considering leaving his or her job.

Features in order of importance can be looked in this plot
![image](https://user-images.githubusercontent.com/28277893/230533357-61e6cbec-a4e8-4342-a47d-ff7964acfa32.png)
