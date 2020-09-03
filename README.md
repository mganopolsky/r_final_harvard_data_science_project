# r_final_harvard_data_science_project
This is Part I of the Capstone Project for the Harvard Data Science Certificate - HarvardX: PH125.9x
This project was completed by Marina Ganopolsky.

**The purpose** of the project was the build a **recommendation system for movie ratings**, test said system on a variety of predictive models, and then **validate the data using RMSE** and a validation data partition. 

**The assignment expects the RMSE of the predicted ratings to be below .9 and ideally, less then .8649.**

The description of the project, as assigned, follows below:
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
You will use the following code to generate your datasets. Develop your algorithm using the edx set. For a final test of your algorithm, predict movie ratings in the validation set (the final hold-out test set) as if they were unknown. RMSE will be used to evaluate how close your predictions are to the true values in the validation set (the final hold-out test set).

Important: The validation data (the final hold-out test set) should NOT be used for training your algorithm and should ONLY be used for evaluating the RMSE of your final algorithm. You should split the edx data into separate training and test sets to design and test your algorithm.

Also remember that by accessing this site, you are agreeing to the terms of the edX Honor Code. This means you are expected to submit your own work and can be removed from the course for substituting another student's work as your own.

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

The following code was provided by the staff to serve as a starting point for the project:

The project as a whole had a number of problems with it. 
* the **archivist** package needs to be installed through devtools::install_github("pbiecek/archivist")
* the movielens dataset was modified to exclude films that have 3 or less reviews, due to a glitch in the ```createDataPartition {caret} ``` function. As per the documentation of the  function, items with 3 counts or less won't show up in both testing and training data.
