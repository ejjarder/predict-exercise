###############################################################################
#
#   predict.exercise.R
#   Author: Eugene Jarder
#
#   Contains utility functions for the Practical Machine Learning Course
#   Project.
#
###############################################################################

require(randomForest) # Used to create the prediction model
require(knitr)        # to generate the report

set.seed(12345)

# excluded.columns
# The columns that will be excluded are metadata for the exercises.
#
excluded.columns <- c('X', 'user_name', 'raw_timestamp_part_1',
                      'raw_timestamp_part_1', 'raw_timestamp_part_2',
                      'cvtd_timestamp', 'new_window', 'num_window')

# R Markdown related information
#
input.rmd <- 'predict.exercise.Rmd'
html.title <- 'Predicting Exercise Execution'

# pml.write.files()
# Helper function to write the predicitons into files. Based on the function
# given on the course page.
#
# parameter:
#   answers - vector containing the predictions
#
pml.write.files <- function(answers) {
    n <- length(answers)
    for (i in 1:n) {
        filename <- paste0("problem_id_",i,".txt")
        write.table(answers[i], file=filename, quote=FALSE,
                    row.names=FALSE,col.names=FALSE)
    }
}

# load.data()
# Load data from the csv file. Treat empty strings as NA.
#
# parameter:
#   csv.file - the csv file to be loaded
#
load.data <- function(csv.file) {
    print(sprintf('Loading %s', csv.file))
    read.csv(csv.file, na.strings = c('NA', ''))    
}

# preprocess.data()
# Preprocess the given data. The preprocessing includes excluding columns as
# per the excluded.columns vector above, and removing all columns that have at
# least one NA in their data.
#
# parameter:
#   data - the data to preprocess
#
preprocess.data <- function(data) {
    data <- data[!(colnames(data) %in% excluded.columns)]
    data[sapply(data, function(x) { sum(is.na(x)) == 0 })]
}

# create.model()
# Create the prediction model. It first loads the data, then prepares the data
# for training. It then creates a prediction model using random forest. 
#
# parameter:
#   train.csv - the csv containing the training data
#
create.model <- function(train.csv) {
    print('create.model()')
    training.data <- load.data(train.csv)
    print('Preprocessing training data...')
    preproc.training.data <- preprocess.data(training.data)
    print('Training the model...')
    prediction.model <- randomForest(classe ~ ., data = preproc.training.data)
    print('Model generated!')
    prediction.model
}

# predict.data()
# Predict the classe of the test data using the given prediction model.
#
# parameter:
#   prediction.model - the prediction model
#   test.csv - the test data
#
predict.data <- function(prediction.model, test.csv) {
    print('predict.data()')
    test.data <- load.data(test.csv)
    print('Preprocessing testing data...')
    preproc.test.data <- preprocess.data(test.data)
    print('Making predictions...')
    prediction <- predict(prediction.model, preproc.test.data)
    print('Done predicting results!')
    prediction
}

# create.writeup()
# Creates the writeup on the course project. It first creates the prediction
# model, then predicts the classe using that prediction model. It will then
# create a writeup using the specified R markdown file, and output it to
# the specified html file.
#
# parameters:
#   train.csv, test.csv - the csv's containing the data used for training
#                         and testing the model, respectively
#   output.html - the directory to output the writeup to
#
create.writeup <- function(train.csv, test.csv, output.dir) {
    prediction.model <- create.model(train.csv)
    prediction <- predict.data(prediction.model, test.csv)
    output.html <- file.path(output.dir, 'index.html')
    dir.create(output.dir, recursive = TRUE)
    sprintf('Preparing a writeup: Using %s to generate %s with title "%s"',
            input.rmd, html.title, output.html)
    knit2html(input.rmd, title = html.title, output = output.html)
}

# Main Body
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 3) {
    create.writeup(args[1], args[2], args[3])
} else {
    print(paste('usage: Rscript predict.exercise.R <training csv>',
                '<testing csv> <output directory>'))
}
