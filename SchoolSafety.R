### This project estimates the possibility of violence in NYC schools within a school year based on certain features 
##  i.e. how many students have registered in a school, what is the proportion of students compared to school capacity,
##  poverty rate and unemployment rate of the borough based on school location, 
##  High School graduation and drop out rate from previous year and so on.
##  in order to train the model, 2013-2016 school year information of NYC schools were used
##  in order to validate the model, 2016-2017 school year data of NYC schools were used
##  the links to download datasets- https://github.com/rumazad/EdxCapStone/blob/master/SchoolSafety.zip
### R version 3.6.1

library(readr)
library(dplyr)
library(tidyr)
library(stringi)
library(plyr)
library(caret)
library(lubridate)
library(e1071)
library(caretEnsemble)
library(feather) 
library(pROC)
library(caTools)
library(randomForest)
library(kernlab)
library(doParallel)
library(ggrepel)

################################### data cleaning starts here for model ################################

## Downloaded raw data from https://data.cityofnewyork.us/browse?q=school+safety
## located in files as- <xxxx>/Data/SchoolSafety/<file name>

schoolData1 <- read.csv(file="./Data/SchoolSafety/2010-2016_School_Safety_Report.csv", header=TRUE, sep=",")
schoolData1 <- as.data.frame(schoolData1, stringsAsFactors = FALSE) %>%  
  select(School.Year,     Location.Code,   Borough,         Register,        RangeA, 
         Major.N,         Oth.N,           NoCrim.N,        Prop.N,          Vio.N,           
         AvgOfMajor.N,    AvgOfOth.N,      AvgOfNoCrim.N,   AvgOfProp.N,     AvgOfVio.N)

schoolData2 <- read.csv(file="./Data/SchoolSafety/2016-2017_School_Safety_Report.csv", header=TRUE, sep=",")
schoolData2 <- as.data.frame(schoolData2, stringsAsFactors = FALSE) %>%  
  select(School.Year,     Location.Code,   Borough,         Register,        RangeA, 
         Major.N,         Oth.N,           NoCrim.N,        Prop.N,          Vio.N,           
         AvgOfMajor.N,    AvgOfOth.N,      AvgOfNoCrim.N,   AvgOfProp.N,     AvgOfVio.N) 

schoolStatistics <- schoolData1 %>% rbind(schoolData2)%>% 
  dplyr::rename(Total.Population = RangeA,
                Criminal.Offense = Major.N,
                Other.Crime = Oth.N,
                NonCriminal = NoCrim.N,
                Property.Crime = Prop.N,
                Violent.Crime = Vio.N,
                AvgOfCriminal = AvgOfMajor.N,
                AvgOfOther = AvgOfOth.N,
                AvgOfNoCrim = AvgOfNoCrim.N,
                AvgOfProp = AvgOfProp.N,
                AvgOfVio = AvgOfVio.N) %>% 
  mutate( Registered.Students = ifelse(Register=="N/A", 0, as.numeric(gsub(",","",as.character(Register)))),
          Total.Population = ifelse(Total.Population=="#N/A", "", as.character(Total.Population)),
          Criminal.Offense = ifelse(Criminal.Offense=="N/A", 0, as.numeric(as.character(Criminal.Offense))),
          Other.Crime = ifelse(Other.Crime=="N/A", 0, as.numeric(as.character(Other.Crime))),
          NonCriminal = ifelse(NonCriminal=="N/A", 0, as.numeric(as.character(NonCriminal))),
          Property.Crime = ifelse(Property.Crime=="N/A", 0, as.numeric(as.character(Property.Crime))),
          Violent.Crime = ifelse(Violent.Crime=="N/A", 0, as.numeric(as.character(Violent.Crime))),
          AvgOfCriminal = ifelse(AvgOfCriminal=="N/A", 0, as.numeric(as.character(AvgOfCriminal))),
          AvgOfOther = ifelse(AvgOfOther=="N/A", 0, as.numeric(as.character(AvgOfOther))),
          AvgOfNoCrim = ifelse(AvgOfNoCrim=="N/A", 0, as.numeric(as.character(AvgOfNoCrim))),
          AvgOfProp = ifelse(AvgOfProp=="N/A", 0, as.numeric(as.character(AvgOfProp))),
          AvgOfVio = ifelse(AvgOfVio=="N/A", 0, as.numeric(as.character(AvgOfVio))),
          School.Population = ifelse(Registered.Students > 0, Registered.Students, as.numeric(0)))

schoolStatistics[is.na(schoolStatistics)] <- 0

## Retrieve total population from the column Total.Population
## sample values of the column as follows
schoolStatistics %>% distinct(Total.Population)

  for(i in 1:nrow(schoolStatistics)){
    if(schoolStatistics$Total.Population[i] != ""){
    
      total <- schoolStatistics$Total.Population[i]
      if(!stringr::str_detect(toString(total), pattern = "N/A")){
      
        if(stringr::str_detect(toString(total), pattern = "\\+")){
          schoolStatistics$School.Population[i]<- "4000"
        }else{
          schoolStatistics$School.Population[i] <- unlist(strsplit(total, "-"))[[2]]
        }
     }
    }
  }

## borough demographics are retrived from nyc.gov 
borough_data <- read.csv(file="./Data/SchoolSafety/Borough Statistics.csv", header=TRUE, sep=",")
borough_data <- as.data.frame(borough_data, stringsAsFactors = FALSE) 

save(borough_data, file="./Data/SchoolSafety/borough_data.Rdata")

## Adding Borough demographics to the original dataset
schoolStatistics <- schoolStatistics %>%   
  inner_join(borough_data, by = c("School.Year", "Borough"))

## Filter Schools with no population
school <- schoolStatistics %>% 
  filter(Registered.Students != 0 , School.Population != 0) %>% 
  mutate(School.Population = as.numeric(School.Population),
         SchoolPopulation.Rate = round((Registered.Students/School.Population)*100, digit = 2),
         FLAG_VIOL_INC = ifelse(AvgOfCriminal > 0 | AvgOfVio > 0, 'yes', 'no'))

save(school, file="./Data/SchoolSafety/school.Rdata")

########################### data cleaning is complete ##########################

####### general characteristics of the training data set ##########
##### Number of Schools Per NYC Borough in Each School Year from 2013 to 2017
  schoolsPerBorough <- school %>%
    filter(Borough != 'O') %>%
    group_by(School.Year, Borough) %>%
    dplyr :: summarise(Total.Schools = n())

  schoolsPerBorough %>%
  ggplot(aes(x = School.Year, y = Total.Schools, fill = Borough)) +
    geom_point() +
    geom_label_repel(aes(label = Total.Schools), size =3,
                     box.padding   = 0.3,
                     point.padding = 0.2,
                     segment.color = 'grey50',
                     direction = 'y') +
    ylab("Number of Schools") +
    xlab("School Year") +
    ylim(0,700) +
    ggtitle("Number of Schools Per NYC Borough (Year 2013-17)") +
    scale_fill_discrete(breaks=c("K", "M", "Q", "R", "X"),
                        labels=c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx"))

##### Rate of Registered Students Per NYC Borough in Each School Year from 2013 to 2017
  df <- school %>%
    group_by(School.Year, Borough) %>%
    dplyr :: summarise(rate = mean(SchoolPopulation.Rate))

  schoolStat <- schoolsPerBorough %>%
    inner_join(df, by = c("School.Year", "Borough")) %>%
    mutate(Students.Rate = round((rate/TotalSchools)*100, digit = 2))

  schoolStat %>%
    ggplot(aes(x = School.Year, y = Students.Rate, fill = Borough)) +
    geom_point() +
    geom_label_repel(aes(label = Students.Rate), size =4,
                     box.padding   = 0.3,
                     point.padding = 0,
                     segment.color = 'grey50',
                     direction = 'y' ) +
    ylab("Student's Rate") +
    xlab("School Year") +
    ylim(0,105) +
    ggtitle("Registered Students' Rate Compared to School Capacity Per NYC Borough") +
    theme(plot.title = element_text(size = 12, face = "bold")) +
    scale_fill_discrete(breaks=c("K", "M", "Q", "R", "X"),
                      labels=c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx"))

##### Rate of Violence Per NYC Borough in Each School Year from 2013 to 2017
  df <- school %>%
    filter(FLAG_VIOL_INC == "yes") %>%
    group_by(School.Year, Borough) %>%
    dplyr :: summarise(violentSchools = n())

  schoolStat <- schoolsPerBorough %>%
              inner_join(df, by = c("School.Year", "Borough")) %>%
              mutate(Violence.Rate = round((violentSchools/TotalSchools)*100, digit = 2))

    schoolStat %>%
    ggplot(aes( x = Violence.Rate, y = School.Year, fill = Borough))+
    geom_col(width = 0.2) +
    ggtitle("Rate of Violence in Schools Per NYC Borough (Year 2013-17)") +
    scale_fill_discrete(breaks=c("K", "M", "Q", "R", "X"),
                        labels=c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx"))

##### NYC Borough demographics from 2013 to 2017
  ## Poverty rate
    borough_data %>%
      filter(Borough != 'O') %>%
      ggplot(aes( x = Poverty.Rate, y = School.Year, fill = Borough))+
      geom_col(width = 0.22, stat = "identity") +
      ggtitle("Poverty Rate Per NYC Borough (Year 2013-17)")

  ## Unemployment Rate
     borough_data %>%
     filter(Borough != 'O') %>%
     ggplot(aes( x = Unemployment.Rate, y = School.Year, fill = Borough))+
     geom_col(width = 0.1, stat = "identity") +
     ggtitle("Unemployment Rate Per NYC Borough (Year 2013-17)") +
     scale_fill_discrete(breaks=c("K", "M", "Q", "R", "X"),
                      labels=c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx"))

#########################################################################################################
  
clean_school_safety_data <- function(school_data, highSchool = FALSE){
  
  if(!exists("school_data", mode = "function") && !highSchool){
    ## school safety model data preparation
    school_data_file      <- paste0(getwd(),"/Data/SchoolSafety/school",".feather")
    school_data <- read_feather(school_data_file)
    school_data$FLAG_VIOL_INC <- relevel(as.factor(school_data$FLAG_VIOL_INC), "yes")
    
    school_data <- school_data %>% 
      select(School.Year,                Registered.Students,       
             Poverty.Rate,               Unemployment.Rate, 
             HighSchoolGraduation.Rate,  DropOut.Rate,   
             SchoolPopulation.Rate,      FLAG_VIOL_INC)
  }else{
    school_data <- school_data %>% 
      select(Registered.Students,        Poverty.Rate,          
             Unemployment.Rate,          HighSchoolGraduation.Rate,  
             DropOut.Rate,               SchoolPopulation.Rate)
  }
}

################################## setting the model code ###############################################

  #school safety model
  set.seed(2019, sample.kind="Rounding")

  school_data <- clean_school_safety_data()
  # generate training set using the school years 2013-2016
  data_train <- school_data %>%  
                filter(School.Year != '2016-17') %>% 
                select(-School.Year)
  #down sample the data for training
  data_train <- downSample(data_train, data_train$FLAG_VIOL_INC, list = FALSE)
  data_train <- data_train %>% select(-c(Class))
  
  mycontrol <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,                           
                            savePredictions = "final",
                            allowParallel = T )
  
  registerDoParallel(cores = 2)
  model_list <- caretList(FLAG_VIOL_INC ~ ., 
                          data = as.data.frame(data_train), 
                          trControl = mycontrol,
                          metric = "ROC",
                          tuneList = list(
                            rf = caretModelSpec(method = "rf", nodesize = 10, tuneLength = 15),
                            svmPoly = caretModelSpec(method = "svmPoly",   tuneLength=10,
                                                     tuneGrid = data.frame(degree = 1,  scale = 1,  C = 1),
                                                     preProcess = c("pca","scale","center")),
                            nnet = caretModelSpec( method = "nnet", tuneLength=6, trace=FALSE, 
                                                   preProcess = c('center', 'scale'))
                          )
                      )
  
  ensem_model <- caretEnsemble(model_list)
  save(ensem_model, file="./Data/SchoolSafety/ensem_model.Rdata")
  ## plotting the variable importance for all 3 models used
  plot(varImp(ensem_model$model$rf),      main = " Variable Importance",  sub = "RandomForest Model")
  plot(varImp(ensem_model$model$svmPoly), main = " Variable Importance",  sub = "Support Vector Machines with Polynomial Kernel")
  plot(varImp(ensem_model$model$nnet),    main = " Variable Importance",  sub = "Neural Network")
 
  fname <- "./Data/SchoolSafety/Model/School_ensembleModel.rds"
  saveRDS(ensem_model, file = fname)
  
################################### validate the model ################################
  
  #school safety model
  set.seed(2019, sample.kind="Rounding")
  
  SCHOOL_MODEL <- readRDS("./Data/SchoolSafety/Model/School_ensembleModel.rds")
  
  # generate test set using the school years 2016-2017
  SCHOOL_DATA_TEST <- school_data %>%  
    filter(School.Year == '2016-17') %>% 
    select(-School.Year)
  
  ensem_pred_yes <- predict(SCHOOL_MODEL, newdata = SCHOOL_DATA_TEST, type="prob")
  ensem_probs <- data.frame(1-ensem_pred_yes, ensem_pred_yes)
  colnames(ensem_probs) <- c("yes", "no") 
  
  ###  AUC value on the test data set
  colAUC(ensem_probs$yes, SCHOOL_DATA_TEST$FLAG_VIOL_INC, plotROC = TRUE) 
  ###  with 3 models, AUC= 80.18%
  
  ggplot(ensem_probs, aes(x=yes)) + 
    geom_density(data = ensem_probs[SCHOOL_DATA_TEST$FLAG_VIOL_INC == "yes",],alpha=0.25, fill = rgb(1,0,0)) + 
    geom_density(data = ensem_probs[SCHOOL_DATA_TEST$FLAG_VIOL_INC == "no",],alpha=0.25, fill = rgb(0,0,1))
  
  ## mean of the prediction would give a basic idea where to start the threshold
  mean(ensem_probs$yes)
  
  #Calculate confusion matrix using threshold = 0.325
  threshold1 <- 0.325
  pred_class <- ifelse(ensem_probs$yes > threshold1, "yes", "no")
  caret::confusionMatrix(as.factor(pred_class), SCHOOL_DATA_TEST$FLAG_VIOL_INC, positive = "yes")
  
  #Calculate confusion matrix using threshold = 0.375
  threshold2 <- 0.375
  pred_class <- ifelse(ensem_probs$yes > threshold2, "yes", "no")
  caret::confusionMatrix(as.factor(pred_class), SCHOOL_DATA_TEST$FLAG_VIOL_INC, positive = "yes")
  
  #Calculate confusion matrix using threshold = 0.45
  threshold3 <- 0.45
  pred_class <- ifelse(ensem_probs$yes > threshold3, "yes", "no")
  caret::confusionMatrix(as.factor(pred_class), SCHOOL_DATA_TEST$FLAG_VIOL_INC, positive = "yes")
  
  #Calculate confusion matrix using threshold = 0.55
  threshold4 <- 0.55
  pred_class <- ifelse(ensem_probs$yes > threshold4, "yes", "no")
  caret::confusionMatrix(as.factor(pred_class), SCHOOL_DATA_TEST$FLAG_VIOL_INC, positive = "yes")
  
  hist(ensem_probs[school_data$FLAG_VIOL_INC == "yes",1], breaks = 100, probability = T, col = rgb(1,0,0,0.5))
  hist(ensem_probs[school_data$FLAG_VIOL_INC == "no",1], breaks = 100, probability = T, col = rgb(0,0,1,0.5), add = T)             

############################### use the best threshold value depending on the need ###################################
## Best threshold value is determined based on Balanced Accuracy 
## meaning if the probability of violence is greater than 0.315, 
## only then the school will be listed as a potential threat in violence in this school year
  threshold <- 0.315
  pred_class <- ifelse(ensem_probs$yes > threshold, "yes", "no")
  caret::confusionMatrix(as.factor(pred_class), SCHOOL_DATA_TEST$FLAG_VIOL_INC, positive = "yes")
###################################################################################################################### 