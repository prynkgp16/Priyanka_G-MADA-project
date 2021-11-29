###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here)#for data loading/saving
library(dplyr)
library(tidymodels)

#path to data
save_data_location <- here::here("data","processed_data","processeddata.rds")


rawdata <- readRDS(save_data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(rawdata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(rawdata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)


##BAR Charts 

B1 <- Northeast_state_data %>% ggplot(aes(x=`SVI Category`, fill=`SVI Category`)) + geom_bar() +
  labs(title= "Bar Chart of Frequency of Counties by SVI category (NOrtheast)") + xlab("SVI CATEGORY") +  ylab("Count") + theme_classic()

print(B1)


B2 <- Midwest_state_data %>% ggplot(aes(x=`SVI Category`, fill=`SVI Category`)) + geom_bar() +
  labs(title= "Bar Chart of Frequency of Counties by SVI category (MIDWEST)") + xlab("SVI Category") +  ylab("Count") + theme_classic()


print(B2)


B3 <- West_state_data %>% ggplot(aes(x=`SVI Category`, fill=`SVI Category`)) + geom_bar() +
  labs(title= "Bar Chart of Frequency of Counties by SVI category (WEST)") + xlab("SVI Category") +  ylab("Count") + theme_classic()

print(B3)

B4 <- South_state_data %>% ggplot(aes(x=`SVI Category`, fill=`SVI Category`)) + geom_bar() +
  labs(title= "Bar Chart of Frequency of Counties by SVI category (SOUTH)") + xlab("SVI Category") +  ylab("Count") + theme_classic()

print(B4)

##SCatter plot between STATE and percent fully vaccinated

p1 <- West_state_top_10 %>% ggplot(aes(x= State, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point(color="red") + geom_smooth(method='lm', color="black") + labs(title= "Scatterplot of West vs  fully vaccinated", subtitle = ("Top 10 Counties,west")) + xlab("west STATES") +  ylab("Percent adults fully vaccinated") + theme_classic()

print(p1)


figure_top_10_West= here("results","West_state_top_10.png")
ggsave(filename = figure_top_10_West, plot=p1)

p2 <- Midwest_top_10 %>% ggplot(aes(x= State, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point(color="blue") + geom_smooth(method='lm', color="black") + labs(title= "Scatterplot of Midwest vs  fully vaccinated", subtitle = ("Top 10 Counties, Midwest")) + xlab("Midwest STATES") +  ylab("Percent adults fully vaccinated") + theme_classic()

print(p2)

figure_top_10_MID= here("results","Midwest_top_10.png")
ggsave(filename = figure_top_10_MID, plot=p2)


p3 <- South_top_10 %>% ggplot(aes(x= State, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point(color="purple") + geom_smooth(method='lm', color="black") + labs(title= "Scatterplot of South states vs  fullyvaccinated", subtitle = ("Top 10 Counties, SOUTH")) + xlab("South STATES") +  ylab("Percent adults fully vaccinated") + theme_classic()
print(p3)

figure_top_10_SOU= here("results","South_top_10.png")
ggsave(filename = figure_top_10_SOU, plot=p3) 



p4 <- NOrtheast_top_10 %>% ggplot(aes(x= State, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point(color="green") + geom_smooth(method='lm', color="black") + labs(title= "Scatterplot of NOrtheast states vs  fullyvaccinated", subtitle = ("Top 10 Counties, NOrtheast")) + xlab("NORTHEAST STATES") +  ylab("Percent adults fully vaccinated") + theme_classic()

print(p4) 

figure_top_10_NO= here("results","NOrtheast_top_10.png")
ggsave(filename = figure_top_10_NO, plot=p4)

## Comparing ethinicity in percent fully vaccincated in all US regions

##Northeast regions :  RACE vs percent fully vaccinated

## HISPANICS

p5 <- NOrtheast_race %>% ggplot(aes(x= Hispanic, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()


p5


figure_race_1_NO= here("results","Northeast_hispanic.png")
ggsave(filename = figure_race_1_NO, plot=p5)


p6 <- NOrtheast_race %>% ggplot(aes(x= Asian, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p6

figure_race_2_NO= here("results","Northeast_asian.png")
ggsave(filename = figure_race_2_NO, plot=p6)

p7 <- NOrtheast_race %>% ggplot(aes(x= Black, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p7


figure_race_3_NO= here("results","Northeast_Black.png")
ggsave(filename = figure_race_3_NO, plot=p7)

p8 <- NOrtheast_race %>% ggplot(aes(x= White, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()
p8


figure_race_4_NO= here("results","Northeast_White.png")
ggsave(filename = figure_race_4_NO, plot=p8)


##Midwest regions :  RACE vs percent fully vaccinated

p9 <- Midwest_race %>% ggplot(aes(x= Hispanic, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()


p9


figure_race_1_MID= here("results","Midwest_hispanic.png")
ggsave(filename = figure_race_1_MID, plot=p9)


p10 <- Midwest_race %>% ggplot(aes(x= Asian, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p10

figure_race_2_MID= here("results","Midwest_asian.png")
ggsave(filename = figure_race_2_MID, plot=p10)

p11 <- Midwest_race %>% ggplot(aes(x= Black, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p11


figure_race_3_MID= here("results","Midwest_Black.png")
ggsave(filename = figure_race_3_MID, plot=p11)

p12 <- Midwest_race %>% ggplot(aes(x= White, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()
p12


figure_race_4_MID= here("results","Midwest_White.png")
ggsave(filename = figure_race_4_MID, plot=p12)



##South states regions :  RACE vs percent fully vaccinated

p13 <- South_race %>% ggplot(aes(x= Hispanic, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()


p13


figure_race_1_Sou= here("results","South_hispanic.png")
ggsave(filename = figure_race_1_Sou, plot=p13)


p14 <- South_race %>% ggplot(aes(x= Asian, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p14

figure_race_2_Sou= here("results","South_asian.png")
ggsave(filename = figure_race_2_Sou, plot=p14)

p15 <- South_race %>% ggplot(aes(x= Black, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p15


figure_race_3_Sou= here("results","South_Black.png")
ggsave(filename = figure_race_3_Sou, plot=p15)

p16 <- South_race %>% ggplot(aes(x= White, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()
p16


figure_race_4_Sou= here("results","South_White.png")
ggsave(filename = figure_race_4_Sou, plot=p16)



##West regions :  RACE vs percent fully vaccinated

p17 <- West_race %>% ggplot(aes(x= Hispanic, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()


p17


figure_race_1_West= here("results","West_hispanic.png")
ggsave(filename = figure_race_1_West, plot=p17)


p18 <- West_race %>% ggplot(aes(x= Asian, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p18

figure_race_2_West= here("results","West_asian.png")
ggsave(filename = figure_race_2_West, plot=p18)

p19 <- West_race %>% ggplot(aes(x= Black, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()

p19


figure_race_3_West= here("results","West_Black.png")
ggsave(filename = figure_race_3_West, plot=p19)

p20 <- West_race %>% ggplot(aes(x= White, y=`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`)) + geom_point()
p20


figure_race_4_West= here("results","West_White.png")
ggsave(filename = figure_race_4_West, plot=p20)

######################################
#Data fitting/statistical analysis
######################################

# fit linear model



lmfit1 <- lm(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~ `White`+ `Asian` + `Black` + `Hispanic` , NOrtheast_race)

# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit1)

#look at fit results
print(lmtable)

#Save table
save_data_location <- here::here("results","lmtable.rds")
saveRDS(lmtable, file = save_data_location)


lmfit2 <- lm(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~ `White`+ `Asian` + `Black` + `Hispanic` ,South_race)


# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)


#Save table
save_data_location <- here::here("results","lmtable2.rds")
saveRDS(lmtable2, file = save_data_location)

lmfit3 <- lm(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~ `White`+ `Asian` + `Black` + `Hispanic` ,Midwest_race)


# place results from fit into a data frame with the tidy function
lmtable3 <- broom::tidy(lmfit3)

#look at fit results
print(lmtable3)


#Save table
save_data_location <- here::here("results","lmtable3.rds")
saveRDS(lmtable3, file = save_data_location)

lmfit4 <- lm(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~ `White`+ `Asian` + `Black` + `Hispanic` ,West_race)


# place results from fit into a data frame with the tidy function
lmtable4 <- broom::tidy(lmfit4)

#look at fit results
print(lmtable4)


#Save table
save_data_location <- here::here("results","lmtable4.rds")
saveRDS(lmtable4, file = save_data_location)


# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

table_file1 = here("results", "resulttable.rds")
saveRDS(lmtable2, file = table_file1)


table_file2 = here("results", "resulttable.rds")
saveRDS(lmtable3, file = table_file2)


table_file3 = here("results", "resulttable.rds")
saveRDS(lmtable4, file = table_file3)

### Decision Tree

data_split <- initial_split(rawdata, prop = 3/4)

train_data <- training(data_split)
 
test_data  <- testing(data_split)


# 5-fold cross-validation

fold_5_data<- vfold_cv(train_data, v = 5, repeats = 5)

# Creating a recipe 

rec1 <- recipe(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~ ., data = train_data)

rec1

#Null model performance

# Creates a recipe that fits null model

rec_null <- recipe(`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` ~  1 , data = train_data)




#Build a model specification


lm_mod <- linear_reg() %>%
  set_engine("lm")



# Null Model Workflow

null_wflow <-
  workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(rec_null)



null_fit<-
  null_wflow %>%
  fit(data = train_data )




# Predictions based on null model

prediction_train<-predict(null_fit, train_data)
prediction_test <-predict(null_fit, test_data)
prediction_train
prediction_test




tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")






##1) model specification,
##2) workflow definition, 
##3) tuning grid specification and 
##4) tuning using cross-validation and the tune_grid() function


##1) model specification


tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

#Tune grid specification:

tree_grid <-
  grid_regular(cost_complexity(),
               tree_depth(),
               levels = 5)


tree_grid


tree_workflow <- workflow() %>%
      add_model(tune_spec) %>%
       add_recipe(rec1)


tree_res <- tree_workflow %>%
       tune::tune_grid(resamples = fold_5_data,
                                           grid = tree_grid,
                                            control = control_grid(verbose = FALSE, save_pred = TRUE),                                    metrics = metric_set(rmse))


##The function collect_metrics() gives us a tidy tibble with all the results


tree_res %>% 
  collect_metrics()




tree_res %>% autoplot()


#Pull out the single set of hyperparameter values for our best decision tree model

best_tree <- tree_res %>%
     select_best("rmse")


best_tree


##Finalize Model


final_wf <- 
  tree_workflow %>% 
  finalize_workflow(best_tree)


final_wf


##fit this final model to the training data and use our test data to estimate the model performance


final_fit <- 
  final_wf %>%
  last_fit(data_split)


final_fit %>%
  collect_metrics()
