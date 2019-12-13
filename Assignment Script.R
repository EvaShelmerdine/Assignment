#First, let's have a look at the data set "my_data"
my_data

#my_data is in wide format rather than long format
#This means that at the moment each row is one participants data rather than one row per observation
#So we need to wrangle the data to convert it into long format (r can only analsye data in long format)
#This requires the package "tidyr" so let's load that by loading the tidyverse package
library(tidyverse)

#Now that "tidyverse" is ready we can use the "gather()" fucntion from "tidyr" to convert data_1 from wide to long format
#This new data set will be named "my_data_long"
#In the "gather()" function we will need to state which data set it is that we are reformatting, what we want the new
#column names to be (condition and reaction time) and
#which columns from the orginal dataset we want to collapse and include in this new long format
my_data_long <- gather(my_data, "condition", "reaction time", c("common_word", "rare_word"))
#Now lets have a look to make sure this reformatting has worked by looking at the structure of "my_data_long"
str(my_data_long)

#Now we want to combine the two datasets "my_data" and "literature_data"
#We can use the "inner_join()" function from "dplyr" (part of the tidyverse package) to join our two datasets
#In both datasets, the same participant number refers to the same individual
#So we can join the two datasets by matching these participant numbers
#This joined dataset will be called "my_data_all"
my_data_all <- inner_join(my_data, literature_data, by = c("participant"))
#Now let's have a look at this to make sure joining the datasets has worked
str(my_data_all)

#Next, we want to view this combined data
#However, to be able to view the data, it must be converted from long format to wide format
#This will be done as before using the "gather()" function
#This new dataset will be called "my_data_all_long"
my_data_all_long <- gather(my_data_all, "condition", "reaction_time", c("common_word", "rare_word", "literature"))
#Let's have a look at the structure for this
str(my_data_all_long)

#So let's use the package "ggplot2" (part of the tidyverse package) to view this data
#From this data I want to see if people react faster to more common words and whether a persons reaction time to words
#is influenced by their exposure to literature
#First I will create a violin plot comparing peoples reactions times to common and rare words
#This requires the package Hmisc so lets turn that on
library(Hmisc)
#Now that's on let's make our graph!
my_data_all_long %>%
  filter(condition != "literature") %>%
  group_by(condition) %>%
    ggplot(aes(x = condition, y = reaction_time, colour = condition, fill = condition)) +
    geom_violin() + geom_jitter(alpha = .25, position = position_jitter(0.05)) +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1)
#NEED TO FIGURE OUT HOW TO MAKE DOTS APPEAR ON VIOLIN PLOT