#First we need to import the data required for this analysis
#This requires the tidyverse package so let's load that
library(tidyverse)

#Now we can import and have a look at the data
my_data <- read_csv("~/Year 3/Reproducible Data Science/Assignment/my_data.csv")
literature_data <- read_csv("~/Year 3/Reproducible Data Science/Assignment/literature_data.csv")

#my_data is in wide format rather than long format
#This means that at the moment each row is one participants data rather than one row per observation
#So we need to wrangle the data to convert it into long format (r can only analsye data in long format)
#This requires the package "tidyr" from the tidyverse package
#We can use the "gather()" fucntion from "tidyr" to convert data_1 from wide to long format
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

#Now our data is in long format we want to label condition as a factor that could affect reaction time
#We can do this using the as.factor() function
my_data_all$condition <- as.factor(my_data_all$condition)

#Next, we want to view this combined data
#However, to be able to view the data, it must be converted from long format to wide format
#This will be done as before using the "gather()" function
#This new dataset will be called "my_data_all_long"
my_data_all_long <- gather(my_data_all, "condition", "reaction_time", c("common_word", "rare_word"))

#Now our data is in long format we want to label condition as a factor that could affect reaction time
#We can do this using the as.factor() function
my_data_all_long$condition <- as.factor(my_data_all_long$condition)
#Let's have a look at the structure for this
str(my_data_all_long)

#So let's use the package "ggplot2" (part of the tidyverse package) to visualise this data
#From this data I want to see if people react faster to more common words and whether a persons reaction time to words
#is influenced by their exposure to literature
#First I will create a violin plot comparing peoples reactions times to common and rare words
#This requires the package Hmisc so lets turn that on
library(Hmisc)
#Now that's on let's make a violin plot.
my_data_all_long %>%
  group_by(condition) %>%
  ggplot(aes(x = condition, y = reaction_time, colour = condition)) +
  geom_violin() +
  geom_jitter(alpha = .5, position = position_jitter(0.05)) +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) +
  labs(title = "Reaction Times to Different Types of Words", x = "Word Type", y = "Reaction Time (ms)")

#We can also visualise the data using a boxplot
my_data_all_long %>%
  group_by(condition) %>%
  ggplot(aes(x = condition, y = reaction_time, colour = condition)) +
  geom_boxplot() +
  guides(colour = FALSE) +
  labs(title = "Reaction Times to Different Types of Words", x = "Word Type", y = "Reaction Time (ms)")

#Through these visualisations we can see people's reaction times to common words were typically faster

#However, a particpants engagement with literature may affect their reaction time to common or rare words (i.e.
#engagment with literature may be acting as a covariate)
#As a result, we will want to remove this co-variate when doing an ANOVA test to determine if reaction time to a word
#is influenced by whether the word is common or rare
#This can be done using an ANCOVA test
#This uses the afex package so we need to turn that on
library(afex)

#Now the emmeans package is turned on, we can build a model which we will perform the ANCOVA on
model <- aov_4(reaction_time ~ literature + condition + (1 + condition | participant), data = my_data_all_long, factorize = FALSE)
#Now using this model, we can perform the ANCOVA test
anova(model)

#We can see that when covarying for literature engagement, the condition does not have a significant effect on the
#reaction time as F<1. 
#This means that whether the word is common or rare does not affect the reaction time to the world when eliminating
#literature engagement as a covariant.

#This may be due to the experiment being underpowered?
#Maybe I can do this?