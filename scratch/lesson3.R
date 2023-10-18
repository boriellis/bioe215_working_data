#Oct 18, 2023
#This script is working through the course for week 3 of Max's BIOE215 on tidyverse



# setting up --------------------------------------------------------------


#load tidyverse
library(tidyverse)


#load the data 
#(you downloaded this to your data folder using "download.file("https://ndownloader.figshare.com/files/2292169","data/portal_data_joined.csv")")
surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)
summary(surveys)

#Q1 - What’s the type of column species_id? Of hindfoot_length?
# species_id is character
# hindfoot_length is double

#Q2: How many rows and columns are in surveys?
#34786 rows, 13 cols



# selecting and filtering rows --------------------------------------------
#Many tidyverse functions let you treat columns in a data frame as variables. In the following sections, we’re not putting column names in quotes.

#keep just a few columns:
select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, species_id, weight_g = weight)

#remove specific columns
select(surveys, -record_id, -species_id)

#ask for certain rows based on some criteria
filter(surveys, year == 1995)
filter(surveys, year == 1995, plot_id == 7)
filter(surveys, month == 2 | day == 20)

#Q3: filter() surveys to records collected in November where hindfoot_length is greater than 36.0

filter(surveys, month == 11 & hindfoot_length > 36.0)

#Q4: Fix these errors:

filter(surveys, year = 1995) #fix by adding a second = :
filter(surveys, year == 1995)

filter(surveys, polt_id == 2) #polt should be plot:
filter(surveys, plot_id == 2)



# pipes -------------------------------------------------------------------

#pipes help with readability for transformations:

# Filter then select without pipes, two ways
# First way
select(filter(surveys, year == 1995), plot_id, species_id, weight)
# Second way
surveys_1995 <- filter(surveys, year == 1995)
surveys_psw <- select(surveys_1995, plot_id, species_id, weight)

#with pipes! read pipes as "then"
surveys_psw <- surveys %>% 
  filter(year == 1995) %>% 
  select(plot_id, species_id, weight)
#tasty huh

#Q5: Use pipes to subset surveys to animals collected before 1995 retaining just the columns year, sex, and weight

survey_ysw <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)


# Add/change columns with mutate ------------------------------------------

#Add a column
surveys %>% 
  mutate(weight_kg = weight / 1000)

#Add multiple columns based on each other
surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

#So many NAs! Add a filter before mutate. (think this is cutting out the rows that don't have weights in the observations)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

#Convert year, month, and day to dates

surveys %>% 
  select(year, month, day) %>% 
  mutate(date_str = paste(year, month, day, sep = "-"),
         date = as.Date(date_str))
#Q6: Create a new data frame from the surveys data that meets the following criteria: contains only the species_id column and a new column called hindfoot_cm containing the hindfoot_length values (currently in mm) converted to centimeters. In this hindfoot_cm column, there are no NAs and all values are less than 3.

#Hint: think about how the commands should be ordered to produce this data frame!

surveys_new <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length/10) %>% 
  filter(hindfoot_cm<3) %>% 
  select(species_id, hindfoot_cm)
  
# I would like to note that this was fun and I am proud of myself



# Split-apply-combine with summarize --------------------------------------

#What’s the average weight of the observed animals by sex?
surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))
#think 42.2 for F, 43.0 for M, and 64.7 for unidentified which seems highly suspect

#Group by multiple columns, e.g. sex and species.

surveys %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))
#why are there still a bunch of NAs? think just bc some spp weren't weighed


#note: our output is still grouped by species_id. By default, summarize() only removes one level of grouping. This usually leads to unexpected results. As the warning suggests, use .groups to drop all groups.

surveys %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            .groups = "drop")

#NaN is the result of calling mean() on an empty vector. Let’s remove NAs before summarizing.

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            .groups = "drop")

#Can also generate multiple summaries per group.
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop")

# Sort with arrange -------------------------------------------------------

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>% 
  arrange(desc(mean_weight))


# Utility functions -------------------------------------------------------

#count() is a shortcut to getting the size of groups
surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

#drop_na() is a shortcut for removing rows with missing values
surveys %>% 
  drop_na(weight, sex) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight),
            .groups = "drop") %>% 
  arrange(desc(mean_weight))

#Q7: How many animals were caught in each plot_type surveyed?
surveys %>% 
  count(plot_type)
#Control                   15611
#Long-term Krat Exclosure   5118
#Rodent Exclosure           4233
#Short-term Krat Exclosure  5906
#Spectab exclosure          3918

#Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). Also add the number of observations (hint: see ?n).
surveys %>% 
  drop_na(species_id, hindfoot_length) %>% 
  group_by(species_id) %>% 
  summarize(meanfootl = mean(hindfoot_length),
            minfootl = min(hindfoot_length),
            maxfootl = max(hindfoot_length),
            n_obs = n(), #using n within summarize works, I tried it outside using mutate which was wrong (counted # groups instead of # obs)
            .groups = "drop") 

  #Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.

surveys %>% 
  drop_na(weight) %>% 
  group_by(species_id, year, genus) %>% #need to include genus here to get it to stay
  select(year, genus, species_id, weight) %>% 
  summarize(max_weight = max(weight), 
            .groups = "drop") %>% 
  arrange(year, desc(max_weight))  #can use multiple columns in arrange to sort sequentially
  



# Joining data ------------------------------------------------------------

#can join columns with left_join(), inner_join()

#Say we have more information about some of our taxa:
count(surveys, taxa)
taxa_iucn <- data.frame(
  taxa = c("Bird", "Rabbit", "Rodent"),
  iucn = c("NT", "LC", "LC")
)
taxa_iucn

#Left join surveys with taxa info by their shared column (the “key”)
surveys_iucn <- left_join(surveys, taxa_iucn, by = "taxa")
head(surveys_iucn)

#Q10: How many records were there for NT and LC taxa?

surveys_iucn %>% 
  drop_na(iucn) %>% 
  count(iucn)
#34322 LS
#450 NT

#left_join() keeps everything from the left. So surveys_iucn has NAs for iucn when taxa wasn’t in taxa_iucn.
#Q11: What kind of taxa do the NAs in iucn correspond to?
surveys_iucn %>% 
  filter(is.na(iucn)) %>% 
  count(taxa)
#all reptiles

#inner_join() only keeps records where the key is in both tables.
surveys_iucn2 <- inner_join(surveys, taxa_iucn, by = "taxa")


#Q12: How many rows are in surveys_iucn2? What rows are in surveys_iucn that aren’t in surveys_iucn2?

nrow(surveys_iucn2) #34772, the missing 14 rows are the reptiles, but according to google anti_join() is the way to do this neatly if I want to see all the missing rows between the two!!
anti_join(surveys_iucn,surveys_iucn2) #sick








