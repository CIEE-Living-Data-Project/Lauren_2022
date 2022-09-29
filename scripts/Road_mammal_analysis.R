### My Data Analysis Project

### Loading packages ---------------------
# Wont be using groundhog, just normal library loading
library(tidyverse)
library(rdryad)
library(skimr)


### Downloading the necessary data -------------------------

# doi : https://doi.org/10.5061/dryad.bcc2fqzd1
rdryad::dryad_get_cache()

# setting custom cache location
rdryad_cache <- rdryad::dryad_get_cache()
rdryad_cache$cache_path_set(full_path = normalizePath("//Users/laurengill/Github/Lauren_2022/data", mustWork = FALSE))
# mustWork = F because path does not (yet) exist
rdryad_cache$cache_path_get()

rdryad::dryad_download(dois = "10.5061/dryad.bcc2fqzd1")

### Reading in the data ---------------------
roadmammal <- read_delim("data/10_5061_dryad_bcc2fqzd1/RawData_JournalSubmission_Final.csv", delim = ";")

### Exploring the data
dim(roadmammal) # [1] 770 rows (observations) and 26 columns (variables)
head(roadmammal) # view the first 6 rows 
names(roadmammal) # viewing the column names
skim(roadmammal) # a summary
str(roadmammal)


## Are some animals more likely to be on a dirt or tarmac road?

### Cleaning the data for this analysis --------------------
#presence of animals is indicated either by <6 or >=6 for small herds or large herds of animals
#for this analysis I do not care about herd size, so will covert data to presence/absence (0s and 1s)

roadmammal_clean <- roadmammal %>% 
  select(`ROAD SURFACE `, Buffalo, Elephant, Giraffe, Impala, Kudu, Warthog, Zebra) %>%
  rename(RoadSurface = `ROAD SURFACE `)%>% #making it easier to type without the space
  mutate_at(vars(-c(RoadSurface)), ~replace(., . != 0, 1)) %>% #changing <6 or >=6 to simple '1's to indicate presence
  mutate_all(~replace(., is.na(.), 0)) %>% #changing all NAs to 0s 
  mutate_at(vars(-c(RoadSurface)), as.numeric)


### Making a graph ---------------------
# making a new branch for figures!
rm_sum <- roadmammal_clean %>% 
  group_by(RoadSurface) %>%
  summarise_each(funs(sum)) %>%
  pivot_longer(!RoadSurface, names_to = "Animal", values_to = "Count")

rm_graph <- ggplot(rm_sum, aes(fill = RoadSurface, x = Animal, y = Count)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = 'Animal Herd Count in Kruger National Park on Dirt or Tar Roads') +
  scale_fill_manual(values = c('burlywood4', 'gray25'))+
  theme_classic()
rm_graph


### Complete!









