#STAT 6021 Project 2 (Hate Crimes in America)
#David Fuentes, Matt Villhauer, Matt Nicklas
#Using data from FiveThirtyEight, RAND Corp, and other sources to see which variables contribute to hate crimes measured as rate per 100K residents 

# #########################################################################
# DATA CLEANSING
# #########################################################################

library(dplyr)

#Read in csv files we will combine into one large csv
elast_df <- read.csv('elasticity.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hc_df <- read.csv('hatecrimes.csv', header = TRUE, fileEncoding='UTF-8-BOM')
police_df <- read.csv('police_killings.csv', header = TRUE, fileEncoding='UTF-8-BOM')
state_map <- read.csv('mapping.csv', header = TRUE, fileEncoding='UTF-8-BOM')
gun_df <- read.csv('RAND_gun_ownership.csv', header = TRUE, fileEncoding='UTF-8-BOM')
fbi_2019 <- read.csv('2019_fbi.csv', header = TRUE, fileEncoding='UTF-8-BOM')

#Count the number of citizen deaths at the hands of the police
pol_count <- police_df %>% count(state)
pol_count

#Filter RAND data for 2016
gun_df <- gun_df[gun_df$Year == 2016,]
gun_df

elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

#Standardize the state-name columns to be the same so join will work 
elast_df<-rename(elast_df, state_abbrev=state)
hc_df<-rename(hc_df, state_full=state)
police_df<-rename(police_df, state_abbrev=state)
gun_df<-rename(gun_df, state_full=STATE)
pol_count<-rename(pol_count, state_abbrev=state)
pol_count<-rename(pol_count, pk_count=n)

#Check columns
colnames(elast_df) 
colnames(hc_df) 
colnames(police_df) 
colnames(state_map)
colnames(pol_count)

#join the data frames into one called hc_df adding on each column
hc_df<-left_join(hc_df, state_map, by = 'state_full')
hc_df<-left_join(hc_df, elast_df, by = 'state_abbrev')
hc_df<-left_join(hc_df, gun_df, by = 'state_full')
hc_df<-left_join(hc_df, pol_count, by = 'state_abbrev')
hc_df<-left_join(hc_df, fbi_2019, by = 'state_full')

#Create police killing DF
police_df<-left_join(police_df, state_map, by = 'state_abbrev')

elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

police_df

#Join all data
df_full<-left_join(police_df, hc_df, by = 'state_full')
df_full

#Remove the extra columns with redundant information, e.g. confederate and state_abbrev, which come from multiple dataframes
df_full<-df_full[ , !(names(df_full) %in% c('state_abbrev.x', 'confederate.x', 'confederate.y', 'state_abbrev.y'))]

# #########################################################################

hc_df[1,] # state = full
hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]

#Factor and add levels to binary data
is.factor(hc_df$confederate)
hc_df$confederate <- factor(hc_df$confederate, levels = c(0, 1), labels = c('No', 'Yes'))

is.factor(hc_df$universl)
hc_df$universl <- factor(hc_df$universl, levels = c(0, 1), labels = c('No', 'Yes'))

is.factor(hc_df$permit)
hc_df$permit <- factor(hc_df$permit, levels = c(0, 1), labels = c('No', 'Yes'))

#calc police killings per capita
hc_df$pk_count[is.na(hc_df$pk_count)] <- 0
hc_df$pk_percap <- hc_df$pk_count / hc_df$population

#hc_df$incidents <- as.numeric(hc_df$incidents)
#hc_df$pop_covered <- as.numeric(hc_df$pop_covered)

#add 2019 FBI hate-crime data
hc_df$fbi_2019_per100k <- hc_df$incidents/hc_df$pop_covered*100000

hc_full_df<-hc_df

# #########################################################################
# This section adds in another two csv files containing hate-group counts per state for 2016 and 2019 and joins to the large dataframe above 

# Add in the 2016 Data
hate_group_2016_df <- read.csv('hate_group_count_2016.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2016_df<-rename(hate_group_2016_df, hate_group_count_2016=HateGroupCount)
hate_group_2016_df<-rename(hate_group_2016_df, state_full=State)

hc_full_df<-left_join(hc_full_df, hate_group_2016_df, by = 'state_full')

hc_full_df$hate_group_count_2016[is.na(hc_full_df$hate_group_count_2016)] <- 0

# Add in the 2019 Data
hate_group_2019_df <- read.csv('hate_group_count_2019.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2019_df<-rename(hate_group_2019_df, hate_group_count_2019=HateGroupCount)
hate_group_2019_df<-rename(hate_group_2019_df, state_full=State)

# #########################################################################

#Adds the hategroupcount as a column on the end of the large combined dataframe
hc_full_df<-left_join(hc_full_df, hate_group_2019_df, by = 'state_full')

hc_full_df$hate_group_count_2019[is.na(hc_full_df$hate_group_count_2019)] <- 0

#Creates a column that tells you wether a state was confederate, has a background check for guns, is neither, or is both
hc_full_df<- transform(hc_full_df, con_uni_combo=ifelse(universl == 'No' & confederate == 'No',
                                                        'Neither', 
                                                        ifelse(universl == 'No' & confederate == 'Yes', 
                                                               'Confederate Only',
                                                               ifelse(universl == 'Yes' & confederate == 'No',
                                                                      'Background Check Only', 
                                                                      'Both'))))

#Finally write to csv that is used in a seperate R file with all of our calcualtions and analysis
write.csv(hc_full_df, 'hate_crimes_full_vfin.csv', row.names=F)













