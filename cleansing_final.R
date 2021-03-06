library(dplyr)

elast_df <- read.csv('elasticity.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hc_df <- read.csv('hatecrimes.csv', header = TRUE, fileEncoding='UTF-8-BOM')
police_df <- read.csv('police_killings.csv', header = TRUE, fileEncoding='UTF-8-BOM')
state_map <- read.csv('mapping.csv', header = TRUE, fileEncoding='UTF-8-BOM')
gun_df <- read.csv('RAND_gun_ownership.csv', header = TRUE, fileEncoding='UTF-8-BOM')
fbi_2019 <- read.csv('2019_fbi.csv', header = TRUE, fileEncoding='UTF-8-BOM')

pol_count <- police_df %>% count(state)

pol_count

gun_df <- gun_df[gun_df$Year == 2016,]

gun_df

elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

elast_df<-rename(elast_df, state_abbrev=state)
hc_df<-rename(hc_df, state_full=state)
police_df<-rename(police_df, state_abbrev=state)
gun_df<-rename(gun_df, state_full=STATE)
pol_count<-rename(pol_count, state_abbrev=state)
pol_count<-rename(pol_count, pk_count=n)

colnames(elast_df) # state = abbrev
colnames(hc_df) # state = full
colnames(police_df) # state = abbrev
colnames(state_map)
colnames(pol_count)

hc_df<-left_join(hc_df, state_map, by = 'state_full')
hc_df<-left_join(hc_df, elast_df, by = 'state_abbrev')
hc_df<-left_join(hc_df, gun_df, by = 'state_full')
hc_df<-left_join(hc_df, pol_count, by = 'state_abbrev')
hc_df<-left_join(hc_df, fbi_2019, by = 'state_full')

police_df<-left_join(police_df, state_map, by = 'state_abbrev')

elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

police_df

df_full<-left_join(police_df, hc_df, by = 'state_full')

df_full
df_full<-df_full[ , !(names(df_full) %in% c('state_abbrev.x', 'confederate.x', 'confederate.y', 'state_abbrev.y'))]


hc_df[1,] # state = full
hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]

is.factor(hc_df$confederate)
hc_df$confederate <- factor(hc_df$confederate, levels = c(0, 1), labels = c('No', 'Yes'))

is.factor(hc_df$universl)
hc_df$universl <- factor(hc_df$universl, levels = c(0, 1), labels = c('No', 'Yes'))

is.factor(hc_df$permit)
hc_df$permit <- factor(hc_df$permit, levels = c(0, 1), labels = c('No', 'Yes'))

hc_df$pk_count[is.na(hc_df$pk_count)] <- 0
hc_df$pk_percap <- hc_df$pk_count / hc_df$population

#hc_df$incidents <- as.numeric(hc_df$incidents)
#hc_df$pop_covered <- as.numeric(hc_df$pop_covered)

hc_df$fbi_2019_per100k <- hc_df$incidents/hc_df$pop_covered*100000

hc_full_df<-hc_df

# add hate group count data. Matt, maybe put the CSV read in at the top with the rest and kind of standardize everything?

# Add in the 2016 Data
hate_group_2016_df <- read.csv('hate_group_count_2016.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2016_df<-rename(hate_group_2016_df, hate_group_count_2016=HateGroupCount)
hate_group_2016_df<-rename(hate_group_2016_df, state_full=State)
#hate_group_2016_df <- hate_group_2016_df[ -c(1)]

hc_full_df<-left_join(hc_full_df, hate_group_2016_df, by = 'state_full')

hc_full_df$hate_group_count_2016[is.na(hc_full_df$hate_group_count_2016)] <- 0

# Add in the 2019 Data
hate_group_2019_df <- read.csv('hate_group_count_2019.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2019_df<-rename(hate_group_2019_df, hate_group_count_2019=HateGroupCount)
hate_group_2019_df<-rename(hate_group_2019_df, state_full=State)
#hate_group_2019_df <- hate_group_2019_df[ -c(1)]

hc_full_df<-left_join(hc_full_df, hate_group_2019_df, by = 'state_full')

hc_full_df$hate_group_count_2019[is.na(hc_full_df$hate_group_count_2019)] <- 0

hc_full_df<- transform(hc_full_df, con_uni_combo=ifelse(universl == 'No' & confederate == 'No',
                       'Neither', 
                       ifelse(universl == 'No' & confederate == 'Yes', 
                       'Confederate Only',
                       ifelse(universl == 'Yes' & confederate == 'No',
                       'Background Check Only', 
                       'Both'))))


write.csv(hc_full_df, 'hate_crimes_full_vfin.csv', row.names=F)

# we don't use the DF below. Maybe we should remove?

write.csv(police_df, 'police_killings_full.csv', row.names=F)