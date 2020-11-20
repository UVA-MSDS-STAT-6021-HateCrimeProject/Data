library(plyr) # to rename columns
library(tidyverse)

elast_df <- read.csv('elasticity.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hc_df <- read.csv('hatecrimes.csv', header = TRUE, fileEncoding='UTF-8-BOM')
police_df <- read.csv('police_killings.csv', header = TRUE, fileEncoding='UTF-8-BOM')
state_map <- read.csv('mapping.csv', header = TRUE, fileEncoding='UTF-8-BOM')


elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

elast_df<-rename(elast_df, state_abbrev=state)
hc_df<-rename(hc_df, state_full=state)
police_df<-rename(police_df, state_abbrev=state)

colnames(elast_df) # state = abbrev
colnames(hc_df) # state = full
colnames(police_df) # state = abbrev
colnames(state_map)

#elast_df<-left_join(elast_df, state_map, by = 'state_abbrev')
hc_df<-left_join(hc_df, state_map, by = 'state_full')
hc_df<-left_join(hc_df, elast_df, by = 'state_abbrev')
police_df<-left_join(police_df, state_map, by = 'state_abbrev')

elast_df[1,] # state = abbrev
hc_df[1,] # state = full
police_df[1,] # state = abbrev

df_full<-left_join(police_df, hc_df, by = 'state_full')


df_full<-df_full[ , !(names(df_full) %in% c('state_abbrev.x', 'confederate.x', 'confederate.y', 'state_abbrev.y'))]


hc_df[1,] # state = full
hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]

is.factor(hc_df$confederate)
hc_df$confederate <- factor(hc_df$confederate, levels = c(0, 1), labels = c('No', 'Yes'))

write.csv(hc_df, 'hate_crimes_full.csv', row.names=F)
write.csv(police_df, 'police_killings_full.csv', row.names=F)
