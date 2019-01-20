#This file contains functions to create visualizations on blight, demolitions, and 
#side lot sales in Detroit, Michigan. 


library(tidyverse)
library(here)
library(ggrepel)
library(magrittr)

#read in csv data
blight_tix <- read_csv(here('detroit_data','Blight_Violations.csv'))
side_lot_sales <- read_csv(here('detroit_data','Side_Lot_Sales.csv'))
past_demolitions <- read_csv(here('detroit_data', 'Detroit_Demolitions.csv'))
land_inv <- read_csv(here('detroit_data', 'Land_Bank_Inventory.csv'))



##BLIGHT
                           
#lollipop: Violation description, frequency, and fine amount is size
blight_tix_graph <- blight_tix %>%
  filter(!is.na(`Fine Amount`)) %>%
  mutate(`Violation Description` = gsub("(.{31,}?)\\s", "\\1\n", `Violation Description`)) %>%
  mutate(`Violation Date` = as.Date(`Violation Date`,format = "%m/%d/%Y")) %>%
  mutate(year = `Violation Date`) %>%
  mutate(year = as.numeric(strftime(year, '%Y'))) %>%
  arrange(year) %>%
  filter(year >= 2006) %>%
  group_by(`Violation Description`, `Violation Code`) %>%
  summarize(count = n(), amount = mean(`Fine Amount`)) %>%
  arrange(amount) %>%
  filter(count > 10000)

blight_plot <- blight_tix_graph %>%
  ggplot(aes(x=factor(`Violation Description`, levels = blight_tix_graph$`Violation Description`), y=amount)) +
  geom_segment(aes(x=factor(`Violation Description`, levels = blight_tix_graph$`Violation Description`), xend=(factor(`Violation Description`, levels = blight_tix_graph$`Violation Description`)), y=0, yend=amount), color = 'gray', size=1.25) +
  geom_point(stat='identity', color='orange',size=3) + 
  geom_text(aes(label = round(amount)), vjust = -1, size=3, alpha=0.5) +
  theme_light() +
  coord_flip() +
  labs(title = 'High Penalties on Waste Pileup in Detroit', subtitle ='Most frequent blight violations since 2006', x = 'Violation Description', caption='Data Source: Detroit Open Data Portal') +
  scale_y_continuous(name = 'Average Fine Amount', breaks = c(0,250,500,750,1000,1250, 1500), labels = c('$0','$250','$500','$750','$1000', '$1250', '$1500'))
  
  
ggsave(here('output','blight.pdf'), plot = blight_plot, dpi = 100, width = 8, height = 5, units = "in")




##Side Lot Sales

side_lot_sales %<>% 
  mutate(`Closing Date` = as.Date(`Closing Date`,format = "%m/%d/%Y")) %>%
  mutate(year = `Closing Date`) %>%
  mutate(year = strftime(year, '%Y'))

neighborhoods_2016 <- side_lot_sales %>%
  group_by(`Neighborhood`, year) %>%
  summarize(count = n()) %>%
  filter(year=='2016') %>%
  filter(count > 50) %>%
  arrange(count)

neighborhoods_2017 <- side_lot_sales %>%
  group_by(`Neighborhood`, year) %>%
  summarize(count = n()) %>%
  filter(year=='2017') %>%
  filter(count > 50) %>%
  arrange(count)

neighborhoods_2018 <- side_lot_sales %>%
  group_by(`Neighborhood`, year) %>%
  summarize(count = n()) %>%
  filter(year=='2018') %>%
  filter(count > 50) %>%
  arrange(count)

all <- intersect(neighborhoods_2016$Neighborhood, neighborhoods_2017$Neighborhood)
neighborhoods <- intersect(all,neighborhoods_2018$Neighborhood)

#line graph: Side lot sales by (select) neighborhood and year
#next: look at this compared to inventory
lot_plot <- side_lot_sales %>%
  filter(`Neighborhood` %in% neighborhoods) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(`Neighborhood`, year) %>%
  filter(year >= 2015) %>%
  filter(year != 2019) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=year, y=count, group=`Neighborhood`)) + 
  geom_line(aes(color=`Neighborhood`)) + 
  theme_light() +
  labs(title = 'Claytown Has the Largest Number of Side Lot Sales', subtitle ='Eight Most Active Regions in the Side Lot Sales Program since 2015', x='Year', y='Number of Side Lots Sold', caption='Data Source: Detroit Open Data Portal')

ggsave(here('output','lot_sales.pdf'), lot_plot, dpi = 100, width = 8, height = 5, units = "in")


#Land Inventory
land_inv %>% 
  filter(`Inventory Status` == 'DLBA Owned Sidelot For Sale') %>% 
  group_by(`Neighborhood`) %>% 
  summarize(count=n()) %>% 
  filter(count > 1000)

for_sale <- land_inv %>%
  group_by(`Inventory Status`, `Council District`) %>%
  filter(`Inventory Status` == 'DLBA Owned Sidelot For Sale') %>%
  summarize(count = n())

for_sale_df <- data.frame('Sale' = for_sale$count, 'District' = for_sale$`Council District`) %>% mutate(District = paste('District', District))

owned <- land_inv %>%
  group_by(`Inventory Status`, `Council District`) %>%
  filter(`Inventory Status` == 'DLBA Owned Vacant Land') %>%
  summarize(count = n())

owned_df <- data.frame('Owned' = owned$count, 'District' = owned$`Council District`) %>% mutate(District = paste('District', District))

sale_owned <- merge(for_sale_df, owned_df, by='District')


district_owned_sale_plot <- sale_owned %>%
  ggplot(aes(x=Sale, y=Owned, group=District)) +
  geom_point(aes(color = District, size=Owned/Sale)) + 
  scale_size(range=c(3,10), breaks = c(1,1.5,2,2.5,3)) + #to manage size of bubbles 
  labs(title = 'District 5 (Downtown Detroit) Owns the Most Vacant Land in Proportion to Sidelots for Sale', subtitle ='Proportion of Owned Vacant Lots to Sidelots for Sale by District', y = 'Number of Owned Vacant Land', x= 'Number of Sidelots for Sale', caption='Data Source: Detroit Open Data Portal', size = 'Proportion of Owned Land \nto Lots For Sale') +
  theme_light()


ggsave(here('output','sales_owned_district.pdf'), plot = district_owned_sale_plot, dpi = 100, width = 9, height = 5, units = "in")





##Exploratory work, please ignore

#bubble
blight_tix %>%
  filter(!is.na(`Fine Amount`)) %>%
  group_by(`Violation Description`, `Violation Code`) %>%
  summarize(count = n(), amount = mean(`Fine Amount`)) %>%
  arrange(count) %>%
  filter(count > 10000) %>%
  ggplot(aes(x=factor(`Violation Description`, levels=unique(blight_tix$`Violation Description`)), y=count, size=amount)) + 
  geom_point(stat='identity', color='orange') + 
  theme_light() +
  coord_flip() +
  scale_size(range=c(3,10), breaks = c(50, 100, 250, 500, 1000)) + #to manage size of bubbles 
  labs(title = '', subtitle ='', caption='Data Source: Detroit Open Data Portal', size = 'Fine Amount ($)')

#payment status - lots of nas
z<-blight_tix %>% group_by(blight_tix$`Violation Description`, blight_tix$`Payment Status`)
summarize(z,count=n())



##Side Lot Sales

side_lot_sales %>%
  group_by(`Neighborhood`,year) %>%
  filter(`Neighborhood`=='Claytown') %>%
  summarize(count=n())


#Demolitions

#demolitions by neighborhood vs side lot sales

past_demolitions %<>% 
  mutate(`Demolition Date` = as.Date(`Demolition Date`,format = "%m/%d/%Y")) %>%
  mutate(year = `Demolition Date`) %>%
  mutate(year = strftime(year, '%Y'))

parcels <- past_demolitions$`Parcel ID`

side_lots_parcels <- side_lot_sales %>% 
  filter(`Parcel ID` %in% parcels)

demolitions_parcels <- past_demolitions %>% 
  filter(`Parcel ID` %in% side_lots_parcels$`Parcel ID`)

#Demolition date to sell date (over time)
df_d <- data.frame('ID'=demolitions_parcels$`Parcel ID`, 'Demolition Date' = demolitions_parcels$`Demolition Date`)

df_s <- data.frame('ID'=side_lots_parcels$`Parcel ID`, 'Closing Date'=side_lots_parcels$`Closing Date`,'Neighborhood' = side_lots_parcels$Neighborhood, 'year' = side_lots_parcels$year)


x<-merge(df_d,df_s,by='ID')

x %<>% 
  mutate(time.sell = Closing.Date - Demolition.Date) %>%
  mutate(time.sell = as.numeric(time.sell)) %>%
  filter(time.sell > 0) #remove negative outliers

#graph: days to sell property from demolition, using neighborhoods from side lot sales
#how to make this clearer?
x %>% filter(Neighborhood %in% neighborhoods) %>%
  ggplot(aes(x=Demolition.Date, y = time.sell)) + 
  geom_point(aes(group=Neighborhood, color = Neighborhood), alpha = 0.5) + geom_smooth(color = 'green', se=F) +
  labs(title = '', subtitle = '', caption='Data Source: Detroit Open Data Portal')

x %>% 
  ggplot(aes(x=Demolition.Date, y = time.sell)) + 
  geom_point(aes(group=Neighborhood)) + geom_jitter()+ geom_smooth(color = 'green', se=F) +
  labs(title = '', subtitle = '', caption='Data Source: Detroit Open Data Portal')

past_demolitions %>%
  filter(`Commercial Building` == 'No') %>%
  ggplot(aes(x=`Demolition Date`, y = `Price`))+
  geom_point(aes(group=`Council_District`)) + geom_jitter()

#Land Inventory

for_sale_neighborhood <- land_inv %>% 
  group_by(`Inventory Status`, `Neighborhood`) %>% 
  filter(`Inventory Status` == 'DLBA Owned Sidelot For Sale') %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 300)

owned_neighborhood <- land_inv %>% 
  group_by(`Inventory Status`, `Neighborhood`) %>% 
  filter(`Inventory Status` == 'DLBA Owned Vacant Land') %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 400)
  

