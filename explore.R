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


test <- blight_tix %>%
  #filter(!is.na(`Payment Status`)) %>%
  mutate(`Violation Date` = as.Date(`Violation Date`,format = "%m/%d/%Y")) %>%
  mutate(year = `Violation Date`) %>%
  mutate(year = as.numeric(strftime(year, '%Y'))) %>%
  filter(!is.na(`Judgment Amount (Total Due)`)) %>%
  filter(`Judgment Amount (Total Due)` > 0) %>%
  mutate(paid = ifelse(`Payment Status` == 'PAID IN FULL', 1, 0)) %>%
  mutate(paid = ifelse(is.na(paid), 0, 1)) %>%
  filter(year > 2004) %>%
  filter(year < 2019) %>%
  group_by(year,paid) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=year, y=count, group=paid)) + geom_area(aes(fill=paid)) +
  labs(title = 'Blight Tickets Are Often Left Unpaid', subtitle = 'Fully Paid and Partially Paid Blight Tickets since 2005', x='Year', y='Number of Tickets') + 
  annotate('text', x=2007, y=4000, label = 'Paid in Full', color = 'white', fontface='bold') + 
  annotate('text', x=2007.5, y=15000, label = 'Partially Paid or Unpaid',color = 'white', fontface='bold') +
  theme_light()


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
  mutate(month = `Demolition Date`) %>%
  mutate(year = strftime(year, '%Y')) %>%
  mutate(month = strftime(month, '%m'))

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
  
#circular bar plot of demolitions over time? (monthly)

past_demolitions %<>% 
  mutate(`Demolition Date` = as.Date(`Demolition Date`,format = "%m/%d/%Y")) %>%
  mutate(month_year = `Demolition Date`) %>%
  mutate(year = strftime(`Demolition Date`, '%Y')) %>%
  filter(year!='2019') %>%
  mutate(month = as.numeric(strftime(`Demolition Date`, '%m'))) %>%
  mutate(month_year = as.Date(paste(strftime(month_year, '%Y-%m-'),'15',sep=''))) %>%
  #mutate(half_year = ifelse(month < 7, paste('1H',year), paste('2H', year))) %>%
  #mutate(half_year = as.factor(half_year)) %>%
  #mutate(half_year = factor(half_year,levels = c('1H 2014', '2H 2014', '1H 2015', '2H 2015', '1H 2016', '2H 2016', '1H 2017', '2H 2017', '1H 2018', '2H 2018')))
  
  
  past_demolitions %>%
  group_by(Council_District, year) %>%
  summarize(count=n(), avg = mean(Price)) %>%
  ggplot(aes(x=Council_District, y=avg, group=year, fill=Council_District, labels=Council_District)) + 
  geom_bar(stat='identity', position = 'dodge') + ylim(-24000, 30000) + coord_polar(start=0) + my_theme


test <- past_demolitions %>%
  group_by(half_year) %>%
  summarize(count=n(), avg=mean(Price)) %>%
  ggplot() +
  geom_bar(aes(x=half_year, y=log(avg)), stat='identity') +
  geom_bar(aes(x=half_year, y=log(count)), stat='identity', color='red')


#stacked area graph of main contractors over time -> by avg price or count? (not much there)

past_demolitions %>%
  group_by(year, `Contractor Name`) %>%
  summarize(count = n(), avg=mean(Price)) %>%
  ggplot(aes(x=year, y=avg, group=`Contractor Name`)) +
  geom_area()


#exploratory: auctions/own it now/ side lots
all_three %>%
  ggplot(aes(x=year, y=count, group=category)) +
  geom_area(aes(fill=category))



geom_bar(stat='identity', position= 'dodge') +
  ylim(-1000000,6000000) +
  coord_polar(start=0) +
  my_theme

ggplot(all_three, aes(x=year, y=total)) + geom_line()+ facet_grid(cols = vars(category)) + my_theme


ggplot(data = all_three, aes(x=count, y = total, group=category, fill=category, size=total/count)) + geom_point() + geom_line()


library(ggridges)

new_a <- auctions %>%
  group_by(category, `Final Sale Price`) %>%
  summarize()

new_b <- own_it_now %>%
  group_by(category, `Final Sale Price`) %>%
  summarize()

new_c <- side_lot_sales %>%
  group_by(category, `Final Sale Price`) %>%
  summarize()

all <- rbind(new_a, new_b)
all <- rbind(all, new_c)

all %>%
  filter(`Final Sale Price` > 0) %>%
  ggplot(aes(x = `Final Sale Price`, y = factor(category, levels = c('Side Lot Sale', 'Own It Now Sale', 'Auctions')), fill = category)) +
  geom_density_ridges() +
  my_theme + scale_y_discrete(expand=(c(0,0))) + xlim(-5000,50000) +
  #theme_ridges() + 
  theme(legend.position = "none") 

own_it_now %>%
  ggplot(aes(x = `Final Sale Price`, y = factor(`Council District`), fill = `Council District`)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  xlim(0,20000)



#blight map remove outside dots

#x<-blight_2018 %>% filter(`Violation Latitude` < 42.3) %>% filter(`Violation Longitude` < -83.2)
#y <- blight_2018 %>% filter((`Violation Longitude` < -83.25) & (`Violation Latitude` < 42.33))

#dots <- data.frame('long' = c(-83.27886,-83.28137,-83.167531573, -83.17150, -83.14558, -82.92131), 'lat' = c(42.35133, 42.36877,42.2512935180001,42.25646,42.30114, 42.39332)) #other points outside of district plot area


#+ geom_point(data = dots, aes(x=X, y=Y), size = 0.25, color = '#ED9007', fill = "#ED9007")
#geom_point(data = blight_2018, aes(x = `Violation Longitude`, y = `Violation Latitude`), size = 0.25, color = '#ED9007', fill = "#ED9007") +
#geom_point(data =x, aes(x = `Violation Longitude`, y = `Violation Latitude`), size = 1, color = "white") +
#geom_point(data =y, aes(x = `Violation Longitude`, y = `Violation Latitude`), size = 1, color = "white") +
#geom_point(data = dots, aes(x = long, y = lat), size = 1, color = "white") +
#geom_sf_label(aes(label = districts), size=2) 



#symbol map of demolitions vs property sold?

c <- st_centroid(x)

ggplot() +
  geom_sf(data=districts_shp) +
  geom_sf(data=c, aes(size=dem, color=total))



f <- here('detroit_data','ACS_17_5YR_S2506','ACS_17_5YR_S2506_with_ann.csv')

homes <- read_csv(f)

homes %<>% select(GEO.id2, HC01_EST_VC10) %>% mutate(zipcode = GEO.id2) %>% mutate(HC01_EST_VC10 = as.numeric(HC01_EST_VC10))

f <- here('detroit_data','City of Detroit Zip Code Boundaries','geo_export_9a7468a0-7334-43b5-9b02-408c4a8a9026.shp')

zips <- st_read(f)


det_zips <- left_join(zips, homes, by = 'zipcode')

ggplot() +
  geom_sf(data=zips) +
  geom_sf(data=det_zips, aes(fill = HC01_EST_VC10)) + scale_fill_gradient(name ='Number of Properties', low = '#31044F', high = '#ED9007')