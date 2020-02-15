#import libs
library(ggplot2)
library(tidyverse)
library(zipcode)
library(mapdata)
library(viridis)
library(gridExtra)
library(grid)
library(lattice)

 

  
#read datasets needed
sba <- read_csv("https://uofi.box.com/shared/static/vi37omgitiaa2yyplrom779qvwk1g14x.csv", 
                col_types = cols(DisbursementGross = col_number(), CreateJob = col_number(), Zip = col_character()))
sba$Zip = clean.zipcodes(sba$Zip)


zipcodes_IL <- read_delim("https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/download/?format=csv&refine.state=IL&timezone=America/Chicago&lang=en&use_labels_for_header=true&csv_separator=%3B", ";")
 

  

 


  
#cleaning sba a little
sba_IL <- subset(sba, sba$State == "IL")
sba_IL$City <- tolower(sba_IL$City)
sba_IL <- summarise_at(group_by(sba_IL, City), vars(DisbursementGross), funs(mean(.,na.rm=FALSE)))
 

  
#assigning quantiles
sba_IL$quantile <- with(sba_IL, cut(DisbursementGross, quantile(DisbursementGross)))
levels(sba_IL$quantile) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
sba_IL$quantile <- as.ordered(sba_IL$quantile)
 

  
#cleaning zipcodes
zipcodes_IL$City <- tolower(zipcodes_IL$City)
zipcodes_IL <- summarise_at(group_by(zipcodes_IL, City), vars(Latitude, Longitude), funs(mean(.,na.rm = FALSE)))
 


  
#merging two datasets
sba_merged <- merge(sba_IL, zipcodes_IL)
 

#assigning sections  
sba_merged$Section = ifelse(sba_merged$Latitude >= 41, "Upper", ifelse(sba_merged$Latitude <= 39, "Middle", "Lower"))

sba_merged$"Mean Disbursement Gross" = sba_merged$DisbursementGross
 


#illinois state outline  
Illinois <- map_data("county") %>% filter(region == "illinois")
 


  
#map section
map <- ggplot() +
  geom_hline(aes(yintercept = 41)) + geom_hline(aes(yintercept = 39)) + 
  geom_polygon(data = Illinois, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=sba_merged, aes(x=Longitude, y=Latitude, size= `Mean Disbursement Gross`,  color= `Mean Disbursement Gross`)) +
  scale_size_continuous(range=c(1,2)) +
  scale_color_viridis(trans="log") +
  theme_void() + coord_map() + guides(size = FALSE) + ggtitle("Mean Disbursement Gross of Illinois Cities \nSplit into Lower, Middle, and Upper Sections") + annotate(geom="text", x=-88, y=41.85169, label="Chicago", color="black")

 
#creating dataset for barplot
sba_merged2 <- summarise_at(group_by(sba_merged, Section), vars(DisbursementGross), funs(mean(.,na.rm=FALSE)))


#bar plot 
bar <- ggplot(data = sba_merged2, aes(x = Section, y = DisbursementGross, fill = Section)) +
  geom_bar(stat = "identity") + ylab("Mean Disbursement Gross in Dollars") + xlab("Section") + theme_minimal() +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +  geom_text(aes(label= trunc(DisbursementGross)), position=position_dodge(width=0.9), vjust=-0.25) + 
  ggtitle("Mean Disbursement Gross Based on Location") + labs(subtitle = "In Dollars")
  
 

#putting together  
grid.arrange(bar, map, layout_matrix = rbind(c(1,1,1,2,2,2,2), c(1,1,1,2,2,2,2)))
 