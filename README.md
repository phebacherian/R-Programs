# R-Programs
My R projects 
library(dplyr)
library(stringi)
library(dplyr)
library(ggplot2)
library(ggpubr)
##  Import data into R environment.
ComcastData <- read.csv((choose.files())
                        , stringsAsFactors=FALSE)
str(ComcastData)

names(ComcastData)
## option 1
names(ComcastData) <- stri_replace_all(regex = "\\.",replacement = "",str =names(ComcastData))
head(ComcastData)
summary(ComcastData)
str(ComcastData)
sapply(ComcastData, function(x) sum(is.na(x)))
## 2. Provide the trend chart for the number of complaints at monthly and daily
# granularity levels.

str(ComcastData)
#ComcastData$DateNew<- gsub(pattern = "/",replacement = "-",ComcastData$Date)

ComcastData$Date1 <- as.Date(x = ComcastData$Date,format = "%d-%m-%Y")
ComcastData$Date2 <- as.Date(x = ComcastData$Date,format = "%m/%d/%Y")
ComcastData$Date_Final <- ifelse(is.na(ComcastData$Date1),as.character(ComcastData$Date2),
                                 as.character(ComcastData$Date1))
ComcastData$Date <- NULL
ComcastData$Date1 <- NULL
ComcastData$Date2 <- NULL
ComcastData$Date <- lubridate::ymd(ComcastData$Date_Final)
ComcastData$Date_Final <- NULL
library(dplyr)

ggplot(data = daily_count,aes(as.POSIXct(Date),count))+
  geom_line(color = "blue")+
  
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Complaint Count",x= "Days",y ="No. of Complaints")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 30),
        plot.title = element_text(hjust = 0.5))
##frequency of complaint type

network_issue <- contains(ComcastData$CustomerComplaint,match = 'network',ignore.case = T)

internet_issue<- contains(ComcastData$CustomerComplaint,match = 'internet',ignore.case = T)

billing_issue <- contains(ComcastData$CustomerComplaint,match = 'bill',ignore.case = T)

email_issue <- contains(ComcastData$CustomerComplaint,match = 'email',ignore.case = T)

charges_issue <- contains(ComcastData$CustomerComplaint,match = 'charge',ignore.case = T)

ComcastData$ComplaintType[internet_issue] <- "Internet"
ComcastData$ComplaintType[network_issue] <- "Network"
ComcastData$ComplaintType[billing_issue]<- "Billing"
ComcastData$ComplaintType[email_issue] <- "Email"
ComcastData$ComplaintType[charges_issue] <- "Charges"
ComcastData$ComplaintType[-c(internet_issue,network_issue,
                             billing_issue,charges_issue,email_issue)] <- "Others"
freq_tab <- data.frame(table(ComcastData$ComplaintType))

##complaint types are maximum 
freq_tab[freq_tab$Freq == max(freq_tab$Freq),]

##CREATING A NEW OPEN_CLOSE VARIABLE 

ComcastData$Open_Closed <- ifelse(ComcastData$Status == "Open"|
                                    ComcastData$Status == "Pending", "Open","Closed")

#STACKED BAR CHART FOR COMPLAINT STATUS

chart_data <- ComcastData %>% group_by(State,Open_Closed) %>%
  summarise(Count = n())

ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = Open_Closed),width = 0.95)+
  theme_light() +
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Complaint Status Stacked Bar Chart ",
       x = "States",y = "No of Complaints",
       fill= "Status")


##State having maximum complaint
ComcastData%>% group_by(State) %>%
  summarise(no_of_comp = n()) %>%
  arrange(desc(no_of_comp)) %>%
  filter(no_of_comp == max(no_of_comp))

##state having high percentage  of unresolved complaints

high_comp_open <- chart_data %>% filter(Open_Closed == "Open") %>%
  arrange(desc(Count))
high_comp_open$per_of_unresolved <- round((high_comp_open$Count/sum(high_comp_open$Count))*100)
high_comp_open[high_comp_open$per_of_unresolved == max(high_comp_open$per_of_unresolved),]
## percentage of complaints resolved till date, which were received through the Internet and customer care cells
resolved_till_date <- ComcastData %>%
  group_by(ReceivedVia,Open_Closed) %>%
  summarise(count = n())
resolved_till_date$percentage <- round((resolved_till_date$count/sum(resolved_till_date$count))*100)
resolved_till_date <- filter(resolved_till_date, Open_Closed == "Closed")
resolved_till_date
install.packages("latexpdf")
library(latexpdf)
