olympics <- olympics <- read_excel("C://Users//johan/Desktop//Upskill//Olympics_data (1).xlsx")
olympics

olympics %>%
  arrange(Year)

olympics %>%
  arrange(Games)

olympics %>%
  filter(Year == 2008)

olympics %>%
  filter(Team == 'Great Britain')

olympics %>%
  select(Team,Sport,Medal)

olympics$Age

mean(olympics$Age)

olympics <- olympics %>%
  mutate(BMI = (olympics$Weight/olympics$Height/olympics$Height)*10000)
olympics

olympics %>%
  group_by(Sex) %>%
  summarise(meanage = mean(age))

olympics %>%
  group_by(Sex) %>%
  summarise(meanheight = mean(Height))

olympics %>%
  group_by(Sex) %>%
  summarise(meanweight = mean(Weight))

olympics %>%
  group_by(Team) %>%
  summarise(meanaget = mean(Age))

olympics %>%
  group_by(Team,Sex) %>%
  summarise(meanagets = mean(Age))

olympics %>% 
  group_by(Sex)%>% 
  count(Medal)

olympics %>% 
  group_by(Team)%>% 
  count(Medal)

x<-c(2,4,NA,6,NA)
is.na(x)
x <- na.omit(x)

x <- c(78, 8, 7, NA, NA)
is.na(x)

x <- na.omit(x)



x <- c(78, 8, 7, NA, NA)
install.packages("imputeTS")
library(imputeTS)
imputeTS::na_mean(x)
imputeTS::na_locf(x)

ggplot(data = olympics, aes(x = Height, y = Weight)) +
  geom_point(size = 2, alpha = 0.5, fill="steelblue") +
  labs(x = "Height of players",
       y = "Weight of players")

ggplot(data = olympics, aes(x = Height, y = Weight)) +
  geom_point(fill="steelblue")



### Histogram
ggplot(data = olympics, aes(x = Age)) + 
  geom_histogram(fill="steelblue")

ggplot(data=olympics, aes(x=Team, y=Medal)) +
  geom_bar(stat="identity")


#### Barchart ####
Medal_Gender <- olympics %>% 
  group_by(Sex)%>% 
  count(Medal)

ggplot(Medal_Gender, aes(x=Sex, y=n, fill=Medal)) + 
  geom_bar(stat="identity") +
  scale_fill_grey()


### Formatting ###
ggplot(Medal_Gender, aes(x=Sex, y=n, fill=Medal)) + 
  geom_bar(stat="identity") +
  ggtitle("Total Medal by Gender") +
  xlab("Gender") +
  ylab("Total Medals")


#### Changing colors ####

ggplot(Medal_Gender, aes(x=Sex, y=n, fill=Medal)) + 
  geom_bar(stat="identity") +
  ggtitle("Total Medal by Gender") +
  xlab("Gender") +
  ylab("Total Medals")+
  scale_fill_manual(values=c("gold4","gold1","gray70"))

####Activity 7#####
## Medals by each country ###
Medal_team <- olympics %>% 
  group_by(Team)%>% 
  count(Medal)

ggplot(Medal_team, aes(x=Team, y=n, fill=Medal)) + 
  geom_bar(stat="identity") +
  ggtitle("Total Medals won by Countries") +
  xlab("Countries") +
  ylab("Total Medals")+
  scale_fill_manual(values=c("gold4","gold1","gray70"))

##### Facet #######
ggplot(Medal_Gender, aes(x = Sex, y = n)) + 
  geom_bar(stat="identity") + 
  facet_grid(cols = vars(Medal))+
  ggtitle("Total Medals by Gender") +
  xlab("Gender") +
  ylab("Total Medals")
