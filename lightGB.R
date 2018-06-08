

if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
pacman::p_load(data.table,ggplot2,DT,magrittr,corrplot,Rmisc,ggalluvial,caret,ModelMetrics,scales,irlba,forcats,forecast,TSA,zoo)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 5)

tic("Total processing time for feature engineering on training data --->")
train <- fread("train_sample.csv",nrows=10, 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "attributed_time", "is_attributed"), , 
               showProgress = FALSE) %>%
#  select(-c(attributed_time)) %>%
  mutate(wday = Weekday(click_time), hour = hour(click_time)) %>% 
#  select(-c(click_time)) %>%
  add_count(ip, wday, hour) %>% rename("nip_day_h" = n) %>%      #count the total number of combinations
  add_count(ip, hour, channel) %>% rename("nip_h_chan" = n) %>%
  add_count(ip, hour, os) %>% rename("nip_h_osr" = n) %>%
  add_count(ip, hour, app) %>% rename("nip_h_app" = n) %>%
  add_count(ip, hour, device) %>% rename("nip_h_dev" = n) %>%
  select(-c(ip))
toc()

train

#plot count 
h1 <- train %>% group_by(app) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% head(15) %>% mutate(app = as.character(app)) %>%
  hchart("bar", hcaes(x = app, y = count, color =-count)) %>%
  hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top Apps")
h1


##alternative
train_1 <- fread("train_sample.csv", showProgress=F)
#test <- fread("../input/test.csv", nrows=1e5, showProgress=F)
#subm <- fread("../input/sample_submission.csv", nrows=1e5, showProgress=F)

fea <- c("os", "channel", "device", "app", "attributed_time", "click_time", "ip","year","month","days","hour")
##add attributes to represent sepecfic zones
train_1$click_time<-as.POSIXct(train_1$click_time,
                             format = "%Y-%m-%d %H:%M",tz = "America/New_York")
train_1$year=year(train_1$click_time)
train_1$month=month(train_1$click_time)
train_1$days=weekdays(train_1$click_time)
train_1$hour=hour(train_1$click_time)
head(train_1)
###
###plot summary
train_1[, lapply(.SD, uniqueN), .SDcols = fea] %>%
  melt(variable.name = "features", value.name = "unique_values") %>%
  ggplot(aes(reorder(features, -unique_values), unique_values)) +
  geom_bar(stat = "identity", fill = "darkgreen") + 
  scale_y_log10(breaks = c(50,100,250, 500, 10000, 50000)) +
  geom_text(aes(label = unique_values), vjust = 1.6, color = "white", size=3.5) +
  theme_minimal() +
  labs(x = "features", y = "Number of unique values")
###

#plot percentage

p1 <- train_1[, .N, by = os][order(-N)][1:20] %>% 
  ggplot(aes(reorder(os, -N), N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "os") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p1


p2 <- train_1[, .N, by = channel][order(-N)][1:20] %>% 
  ggplot(aes(reorder(channel, -N), N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "channel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

p3 <- train_1[, .N, by = device][order(-N)][1:20] %>% 
  ggplot(aes(reorder(device, -N), N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "device") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3

p5 <- train_1[, .N, by = hour] %>% 
  ggplot(aes(hour, N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "hours") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5

p4 <- train[, .N, by = app][order(-N)][1:10] %>% 
  ggplot(aes(reorder(app, -N), N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "app") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

multiplot(p1, p2, p3, p4, layout = matrix(1:4, 2, 2))

p6 <- train_1[, "ip"][order(ip)] %>% unique() %>% 
  ggplot() +
  geom_point(aes(x=seq_along(ip), y=ip), size = 0.25, shape=18)+
  theme_minimal() +
  labs(x = "") +
  scale_y_continuous(name="ip", labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) 
p6   

p7 <- train_1[, .N, by = ip][order(-N)][1:10] %>% 
  ggplot(aes(reorder(ip, -N), N)) +
  geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal() +
  geom_text(aes(label = round(N / sum(N), 2)), vjust = 1.6, color = "white", size=2.5) +
  labs(x = "ip")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p7


data_rate=X[, .N, by = c("hour", "mday", "is_attributed")][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")]

data_rate_0=data_rate[data_rate$is_attributed==0,]
data_rate_1=data_rate[data_rate$is_attributed==1,]
data_rate_f=NULL
data_rate_f$dt=data_rate_1$dt
data_rate_f$r=NULL
i=1
for (t in data_rate_f$dt){
  data_rate_f$r[i]=data_rate_1[data_rate_1$dt==t,]$N/data_rate_0[data_rate_0$dt==t,]$N
  i=i+1
}
plot(data_rate_f$dt,data_rate_f$r)

##compare the ratio
##compare the 

X <- copy(train)[, `:=`(hour = hour(click_time),
                        mday = mday(click_time),
                        click_time = as.POSIXct(click_time, format="%Y-%m-%d %H:%M:%S"),
                        is_attributed = factor(is_attributed))]

X <- copy(train_1)[, `:=`(hour = hour(click_time),
                        mday = mday(click_time),
                        click_time = as.POSIXct(click_time, format="%Y-%m-%d %H:%M:%S"),
                        is_attributed = factor(is_attributed))]

X[, .N, by = c("hour", "mday")
  ][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")] %>% 
  ggplot(aes(dt, N)) + 
  geom_line(color="red") +
  ggtitle("Clicks per hour")+
  xlab("Day-Hour") + 
  ylab("Number of clicks")+
  theme_minimal()+
  scale_x_datetime(labels = date_format("%d-%HH"), date_breaks = "8 hours")

X2 <- copy(train_1[is_attributed==1,])[, `:=`(hour = hour(click_time),
                          mday = mday(click_time),
                          click_time = as.POSIXct(click_time, format="%Y-%m-%d %H:%M:%S"),
                          is_attributed = factor(is_attributed))]
X1[, .N, by = c("hour", "mday")
  ][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")] %>% 
  ggplot(aes(dt, N)) + 
  geom_line(color="red") +
  ggtitle("Clicks per hour")+
  xlab("Day-Hour") + 
  ylab("Number of clicks")+
  theme_minimal()+
  scale_x_datetime(labels = date_format("%d-%HH"), date_breaks = "8 hours")
X2[, .N, by = c("hour", "mday")
   ][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")] %>% 
  ggplot(aes(dt, N)) + 
  geom_line(color="red") +
  ggtitle("download vs hours")+
  xlab("Day-Hour") + 
  ylab("Number of clicks")+
  theme_minimal()+
  scale_x_datetime(labels = date_format("%d-%HH"), date_breaks = "8 hours")



##alternative
pt2 <- X[is_attributed==1, .N, by = c("hour", "is_attributed")] %>% 
  ggplot(aes(hour, N)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  ggtitle("Number of downloads per hour")+
  xlab("Hour") + 
  ylab("Number of downloads")+
  theme_minimal()



