library(dplyr)
library(xgboost)
library(caret)
library(data.table)

##first look at the sample training data
sm_data=read.csv("train_sample.csv")
train_1 <- fread("train_sample.csv", showProgress=F)
fea <- c("os", "channel", "device", "app", "attributed_time", "click_time", "ip","year","month","days","hour")

#plot unique value
train_1[, lapply(.SD, uniqueN), .SDcols = fea] %>%
  melt(variable.name = "features", value.name = "unique_values") %>%
  ggplot(aes(reorder(features, -unique_values), unique_values)) +
  geom_bar(stat = "identity", fill = "darkgreen") + 
  scale_y_log10(breaks = c(50,100,250, 500, 10000, 50000)) +
  geom_text(aes(label = unique_values), vjust = 1.6, color = "white", size=3.5) +
  theme_minimal() +
  labs(x = "features", y = "Number of unique values")


##seems that we only have 4 days of data, thus there may not be a pattern difference between days [our data isn't enough to figure out the difference between days]
## Some count plot. Fiure out how the clicks are distributed with different attributes values 
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

##ratio compare through time{want figure out if there is difference bwteen hours}

data_rate=X[, .N, by = c("hour", "mday", "is_attributed")][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")]
data_rate_0=data_rate[data_rate$is_attributed==0,]
data_rate_1=data_rate[data_rate$is_attributed==1,]
data_rate_f=NULL
data_rate_f$hour=data_rate_1$hour
data_rate_f$r=NULL
i=1
for (t in data_rate_f$hour){
  data_rate_f$r[i]=sum(data_rate_1[data_rate_1$hour==t,]$N)/sum(data_rate_0[data_rate_0$hour==t,]$N)
  i=i+1
}
plot(data_rate_f$hour,data_rate_f$r)

##plot clicks and downloads over time


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
