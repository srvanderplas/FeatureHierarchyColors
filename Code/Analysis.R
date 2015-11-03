library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)

#---- Anonymize Data -----------------------------------------------------------
userdata <- read_csv("Data/turk19_results.csv", col_types = "ccccccccc")
users <- read_csv("Data/turk19_users.csv")

userlist <- unique(c(userdata$nick_name, users$nick_name))
iplist <- unique(c(userdata$ip_address, users$ip_address))

# table(userdata$ip_address, userdata$nick_name)
userdata$nick_name2 <- userdata$nick_name
userdata$ip_address2 <- userdata$ip_address
userdata$ip_address <- as.numeric(factor(userdata$ip_address2, levels=iplist))
userdata$nick_name <- as.numeric(factor(userdata$nick_name2, levels=userlist))

users$nick_name2 <- users$nick_name
users$ip_address2 <- users$ip_address
users$nick_name <- as.numeric(factor(users$nick_name2, levels=userlist))
users$ip_address <- as.numeric(factor(users$ip_address2, levels=iplist))

write_csv(userdata[,-which(names(userdata)%in%c("ip_address2", "nick_name2"))], "./Data/turk19_results_anon.csv")
write_csv(users[,-which(names(users)%in%c("ip_address2", "nick_name2"))], "./Data/turk19_users_anon.csv")
#-------------------------------------------------------------------------------

userdata <- read_csv("./Data/turk19_results_anon.csv", col_types = "ccccccccc")
users <- read_csv("Data/turk19_users_anon.csv")
userdata %<>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    i = as.numeric(str_sub(pic_id, 1, -3)),
    j = c("color", "colorEllipse", "colorEllipseTrendError", "colorTrend")[1+as.numeric(str_sub(pic_id, -2, -2))],
    k = c("colorbrewer", "dichromat", "ggplot", "tableau", "turk16")[1+as.numeric(str_sub(pic_id, -1, -1))]
  )


answers <- read_csv("Images/Lineups/picture-details.csv") %>%
  mutate(
    trend.target = str_extract(obs_plot_location, "^\\d{1,2}"),
    cluster.target = str_extract(obs_plot_location, "\\d{1,2}$"),
    pic_id = as.character(pic_id)
  )

userdata2 <- left_join(userdata, select(answers, pic_id, sample_size, test_param, param_value, obs_plot_location, trend.target, cluster.target)) %>%
  select(-description, -ip_address) %>%
  mutate(trend_id = trend.target==response_no,
         cluster_id = cluster.target == response_no)

plot.summary <- userdata2 %>% group_by(i, j, k, pic_id) %>%
  summarize(pct.trend = mean(trend_id),
            pct.cluster = mean(cluster_id)) %>%
  gather(key = "Type", value = "Prop.Correct", -i, -j, -k, -pic_id) %>%
  mutate(Type = str_replace(Type, "pct\\.", "")) %>%
  ungroup()

library(ggplot2)
ggplot(data = plot.summary) +
  geom_point(aes(x = i, y = Prop.Correct, color = Type)) +
  facet_grid(k ~ j)

glm()
