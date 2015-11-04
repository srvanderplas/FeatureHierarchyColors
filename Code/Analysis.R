library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)

#---- Anonymize Data -----------------------------------------------------------
# userdata <- read_csv("Data/turk19_results.csv", col_types = "ccccccccc")
# users <- read_csv("Data/turk19_users.csv")
#
# userlist <- unique(c(userdata$nick_name, users$nick_name))
# iplist <- unique(c(userdata$ip_address, users$ip_address))
#
# # table(userdata$ip_address, userdata$nick_name)
# userdata$nick_name2 <- userdata$nick_name
# userdata$ip_address2 <- userdata$ip_address
# userdata$ip_address <- as.numeric(factor(userdata$ip_address2, levels=iplist))
# userdata$nick_name <- as.numeric(factor(userdata$nick_name2, levels=userlist))
#
# users$nick_name2 <- users$nick_name
# users$ip_address2 <- users$ip_address
# users$nick_name <- as.numeric(factor(users$nick_name2, levels=userlist))
# users$ip_address <- as.numeric(factor(users$ip_address2, levels=iplist))
#
# write_csv(userdata[,-which(names(userdata)%in%c("ip_address2", "nick_name2"))], "./Data/turk19_results_anon.csv")
# write_csv(users[,-which(names(users)%in%c("ip_address2", "nick_name2"))], "./Data/turk19_users_anon.csv")
#-------------------------------------------------------------------------------

userdata <- read_csv("./Data/turk19_results_anon.csv", col_types = "ccccccccc")
users <- read_csv("Data/turk19_users_anon.csv")
userdata %<>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    plot = as.numeric(str_sub(pic_id, 1, -3)),
    aes = c("color", "colorEllipse", "colorEllipseTrendError", "colorTrend")[1+as.numeric(str_sub(pic_id, -2, -2))],
    palette = c("colorbrewer", "dichromat", "ggplot", "tableau", "turk16")[1+as.numeric(str_sub(pic_id, -1, -1))]
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
         cluster_id = cluster.target == response_no,
         k = str_sub(param_value, 3, 3) %>% as.numeric(),
         sd.trend = str_sub(param_value, 12, 15) %>% as.numeric(),
         sd.cluster = str_sub(param_value, -4, -1) %>% as.numeric())

plot.summary <- userdata2 %>% group_by(plot, aes, palette, pic_id, k, sd.trend, sd.cluster) %>%
  summarize(pct.trend = mean(trend_id),
            pct.cluster = mean(cluster_id)) %>%
  gather(key = "Type", value = "Prop.Correct", -plot, -aes, -palette, -pic_id, -k, -sd.trend, -sd.cluster) %>%
  mutate(Type = str_replace(Type, "pct\\.", "")) %>%
  ungroup()

library(ggplot2)
ggplot(data = plot.summary) +
  geom_boxplot(aes(x = palette, y = Prop.Correct, color = Type)) +
  facet_grid(k ~ aes) + coord_flip()

ggplot(data = plot.summary) +
  geom_boxplot(aes(x = aes, y = Prop.Correct, color = Type)) +
  facet_grid(k ~ palette) + coord_flip()

ggplot(data = plot.summary) +
  geom_boxplot(aes(x = palette, y = Prop.Correct, color = Type)) +
  facet_grid(k + Type ~ aes) + coord_flip()

library(lme4)
turk19 <- subset(userdata2, plot %in% c(25:36, 43:54))

# models are not converging ...
trend <- glmer(trend_id ~ palette*pic_id  + (1|nick_name), family = binomial(), data=turk19)
cluster <- glmer(cluster_id ~ palette + aes + sample_size + (1|pic_id) + (1|nick_name), family = binomial(), data=userdata2)
