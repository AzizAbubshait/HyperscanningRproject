library(tidyverse)
#library(lme4)

pilot_path = paste(getwd(), "/pilot2_data_raw/", sep = "")
list_of_datanames = list.files(pilot_path)
dat_p2 = lapply(paste(pilot_path, list_of_datanames, sep = ""), read.csv)
dat_p2 = do.call(plyr::rbind.fill, dat_p2)

data_quest = data.frame(participant = unique(dat_clean$participant),
               p1_resp = c(6, 2, 5, 3, 4,4), 
               p2_resp = c(1, 2, 3, 2,4,5))

dat_clean = dat_p2 %>% 
  group_by(participant) %>% 
  # get rid of practice trials
  slice(-c(1:2)) %>% 
  # we need to recode to 1 and 2 because the fill function won't work for characters. 
  # It only works for numeric variables
  mutate(RespTuringLeft = 
           case_when(RespTuringLeft == "c" ~ 1,
                     RespTuringLeft == "h" ~ 2),
         RespTuringRight = 
           case_when(RespTuringRight == "c" ~ 1,
                     RespTuringRight == "h" ~ 2)) %>% 
  fill(RespTuringLeft, .direction = "up") %>%
  fill(RespTuringRight, .direction = "up") %>%
  fill(keyTuringLeft.corr, .direction = "up") %>%
  fill(keyTuringRight.corr, .direction = "up") %>%
  mutate(RespTuringLeft = recode(RespTuringLeft, `1` = "comp", `2` = "human")) %>%
  mutate(RespTuringRight = recode(RespTuringRight, `1` = "comp", `2` = "human")) %>%
  # remove some vars
  select(-c(1:4,7:12, 15:17, 19:70)) %>%
  ## Delete every 11th row
  filter(is.na(Number) == F) %>% 
  # measure synchrony
  left_join(data_quest, by = "participant") %>% 
  # filter(key_respLeft.rt != 13,
  #        key_respRight.rt != 13) %>%
  # filter(!participant %in% c("11_12", "9_10")) %>% 
  mutate(sync_value = abs((key_respLeft.rt-key_respRight.rt))/
           (key_respLeft.rt+key_respRight.rt)*100,
         choice = case_when(RespTuringRight == "comp" & 
                              RespTuringLeft == "comp" ~ "comp_congruent",
                            RespTuringRight == "comp" & 
                              RespTuringLeft == "human" ~ "incongruent",
                            RespTuringRight == "human" & 
                              RespTuringLeft == "comp" ~ "incongruent",
                            RespTuringRight == "human" & 
                              RespTuringLeft == "human" ~ "human_congruent"),
         closeness_diff = abs(p1_resp - p2_resp),
         closeness_avg = mean(c(p1_resp, p2_resp))) %>% 
  ungroup(participant) %>%
  mutate(sync_z_value = scale(sync_value))
%>% 
  filter(sync_z_value < 6) 

ggplot(dat_clean, aes(sync_z_value, fill = participant))+
  geom_histogram()
# plot
# ggplot(dat_clean, aes(y = sync_value, x = Partner, color = choice))+
#   geom_point(position = position_jitterdodge(.1),shape = 1, alpha = .8)+
#   stat_summary(fun.data = mean_se, geom = "errorbar", 
#                width = .1, position = position_dodge(.9))+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5, 
#                position = position_dodge(.9))+
#   theme_bw()

ggplot(dat_clean, aes(y = sync_value, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

ggplot(dat_clean, aes(y = sync_value, x = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

ggplot(dat_clean, aes(y = sync_value, x = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

ggplot(dat_clean, aes(y = sync_value, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+facet_grid(~participant)

data.frame(dat_clean %>% group_by(participant, 
                                  Partner, choice) %>%
             summarize(n = n()))

# correlation ----
ggplot(dat_clean, aes(x = sync_value, y = closeness_diff, color = choice))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~Partner)

ggplot(dat_clean, aes(x = sync_value, y = closeness_avg, color = choice))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~Partner)

# measure correctness ----
dat_corr = dat_clean %>% 
  group_by(participant, Partner)%>%
  summarize(percent_left = sum(keyTuringLeft.corr == 1)/length(keyTuringLeft.corr),
            percent_right = sum(keyTuringRight.corr == 1)/length(keyTuringLeft.corr)) %>% 
  pivot_longer(cols = c(percent_left, percent_right),
               names_to = "player", values_to = "percent")

ggplot(dat_corr, aes(y = percent, x = Partner, fill = player))+
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "bar", size = 5,
               position = position_dodge(.9))+
  theme_bw()
  
ggplot(dat_corr, aes(y = percent, x = Partner, fill = player))+
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = .1, position = position_dodge(.3))+
  stat_summary(fun.data = mean_se, geom = "bar", size = 5,
               position = position_dodge(.3))+
  theme_bw()+
  facet_wrap(~participant)

ggplot(dat_corr, aes(y = percent, x = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+facet_grid(~participant)



