library(tidyverse)
library(broom)
library(lme4)
library(lmerTest)
library(roll)

# do some cleaning ---- 
pilot_path = paste(getwd(), "/pilot data/", sep = "")
list_of_datanames = list.files(pilot_path)

dat = lapply(paste(pilot_path, list_of_datanames, sep = ""), read.csv)
dat = do.call(plyr::rbind.fill, dat)

dat_clean = dat %>% 
  group_by(participant) %>% 
  mutate(resp_choice = 
           case_when(key_resp_2.keys == "c" ~ 1,
                     key_resp_2.keys == "h" ~ 2)) %>%
  fill(resp_choice, .direction = "up") %>%
  fill(key_resp_2.corr, .direction = "up") %>%
  mutate(resp_choice = recode(resp_choice, `1` = "comp", `2` = "human")) %>%
  slice(-c(1:8)) %>%
  select(-c(1:3,5, 9:14, 17,18, 21:92, 96:118, 121:123, 125:137,139:144)) %>%
  filter(is.na(Number) == F) %>% ## Delete every 11th row
  mutate(sync_value = abs((RTLeft-RTRight))/(RTLeft+RTRight)*100) %>% # measure synchrony
  ungroup(participant) %>%
  mutate(diff_performance_RTRight = Number - RTRight,
         diff_performance_RTLeft = Number - RTLeft)%>%
  pivot_longer(cols = c(diff_performance_RTRight, 
                        diff_performance_RTLeft), 
             names_to = "RTPlayer", values_to = "RTdiff") 

data.frame(dat_clean %>% group_by(participant, 
                                  Partner, resp_choice) %>%
             summarize(n = n()/20))

ggplot(dat_hist, aes(n, fill = resp_choice))+
  geom_histogram()+
  theme_bw()+facet_wrap(~Partner)

ggplot(dat_clean, aes( y = RTdiff, Partner, color = resp_choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()
ggplot(dat_clean, aes( y = RTdiff, Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()
ggplot(dat_clean, aes( y = RTdiff, sync_value, color = Partner))+
  geom_point()+
  geom_smooth(method = "lm")


# plot sync value from Mu et al.
ggplot(dat_clean, aes(y = sync_value, x = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  facet_wrap(~resp_choice)+
  theme_bw()+
  ggtitle("Sync values from Mu et al. panel 1 = p1, panel 2 = p2")

ggplot(dat_clean, aes(y = sync_value,x = Partner, color = resp_choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+ggtitle("color is response, x axis is actual partner")

ggplot(dat_clean, aes(y = sync_value, x = as.factor(TuringResponsePlayer), color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()

summary(lmer(sync_value ~ Partner*resp_choice + (1|participant), dat_clean))

# measure correctness of choice ----
dat_corr = dat_clean %>% 
  group_by(participant, TuringResponsePlayer, Partner)%>%
  summarize(percent = sum(key_resp_2.corr == 1)/length(key_resp_2.corr)) %>% # measure synchrony
  mutate(TuringResponsePlayer = as.factor(TuringResponsePlayer))

ggplot(dat_corr, aes(y = percent, x = Partner, color = Partner))+
  geom_jitter(position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+
  ggtitle("% of correctness. x axis and color are the same variable")

ggplot(dat_corr, aes(y = percent, x = TuringResponsePlayer, color = TuringResponsePlayer))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+
  ggtitle("% of correctness. x shows participant location")

ggplot(dat_corr, aes(y = percent, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+facet_wrap(~participant)+
  ggtitle("correctness of each dyad as a function of partner type")

# logistic regression ----
dat_logistic = dat_clean %>%
  group_by(participant, Partner, TuringResponsePlayer, resp_choice) %>%
  summarize(RTRight=mean(RTRight), RTLeft=mean(RTLeft),
            sync_value=mean(sync_value)) %>%
  mutate(Partner2 = case_when(Partner == "Computer" ~ 1,
                             Partner == "Human" ~ 0),
         resp_choice2 = case_when(resp_choice == "comp" ~ 1,
                                  resp_choice == "human" ~ 0)) %>%
  pivot_longer(cols = c(RTRight, RTLeft), 
              names_to = "RTPlayer")

#plot RT and correctness as a function of player
ggplot(dat_logistic, aes(y = value, x = RTPlayer))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()
  
# go back to logistic
summary(glmer(Partner2 ~ sync_value*resp_choice*TuringResponsePlayer + (1|participant), 
                family = "binomial", data = dat_logistic))
summary(glm(Partner2 ~ sync_value*resp_choice*TuringResponsePlayer, 
      family = "binomial", data = dat_logistic))

summary(glmer(Partner2 ~ sync_value*resp_choice*value + (1|participant), 
            family = "binomial", data = dat_logistic))

ggplot(dat_logistic, aes(y = Partner2, x = sync_value, color = resp_choice))+
  geom_smooth(method="glm", formula= (y ~ log(x)), 
              method.args = list(family =  "binomial"), se = F)+
  theme_bw()+facet_wrap(~TuringResponsePlayer)+
  ggtitle("predicting partner type based on sync valye and resp choice. 1 = computer, 0 = hum")

ggplot(dat_logistic, aes(y = Partner2, x = value, color = resp_choice))+
  geom_smooth(method="glm", formula= (y ~ log(x)), 
              method.args = list(family =  "binomial"), se = F)+
  theme_bw()+
  ggtitle("value is rsponse time.1 = computer, 0 = hum")

ggplot(dat_logistic, aes(y = resp_choice2, x = sync_value, color = Partner))+
  geom_smooth(method="glm", formula= (y ~ log(x)), 
              method.args = list(family =  "binomial"), se = F)+
  theme_bw()+ggtitle("y is choice. 1 = computer, 0 = hum")

ggplot(dat_logistic, aes(y = resp_choice2, x = value, color = Partner))+
  geom_smooth(method="glm", formula= (y ~ log(x)), 
              method.args = list(family =  "binomial"), se = F)+
  theme_bw()+ggtitle("y is choice. x is RT. 1 = computer, 0 = hum")

summary(glmer(resp_choice2 ~ sync_value*Partner*value + (1|participant), 
              family = "binomial", data = dat_logistic))

# compute correlation for each dyad ----
dat_corr = dat_clean %>% 
  group_by(participant, Block.thisN, Partner, resp_choice) %>% 
  do(corr = cor(.$RTRight,.$RTLeft)) %>%
  tidy(corr) 

ggplot(dat_corr, aes(y = x, x = Partner, color = resp_choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+
  ggtitle("compute overall correlation for each block for each participant")

#compute rolling correlation ----
library(corrr)
library(tidyr)
library(broom)
# Correlation table
rolling_corr_dat = dat_clean %>%
  group_by(participant, Partner, Block.thisN, resp_choice) %>%
  summarize(rc = roll_cor(RTLeft, RTRight, width = 3)) %>% drop_na()

ggplot(rolling_corr_dat, aes(y = rc, x = Partner, color = resp_choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, position = position_dodge(.9))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, position = position_dodge(.9))+
  theme_bw()+
  ggtitle("rolling correlation. Width = 3")
ggplot(rolling_corr_dat, aes(y = Block.thisN, x = rc, color = Partner))+
  geom_point()+geom_smooth(method = "lm", se = F)+
  facet_wrap(~resp_choice)

# compute cross correlation ----
cross_corr = dat_clean %>% 
  group_by(participant, Partner, resp_choice) %>%
  do(model = ccf(.$RTRight, .$RTLeft)) %>%
  tidy(model) 

ggplot(cross_corr, aes(y = acf, x = lag, color = Partner))+
  geom_point()+
  facet_grid(~resp_choice)

ggplot(cross_corr, aes(y = acf, x = lag, color = Partner))+
  geom_point()+
  geom_smooth(method = "loess", se = F)+
  facet_grid(~resp_choice)+
  ylim(-.5,1)+
  ggtitle("cross-correlation")

ggCcf(y = cross_corr$acf, x = cross_corr$lag)+
  facet_wrap(~cross_corr$Partner)

ggplot(cross_corr, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity")+
  ylab("Cross correlation")+
  scale_y_continuous(limits = c(-1, 1))+
  theme_bw()+
  facet_wrap(~cross_corr$Partner)

cross_corr %>% 
  ggplot(aes(lag, acf)) +
  #geom_rect(xmin = -10, xmax = 0, ymin = -Inf, ymax = Inf, fill = 'grey90', alpha = 0.1) +
  geom_hline(yintercept = c(-0.1, 0, 0.1), linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_point(aes(group = resp_choice, color = Partner), alpha = 2 / 3) +
  facet_grid(~resp_choice) +
  theme_bw() 
