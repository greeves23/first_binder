#continous data factoral 2x2 design
#DV = Time
#IV = Prime, Target
#levels = Negative, Positive

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

Prime_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/factorial_data.csv")

head(Prime_data)

tidied_prime_data <- Prime_data %>%
  mutate(Subject = factor(Subject), Item = factor(Item), Time = Time,
         Prime = factor(Prime), Target = factor(Target))

head(tidied_prime_data)

str(tidied_prime_data)

tidied_prime_data %>%
  group_by(Prime, Target) %>%
  summarise(mean_time = mean(time), sd_time = sd(time))

library(visdat) # missing data so need to visualise this
vis_miss(tidied_prime_data) # this visualises missing data in graph or can use:

tidied_prime_data %>%
  filter(!is.na(Time)) %>%
  group_by(Prime, Target) %>%
  summarise(mean_time = mean(Time), sd_time = sd(Time))

# matched conditions have quicker response time, negative negative seems fastest

tidied_prime_data %>%
  filter(!is.na(Time)) %>%
  ggplot(aes(x = Prime:Target, y = Time, colour = Prime:Target)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(x = "Prime X Target",
       y = "Time (ms.)") +
  theme_minimal() +
  coord_flip()

#positive positive seems slowest because had the most variability, positive negative
# seems fastest because it has the least variability 

#do contrasts to stop use of dummy data 
contrasts(tidied_prime_data$Prime) <- matrix(c(.5, -.5))
contrasts(tidied_prime_data$Target) <- matrix(c(.5, -.5))

prime_model <- lmer(Time ~ Prime * Target + 
                          (1 + Prime + Target | Subject) +
                          (1 + Prime | Item), 
                        data = tidied_prime_data)
check_model(prime_model)

library(fitdistrplus)
missing_prime_removed <- tidied_prime_data %>%
  filter(!is.na(Time))

descdist(missing_prime_removed$Time)

gamma_prime_model <- glmer(Time ~ Prime * Target + 
                                 (1 + Prime + Target | Subject) +
                                 (1 + Target | Item), 
                               family = Gamma,
                               nAGQ = 0,
                               data = tidied_prime_data)

summary(prime_model)
# interaction is significant but no main effects

summary(gamma_prime_model)
# interaction sig but no main effects

emmeans(prime_model, pairwise ~ Prime*Target, adjust = "none")
# interaction sig based on negative Primes before neg vs pos targets
# Pos targets were responded to more quickly following Pos vs. Neg primes