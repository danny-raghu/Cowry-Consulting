library(tidyverse)
library(afex)
library(emmeans)

a <- read_csv("Modified_Exp_Results.csv")
as.tibble(a)

a <- a %>% filter(a$Do_You == "Y" & a$Takes_Pledge == "Y")
a <- a %>%  mutate(Group = factor(Group), Agree_Q = factor(Agree_Q), Do_You = factor(Do_You), Frequency = factor(Frequency), App = factor(App))
str(a)

a %>% filter(Agree_Q == "Y") %>% count()
a %>% filter(Group == "Control", App == "N") %>% count()
a %>% filter(Group == "Control", App == "Y", Frequency == "R") %>% count()
a %>% filter(Group == "Treatment", App == "N") %>% count()
a %>% filter(Group == "Treatment", App == "Y", Frequency == "R") %>% count()


a %>% ggplot(aes(x = Group))+
  geom_bar(aes(fill = App), position = "dodge")

a %>% filter(App == "Y", Group == "Treatment") %>%
  ggplot(aes(x = Frequency))+
  geom_bar()

a %>% filter(Group == "Control") %>% group_by(Frequency) %>% count()


tbl <- table(a$App, a$Group)
tbl
chi <- chisq.test(tbl, correct = F)
chi
lm1 <-  glm(App ~ Group, data = a, family = 'binomial')
summary(lm1)
