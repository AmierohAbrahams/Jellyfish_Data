# Honours Marine Assignment
# Jellyfish polyp growth


# Loading libraries -------------------------------------------------------
library(tidyverse)
library(ggpubr)

# Loading the data --------------------------------------------------------

Data_csv1 <- read_csv("Data_csv1.csv")
jelly <- read_csv("Data_csv1.csv")

# Creating a theme for the graphs -----------------------------------------
theme1 <- function(base_size = 10, base_family = "serif"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.title = element_text(size = 10),
          legend.key=element_rect(colour=NA, fill =NA),
          panel.grid = element_blank(),   
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.background = element_rect(fill = "white", colour = "black"), 
          strip.background = element_rect(fill = NA)
    )
}

# Focusing or analysing each of the series separately
# How does temp affect the growth rate of jellyfish polyps
# Selected the final day of series  A
# Doing each series separately
A12 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "12") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`)) 
  
A14 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "14") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`)) 
  
A16 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "16") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

A18 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "18") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

A20 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "20") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

A22 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "22") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

A24 <- jelly %>% 
  filter(Series == "A") %>%  
  filter(Age == "87") %>% 
  filter(Temp == "24") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

combined_means_A <- rbind(A12, A14, A16, A18, A20, A22, A24) %>% 
  mutate(temp = c("12","14","16","18","20","22","24")) %>% 
  mutate(series = c("A","A","A","A","A","A","A"))

# Series C
C12 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "12") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`)) 

C14 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "14") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`)) 

C16 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "16") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

C18 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "18") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

C20 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "20") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

C22 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "22") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

C24 <- jelly %>% 
  filter(Series == "C") %>%  
  filter(Age == "36") %>% 
  filter(Temp == "24") %>%
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`))

combined_means_C <- rbind(C12, C14, C16, C18, C20, C22, C24) %>% 
  mutate(temp = c("12","14","16","18","20","22","24")) %>% 
  mutate(series = c("C","C","C","C","C","C","C"))


# Ignore ------------------------------------------------------------------
# final_ANC <- rbind(combined_means_A,combined_means_C) %>% 
#   mutate(temp = c("12","14","16","18","20","22","24","12","14","16","18","20","22","24") ) %>% 
#   mutate(series = c("A","A","A","A","A","A","A","C","C","C","C","C","C","C"))

# Focusing on series B 
# Series B comprise of two different species 
# Each of the other series A and C only have one species present
B12 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "12") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B14 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "14") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`, na.rm = TRUE))

B16 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "16") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`, na.rm = TRUE))
B18 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "18") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`, na.rm = TRUE))
B20 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "20") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B22 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "22") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`, na.rm = TRUE))

B24 <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "CX") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "24") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

combined_means_Bcx <- rbind(B12, B14, B16, B18, B20, B22, B24) %>%  #B12cf, B14cf, B16cf, B18cf, B20cf, B22cf, B24cf) %>% 
  mutate(temp = c("12","14","16","18","20","22","24")) %>% #"12","14","16","18","20","22","24")) %>% 
  mutate(series = c("CX","CX","CX","CX","CX","CX","CX")) %>% 
  mutate(age = c("36","36","36","36","36","36","36"))

B12cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "12") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B14cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "14") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B16cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "16") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))
B18cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "18") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))
B20cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "20") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B22cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "22") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

B24cf <- jelly %>% 
  filter(Series == "B") %>% 
  filter(Species == "Cf") %>% 
  filter(Age == "36") %>% 
  filter(Temp == "24") %>% 
  summarise(mean_cum = mean(`Cum N`), 
            mean_R1 = mean(`R 1`,na.rm = TRUE))

combined_means_BCf <- rbind(B12cf, B14cf, B16cf, B18cf, B20cf, B22cf, B24cf)%>% 
  mutate(temp = c("12","14","16","18","20","22","24")) %>% 
  mutate(series = c("B","B","B","B","B","B","B"))

# ABC w CX 
# final_ABC <- rbind(combined_means_A, combined_means_BCf, combined_means_C, combined_means_Bcx) %>% 
#   mutate(temp = c("12","14","16","18","20","22","24","12","14","16","18","20","22","24","12","14","16","18","20","22","24","12","14","16","18","20","22","24")) %>% 
#   mutate(series = c("A","A","A","A","A","A","A","B","B","B","B","B","B","B","C","C","C","C","C","C","C","CX","CX","CX","CX","CX","CX","CX")) %>% 
#   mutate(age = c("87","87","87","87","87","87","87","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36","36"))
# ABC w/o CX


________________________________________________________________________________________________________________________________
# Note that the rate and cum N graphs were taken on the last day and thus cannot be compared to the other graphs were all the polyps were included
final_ABC <- rbind(combined_means_A, combined_means_BCf, combined_means_C) %>% 
  mutate(temp = c("12","14","16","18","20","22","24","12","14","16","18","20","22","24","12","14","16","18","20","22","24")) %>% 
  mutate(series = c("A","A","A","A","A","A","A","B","B","B","B","B","B","B","C","C","C","C","C","C","C")) %>% 
  mutate(age = c("87","87","87","87","87","87","87","36","36","36","36","36","36","36","36","36","36","36","36","36","36"))


rate_cf <- ggplot(final_ABC, aes(x = temp, y = mean_R1)) +
  geom_line(aes(group = series), alpha = 0.5) +
  geom_point(aes(colour = series)) +
  # scale_colour_distiller(palette = "Spectral") +
  facet_wrap(~series, ncol = 3) +
  labs(x = "Temperature (°C)", y = "Growth Rate")+
  theme1()

rate_cf
# Interpretations of the graphs above -------------------------------------
# Graph A grows slower than B and C
# The optimal temperature growth is between 12 and 16 degrees and 22-24
# Graph A and c- optimal temp 12-14
# Graph B optimal temp from 14-16 degree celcuis
# reason for dip 20?

mean_cf <- ggplot(final_ABC, aes(x = temp, y = mean_cum)) +
  geom_line(aes(group = series), alpha = 0.5) +
  geom_point(aes(colour = series)) +
  # scale_colour_distiller(palette = "Spectral") +
  facet_wrap(~series, ncol = 3) +
  labs(x = "Temperature (°C)", y = "Cumulative number") +
  theme1()

mean_cf
_________________________________________________________________________________________________________________________________________


# B and C decreases at 20 degrees
# A cum no. decreases 16-22 degrees with rapid increase after 22
# the cummelative means are much higher for A
# 

# Error point graph??
# ggplot(jelly, aes(x = Age, y = "Cum N")) +
#   geom_point(aes(colour = Temp)) +
#   geom_line(aes(colour = Temp, group = Series), alpha = 0.5)+
#   theme1()

temp12 <- jelly %>% 
  filter(Temp == 12) %>% 
  group_by(Series)

# ggplot(temp12, aes(x = Age, y = `Cum N`)) +
#   geom_line(aes(colour = Series, group = Polyp), alpha = 0.5) +
#   geom_point(aes(shape = Series, group = Polyp))+
#   facet_wrap(~Series, ncol = 3) +
#   theme1()

jelly_excludedCX <- jelly %>% 
  filter(Species == "Cf")

ggplot(jelly_excludedCX, aes(x = Age, y = `Cum N`)) +
  geom_line(aes(colour = Temp, group = Polyp), alpha = 0.5) +
  scale_colour_distiller(palette = "Spectral") +
  facet_wrap(~Series, ncol = 3) +
  labs(x = "Age (Days)", y = "Cumulative number") +
  theme1()

# At low temp cummalative numbers were relativley low. As temperatures increase cummalative numbers increase
# There are some outliers present which shows a constant increase in cummalative number-- This can be mentioned in the discussion
# In series A the Cummalative number of polys increased much more as such it could be seen that 36 days(present in series B and c) was not the optimal temperature 
# over a 36 day period polyp number slighly increases but in series A we see  a steeper increase in cummalive number due to the longer period (Series A- 87days)
# Series B and C- 36day period

ggplot(jelly_excludedCX, aes(x = Age, y = `R 1`)) +
  geom_line(aes(colour = Temp, group = Polyp), alpha = 0.5) +
  scale_colour_distiller(palette = "Spectral") +
  facet_wrap(~Series, ncol = 3) +
  labs(x = "Age (Days)", y = "R 1") +
  theme1()

# Polyps need optimal conditions to grow where previous graphs has shown that warmer temperatures result in optimal growth rates
# from this graphs we see that polyps in colder temperature take longer for budding 

unique(jelly$Polyp)

# rate_cx <- ggplot(combined_means_Bcx, aes(x = temp, y = mean_R1)) +
#   geom_line(aes(group = series), alpha = 0.5) +
#   geom_point(aes(colour = series)) +
#   scale_colour_manual(values = "purple")+
#   # scale_colour_distiller(palette = "Spectral") +
#   # facet_wrap(~series, ncol = 2) +
#   scale_y_continuous(limits = c(0.8, 1.1))+
#   labs(x = "Temp", y = "Growth Rate")
# 
# mean_cx <- ggplot(combined_means_Bcx, aes(x = temp, y = mean_cum)) +
#   geom_line(aes(group = series), alpha = 0.5) +
#   geom_point(aes(colour = series)) +
#   scale_colour_manual(values = "purple")+
#   # scale_colour_distiller(palette = "Spectral") +
#   # facet_wrap(~series, ncol = 2) +
#   scale_y_continuous(limits = c(0, 40))+
#   labs(x = "Temp", y = "Cumulative number")
# 
# ggarrange(rate_cf, rate_cx)
# 
# ggarrange(mean_cf, mean_cx)
# 


# Statistical analysis on the data ----------------------------------------
# t test  -----------------------------------------------------------------

# test normality

shapiro.test(final_ABC$mean_cum)
# p-value = 0.06734 = not normal 
shapiro.test(final_ABC$mean_R1)
# p-value = 0.01526 = normal

# t test and anova --------------------------------------------------------
# MEAN_CUM NOT NORMAL SO DO - WILCX
t.test(mean_cum ~ age, data=final_ABC, var.equal = TRUE)
# not sig  

# Rate with age
t.test(mean_R1 ~ age, data=final_ABC, var.equal = TRUE)

# WRONG
try1 <- aov(mean_cum ~ temp, data=final_ABC)
summary(try1)
# not significsnt
# Df Sum Sq Mean Sq F value Pr(>F)
# temp         6  615.5  102.58    1.33  0.308
# Residuals   14 1080.1   77.15  

# Analysing the rate
try2 <- aov(mean_R1 ~ temp, data=final_ABC)
summary(try2)
# not significant 
# Df   Sum Sq   Mean Sq F value Pr(>F)
# temp         6 0.005256 0.0008759   2.196  0.106
# Residuals   14 0.005584 0.0003989 


# Does the means of each series differ?  
# are the means between two populations significantly different.
aov(mean_R1 ~ series, data=final_ABC)

summary(aov(mean_R1 ~ series, data=final_ABC))
# Df   Sum Sq   Mean Sq F value  Pr(>F)   
# series       2 0.005101 0.0025503   7.999 0.00327 **
# Residuals   18 0.005739 0.0003188 

summary(aov(mean_cum~series*as.factor(age), data = final_ABC))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# series       2  918.0   459.0   10.62 0.000897 ***
#   Residuals   18  777.6    43.2

summary(aov(mean_cum ~ series, data=final_ABC))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# series       2  918.0   459.0   10.62 0.000897 ***
#   Residuals   18  777.6    43.2                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(aov(mean_cum~series*as.factor(temp), data = final_ABC))


summary(aov(mean_cum~temp*as.factor(age), data = final_ABC))

# Df Sum Sq Mean Sq F value   Pr(>F)    
# temp                 6  615.5   102.6  27.037 0.000167 ***
#   as.factor(age)       1  894.9   894.9 235.866  1.2e-06 ***
#   temp:as.factor(age)  6  158.6    26.4   6.966 0.010931 *  
#   Residuals            7   26.6     3.8                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Temp and age is significant( all 3 factors are significant)

TukeyHSD((aov(mean_cum~temp*as.factor(age), data = final_ABC)))
plot(TukeyHSD((aov(mean_cum~temp*as.factor(age), data = final_ABC))))


# Rate --------------------------------------------------------------------

summary(aov(mean_R1~temp*as.factor(age), data = final_ABC))
# Df   Sum Sq  Mean Sq F value   Pr(>F)    
# temp                 6 0.005256 0.000876  19.432 0.000489 ***
#   as.factor(age)       1 0.004917 0.004917 109.084 1.61e-05 ***
#   temp:as.factor(age)  6 0.000351 0.000059   1.299 0.366411    
# Residuals            7 0.000316 0.000045                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD((aov(mean_R1~temp*as.factor(age), data = final_ABC)))
plot(TukeyHSD((aov(mean_R1~temp*as.factor(age), data = final_ABC))))

# KRUSKALMC ---------------------------------------------------------------
# USE THIS AS MEAN_CUM not normally distributed.

kruskal.test(mean_cum ~ temp*as.factor(age), data=final_ABC)


# Transforming the data ---------------------------------------------------
try1 <- final_ABC %>% 
  mutate(log_mean_cum = log(mean_cum)) 
  # mutate(log_mean_R1 = log(mean_R1))

shapiro.test(try1$log_mean_cum)


# density plot  -----------------------------------------------------------

# ggplot(data = final_ABC, aes(x = mean_cum))+
#   geom_density()
# #RIGHT SKEWED
# ggplot(data = final_ABC, aes(x = mean_cum))+
#   geom_histogram()
# 
# try2 <- final_ABC %>% 
#   mutate(log10_mean_cum = log10(mean_cum))
# 
# shapiro.test(try2$log10_mean_cum)
# 
# ggplot(data = try2, aes(x = log10_mean_cum))+
#   geom_density()

try3 <- final_ABC %>% 
  mutate(recip_mean_cum = 1/(mean_cum))

shapiro.test(try3$recip_mean_cum)
# data:  try3$recip_mean_cum
# W = 0.80453, p-value = 0.0007701

ggplot(data = try3, aes(x = recip_mean_cum))+
  geom_density()

summary(aov(recip_mean_cum ~ series, data=try3))
# Df  Sum Sq  Mean Sq F value Pr(>F)  
# series       2 0.02118 0.010589    3.32 0.0592 .
# Residuals   18 0.05741 0.003189 

TukeyHSD((aov(recip_mean_cum ~ series, data=try3)))
plot(TukeyHSD((aov(recip_mean_cum ~ series, data=try3))))


# fuck2 <- ggplot(final_ABC, aes(x = age, y = mean_cum)) +
#   geom_line(aes(group = temp), alpha = 0.5) +
#   geom_point(aes(colour = temp)) +
#   # scale_colour_distiller(palette = "Spectral") +
#   facet_wrap(~series, ncol = 3) +
#   labs(x = "Temp", y = "Cumulative number") +
#   theme1()
#  
# fuck2



