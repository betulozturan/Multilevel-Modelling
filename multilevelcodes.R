library(rio)                                         # load required package
library(haven)
install.packages("dplyr")
install.packages("broom")

library("devtools")
library(lme4)
library(dplyr)
library(stringr)


WV_2018 <- readRDS("F00007762-WV6_Data_R_v20180912.rds")

#################
### INFORMATION ON DATA
################# 

# v96 income inequality 
# v229 employement status
# v230 sector of employement
# v238 social class
# v239 income
# v240 sex
# v 242 age
# v23 satisfaction with your life
# v57 marital status
# v58 number of chidren
# v59 financial satisfaction
# V235 Are you the chief wage earner in your house
# V238 Social class (subjective)
# V239 Scale of incomes
# V240 Sex
# v241 zear of birth
# V242 Age
# V248 Highest educational level attained
# V265 Respondent's occupation

wvdat <- WV_2018[, c("V2", "V96", "V229", "V230", "V238", "V59", "V239" , "V240" , "V242", "V248")] 

# V 96 income inequality : 1 incomes should be more equal / 10: We need larger income differences as incentives for individual effort
# V 238 1.- Upper class / 2.- Upper middle class /3.- Lower middle class / 4.- Working class/ 5.- Lower class
wvdat$V96[wvdat$V96 == "-1" |wvdat$V96 == "-2" |wvdat$V96 == "-3" |wvdat$V96 == "-4" |wvdat$V96 == "-5"] <- NA
wvdat$V238[wvdat$V238 == "-1" |wvdat$V238 == "-2" |wvdat$V238 == "-3" |wvdat$V238 == "-4" |wvdat$V238 == "-5"] <- NA
wvdat$V239[wvdat$V239 == "-1" |wvdat$V239 == "-2" |wvdat$V239 == "-3" |wvdat$V239 == "-4" |wvdat$V239 == "-5"] <- NA
wvdat$V2[wvdat$V2 == "-1" |wvdat$V2 == "-2" |wvdat$V2 == "-3" |wvdat$V2 == "-4" |wvdat$V2 == "-5"] <- NA
wvdat$V59[wvdat$V59 == "-1" |wvdat$V59 == "-2" |wvdat$V59 == "-3" |wvdat$V59 == "-4" |wvdat$V59 == "-5"] <- NA

names(wvdat)[names(wvdat) == "V2"] <- "country"
names(wvdat)[names(wvdat) == "V96"] <- "incequal"
names(wvdat)[names(wvdat) == "V229"] <- "employment"
names(wvdat)[names(wvdat) == "V230"] <- "sector"
#names(wvdat)[names(wvdat) == "V238"] <- "status"
names(wvdat)[names(wvdat) == "V239"] <- "incomescale"
names(wvdat)[names(wvdat) == "V59"] <-  "finsat"
names(wvdat)[names(wvdat) == "V242"] <- "age"
names(wvdat)[names(wvdat) == "V240"] <- "gender"
names(wvdat)[names(wvdat) == "V248"] <- "education"


#COUNTRY VARIABLE ADJUSTMENT

usa = wvdat[wvdat['country']==c(840) ,] 
brazil <- wvdat[wvdat['country']==c(76) ,] 
chile<- wvdat[wvdat['country']==c(152) ,] 
colombia <- wvdat[wvdat['country']==c(170) ,] 
ecuador <- wvdat[wvdat['country']==c(218) ,]
haiti <- wvdat[wvdat['country']==c(332) ,] 
mexico <- wvdat[wvdat['country']==c(484) ,] 
uruguay <- wvdat[wvdat['country']==c(858) ,] 
wvdat_bind <- rbind(usa, brazil, chile, colombia, ecuador, haiti, mexico, uruguay) 
                    
#level <- c("usa", "brazil", "chile", "colombia", "ecuador", "haiti", "mexico", "uruguay"))

wvdat_bind$country[wvdat_bind$country == 840] <- "usa"
wvdat_bind$country[wvdat_bind$country == 76] <- "brazil"
wvdat_bind$country[wvdat_bind$country == 152] <- "chile"
wvdat_bind$country[wvdat_bind$country == 170] <- "colombia"
wvdat_bind$country[wvdat_bind$country == 218] <- "ecuador"
wvdat_bind$country[wvdat_bind$country == 332] <- "haiti"
wvdat_bind$country[wvdat_bind$country == 484] <- "mexico"
wvdat_bind$country[wvdat_bind$country == 858] <- "uruguay"

#INCEQUAL

summary(wvdat_bind$incequal) # cleaned 
#10 -incomes should be more equal
# 1- we need income differences as incentives

wvdat_bind$incequalnum <- NULL
wvdat_bind$incequalnum[wvdat_bind$incequal == 1] <- 10
wvdat_bind$incequalnum[wvdat_bind$incequal == 2] <- 9
wvdat_bind$incequalnum[wvdat_bind$incequal == 3] <- 8 
wvdat_bind$incequalnum[wvdat_bind$incequal == 4] <- 7 
wvdat_bind$incequalnum[wvdat_bind$incequal == 5] <- 6 
wvdat_bind$incequalnum[wvdat_bind$incequal == 6] <- 5 
wvdat_bind$incequalnum[wvdat_bind$incequal == 7] <- 4
wvdat_bind$incequalnum[wvdat_bind$incequal == 8] <- 3
wvdat_bind$incequalnum[wvdat_bind$incequal == 9] <- 2
wvdat_bind$incequalnum[wvdat_bind$incequal == 10] <- 1 

table(wvdat_bind$incequalnum, wvdat_bind$incequal)

#ADD GINI VARIABLE AS COUNRTY LEVEL PREDICTOR
wvdat_bind['gini'] <- 40.7
wvdat_bind$gini[wvdat_bind['country']=="brazil"] <- 52.475
wvdat_bind$gini[wvdat_bind['country']=="chile"] <- 47.45
wvdat_bind$gini[wvdat_bind['country']=="colombia"] <- 53.28
wvdat_bind$gini[wvdat_bind['country']=="ecuador"] <- 46.52
wvdat_bind$gini[wvdat_bind['country']=="haiti"] <-41.1
wvdat_bind$gini[wvdat_bind['country']=="mexico"] <-45.5
wvdat_bind$gini[wvdat_bind['country']=="uruguay"] <-41.44

#EMPLOYMENT VARIABLE ADJUSTMENT 

#binary variable employed or not 
summary(wvdat_bind$employment)
wvdat_bind$employment1 <- wvdat_bind$employment
wvdat_bind$employment1[wvdat_bind$employment1== 1] <- 1
wvdat_bind$employment1[wvdat_bind$employment1== 3] <- 1
wvdat_bind$employment1[wvdat_bind$employment1 != 1] <- 0

#SECTOR VARIABLE ADJUSTMENT

wvdat_bind$sector[wvdat_bind$sector == 1] <- "public"
wvdat_bind$sector[wvdat_bind$sector == 2] <- "private"
wvdat_bind$sector[wvdat_bind$sector == 3] <- "nonprofit"
wvdat_bind$sector[wvdat_bind$sector == 4] <- NA
wvdat_bind$sector[wvdat_bind$sector == -5] <- NA
wvdat_bind$sector[wvdat_bind$sector == -4] <- NA
wvdat_bind$sector[wvdat_bind$sector == -3] <- NA
wvdat_bind$sector[wvdat_bind$sector == -2] <- NA
wvdat_bind$sector[wvdat_bind$sector == -1] <- NA


#GENDER NUMERIC
# male 0 / female 1 
wvdat_bind$gendernumeric <- wvdat_bind$gender
wvdat_bind$gendernumeric[wvdat_bind$gender == 1] <- 0
wvdat_bind$gendernumeric[wvdat_bind$gender == 2] <- 1
table(wvdat_bind$gender,wvdat_bind$gendernumeric)

#GENDER 

wvdat_bind$gender[wvdat_bind$gender == 1] <- "male"
wvdat_bind$gender[wvdat_bind$gender == 2] <- "female"

#EDUCATION

wvdat_bind$education[wvdat_bind$education == -5] <-NA
wvdat_bind$education[wvdat_bind$education == -4] <-NA
wvdat_bind$education[wvdat_bind$education == -3] <-NA
wvdat_bind$education[wvdat_bind$education == -2] <-NA
wvdat_bind$education[wvdat_bind$education == -1] <-NA

wvdat_bind$educationchar <- wvdat_bind$education


wvdat_bind$educationchar[wvdat_bind$education == 1] <- "noeducation"
wvdat_bind$educationchar[wvdat_bind$education == 2] <- "incompleteprimary"
wvdat_bind$educationchar[wvdat_bind$education == 3] <- "completeprimary"
wvdat_bind$educationchar[wvdat_bind$education == 4] <- "incompletetechsecondary"
wvdat_bind$educationchar[wvdat_bind$education == 5] <- "completetechsecondary"
wvdat_bind$educationchar[wvdat_bind$education == 6] <- "incompletesecondary"
wvdat_bind$educationchar[wvdat_bind$education == 7] <- "completesecondary"
wvdat_bind$educationchar[wvdat_bind$education == 8] <- "uniwodegree"
wvdat_bind$educationchar[wvdat_bind$education == 9] <- "unidegree"


anyNA(wvdat_bind$educationchar)
anyNA(wvdat_bind$education)

table(wvdat_bind$educationchar, wvdat_bind$education)

#FINSAT 

summary(wvdat_bind$finsat)

# 1-completely dissatisfied 2 3 4 5 6 7 8 9 10-completely satisfied

#AGE 

summary(wvdat_bind$age)

#SOCIAL CLASS
# status 1 upper 5 lower
#class 1 lower 5 upper
names(wvdat_bind)[names(wvdat_bind) == "V238"] <- "status"
wvdat_bind$class <- NULL
wvdat_bind$class[wvdat_bind$status == 1] <- 5
wvdat_bind$class[wvdat_bind$status == 2] <- 4 
wvdat_bind$class[wvdat_bind$status == 3] <- 3 
wvdat_bind$class[wvdat_bind$status == 4] <- 2 
wvdat_bind$class[wvdat_bind$status == 5] <- 1 
wvdat_bind$class[wvdat_bind$status == NA] <- NA

summary(wvdat_bind$class)
table(wvdat_bind$class, wvdat_bind$status)

# upper class 1 2 3 4 5 lower class

#INCOME 

summary(wvdat_bind$incomescale)

# lower step 1 2 3 4 5 6 7 8 9 10 tenth step  

#class
#incequalnum
#employment1

##################
###REGRESSION AND MULTILEVEL
###################
install.packages("lme4")
install.packages(c("arm", "foreach", "doParallel", "data.table", "purrr"))
install.packages("StanHeaders")   
library(lme4)
library(rstan)
library(arm)
install.packages(c("foreach", "doParallel"))
library(foreach)
library(doParallel)
library(data.table)
library(purrr)
library(stringr)
library(dplyr)
library(stringr)

source("mlAMEs.R")


registerDoParallel(6) 



#regression model with complete pooling
allreg <- lm(formula = incequalnum ~ class + finsat + education + gender + incomescale + age, data = wvdat_bind)
summary(allreg)
#Ml without Predictor
mlwopre <- lmer(formula = incequalnum ~ 1 + (1|country), wvdat_bind)
mlwgroup <- lmer(formula= incequalnum ~ class + finsat + incomescale + gender +education + age + (1 + gini|country) , wvdat_bind)
mlwind <- lmer(formula= incequalnum ~ class + finsat + incomescale + gender +education + age + (1|country) , wvdat_bind)

ranef(mlwgroup)
fixef(mlwgroup)

sims_mlwgroup <- sim(object = mlwgroup, n.sims = 1000)
glimpse(sims_mlwgroup)
fixed_coefs <- sims_mlwgroup@fixef
country_intercepts <- sims_mlwgroup@ranef$country[,,1]
lower = quantile(fixed_coefs[,"class"], prob = 0.025) # lower bound

upper = quantile(fixed_coefs[,"class"], prob = 0.975) # upper

fit = quantile(sims_mlwgroup, prob = 0.5)
sims_mlwgroup %>% 
        group_by(x) %>% 
        summarize(lower = quantile(y, prob = 0.025),
                  upper = quantile(y, prob = 0.975),
                  fit = quantile(y, prob = 0.50))

data <- dplyr::select(wvdat_bind, country) 
posteriors <- list(mu = fixed_coefs[,],
                   beta_sex = fixed_coefs[,"class"], # this is fixed
                   alpha_county = country_intercepts) # this is as observed
glimpse(posteriors)
linear_predictor <- "mu[n] + alpha_county[n, country] + beta_sex[n] * x[k]"

popavg_pred_1 <- mlAMEs(data = data, 
                        levels = levels,
                        draws = 1000, 
                        posterior = posteriors, 
                        linear_predictor = linear_predictor, 
                        type = "linear")

glimpse(popavg_pred_1)

popavg_pred_1 %>% 
        group_by(x) %>% 
        summarize(lower = quantile(y, prob = 0.025),
                  upper = quantile(y, prob = 0.975),
                  fit = quantile(y, prob = 0.50))
ggplot() +
        geom_hline(yintercept = fixef(mlwgroup)[1]) +
        geom_point(data = wvdat_bind, aes(x = country, y = incequalnum), color = "grey", 
                   position = position_jitter(), alpha = 0.2) +
        geom_pointrange(data = country_estimates, aes(x = country, y = fit, ymin = lower, ymax = upper)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y = "Attitudes of Income equality", x = "") +
        coord_cartesian(ylim = c(-10, 10))

library(ggplot2)

ggplot(wvdat_bind, aes(x = incomescale, y = incequalnum)) + 
        geom_point() +
        stat_smooth(method = "lm", col = "red") 
class(wvdat_bind$education)

ggplot(wvdat_bind, aes(x=incomescale, y=incequalnum) +
        geom_smooth(method="lm") +
        geom_point(size=3) +
        theme_bw() + 
        xlab("Income level") +
        ylab("Attitudes towards Income Equality") +
        ggtitle("Class and Equality Perception") + 
        expand_limits(y=0) +
        scale_y_continuous(breaks = 0:10) + 
        scale_x_continuous(breaks = 1:5))


library(stargazer)
stargazer(allreg, 
          mlwopre,
          mlwind,
          mlwgroup,
          out = "table.html", 
          digits = 3,
          dep.var.labels = "Attitudes towards Income Equality",
          order = c("class",                       
                    "finsat",
                    "incomescale",                     
                    "gender",       
                    "age",                 
                    "education"
                    ),
          covariate.labels = c("Socioeconomic Status",
                               "Financial Satisfaction",
                               "Income level",
                               "Gender",
                               "Age",
                               "Education"
          )
)




ggplot() +
        geom_pointrange(data = wvdat_bind, aes(x = country, y = depvarreg)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y = "Xenophobic tendency", x = "") +
        coord_cartesian(ylim = c(-1,1))

coef(M1)
fixef(M1)
ranef(M4)

##################
###INTERPRETATION
#################

                


M5 <- lmer(formula= incequalnum ~ class + gini (1 + class |country) , wvdat_bind)
summary(M5)

sims_M5 <- sim(object = M5, n.sims = 1000)
glimpse(sims_M5)
fixed_coefs <- sims_M5@fixef
country_intercepts <- sims_M5@ranef$country[,,1]
colnames(fixed_coefs)[1:2] <- c("mu", "income scale")
country_intercepts[1:10, 1:8]
quantile(fixed_coefs[,"income scale"], prob = 0.025) # lower bound

quantile(fixed_coefs[,"income scale"], prob = 0.975) # upper bound

head(ranef(M5)$country, 2)

# 95% confidence interval for varying-intercept of USA
quantile(country_intercepts[,unique(wvdat_bind$country)[4]], prob = 0.025) # lower bound

quantile(country_intercepts[,unique(wvdat_bind$country)[4]], prob = 0.975) # lower bound

# Step 2 for first parameter draw
expected_values <- fixed_coefs[1, "mu"] + # extract intercept, (scalar)
        fixed_coefs[1, "income scale"] + # extract parameter for each observation's sex (vector)
        country_intercepts[1, as.character(wvdat_bind$country)] # extract relevant parameter for each observation's county

# step 3 for first parameter draw
average_pred <- rep(NA, times = nrow(fixed_coefs)) # set up vector for storage
average_pred[1] <- mean(expected_values) # average over cases
average_pred[1:10]

source("mlAMEs.R")

data <- dplyr::select(incequalnum, country) 

# set levels
levels <- list(x = 0:1) # 0 is female, 1 is male

# assemble the sampling distribution of each estimated parameter in a named list
posteriors <- list(mu = fixed_coefs[,"mu"],
                   beta_sex = fixed_coefs[,"income scale"], # this is fixed
                   alpha_country = country_intercepts) # this is as observed
glimpse(posteriors)

linear_predictor <- "mu[n] + alpha_country[n, country] + beta_income[n] * x[k]"

country_estimates <- data.frame(sims_M5@ranef$country[,,1])

# rename columns of county_intercepts to match original names in the data
country_estimates <- gather(data = country_estimates, key = "country", value = "estimate")

# compute point estimates and 95% confidence intervals
country_estimates <- country_estimates %>% 
        group_by(country) %>%
        summarize(fit = mean(estimate),
                  lower = quantile(estimate, probs = 0.025),
                  upper = quantile(estimate, probs = 0.975))

##################
###VISUALIZATION
##################
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(stringr)


ggplot(data = wvdat_bind, mapping = aes(x = incequalnum, y = incomescale))
ggplot(data = wvdat_bind, mapping = aes(x = incequalnum, y = incomescale)) + geom_point()

geom_pointrange(data = state_estimates, aes(x = state, y = fit, ymin = lower, ymax = upper)) +

#Ulke ulke ggplot
ggplot() +
        geom_hline(yintercept = fixef(M5)[1]) +
        geom_point(data = wvdat_bind, aes(x = country, y = incequalnum), color = "grey", 
                   position = position_jitter(), alpha = 0.2) +
        geom_pointrange(data = country_estimates, aes(x = country, y = fit, ymin = lower, ymax = upper)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y = "Income equality support", x = "")


#grafik renkli
country_estimates <- data.frame(sims_M5@ranef$country[,,1])

# pick the counties of interest and adjust the column names
country_estimates <- country_estimates[,c("usa", "brazil","chile", "colombia","ecuador","haiti", "mexico", "uruguay")]
colnames(county_estimates) <- c("Freiburg", "Karlsruhe", "Bielefeld", "Krefeld", "Chemnitz", "Dresden", "Bremen", "Bremerhaven")

# one column for the counties, one column for the respective estimates
country_estimates <- gather(data = country_estimates, key = "country", value = "estimate")

# add a state column
country_estimates$country <- ifelse(country_estimates$country == "usa" ,
                                 ifelse(country_estimates$country == "brazil",
                                        ifelse(country_estimates$country == "chile" ,
                                                ifelse(country_estimates$country == "colombia" ,
                                                       ifelse(country_estimates$country == "ecuador",
                                                              ifelse(country_estimates$country == "haiti",
                                                                     ifelse(country_estimates$country == "mexico",
                                                                            ifelse(country_estimates$country == "uruguay"))))))))
ggplot(data = country_estimates, aes(estimate, group = country, fill = country)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("pink", "darkorchid3", "darkred", "darkolivegreen3","darkcyan", "darkgoldenrod3", "antiquewhite3", "coral2")) +
        theme_bw() +
        labs(x = "Equal income", y = "Density")

ggplot(data = deneme1, mapping = aes(x = Var2, y = Var1)) + geom_point()


summary(wvdat_bind$employment1)
deneme = table(wvdat_bind$employment1, wvdat_bind$country)
levels(deneme)=c("employment","country" )
summary(deneme)
deneme1= data.frame(deneme)
deneme1

ggplot(data = deneme1) +
        geom_bar(mapping = aes(x = Var1, y = Freq), stat = "Var2")
demo <- tribble(
        ~country,         ~employment,
        "brazil",       637/(637+849),
        "chile",        502/(498+502),
        "colombia",     712/(712+800),
        "ecuador",      645/(645+557),
        "haiti",        836/(836+1160),
        "mexico",       800/2000,
        "uruguay",      482/(482+518),
        "usa",          1037/(1037+1195)
        
)

demo
ggplot(data = demo) +
        geom_bar(mapping = aes(x = country, y = employment, colour = country), stat = "identity") 
+ ylim(0,1)

educreg= lm(formula= incequalnum ~ education, data= wvdat_bind)
summary(educreg)

depvarreg<- lmer(formula= incequalnum~1+ (1|country), data=wvdat_bind)
sims_depvarreg <- sim(object = depvarreg, n.sims = 1000)
glimpse(sims_depvarreg)
fixed_coefs <- sims_depvarreg@fixef
country_intercepts <- sims_depvarreg@ranef$country[,,1]
state_estimates <- gather(data = state_estimates, key = "state", value = "estimate")

# compute point estimates and 95% confidence intervals
country_estimates <- country_estimates %>% 
        group_by(country) %>%
        summarize(fit = mean(estimate),
                  lower = quantile(estimate, probs = 0.025),
                  upper = quantile(estimate, probs = 0.975))
country_estimates$country <- reorder(country_estimates$country, country_estimates$fit)

# apply ordering to data
wvdat_bind$coun <- factor(wvdat_bind$coun, levels = levels(country_estimates$country))
ggplot() +
        geom_hline(yintercept = fixef(depvarreg)[1]) +
        geom_pointrange(data = country_estimates, aes(x = country, y = fit, ymin = lower, ymax = upper)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y = "Attitude towards Income Equality", x = "")
library(lme4)
df_no_pooling <- lmList(formula= incequalnum ~ 1 | country, data=wvdat_bind) %>%
        coef() %>%
        
head(df_no_pooling)

######################
##COmplete and no pooling gosterimi 
######################
class_comp_pool <- lm(formula = incequalnum ~ class , wvdat_bind)

df_pooled <- data_frame(
        Model = "Complete pooling",
        Country = unique(wvdat_bind$country),
        Intercept = coef(class_comp_pool)[1], 
        Slope = coef(class_comp_pool)[2])

head(df_pooled)

library(dplyr)
library(tibble)
df_no_pooling <- lmList(incequalnum ~ class |country, wvdat_bind) %>% 
        coef() %>% 
        # Subject IDs are stored as row-names. Make them an explicit column
        rownames_to_column("country") %>% 
        rename(Intercept = `(Intercept)`, Slope = class) %>% 
        add_column(Model = "No pooling")
       

head(df_no_pooling)
library(ggplot2)
# COMPLETE POOLIG AND NO POOLING GRAPH 
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
        left_join(wvdat_bind, by = "country")
p_model_comparison <- ggplot(df_models) + 
        aes(x = class, y = incequalnum) + 
        # Set the color mapping in this layer so the points don't get a color
        geom_abline(aes(intercept = Intercept, slope = Slope, color = Model),
                    size = .75) + 
        geom_point() +
        facet_wrap("country") +
        labs(x = "Class", y = "Attitudes to Equality") + 
        scale_x_continuous(breaks = 0:5) + 
        # Fix the color palette 
        scale_color_brewer(palette = "Dark2") + 
        theme(legend.position = "top")

p_model_comparison










