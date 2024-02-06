library(tidyverse)
library(survival)
library(survminer)
library(ggpubr)
install.packages("survminer")
frmgham <- read_rds("frmgham_data_cleaned.RData")

#1A 
frmgham$TDeath <- Surv(frmgham$TIMEDTH, frmgham$DEATH)
ggsurvplot(survfit(TDeath ~ BPMEDS, data = frmgham), risk.table=TRUE, conf.int = TRUE, pval = TRUE)

survdiff(formula = TDeath~BPMEDS, data=frmgham)

#1B
frmghamC <- frmgham[complete.cases(frmgham),]

Cox <- coxph(Surv(TIMEDTH,DEATH) ~ BPMEDS, data = frmgham)
summary(Cox)

mCox<-coxph(Surv(TIMEDTH,DEATH) ~ SEX + TOTCHOL + AGE + SYSBP + DIABP + CURSMOKE + CIGPDAY + BMI + DIABETES + BPMEDS + HEARTRTE + GLUCOSE + as.factor(educ) + PREVCHD, data = frmgham)
summary(mCox)
diastolic blood pressure, BMI, Serum Total Cholesterol (mg/dL), and heart rate removed. 
mCoxReduced<-step(mCox)

cox.zph(mCoxReduced)

mCoxStrat <- coxph(Surv(TIMEDTH,DEATH) ~ SEX + AGE + SYSBP + CURSMOKE + CIGPDAY + DIABETES + 
                     BPMEDS + GLUCOSE + as.factor(educ) + strata(PREVCHD), data = frmgham )
cox.zph(mCoxStrat)
ggcoxzph(cox.zph(mCoxStrat))
AIC(Cox, mCox, mCoxStrat )

summary(mCoxStrat)



#2A 
mW <- survreg(Surv(TIMEDTH, DEATH) ~ SEX + TOTCHOL + AGE + SYSBP + DIABP + CURSMOKE + CIGPDAY + BMI + DIABETES + BPMEDS + HEARTRTE + GLUCOSE + as.factor(educ) + PREVCHD, data = frmgham, dist ="weibull") 
summary(mW)

exp(cbind(Cox=c(NA,coef(mCox)),Weibull=-coef(mW)))

mWReduced <- step(mW)
summary(mWReduced)

#3A
library(tidyverse)
OAC <- read_rds("OAC_dat.RData")
install.packages("lme4")
library(lme4)
install.packages("epitools")
library(epitools)
OAC$alive <- OAC$total - OAC$deaths
AT <- sum(OAC[OAC$treated==1,]$alive)
AC <- sum(OAC[OAC$treated==0,]$alive)
DT <- sum(OAC[OAC$treated==1,]$deaths)
DC <- sum(OAC[OAC$treated==0,]$deaths)
OR <- (DT*AC)/(DC*AT)
OR
ORR <- ((691/5044)/(4353/5044))/((850/5012)/(4162/5012)); ORR
oddstable <- matrix(c(DT,AT,DC,AC), ncol=2,byrow=TRUE)
colnames(oddstable) <- c("deaths","alive")
rownames(oddstable) <- c("treated","control")
oddstable <- as.table(oddstable)
oddstable
oddsratio(oddstable)

#3B 
m1 <- glm(cbind(deaths, total-deaths)~treated, family = "binomial", data=OAC)
m2 <- glmer(cbind(deaths,total-deaths)~treated+(treated|study), family="binomial", data=OAC)
summary(m1)
exp(-0.26676)
exp(-1.5)
AIC(m1, m2)

#4A
library(tidyverse)
PD <- read_rds("PD_dat.RData")
ggplot(data=PD[which(PD$ID<=30),],aes(x=Visit_number, y=UPDRS)) +
                 geom_point()+
                 stat_smooth(method="lm", fullrange=TRUE)+
                 xlab("VISIT NUMBER")+ylab("UPDRS")+
                 facet_wrap(~Apathy_Baseline)+
                 theme(axis.title=element_text(size=8), axis.text = element_text(size =5), 
                       strip.text=element_text(size=5))

m1PD <- lmer(UPDRS ~ Apathy_Baseline + Visit_number + Age_Baseline + (1 | ID), data=PD)  
m2PD <- lmer(UPDRS ~ Apathy_Baseline + Visit_number + Age_Baseline + (Visit_number | ID), data=PD)  
m3PD <- lmer(UPDRS ~ Apathy_Baseline + Visit_number + (Visit_number | ID), data=PD)   
AIC(m1PD, m2PD, m3PD)

summary(m2PD)
confint(m2PD)
