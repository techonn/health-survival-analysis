library(tidyverse)
library(survival)
library(survminer)
library(ggpubr)
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

mCoxReduced<-step(mCox)

cox.zph(mCoxReduced)

mCoxStrat <- coxph(Surv(TIMEDTH,DEATH) ~ SEX + AGE + SYSBP + CURSMOKE + CIGPDAY + DIABETES + 
                     BPMEDS + GLUCOSE + as.factor(educ) + strata(PREVCHD), data = frmgham )
cox.zph(mCoxStrat)
ggcoxzph(cox.zph(mCoxStrat))
AIC(Cox, mCox, mCoxStrat )

summary(mCoxStrat)