library(lme4)
library(lmerTest)
library(sjPlot)
library(sjmisc)

#kenyadata
## Variables
## DVs: Eng Spelling, Eng LPM, Eng WPM, Swa LPM, Swa WPM
## IVs: gender, ses, age, group (lang3_swaref), Phono Aware (BL_gll1_bgsnds), Recep Vocab (BL_gll3_rcplang)

###MIJIKENDA REFRENCE GROUP
kenyadata$lang3_swaref<-relevel(kenyadata$lang3_swaref, "Mijikenda")

#DV: Spelling
model0_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_spell)

model1_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_spell)

model2_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_spell)

model3_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_spell)

anova(model0_mijiref_spell,model1_mijiref_spell,model2_mijiref_spell,model3_mijiref_spell)
tab_model(model2_mijiref_spell, show.se=TRUE, show.std=TRUE)
ranova(model2_mijiref_spell)

#DV: English Letter per Minute
model0_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_leng)

model1_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_leng)

model2_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_leng)

model3_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_leng)

anova(model0_mijiref_leng,model1_mijiref_leng,model2_mijiref_leng,model3_mijiref_leng)
tab_model(model1_mijiref_leng, show.se=TRUE, show.std=TRUE)
ranova(model1_mijiref_leng)

#DV: English Word per Minute

model0_mijiref_weng2<-lmer(wpm_eng~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_weng) #singular fit

model0_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_weng)

model1_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_weng)

model2_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref+(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_weng)

model3_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_weng)

anova(model0_mijiref_weng,model1_mijiref_weng,model2_mijiref_weng,model3_mijiref_weng)
tab_model(model3_mijiref_weng, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_weng)

#DV: Swahili Letter per Minute
model0_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_lswa)

model1_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_lswa)

model2_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_lswa)

model3_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_lswa)

anova(model0_mijiref_lswa,model1_mijiref_lswa,model2_mijiref_lswa,model3_mijiref_lswa)
tab_model(model3_mijiref_lswa, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_lswa)

#DV: Swahili Word per Minute
model0_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_wswa)

model1_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_wswa)

model2_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_wswa)

model3_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_wswa)

anova(model0_mijiref_wswa,model1_mijiref_wswa,model2_mijiref_wswa,model3_mijiref_wswa)
tab_model(model3_mijiref_wswa, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_wswa)

###SWAHILI REFRENCE GROUP
kenyadata$lang3_swaref<-relevel(kenyadata$lang3_swaref, "Swahili")

#DV: Spelling
model0_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_spell)

model1_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_spell)

model2_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_spell)

model3_mijiref_spell<-lmer(spell~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_spell)

anova(model0_mijiref_spell,model1_mijiref_spell,model2_mijiref_spell,model3_mijiref_spell)
tab_model(model2_mijiref_spell, show.se=TRUE, show.std=TRUE)
ranova(model2_mijiref_spell)

#DV: English Letter per Minute
model0_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_leng)

model1_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_leng)

model2_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_leng)

model3_mijiref_leng<-lmer(lpm_eng~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_leng)

anova(model0_mijiref_leng,model1_mijiref_leng,model2_mijiref_leng,model3_mijiref_leng)
tab_model(model1_mijiref_leng, show.se=TRUE, show.std=TRUE)
ranova(model1_mijiref_leng)

#DV: English Word per Minute

model0_mijiref_weng2<-lmer(wpm_eng~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_weng) #singular fit

model0_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_weng)

model1_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_weng)

model2_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref+(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_weng)

model3_mijiref_weng<-lmer(wpm_eng~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_weng)

anova(model0_mijiref_weng,model1_mijiref_weng,model2_mijiref_weng,model3_mijiref_weng)
tab_model(model3_mijiref_weng, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_weng)

#DV: Swahili Letter per Minute
model0_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_lswa)

model1_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_lswa)

model2_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_lswa)

model3_mijiref_lswa<-lmer(lpm_swa~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1+wave|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_lswa)

anova(model0_mijiref_lswa,model1_mijiref_lswa,model2_mijiref_lswa,model3_mijiref_lswa)
tab_model(model3_mijiref_lswa, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_lswa)

#DV: Swahili Word per Minute
model0_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model0_mijiref_wswa)

model1_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave+lang3_swaref+BL_gll1_bgsnds+BL_gll3_rcplang+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model1_mijiref_wswa)

model2_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave*(BL_gll1_bgsnds+BL_gll3_rcplang)+lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model2_mijiref_wswa)

model3_mijiref_wswa<-lmer(wpm_swa~male+BL_ses+age_child+wave*lang3_swaref*(BL_gll1_bgsnds+BL_gll3_rcplang)+(1|child_id), data=kenyadata, na.action=na.omit)
summary(model3_mijiref_wswa)

anova(model0_mijiref_wswa,model1_mijiref_wswa,model2_mijiref_wswa,model3_mijiref_wswa)
tab_model(model3_mijiref_wswa, show.se=TRUE, show.std=TRUE)
ranova(model3_mijiref_wswa)
