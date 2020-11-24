## Agent Based Modelling in R using RNet ##
install.packages("rJava")
install.packages("RNetLogo")

install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")

library(rJava)
library(RNetLogo)

library(dplyr)
library(reshape2)
library(ggplot2)


### Simulation storage
### Compiling full model for graphing / analysis ###

# Version for 0 Prior variance run
cascade_model_BSCM_pilot <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
write.csv(cascade_model_BSCM_pilot, file = "cascade_model_BSCM_pilot_4_social_influence.csv")
# 
# # loading model and adding opinion proportion to the model
# cascade_model_BSCM_pilot <- data.frame(read.csv('~/cascade_model_BSCM_pilot.csv'))
# cascade_model_BSCM_pilot$Opinion_Prop <- (cascade_model_BSCM_pilot$Opinion_A / (cascade_model_BSCM_pilot$Opinion_A + cascade_model_BSCM_pilot$Opinion_B))
# 
# # Version for .1 Prior variance run
# #model_C2Pvar.1_full <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# #write.csv(model_C2Pvar.1_full, file = "Cascade_model_C2Pvar.1_full.csv")
# 
# # Version for .2 Prior variance run
# #model_C2Pvar.2_full <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# #write.csv(model_C2Pvar.2_full, file = "Cascade_model_C2Pvar.2_full.csv")
# 
# #model_C2_PVar_ALL <- rbind(model_C2Pvar0_full,model_C2Pvar.1_full,model_C2Pvar.2_full)
# #write.csv(model_C2_PVar_ALL, file = "Cascade_model_C2_PVar_ALL.csv")
# 
# # Model version with Singular opinion proportion
# model_C2LV_full <- read.csv("Cascade_model_2LV_df3_full.csv")
# model_C2LV_full$P_H_Given_C <- factor(model_C2LV_full$P_H_Given_C, levels = c('+/- 0.2','+/- 0.1','Neutral'))
# #model_C6_full <- transform(model_C6_full, Opinion_Prop = (Opinion_A/(Opinion_A + Opinion_B))
# 
# ######################
# ## Graphing Model 1 ##
# ######################
# 
# ## proportion of believers figures
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Opinion_Prop), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 1, by = .1),1), limits = c(0, 1)) +
#   theme_bw() +
#   #scale_color_manual(values=c("#fc0000", "#006dfc", "#000000"), labels=c('+/- 0.2','+/- 0.1','Neutral')) +
#   #geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   #geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   labs(x='Interconnectivity (%)', y='Global Proportion of Opinions', linetype='Opinion Strength') +
#   theme(text = element_text(size = 14),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)
# 
# 
# ## peak rates of spread
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Peak_Rate), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
#   theme_bw() +
#   #scale_color_manual(values=c("#000000")) +
#   labs(x='Interconnectivity (%)', y='Peak Rate of Spread (%)', linetype='Opinion Strength') +
#   theme(text = element_text(size = 13),
#         #legend.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)
# 
# 
# ## Degree of Clustering
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Prcnt_Same_Clust), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
#   theme_bw() +
#   scale_color_manual(values=c("#000000")) +
#   #geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   #geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   labs(x='Interconnectivity (%)', y='Clustering (% Like-minded Neighbors)', linetype='Opinion Strength') +
#   theme(text = element_text(size = 20),
#         #legend.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)



