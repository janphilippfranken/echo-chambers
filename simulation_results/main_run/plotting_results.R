## Agent Based Modelling in R using RNet ##
library(rJava)
library(RNetLogo)

library(dplyr)
library(reshape2)
library(ggplot2)


### Simulation storage
### Compiling full model for graphing / analysis ###

# Version for 0 Prior variance run
# cascade_model_BSCM_pilot <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# write.csv(cascade_model_BSCM_pilot, file = "cascade_model_BSCM_pilot_5_social_influence.csv")
# 
# # loading model and adding opinion proportion to the model
cascade_model_BSCM_pilot <- data.frame(read.csv('/Users/jan-philippfranken/documents/github/information_cascades_jpf_tdp_2020/simulation_results/main_run/cascade_model_BSCM_5_asocial_agents.csv'))
cascade_model_BSCM_pilot$Opinion_Prop <- (cascade_model_BSCM_pilot$Opinion_A / (cascade_model_BSCM_pilot$Opinion_A + cascade_model_BSCM_pilot$Opinion_B))
hist(cascade_model_BSCM_pilot$Opinion_Prop)
hist(cascade_model_BSCM_pilot$Prcnt_Same_Clust)

b <-cascade_model_BSCM_pilot$Prcnt_Same_Clust
d <- cascade_model_BSCM_pilot$Opinion_Prop
# 
# # Version for .1 Prior variance run
# #model_C2Pvar.1_full <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# #write.csv(model_C2Pvar.1_full, file = "Cascade_model_C2Pvar.1_full.csv")
# 
l
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
# need to change the name for the other data set to social 3
social1 <- ggplot(cascade_model_BSCM_pilot, aes(x = Links, linetype=Expertise_Influence)) +
   stat_summary(fun=mean, aes(y = Opinion_Prop), geom="line", size = .8) +
   scale_x_continuous(breaks = round(seq(min(cascade_model_BSCM_pilot$Links) - 0.5, max(cascade_model_BSCM_pilot$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
   scale_y_continuous(breaks = round(seq(0.48, 0.52, by = 0.05),0.05), limits = c(0.48, 0.52)) +
   theme_bw() +
   scale_color_manual(values=c("green", "#006dfc", "#000000"), labels=c('+/- 0.40','+/- 0.20','0.00')) +
   geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
   geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
   labs(x='Interconnectivity (%)', y='Global Proportion of Opinions', linetype=expression('Expertise strength'~tau) +
   theme(text = element_text(size = 14),
         panel.grid = element_blank(),
         axis.title.y = element_text(hjust = 0.5, vjust = 1),
         legend.position = 'right') +
   facet_grid(. ~ P_Prop)
social3 <- social3 + labs(title = "", x="", y="")
social1 <- social1 + labs(title = "", x="", y="")
# 
## peak rates of spread
 ggplot(cascade_model_BSCM_pilot, aes(x = Links, linetype=Expertise_Influence)) +
   stat_summary(fun.y=mean, aes(y = Peak_Rate), geom="line", size = .8) +
   scale_x_continuous(breaks = round(seq(min(cascade_model_BSCM_pilot$Links) - 0.5, max(cascade_model_BSCM_pilot$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
   theme_bw() +
   scale_color_manual(values=c("#000000")) +
   labs(x='Interconnectivity (%)', y='Peak Rate of Spread (%)', linetype='Expertise') +
   theme(text = element_text(size = 13),
         #legend.title = element_blank(),
         panel.grid = element_blank(),
         axis.title.y = element_text(hjust = 0.5, vjust = 1),
         legend.position = 'right') +
   facet_grid(. ~ P_Prop)
# 
# 
## Degree of Clustering
# need to change the name for the other data set to social 4
social2 <- ggplot(cascade_model_BSCM_pilot, aes(x = Links, linetype=Expertise_Influence)) +
   stat_summary(fun.y=mean, aes(y = Prcnt_Same_Clust), geom="line", size = .8) +
   scale_x_continuous(breaks = round(seq(min(cascade_model_BSCM_pilot$Links) - 0.5, max(cascade_model_BSCM_pilot$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
   theme_bw() +
   scale_color_manual(values=c("#000000")) +
   geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
   geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
  labs(x='Interconnectivity (%)', y='Clustering (% Like-minded Neighbors)', linetype='Expertise') +
   theme(text = element_text(size = 14),
         #legend.title = element_blank(),
         panel.grid = element_blank(),
         axis.title.y = element_text(hjust = 0.5, vjust = 1),
         legend.position = 'right') +
   facet_grid(. ~ P_Prop)
social4 <- social4 + labs(title = "", x="", y="")
social2 <- social2 + labs(title = "", x="", y="")


library(rlang)
library(ggpubr)
library(MuMIn)
social1
social2
social3
social4
# then create figure combining single plots 
fullFigure <- ggarrange(social1,social3,social2,social4, rremove("x.text"), 
                        labels = c("A","B","C","D"),
                        ncol = 2, nrow = 2)

fullFigure
annotate_figure(fullFigure,
                bottom = text_grob("Interconnectivity (%)", color = "black",
                                   hjust = 0.5, x = 0.5, size = 16))
                #,
                #left = text_grob("Clustering (% of like-minded neighbors)", color = "black", rot = 90, size = 16))

ggplot(kk, aes(x=all_opinion_props, color=all_agents)) +
   geom_histogram(fill="white", position="dodge")+
   theme(legend.position="top")
# Add mean lines


aa <- ggplot(kk, aes(x=Global_Opinions, fill=Agents)) +
  theme_minimal() 
aa
aa + geom_histogram(aes(fill = Agents), binwidth = 0.001) + theme_minimal() +  labs(x="Global Opinion Proportions", y="Count") 

aa + scale_color_grey()+scale_fill_grey() +
   theme_classic() + 
   geom_histogram(aes(fill = Agents), binwidth = 0.001)  +  
   
   labs(x="Global Belief Proportions (h vs \neg h)", y="Count") +
   labs(fill = "Agent population") + theme(legend.position = c(0.7, 0.2),
                                         legend.direction = "horizontal") 




ggplot(kk, aes(x=all_opinion_props, color=all_agents)) +
   geom_histogram(fill="white", alpha=0.5, position="identity")

ggplot(data=kk, xName='weight',
                  groupName='sex', legendPosition="top",
                  alpha=0.5, addDensity=TRUE,
                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1.5)
