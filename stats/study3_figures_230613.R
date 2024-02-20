#-----------------------------------SETUP------------------------------

#Study 3 Figures
#HCP, HCP REP, HCPD, and NSD datasets
#Written by M. Peterson under MIT License 2023
#Nielsen Brain and Behavior Lab

#load libraries
library(mosaic)
library(dplyr)
library(ggpubr)
library(broom)
library(tidyverse)
library(rstatix)
library(psych) #ICC, sphericity, KMO
library(circlize) #chord diagrams
library(readxl) #load excel files
library(gridExtra) #grid of scatterplots
library(reshape)
library(MatchIt) #match participants

#----------------------------------ALL DEMOS----------------------------
#Load ALL .csv
study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")

#Formatting for Demos
study3_demos <- subset(study3, NewNetwork=="1")

#FIGURES    
    #1. Age raincloud plot
    #Filter NSD to just one set (both sets=duplication)
    study3_demos_age <- subset(study3_demos, dataset!="TASK")
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
    ggplot(study3_demos_age, aes(x = dataset, y = Age_in_Yrs, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(11, 36)), clip = "off")+
      labs(y="Age (Years)", x="")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study3_Demos_AgeRain_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    
    #2. FD_AVG Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study3_demos, aes(x = dataset, y = FD_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0.037, .17)), clip = "off")+
      labs(y="Mean FD (mm)", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", hjust = 0.6, angle=90), axis.text.x =element_text(colour = "black", angle=20, hjust = 1, vjust=1))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study3_Demos_AvgFD_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    
    
    #3. MEAN FD x AGE
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study3_demos, aes(x=Age_in_Yrs, y=FD_avg, color=dataset))+
      labs(x = 'Age (Years)', y = 'Mean FD (mm)')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette,  labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette,  labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      geom_point(aes(fill=dataset), colour="black", pch=21)+
      scale_y_continuous(limits = c(0.03, 0.2))+
      geom_smooth(method=lm, aes(fill=dataset), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position="none", legend.title=element_blank(), legend.text=element_text(colour = "black", size = 10), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study3_Demos_AvgFDxAge_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    
    #4. DVARS_avg Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study3_demos, aes(x = dataset, y = DVARS_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(25, 50)), clip = "off")+
      labs(y="Mean DVARS", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", angle=20, hjust = 0.6, vjust=.9))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study3_Demos_AvgDVARS_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    
    #5. Total volumes Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study3_demos, aes(x = dataset, y = Sum_Volumes, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(949, 4750)), clip = "off")+
      labs(y="Total Volumes Available", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=0, hjust = 0.5, vjust=.9))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study3_Demos_TotVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    
    
    #6. Percent available volumes by dataset (raincloud)
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study3_demos, aes(x = dataset, y = Percent_Vols, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(50, 100)), clip = "off")+
      labs(y="% Volumes Available", x="")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=20, hjust = 1, vjust=1))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study3_Demos_PercentVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)

    
#FIGURES: POINTS TO LEFT
    #1. Age raincloud plot (points to left of boxes)
    #Filter NSD to just one set (both sets=duplication)
    study1_demos_age <- subset(study1_demos, dataset!="TASK")
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
    ggplot(study1_demos_age, aes(x = dataset, y = Age_in_Yrs, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.9, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA,
        position= position_nudge(x=.3)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .2
        )
      )+
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(11, 36)), clip = "off")+
      labs(y="Age (Years)", x="")+
      annotate("segment", x = 1.3, xend = 1.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 2.3, xend = 2.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 3.3, xend = 3.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 4.3, xend = 4.3, y = 9.3, yend = 9.7,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = .6), axis.text.x =element_text(colour = "black", hjust = 0.1))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AgeRain_230630.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    #2. FD_AVG Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = FD_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.75, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .2, 
        outlier.shape = NA,
        position= position_nudge(x=.25)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .08, dodge.width=0
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0.037, .17)), clip = "off")+
      labs(y="Mean FD (mm)", x=" ")+
      annotate("segment", x = 1.25, xend = 1.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 2.25, xend = 2.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 3.25, xend = 3.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 4.25, xend = 4.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 5.25, xend = 5.25, y = 0.027, yend = 0.03,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", hjust = 0.6, angle=90), axis.text.x =element_text(colour = "black", angle=20, hjust = 0.3, vjust=.45))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AvgFD_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    
    #6. Percent available volumes by dataset (raincloud)
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = Percent_Vols, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.8, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA, 
        position= position_nudge(x=.28)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .2, dodge.width=0
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(50, 100)), clip = "off")+
      labs(y="% Volumes Available", x="")+
      annotate("segment", x = 1.3, xend = 1.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 2.3, xend = 2.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 3.3, xend = 3.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 4.3, xend = 4.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 5.3, xend = 5.3, y = 46.5, yend = 47.5,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=20, hjust = .5, vjust=.7))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_PercentVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
#---------------------------------FREQ STATS-----------------------------
#SETUP
    #Load ALL .csv
    study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
    
    #Formatting for Demos
    study3_demos <- subset(study3, NewNetwork=="1")

#HCP-DISC    
    HCPDISC_demos <- subset(study3_demos, dataset=="HCP-DISC")    
    
    #AGE
    favstats(HCPDISC_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPDISC_demos$Sex_Bin)
    #Avg FD
    favstats(HCPDISC_demos$FD_avg)
    #%VOLS
    favstats(HCPDISC_demos$Percent_Vols)

#HCP-REP    
    HCPREP_demos <- subset(study3_demos, dataset=="HCP-REP")    
    
    #AGE
    favstats(HCPREP_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPREP_demos$Sex_Bin)
    #Avg FD
    favstats(HCPREP_demos$FD_avg)
    #%VOLS
    favstats(HCPREP_demos$Percent_Vols)
    
#HCPD    
    HCPD_demos <- subset(study3_demos, dataset=="HCPD")    
    
    #AGE
    favstats(HCPD_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPD_demos$Sex_Bin)
    #Avg FD
    favstats(HCPD_demos$FD_avg)
    #%VOLS
    favstats(HCPD_demos$Percent_Vols)
    
#NSD
    NSDR_demos <- subset(study3_demos, dataset=="REST")    
    #AGE
    favstats(NSDR_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(NSDR_demos$Sex_Bin)
    #Avg FD
    favstats(NSDR_demos$FD_avg)
    #%VOLS
    favstats(NSDR_demos$Percent_Vols)
    
    
    NSDT_demos <- subset(study3_demos, dataset=="TASK")    
    #AGE
    favstats(NSDT_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(NSDT_demos$Sex_Bin)
    #Avg FD
    favstats(NSDT_demos$FD_avg)
    #%VOLS
    favstats(NSDT_demos$Percent_Vols)
    
#--------------------------------WANG2014 GROUP REPLICATION: 7N-------------------------------------
#SETUP
  #Load HCP-DISC and HCP-REP AI_7N  
  AI_7N_232 <- read.csv("C:/Users/maddy/box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL_7N/avg_ai/MSHBM_LONG_AVG_AI_HCP_7N_INDAI_Yeo2011GroupParc_230221.csv") #This is with Yeo2011 7N parc boundaries
  AI_7N_REP <- read.csv("C:/Users/maddy/box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/7N_avg_ai/MSHBM_LONG_AVG_AI_HCP_REP_7N_INDAI_GroupParc_230221.csv")
  
  #Merge both datasets
  AI_7N <- merge(AI_7N_232, AI_7N_REP, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "LH_AVG_AI", "RH_AVG_AI", "Network_AVG_AI"), all=TRUE)
  
  #Drop Network 0 (medial wall)
  AI_7N <- subset(AI_7N, Network!=0)
  

  #Separate in HCP-DISC and HCP-REP datasets
      #Load ALL .csv
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      
      #Formatting for Demos
      study3_demos <- subset(study3, NewNetwork=="1")
      
      #Grab subjids lists
      HCPDISC <- subset(study3_demos, dataset=="HCP-DISC")
      HCPDISC_IDS <- HCPDISC$SUBJID
      HCPREP <- subset(study3_demos, dataset=="HCP-REP")
      HCPREP_IDS <- HCPREP$SUBJID
  
      #HCP-DISC AI
      HCPDISC_AI7N <- AI_7N[AI_7N$SUBJID %in% HCPDISC_IDS, ]
      #HCP-REP AI
      HCPREP_AI7N <- AI_7N[AI_7N$SUBJID %in% HCPREP_IDS, ]
  
  #HCP-DISC: Take mean and SE for each network LH and RH AI
    LH_MEAN <- aggregate(LH_AVG_AI ~ Network, data = HCPDISC_AI7N, FUN = mean)
    RH_MEAN <- aggregate(RH_AVG_AI ~ Network, data = HCPDISC_AI7N, FUN = mean)
    
    names(LH_MEAN)[2] <- "MEAN_LH_AI"
    names(RH_MEAN)[2] <- "MEAN_RH_AI"
    LH_MEAN$MEAN_LH_AI <- LH_MEAN$MEAN_LH_AI*100
    RH_MEAN$MEAN_RH_AI <- RH_MEAN$MEAN_RH_AI*100
    
    mean_df <- merge(LH_MEAN, RH_MEAN, by=c("Network"), all=TRUE)
    
    std <- function(x) sd(x)/sqrt(length(x))
    LH_STD <- aggregate(LH_AVG_AI ~ Network, data = HCPDISC_AI7N, FUN = std)
    RH_STD <- aggregate(RH_AVG_AI ~ Network, data = HCPDISC_AI7N, FUN = std)
    
    names(LH_STD)[2] <- "SE_LH"
    names(RH_STD)[2] <- "SE_RH"
    LH_STD$SE_LH <- LH_STD$SE_LH*100
    RH_STD$SE_RH <- RH_STD$SE_RH*100
    se_df <- merge(LH_STD, RH_STD, by=c("Network"), all=TRUE)
   
    HCPDISC_AI_df <- merge(se_df, mean_df, by=c("Network"), all=TRUE)
    
  #HCP-REP: Take mean and SE for each network LH and RH AI
    LH_MEAN <- aggregate(LH_AVG_AI ~ Network, data = HCPREP_AI7N, FUN = mean)
    RH_MEAN <- aggregate(RH_AVG_AI ~ Network, data = HCPREP_AI7N, FUN = mean)
    
    names(LH_MEAN)[2] <- "MEAN_LH_AI"
    names(RH_MEAN)[2] <- "MEAN_RH_AI"
    LH_MEAN$MEAN_LH_AI <- LH_MEAN$MEAN_LH_AI*100
    RH_MEAN$MEAN_RH_AI <- RH_MEAN$MEAN_RH_AI*100
    
    mean_df <- merge(LH_MEAN, RH_MEAN, by=c("Network"), all=TRUE)
    
    std <- function(x) sd(x)/sqrt(length(x))
    LH_STD <- aggregate(LH_AVG_AI ~ Network, data = HCPREP_AI7N, FUN = std)
    RH_STD <- aggregate(RH_AVG_AI ~ Network, data = HCPREP_AI7N, FUN = std)
    
    names(LH_STD)[2] <- "SE_LH"
    names(RH_STD)[2] <- "SE_RH"
    LH_STD$SE_LH <- LH_STD$SE_LH*100
    RH_STD$SE_RH <- RH_STD$SE_RH*100
    se_df <- merge(LH_STD, RH_STD, by=c("Network"), all=TRUE)
    
    HCPREP_AI_df <- merge(se_df, mean_df, by=c("Network"), all=TRUE)
    
    
#HCP-DISC FIGURES
  #LH AVG Network AI Boxplot 
  HCPDISC_AI_df$Network <- as.factor(HCPDISC_AI_df$Network)
  CBIG_Palette_7N <- c("#A753B1", "#7A9DC5", "#439B33", "#E667FF", "#FDFFCE", "#F4BD46", "#DF7380")
  ggplot(HCPDISC_AI_df, aes(x=Network, y=MEAN_LH_AI, fill=Network))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_LH_AI-SE_LH, ymax=MEAN_LH_AI+SE_LH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'LH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette_7N)+
    scale_x_discrete(labels=c("Visual", "Somatomotor", "Dorsal Attention", "Ventral Attention", "Limbic", "Frontoparietal", "Default"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,4.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-DISC_7N_GROUPParc_LH_AVG_AI_230626.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
  #RH AVG Network AI Barplot 
  HCPDISC_AI_df$Network <- as.factor(HCPDISC_AI_df$Network)
  CBIG_Palette_7N <- c("#A753B1", "#7A9DC5", "#439B33", "#E667FF", "#FDFFCE", "#F4BD46", "#DF7380")
  ggplot(HCPDISC_AI_df, aes(x=Network, y=MEAN_RH_AI, fill=Network))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_RH_AI-SE_RH, ymax=MEAN_RH_AI+SE_RH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'RH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette_7N)+
    scale_x_discrete(labels=c("Visual", "Somatomotor", "Dorsal Attention", "Ventral Attention", "Limbic", "Frontoparietal", "Default"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,4.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-DISC_7N_GROUPParc_RH_AVG_AI_230626.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
#HCP-REP FIGURES
  #LH AVG Network AI Boxplot 
  HCPREP_AI_df$Network <- as.factor(HCPREP_AI_df$Network)
  CBIG_Palette_7N <- c("#A753B1", "#7A9DC5", "#439B33", "#E667FF", "#FDFFCE", "#F4BD46", "#DF7380")
  ggplot(HCPREP_AI_df, aes(x=Network, y=MEAN_LH_AI, fill=Network))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_LH_AI-SE_LH, ymax=MEAN_LH_AI+SE_LH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'LH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette_7N)+
    scale_x_discrete(labels=c("Visual", "Somatomotor", "Dorsal Attention", "Ventral Attention", "Limbic", "Frontoparietal", "Default"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,4.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-REP_7N_GROUPParc_LH_AVG_AI_230626.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
  #RH AVG Network AI Barplot 
  HCPREP_AI_df$Network <- as.factor(HCPREP_AI_df$Network)
  CBIG_Palette_7N <- c("#A753B1", "#7A9DC5", "#439B33", "#E667FF", "#FDFFCE", "#F4BD46", "#DF7380")
  ggplot(HCPREP_AI_df, aes(x=Network, y=MEAN_RH_AI, fill=Network))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_RH_AI-SE_RH, ymax=MEAN_RH_AI+SE_RH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'RH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette_7N)+
    scale_x_discrete(labels=c("Visual", "Somatomotor", "Dorsal Attention", "Ventral Attention", "Limbic", "Frontoparietal", "Default"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,4.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-REP_7N_GROUPParc_RH_AVG_AI_230626.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
#--------------------------------WANG2014 GROUP REPLICATION: 17N-------------------------------------
#SETUP
  #Load HCP-DISC and HCP-REP AI_17N  
  AI_17N_232 <- read.csv("C:/Users/maddy/box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/17N_avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_17N_INDAI_YeoGroupParc_230221.csv") #This is with Yeo2011 7N parc boundaries
  AI_17N_REP <- read.csv("C:/Users/maddy/box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/17N_avg_ai/MSHBM_LONG_AVG_AI_HCP_REP_17N_INDAI_YeoGroupParc_230221.csv")
  
  #Merge both datasets
  AI_17N <- merge(AI_17N_232, AI_17N_REP, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "LH_AVG_AI", "RH_AVG_AI", "Network_AVG_AI"), all=TRUE)
  
  #Drop Network 0 (medial wall)
  AI_17N <- subset(AI_17N, Network!=0)
  
  #RE-order networks to match CBIG labels
  mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
  AI_17N$NewNetwork <- recode(AI_17N$Network, !!!mapping)
  
  #Grab IDS
      #Load ALL .csv
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      
      #Formatting for Demos
      study3_demos <- subset(study3, NewNetwork=="1")
      
      #Grab subjids lists
      HCPDISC <- subset(study3_demos, dataset=="HCP-DISC")
      HCPDISC_IDS <- HCPDISC$SUBJID
      HCPREP <- subset(study3_demos, dataset=="HCP-REP")
      HCPREP_IDS <- HCPREP$SUBJID
      
      #HCP-DISC AI
      HCPDISC_AI17N <- AI_17N[AI_17N$SUBJID %in% HCPDISC_IDS, ]
      #HCP-REP AI
      HCPREP_AI17N <- AI_17N[AI_17N$SUBJID %in% HCPREP_IDS, ]
      
#HCP-DISC: Take mean and SE for each network LH and RH AI
  LH_MEAN <- aggregate(LH_AVG_AI ~ NewNetwork, data = HCPDISC_AI17N, FUN = mean)
  RH_MEAN <- aggregate(RH_AVG_AI ~ NewNetwork, data = HCPDISC_AI17N, FUN = mean)
  
  names(LH_MEAN)[2] <- "MEAN_LH_AI"
  names(RH_MEAN)[2] <- "MEAN_RH_AI"
  LH_MEAN$MEAN_LH_AI <- LH_MEAN$MEAN_LH_AI*100
  RH_MEAN$MEAN_RH_AI <- RH_MEAN$MEAN_RH_AI*100
  
  mean_df <- merge(LH_MEAN, RH_MEAN, by=c("NewNetwork"), all=TRUE)
  
  std <- function(x) sd(x)/sqrt(length(x))
  LH_STD <- aggregate(LH_AVG_AI ~ NewNetwork, data = HCPDISC_AI17N, FUN = std)
  RH_STD <- aggregate(RH_AVG_AI ~ NewNetwork, data = HCPDISC_AI17N, FUN = std)
  
  names(LH_STD)[2] <- "SE_LH"
  names(RH_STD)[2] <- "SE_RH"
  LH_STD$SE_LH <- LH_STD$SE_LH*100
  RH_STD$SE_RH <- RH_STD$SE_RH*100
  se_df <- merge(LH_STD, RH_STD, by=c("NewNetwork"), all=TRUE)
  
  HCPDISC_AI_df <- merge(se_df, mean_df, by=c("NewNetwork"), all=TRUE)
  
#HCP-REP: Take mean and SE for each network LH and RH AI
  LH_MEAN <- aggregate(LH_AVG_AI ~ NewNetwork, data = HCPREP_AI17N, FUN = mean)
  RH_MEAN <- aggregate(RH_AVG_AI ~ NewNetwork, data = HCPREP_AI17N, FUN = mean)
  
  names(LH_MEAN)[2] <- "MEAN_LH_AI"
  names(RH_MEAN)[2] <- "MEAN_RH_AI"
  LH_MEAN$MEAN_LH_AI <- LH_MEAN$MEAN_LH_AI*100
  RH_MEAN$MEAN_RH_AI <- RH_MEAN$MEAN_RH_AI*100
  
  mean_df <- merge(LH_MEAN, RH_MEAN, by=c("NewNetwork"), all=TRUE)
  
  std <- function(x) sd(x)/sqrt(length(x))
  LH_STD <- aggregate(LH_AVG_AI ~ NewNetwork, data = HCPREP_AI17N, FUN = std)
  RH_STD <- aggregate(RH_AVG_AI ~ NewNetwork, data = HCPREP_AI17N, FUN = std)
  
  names(LH_STD)[2] <- "SE_LH"
  names(RH_STD)[2] <- "SE_RH"
  LH_STD$SE_LH <- LH_STD$SE_LH*100
  RH_STD$SE_RH <- RH_STD$SE_RH*100
  se_df <- merge(LH_STD, RH_STD, by=c("NewNetwork"), all=TRUE)
  
  HCPREP_AI_df <- merge(se_df, mean_df, by=c("NewNetwork"), all=TRUE)
  

#HCP-DISC FIGURES
  #LH AVG Network AI Boxplot 
  HCPDISC_AI_df$NewNetwork <- as.factor(HCPDISC_AI_df$NewNetwork)
  CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
  network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
  HCPDISC_AI_df$NewNetwork <- factor(HCPDISC_AI_df$NewNetwork, level = network_order)
  ggplot(HCPDISC_AI_df, aes(x=NewNetwork, y=MEAN_LH_AI, fill=NewNetwork))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_LH_AI-SE_LH, ymax=MEAN_LH_AI+SE_LH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'LH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette)+
    scale_x_discrete(labels=c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,5.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-DISC_17N_GROUPParc_LH_AVG_AI_230626.png"), width = 6.9, height = 1.8,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
  #RH AVG Network AI Barplot 
  HCPDISC_AI_df$NewNetwork <- as.factor(HCPDISC_AI_df$NewNetwork)
  CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
  network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
  HCPDISC_AI_df$NewNetwork <- factor(HCPDISC_AI_df$NewNetwork, level = network_order)
  ggplot(HCPDISC_AI_df, aes(x=NewNetwork, y=MEAN_RH_AI, fill=NewNetwork))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_RH_AI-SE_RH, ymax=MEAN_RH_AI+SE_RH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'RH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette)+
    scale_x_discrete(labels=c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,5.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-DISC_17N_GROUPParc_RH_AVG_AI_230626.png"), width = 6.9, height = 1.8,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
#HCP-REP FIGURES
  #LH AVG Network AI Boxplot 
  HCPREP_AI_df$NewNetwork <- as.factor(HCPREP_AI_df$NewNetwork)
  CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
  network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
  HCPREP_AI_df$NewNetwork <- factor(HCPREP_AI_df$NewNetwork, level = network_order)
  ggplot(HCPREP_AI_df, aes(x=NewNetwork, y=MEAN_LH_AI, fill=NewNetwork))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_LH_AI-SE_LH, ymax=MEAN_LH_AI+SE_LH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'LH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette)+
    scale_x_discrete(labels=c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,5.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-REP_17N_GROUPParc_LH_AVG_AI_230626.png"), width = 6.9, height = 1.8,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
  #RH AVG Network AI Barplot 
  HCPREP_AI_df$NewNetwork <- as.factor(HCPREP_AI_df$NewNetwork)
  CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
  network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
  HCPREP_AI_df$NewNetwork <- factor(HCPREP_AI_df$NewNetwork, level = network_order)
  ggplot(HCPREP_AI_df, aes(x=NewNetwork, y=MEAN_RH_AI, fill=NewNetwork))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=MEAN_RH_AI-SE_RH, ymax=MEAN_RH_AI+SE_RH), width=.2,position=position_dodge(.9))+
    labs(x = '', y = 'RH Mean AI (%)')+
    scale_fill_manual(values=CBIG_Palette)+
    scale_x_discrete(labels=c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
    scale_y_continuous(expand = c(0, 0), limits=c(0,5.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x = element_text(colour = "black", size=10,vjust =1,hjust=1, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position ="none", legend.title=element_text(colour = "black"), legend.text=element_text(colour = "black"), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP-REP_17N_GROUPParc_RH_AVG_AI_230626.png"), width = 6.9, height = 1.8,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
  
  
      
  
#----------------------------------HCP TEST-RETEST--------------------------------
#Load HCP test-retest AI data for LH and RH AI
    RETEST1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/RETEST1/avg_ai/MSHBM_LONG_AVG_AI_HCP_RETEST1_230221.csv")
    RETEST2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/RETEST2/avg_ai/MSHBM_LONG_AVG_AI_HCP_RETEST2_230221.csv")
  
    #Drop medial wall
    RETEST1 <- subset(RETEST1, Network!=0)
    RETEST2 <- subset(RETEST2, Network!=0)
    
    #Reorder CBIG Networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    RETEST1$NewNetwork <- recode(RETEST1$Network, !!!mapping)
    RETEST2$NewNetwork <- recode(RETEST2$Network, !!!mapping)
    
    #RENAME AVG_AI variables to distinguish between 1 and 2
    names(RETEST1)[5] <- "RH_AVG_AI_R1"
    names(RETEST1)[6] <- "LH_AVG_AI_R1"
    names(RETEST2)[5] <- "RH_AVG_AI_R2"
    names(RETEST2)[6] <- "LH_AVG_AI_R2"
    
    #Merge RETEST datasets
    AI_RETEST <- merge(RETEST1, RETEST2, by=c("SUBJID", "NewNetwork"), all=FALSE)
    
    #Load demos
    HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
    HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
    FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_HCP_ALL_230515.csv")
    DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_HCP_ALL_230515.csv")
    TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_HCP_230515.csv")
    
    #Format data
    names(HCP_DEMOS)[1] <- "SUBJID"
    names(HCP_R_DEMOS)[1] <- "SUBJID"
    names(FD_AVG)[1] <- "SUBJID"
    names(DVARS_AVG)[1] <- "SUBJID"
    names(TOT_VOLS)[1] <- "SUBJID"
    
    FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
    DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
    TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
    TOT_VOLS$Percent_Vols <- TOT_VOLS$Sum_Volumes / (1196*4) #Number of volumes after skip4 = 1196 x4 runs
    
    #Merge HCP datasets
    comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
    comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
    comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
    HCP_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
    
    #Variable for sex (Male=1, Female=2; same as UT-TD)
    HCP_df$sex <- ifelse(HCP_df$Gender == "F", 2, ifelse(HCP_df$Gender == "M", 1, NA))
    
    #Just include relevant columns
    HCP_df <- HCP_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness")]
    
    #Merge RETEST and DEMOS datasets
    AI_RETEST_HCP <- merge(AI_RETEST, HCP_df, by = c("SUBJID"), all=TRUE)
    
    #Fence outliers for AI TEST and RETEST
        for (network in 1:17) {
          # Subset the dataset based on the current NewNetwork value
          subset_indices <- AI_RETEST_HCP$NewNetwork == network
          
          # Loop through each variable to apply the fencing method
          variables <- c("RH_AVG_AI_R1", "LH_AVG_AI_R1", "RH_AVG_AI_R2", "LH_AVG_AI_R2")
          for (variable in variables) {
            subset_data <- AI_RETEST_HCP[subset_indices, variable]
            
            # Calculate quartiles (Q1 and Q3) and IQR for the current subset
            Q1 <- quantile(subset_data, 0.25)
            Q3 <- quantile(subset_data, 0.75)
            IQR <- Q3 - Q1
            
            # Calculate upper and lower limits for the current subset
            upper_limit <- Q3 + 1.5 * IQR
            lower_limit <- Q1 - 1.5 * IQR
            
            # Create a new column with "_Fenced" suffix for each variable
            fence_column <- paste0(variable, "_Fenced")
            
            # Replace outliers with NA in the new Fenced column
            AI_RETEST_HCP[subset_indices, fence_column] <- subset_data
            AI_RETEST_HCP[subset_indices, fence_column][subset_data < lower_limit | subset_data > upper_limit] <- NA
          }
        }
    
    
    
    #Surface area (do not delete)
      #Load test-retest NSAR/SAI values
      RETEST1_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/RETEST1/HCP_RETEST1_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
      RETEST2_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/RETEST2/HCP_RETEST2_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
      #Formatting variables
      RETEST1_SA$SUBJID <- gsub("^.{0,4}", "", RETEST1_SA$SUBJID) #remove "sub-" string
      RETEST2_SA$SUBJID <- gsub("^.{0,4}", "", RETEST2_SA$SUBJID) #remove "sub-" string
      RETEST1_SA$Network <- gsub("^.{0,8}", "", RETEST1_SA$NETWORK) #remove "NETWORK-" string
      RETEST2_SA$Network <- gsub("^.{0,8}", "", RETEST2_SA$NETWORK) #remove "NETWORK-" string
      
      #Drop medial wall
      RETEST1_SA <- subset(RETEST1_SA, Network!=0) #drop network0
      RETEST2_SA <- subset(RETEST2_SA, Network!=0) #drop network0
      
      #Switch network ordering to reflect CBIG legend ordering
      mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
      oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
      RETEST1_SA$NewNetwork <- mapping[ match(RETEST1_SA$Network, oldvalues) ]
      RETEST1_SA <- RETEST1_SA[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
      RETEST2_SA$NewNetwork <- mapping[ match(RETEST2_SA$Network, oldvalues) ]
      RETEST2_SA <- RETEST2_SA[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
      
      # create % surface area per hemisphere (total SA: 126300.7)
      RETEST1_SA$LH_SA_PERCENT_R1 <- (RETEST1_SA$LH_SA/63103.74)*100
      RETEST1_SA$RH_SA_PERCENT_R1 <- (RETEST1_SA$RH_SA/63196.98)*100
      RETEST2_SA$LH_SA_PERCENT_R2 <- (RETEST2_SA$LH_SA/63103.74)*100
      RETEST2_SA$RH_SA_PERCENT_R2 <- (RETEST2_SA$RH_SA/63196.98)*100
      
      #Merge RETEST SA datasets
      SA_RETEST <- merge(RETEST1_SA, RETEST2_SA, by = c("SUBJID", "NewNetwork"), all=TRUE)
      
      #Calculate mean SA (LH and RH) across network
      LH_SA_MEAN_R1 <- aggregate(LH_SA_PERCENT_R1 ~ NewNetwork, data = SA_RETEST, FUN = mean)
      RH_SA_MEAN_R1 <- aggregate(RH_SA_PERCENT_R1 ~ NewNetwork, data = SA_RETEST, FUN = mean)
      mean_df <- merge(LH_SA_MEAN_R1, RH_SA_MEAN_R1, by=c("NewNetwork"), all=TRUE)
      
      LH_SA_MEAN_R2 <- aggregate(LH_SA_PERCENT_R2 ~ NewNetwork, data = SA_RETEST, FUN = mean)
      RH_SA_MEAN_R2 <- aggregate(RH_SA_PERCENT_R2 ~ NewNetwork, data = SA_RETEST, FUN = mean)
      mean_df <- merge(mean_df, RH_SA_MEAN_R2, by=c("NewNetwork"), all=TRUE)
      mean_df <- merge(mean_df, LH_SA_MEAN_R2, by=c("NewNetwork"), all=TRUE)
      
      #LH and RH mean across RETEST datasets
      mean_df$MEAN_LH_SA <- (mean_df$LH_SA_PERCENT_R1 + mean_df$LH_SA_PERCENT_R2)/2
      mean_df$MEAN_RH_SA <- (mean_df$RH_SA_PERCENT_R1 + mean_df$RH_SA_PERCENT_R2)/2
      
    
    
    
        
#Calculate subject ICC for HCP (n=232) dataset
  #RH FIRST    
    #Calculate subject ICC for HCP dataset 
    # Create an empty dataframe to store ICC3 values
    icc_rh_df <- data.frame(AI_RH_ICC = numeric(0))
    for (subj_id in unique(AI_RETEST_HCP$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(AI_RETEST_HCP, SUBJID == subj_id)[, c("RH_AVG_AI_R1_Fenced", "RH_AVG_AI_R2_Fenced")]
      
      # Rename dataframe
      df_name <- paste("ai_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_rh_df <- rbind(icc_rh_df, data.frame(AI_RH_ICC = icc3_value))
    }
    
    #Add SUBJID variable: 
    icc_rh_df$SUBJID <- unique(AI_RETEST_HCP$SUBJID)
    
    #Add dataset variable
    icc_rh_df$dataset <- "HCP"
    
  #LH (WITHIN-SUBJECT    
    #Calculate subject ICC for HCP dataset 
    # Create an empty dataframe to store ICC3 values
    icc_lh_df <- data.frame(AI_LH_ICC = numeric(0))
    for (subj_id in unique(AI_RETEST_HCP$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(AI_RETEST_HCP, SUBJID == subj_id)[, c("LH_AVG_AI_R1_Fenced", "LH_AVG_AI_R2_Fenced")]
      
      # Rename dataframe
      df_name <- paste("ai_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_lh_df <- rbind(icc_lh_df, data.frame(AI_LH_ICC = icc3_value))
    }
    
    #Add SUBJID variable: 
    icc_lh_df$SUBJID <- unique(AI_RETEST_HCP$SUBJID)
    
    #Add dataset variable
    icc_lh_df$dataset <- "HCP"
    

#Calculate network-level ICC (HCP dataset)
    #RH
    #Calculate subject ICC for HCP dataset 
    # Create an empty dataframe to store ICC3 values
    icc_net_rh_df <- data.frame(AI_RH_ICC = numeric(0))
    for (network in unique(AI_RETEST_HCP$NewNetwork)) {
      # Create dataframe subset
      df_subset <- subset(AI_RETEST_HCP, NewNetwork == network)[, c("RH_AVG_AI_R1_Fenced", "RH_AVG_AI_R2_Fenced")]
      
      # Rename dataframe
      df_name <- paste("ai_wide_dataR.", network, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_net_rh_df <- rbind(icc_net_rh_df, data.frame(AI_RH_ICC = icc3_value))
    }
    
    #Add SUBJID variable: 
    icc_net_rh_df$NewNetwork <- unique(AI_RETEST_HCP$NewNetwork)
    
    #Add dataset variable
    icc_net_rh_df$dataset <- "HCP"
    
     
    #LH (NETWORK)
    #Calculate subject ICC for HCP dataset 
    # Create an empty dataframe to store ICC3 values
    icc_net_lh_df <- data.frame(AI_LH_ICC = numeric(0))
    for (network in unique(AI_RETEST_HCP$NewNetwork)) {
      # Create dataframe subset
      df_subset <- subset(AI_RETEST_HCP, NewNetwork == network)[, c("LH_AVG_AI_R1_Fenced", "LH_AVG_AI_R2_Fenced")]
      
      # Rename dataframe
      df_name <- paste("ai_wide_dataR.", network, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_net_lh_df <- rbind(icc_net_lh_df, data.frame(AI_LH_ICC = icc3_value))
    }
    
    #Add SUBJID variable: 
    icc_net_lh_df$NewNetwork <- unique(AI_RETEST_HCP$NewNetwork)
    
    #Add dataset variable
    icc_net_lh_df$dataset <- "HCP"   
    
#FIGURES
  #NETWORK SCATTERPLOTS
    #Change AI into %
    AI_RETEST_HCP$RH_AVG_AI_R1_Fenced <- AI_RETEST_HCP$RH_AVG_AI_R1_Fenced*100
    AI_RETEST_HCP$RH_AVG_AI_R2_Fenced <- AI_RETEST_HCP$RH_AVG_AI_R2_Fenced*100
    AI_RETEST_HCP$LH_AVG_AI_R1_Fenced <- AI_RETEST_HCP$LH_AVG_AI_R1_Fenced*100
    AI_RETEST_HCP$LH_AVG_AI_R2_Fenced <- AI_RETEST_HCP$LH_AVG_AI_R2_Fenced*100
    
    
    #RH
    for (n in 1:17) {
      subsetted_data <- subset(AI_RETEST_HCP, NewNetwork == n)  # Subsetting the dataset based on Network
      rh_icc <- icc_net_rh_df$AI_RH_ICC
      rh_icc_ordered <- rh_icc[order(icc_net_rh_df$NewNetwork)]
      Network_Names<- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
        
      min_value <- min(subsetted_data$RH_AVG_AI_R1_Fenced, subsetted_data$RH_AVG_AI_R2_Fenced)
      max_value <- max(subsetted_data$RH_AVG_AI_R1_Fenced, subsetted_data$RH_AVG_AI_R2_Fenced)
      
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
      subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
      
        filename <- paste("Study3_HCP_AI_RH_Plots_Network", n, "_230615.png", sep = "")
        plot_title <- paste(Network_Names[n], " ICC: ", format(round(rh_icc_ordered[n], 2), nsmall = 2), sep="")

        ggplot(subsetted_data, aes(x = RH_AVG_AI_R2_Fenced, y = RH_AVG_AI_R1_Fenced, color = NewNetwork)) +
          labs(x = "RH AI % Session 2", y = "RH AI % Session 1") +
          labs(fill = " ", color = " ") +
          ggtitle(plot_title)+
          geom_point(aes(fill = NewNetwork),color="black", pch = 21) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
          geom_smooth(aes(color = NewNetwork), method = "lm", size = 0.75, se = FALSE) +
          scale_y_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.0f", x))) +
          scale_x_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.0f", x))) +
          scale_color_manual(values = CBIG_Palette[n]) +
          scale_fill_manual(values = CBIG_Palette[n]) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0, size=10),
                axis.title = element_text(colour = "black", size = 10),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=9),
                axis.text.x = element_text(colour = "black", size=9),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 11),
                legend.text = element_text(colour = "black", size = 11),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 1.675, height = 1.675,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    }
    
    
    #LH
    for (n in 1:17) {
      subsetted_data <- subset(AI_RETEST_HCP, NewNetwork == n)  # Subsetting the dataset based on Network
      lh_icc <- icc_net_lh_df$AI_LH_ICC
      lh_icc_ordered <- lh_icc[order(icc_net_lh_df$NewNetwork)]
     
      min_value <- min(subsetted_data$LH_AVG_AI_R1_Fenced, subsetted_data$LH_AVG_AI_R2_Fenced)
      max_value <- max(subsetted_data$LH_AVG_AI_R1_Fenced, subsetted_data$LH_AVG_AI_R2_Fenced)
      
      Network_Names<- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
      subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
      
      filename <- paste("Study3_HCP_AI_LH_Plots_Network", n, "_230615.png", sep = "")
      plot_title <- paste(Network_Names[n], " ICC: ", format(round(lh_icc_ordered[n], 2), nsmall = 2), sep="")
      
      ggplot(subsetted_data, aes(x = LH_AVG_AI_R2_Fenced, y = LH_AVG_AI_R1_Fenced, color = NewNetwork)) +
        labs(x = "LH AI % Session 2", y = "LH AI % Session 1") +
        labs(fill = " ", color = " ") +
        ggtitle(plot_title)+
        geom_point(aes(fill = NewNetwork),color="black", pch = 21) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
        geom_smooth(aes(color = NewNetwork), method = "lm", size = 0.75, se = FALSE) +
        scale_y_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.0f", x))) +
        scale_x_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.0f", x))) +
        scale_color_manual(values = CBIG_Palette[n]) +
        scale_fill_manual(values = CBIG_Palette[n]) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0, size=10),
              axis.title = element_text(colour = "black", size = 10),
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=9),
              axis.text.x = element_text(colour = "black", size=9),
              legend.position = "none",
              legend.title = element_text(colour = "black", size = 12),
              legend.text = element_text(colour = "black", size = 12),
              legend.background = element_rect(fill = "white", size = 0.5),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
              axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
              panel.border = element_blank(),
              panel.background = element_blank())
      
      ggsave(filename = filename, width = 1.675, height = 1.675,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
    }    
    

#Network ICC x %SA        
    #Merge mean retest %SA with network ICC datasets
    network_icc <- merge(mean_df, icc_net_lh_df, by="NewNetwork", all=TRUE)
    network_icc <- merge(network_icc, icc_net_rh_df, by="NewNetwork", all=TRUE)
    
    #FIGURES: 
      #RH: RH AI ICC x Mean % RH SA
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        network_icc$NewNetwork <- as.factor(network_icc$NewNetwork)
        filename <- "Study3_HCP_AI_RH_ICC_SA_Plot_230615.png"
        ggplot(network_icc, aes(x = MEAN_RH_SA, y = AI_RH_ICC, color = NewNetwork)) +
          labs(x = "Mean RH % SA", y = "RH AI ICC") +
          labs(fill = " ", color = " ") +
          geom_point(aes(fill = NewNetwork), pch = 21, colour="black") +
          #geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange", size = 1) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette) +
          scale_fill_manual(values = CBIG_Palette) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
        
    
    #LH: LH AI ICC x Mean % LH SA
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        network_icc$NewNetwork <- as.factor(network_icc$NewNetwork)
        filename <- "Study3_HCP_AI_LH_ICC_SA_Plot_230615.png"
        ggplot(network_icc, aes(x = MEAN_LH_SA, y = AI_LH_ICC, color = NewNetwork)) +
          labs(x = "Mean LH % SA", y = "LH AI ICC") +
          labs(fill = " ", color = " ") +
          geom_point(aes(fill = NewNetwork), pch = 21, colour="black") +
          #geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange", size = 1) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette) +
          scale_fill_manual(values = CBIG_Palette) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
        

#NETWORK ICC x tSNR
    #Load tSNR for HCP 232
    tSNR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/ALL/HCP_Network_AVG_SNR_230614.csv")
    #Match tSNR networks
    mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
    oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    tSNR$NewNetwork <- mapping[ match(tSNR$Network, oldvalues) ]
    
    #Average tSNR within network for LH and RH
    LH_MEAN <- aggregate(LH_AVG_SNR ~ NewNetwork, data = tSNR, FUN = mean)
    RH_MEAN <- aggregate(RH_AVG_SNR ~ NewNetwork, data = tSNR, FUN = mean)
    mean_df <- merge(LH_MEAN, RH_MEAN, by=c("NewNetwork"), all=TRUE)
    
    #Merge mean SNR with network ICC datasets
        network_icc <- merge(mean_df, icc_net_lh_df, by="NewNetwork", all=TRUE)
        network_icc <- merge(network_icc, icc_net_rh_df, by="NewNetwork", all=TRUE)
        
    #Spearman correlation
        #LH
        cor.test(network_icc$LH_AVG_SNR, network_icc$AI_LH_ICC, method = 'spearman')
        #RH
        cor.test(network_icc$RH_AVG_SNR, network_icc$AI_RH_ICC, method = 'spearman')
        
        
      #FIGURES: 
        #RH: RH AI ICC x Mean tSNR
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        network_icc$NewNetwork <- as.factor(network_icc$NewNetwork)
        filename <- "Study3_HCP_ICC_SNR_RH_Plot_230630.png"
        ggplot(network_icc, aes(x = RH_AVG_SNR, y = AI_RH_ICC, color = NewNetwork)) +
          labs(x = "Mean RH SNR", y = "RH AI ICC") +
          labs(fill = " ", color = " ") +
          geom_point(aes(fill = NewNetwork), pch = 21, colour="black", size=2) +
          #geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange", size = 1) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          stat_cor(method="spearman")+
          scale_color_manual(values = CBIG_Palette) +
          scale_fill_manual(values = CBIG_Palette) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
        
        
        #LH: LH AI ICC x Mean tSNR
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        network_icc$NewNetwork <- as.factor(network_icc$NewNetwork)
        filename <- "Study3_HCP_ICC_SNR_LH_Plot_230630.png"
        ggplot(network_icc, aes(x = as.numeric(LH_AVG_SNR), y = AI_LH_ICC, color = NewNetwork)) +
          labs(x = "Mean LH SNR", y = "LH AI ICC") +
          labs(fill = " ", color = " ") +
          stat_cor(method="spearman")+
          geom_point(aes(fill = NewNetwork), pch = 21, colour="black", size=2) +
          #geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange", size = 1) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette) +
          scale_fill_manual(values = CBIG_Palette) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
        
#-----------------------------------HCP LANG LAT & AI---------------------
#SETUP  
  #Load LH & RH AI, zstat data
  ZSTAT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/lang_lat/zstats/MSHBM_LONG_AVG_ZSTAT_HCP_230318.csv")
  AIALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
  
  #Drop medial wall
  ZSTAT <- subset(ZSTAT, Network!=0)
  AIALL <- subset(AIALL, Network!=0)
  
  #Reorder CBIG Networks
  mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
  AIALL$NewNetwork <- recode(AIALL$Network, !!!mapping)
  ZSTAT$NewNetwork <- recode(ZSTAT$Network, !!!mapping)
  
  #Subset to network 5
  ZSTAT <- subset(ZSTAT, NewNetwork==5)
  AIALL <- subset(AIALL, NewNetwork==5)
  
  #Create ZSTAT asymmetry index variable
  ZSTAT$ZAI <- (ZSTAT$LH_AVG_Z - ZSTAT$RH_AVG_Z) / (ZSTAT$LH_AVG_Z + ZSTAT$RH_AVG_Z)
  
  #Merge ZSTAT and SAI datasets
  LANG <- merge(AIALL, ZSTAT, by =c("SUBJID"), all=FALSE)
  
#FIGURES  
  #Plot: ZSTAT avg within LH for network 5 x AI for language network 
  #With Spearman coefficient
  ggplot(LANG, aes(x=ZAI, y=LH_AVG_AI))+
    labs(x = "Language Task Laterality", y = 'Language Network LH AI')+
    labs(fill = " ")+
    labs(color = " ")+
    geom_point(fill="#E69F00", pch=21)+
    geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
    stat_cor(method="spearman")+
    #scale_y_continuous(limits = c(-.50, .95))+
    #scale_x_continuous(limits = c(-5, 5))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP_LangLat_Zstat_LH_AI_230613.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
  
  #Plot: RH AI
  #With Spearman coefficient
  ggplot(LANG, aes(x=ZAI, y=RH_AVG_AI))+
    labs(x = "Language Task Laterality", y = 'Language Network RH AI')+
    labs(fill = " ")+
    labs(color = " ")+
    geom_point(fill="#E69F00", pch=21)+
    geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
    stat_cor(method="spearman")+
    #scale_y_continuous(limits = c(-.50, .95))+
    #scale_x_continuous(limits = c(-5, 5))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
          axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
          legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
          axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
  ggsave(filename = paste("Study3_HCP_LangLat_Zstat_RH_AI_230613.png"), width = 3.35, height = 3.35,
         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
  
  
  
  
  
  
#----------------------------------HCP STABLE EST.: NETWORK AI-----------------
#SETUP
    #Load files: AI
    AI5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/5MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_5MIN_230221.csv")
    AI10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/10MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_10MIN_230221.csv")
    AI15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/15MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_15MIN_230221.csv")
    AI20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/20MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_20MIN_230221.csv")
    AI25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/25MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_25MIN_230221.csv")
    AI30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/30MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_30MIN_230221.csv")
    AI2SESS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/2SESS/avg_ai/MSHBM_LONG_AVG_AI_HCP_2SESS_230221.csv")
    
    #Drop Network 0 (medial wall)
    #AI
    AI5MIN <- subset(AI5MIN, Network!=0)
    AI10MIN <- subset(AI10MIN, Network!=0)
    AI15MIN <- subset(AI15MIN, Network!=0)
    AI20MIN <- subset(AI20MIN, Network!=0)
    AI25MIN <- subset(AI25MIN, Network!=0)
    AI30MIN <- subset(AI30MIN, Network!=0)
    AI2SESS <- subset(AI2SESS, Network!=0)
   
    #Reorder networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    
    #AI
    AI5MIN$NewNetwork <- recode(AI5MIN$Network, !!!mapping)
    AI10MIN$NewNetwork <- recode(AI10MIN$Network, !!!mapping)
    AI15MIN$NewNetwork <- recode(AI15MIN$Network, !!!mapping)
    AI20MIN$NewNetwork <- recode(AI20MIN$Network, !!!mapping)
    AI25MIN$NewNetwork <- recode(AI25MIN$Network, !!!mapping)
    AI30MIN$NewNetwork <- recode(AI30MIN$Network, !!!mapping)
    AI2SESS$NewNetwork <- recode(AI2SESS$Network, !!!mapping)
    
    #SA lateralization variable
    # create a character vector of the data frame names
    df_names <- c("AI5MIN", "AI10MIN", "AI15MIN", "AI20MIN", "AI25MIN", "AI30MIN", "AI2SESS")
    
   
#Calculate within-network ICC (RH and LH separately)   
    #Question: How much data is needed for a stable estimate of the AI?
    #Calculate ICC for AI(NETWORK basis) 
    #Create wide-format of AI data
    rh_wide_data <- data.frame(
      NewNetwork = AI5MIN$NewNetwork,
      AI_5min = AI5MIN$RH_AVG_AI,
      AI_10min = AI10MIN$RH_AVG_AI,
      AI_15min = AI15MIN$RH_AVG_AI,
      AI_20min = AI20MIN$RH_AVG_AI,
      AI_25min = AI25MIN$RH_AVG_AI,
      AI_30min = AI30MIN$RH_AVG_AI,
      AI_2SESS = AI2SESS$RH_AVG_AI
    )
    
    lh_wide_data <- data.frame(
      NewNetwork = AI5MIN$NewNetwork,
      AI_5min = AI5MIN$LH_AVG_AI,
      AI_10min = AI10MIN$LH_AVG_AI,
      AI_15min = AI15MIN$LH_AVG_AI,
      AI_20min = AI20MIN$LH_AVG_AI,
      AI_25min = AI25MIN$LH_AVG_AI,
      AI_30min = AI30MIN$LH_AVG_AI,
      AI_2SESS = AI2SESS$LH_AVG_AI
    )
    
 #RH first   
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(RH_AI_ICC_5MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_5min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(RH_AI_ICC_5MIN = icc3_value))
    }
    #ICC 10MIN
    icc_df2 <- data.frame(RH_AI_ICC_10MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_10min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(RH_AI_ICC_15MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_15min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(RH_AI_ICC_20MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_20min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(RH_AI_ICC_25MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_25min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(RH_AI_ICC_30MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, NewNetwork == i)[, c("AI_30min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add NewNetwork variable: 
    icc_df$NewNetwork <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    
    #Format icc_df LONG
    rh_icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('RH_AI_ICC_')),
        names_to = "TIME", 
        values_to = "RH_AI_ICC"
      )
    
    rh_icc_long$TIME <- as.factor(rh_icc_long$TIME)
    rh_icc_long$RH_AI_ICC <- as.numeric(rh_icc_long$RH_AI_ICC)
    rh_icc_long$NewNetwork <- as.factor(rh_icc_long$NewNetwork)
    
    rh_icc_long$TIME <- gsub("^.{0,10}", "", rh_icc_long$TIME) #remove extraneous characters from $TIME
    rh_icc_long$TIME <- gsub("...$", "", rh_icc_long$TIME)
    
    
#AVGAI NETWORK ICC FIGURES   
    #Fig. 1: RH ICC x TIME per NETWORK
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    ggplot(rh_icc_long, aes(x=factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y=RH_AI_ICC, color=NewNetwork))+
      geom_line(aes(group=NewNetwork))+
      labs(x = "Quantity of Data (Minutes)", y = 'RH AI ICC')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=CBIG_Palette)+
      scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=factor(NewNetwork)), colour="black", pch=21)+
      scale_y_continuous(limits=c(0,0.75), expand=c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study3_HCP_StableEst_AI_RH_Network_ICC_230613.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
    
    
    #FIG: RH Distributions only (not individual networks)
      Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
      ggplot(rh_icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = RH_AI_ICC, fill=TIME)) + 
        ggdist::stat_halfeye(
          adjust = .5, 
          width = .6, 
          .width = 0, 
          justification = -.3, 
          point_colour = "NA") + 
        geom_boxplot(
          width = .25, 
          outlier.shape = 21, 
          outlier.fill = NULL
        ) +
        #geom_point(
        #  size = 1.3,
        #  alpha = .3,
        #  position = position_jitter(
        #    seed = 1, width = .1
        #  )
        #) + 
        coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
        labs(y="Network RH AI ICC", x="Quantity of Data (Minutes)")+
        scale_colour_manual(values=Palette)+
        scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
        #scale_x_discrete(labels=c("Autism", "Control")) +
        theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
        theme(panel.background = element_blank())+
        theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
      ggsave(filename = paste("Study1_HCP_StableEst_AI_RH_NETWORK_ICC_Rain_230801.png"), width = 3.35, height = 3.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
    
    
    
#LH NETWORK AI ICC   
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(LH_AI_ICC_5MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_5min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(LH_AI_ICC_5MIN = icc3_value))
    }
    #ICC 10MIN
    icc_df2 <- data.frame(LH_AI_ICC_10MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_10min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(LH_AI_ICC_15MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_15min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(LH_AI_ICC_20MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_20min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(LH_AI_ICC_25MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_25min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(LH_AI_ICC_30MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, NewNetwork == i)[, c("AI_30min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add NewNetwork variable: 
    icc_df$NewNetwork <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    
    #Format icc_df LONG
    lh_icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('LH_AI_ICC_')),
        names_to = "TIME", 
        values_to = "LH_AI_ICC"
      )
    
    lh_icc_long$TIME <- as.factor(lh_icc_long$TIME)
    lh_icc_long$LH_AI_ICC <- as.numeric(lh_icc_long$LH_AI_ICC)
    lh_icc_long$NewNetwork <- as.factor(lh_icc_long$NewNetwork)
    
    lh_icc_long$TIME <- gsub("^.{0,10}", "", lh_icc_long$TIME) #remove extraneous characters from $TIME
    lh_icc_long$TIME <- gsub("...$", "", lh_icc_long$TIME)
    
    
    #AVGAI NETWORK ICC FIGURES   
    #Fig. 1: LH ICC x TIME per NETWORK
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    ggplot(lh_icc_long, aes(x=factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y=LH_AI_ICC, color=NewNetwork))+
      geom_line(aes(group=NewNetwork))+
      labs(x = "Quantity of Data (Minutes)", y = 'LH AI ICC')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=CBIG_Palette)+
      scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=factor(NewNetwork)), colour="black", pch=21)+
      scale_y_continuous(limits=c(0,0.75), expand=c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study3_HCP_StableEst_AI_LH_Network_ICC_230613.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
    
    
    
    #FIG: LH Distributions only (not individual networks)
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(lh_icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = LH_AI_ICC, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
      ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
      labs(y="Network LH AI ICC", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_AI_LH_NETWORK_ICC_Rain_230801.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
    

#------------------------------------HCP STABLE EST: SUBJECT AI--------------------
#SETUP
    #Load files: AI
    AI5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/5MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_5MIN_230221.csv")
    AI10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/10MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_10MIN_230221.csv")
    AI15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/15MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_15MIN_230221.csv")
    AI20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/20MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_20MIN_230221.csv")
    AI25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/25MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_25MIN_230221.csv")
    AI30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/30MIN/avg_ai/MSHBM_LONG_AVG_AI_HCP_30MIN_230221.csv")
    AI2SESS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/2SESS/avg_ai/MSHBM_LONG_AVG_AI_HCP_2SESS_230221.csv")
    
    #Drop Network 0 (medial wall)
    #AI
    AI5MIN <- subset(AI5MIN, Network!=0)
    AI10MIN <- subset(AI10MIN, Network!=0)
    AI15MIN <- subset(AI15MIN, Network!=0)
    AI20MIN <- subset(AI20MIN, Network!=0)
    AI25MIN <- subset(AI25MIN, Network!=0)
    AI30MIN <- subset(AI30MIN, Network!=0)
    AI2SESS <- subset(AI2SESS, Network!=0)
    
    #Reorder networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    
    #AI
    AI5MIN$NewNetwork <- recode(AI5MIN$Network, !!!mapping)
    AI10MIN$NewNetwork <- recode(AI10MIN$Network, !!!mapping)
    AI15MIN$NewNetwork <- recode(AI15MIN$Network, !!!mapping)
    AI20MIN$NewNetwork <- recode(AI20MIN$Network, !!!mapping)
    AI25MIN$NewNetwork <- recode(AI25MIN$Network, !!!mapping)
    AI30MIN$NewNetwork <- recode(AI30MIN$Network, !!!mapping)
    AI2SESS$NewNetwork <- recode(AI2SESS$Network, !!!mapping)
    
  
#Calculate RH AI ICC on a subject basis
    #Create wide-format of SAI data
    rh_wide_data <- data.frame(
      SUBJID = AI5MIN$SUBJID,
      AI_5min = AI5MIN$RH_AVG_AI,
      AI_10min = AI10MIN$RH_AVG_AI,
      AI_15min = AI15MIN$RH_AVG_AI,
      AI_20min = AI20MIN$RH_AVG_AI,
      AI_25min = AI25MIN$RH_AVG_AI,
      AI_30min = AI30MIN$RH_AVG_AI,
      AI_2SESS = AI2SESS$RH_AVG_AI
    )
    
    lh_wide_data <- data.frame(
      SUBJID = AI5MIN$SUBJID,
      AI_5min = AI5MIN$LH_AVG_AI,
      AI_10min = AI10MIN$LH_AVG_AI,
      AI_15min = AI15MIN$LH_AVG_AI,
      AI_20min = AI20MIN$LH_AVG_AI,
      AI_25min = AI25MIN$LH_AVG_AI,
      AI_30min = AI30MIN$LH_AVG_AI,
      AI_2SESS = AI2SESS$LH_AVG_AI
    )
    
    
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(RH_AI_ICC_5MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_5min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(RH_AI_ICC_5MIN = icc3_value))
    }
    
    #ICC 10MIN
    icc_df2 <- data.frame(RH_AI_ICC_10MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_10min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(RH_AI_ICC_15MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_15min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(RH_AI_ICC_20MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_20min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(RH_AI_ICC_25MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_25min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(RH_AI_ICC_30MIN = numeric(0))
    for (subj_id in unique(rh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(rh_wide_data, SUBJID == subj_id)[, c("AI_30min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("rh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(RH_AI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add SUBJID variable: 
    icc_df$SUBJID <- unique(rh_wide_data$SUBJID)
    
    #Format icc_df LONG
    rh_icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('RH_AI_ICC_')),
        names_to = "TIME", 
        values_to = "RH_AI_ICC"
      )
    
    rh_icc_long$TIME <- as.factor(rh_icc_long$TIME)
    rh_icc_long$RH_AI_ICC <- as.numeric(rh_icc_long$RH_AI_ICC)
    rh_icc_long$SUBJID <- as.factor(rh_icc_long$SUBJID)
    
    rh_icc_long$TIME <- gsub("^.{0,10}", "", rh_icc_long$TIME) #remove extraneous characters from $TIME
    rh_icc_long$TIME <- gsub("...$", "", rh_icc_long$TIME)
    
    
    
    
#RH FIGURE    
    #Fig. 2: Subject RAINCLOUD OF RH AI X TIME
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(rh_icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = RH_AI_ICC, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
      ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1.0)), clip = "off")+
      labs(y="Subject RH AI ICC", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_AI_RH_Subject_ICC_Rain_230613.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
    
    

    
    
#Calculate LH AI ICC on a subject basis
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(LH_AI_ICC_5MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_5min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(LH_AI_ICC_5MIN = icc3_value))
    }
    
    #ICC 10MIN
    icc_df2 <- data.frame(LH_AI_ICC_10MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_10min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(LH_AI_ICC_15MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_15min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(LH_AI_ICC_20MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_20min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(LH_AI_ICC_25MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_25min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(LH_AI_ICC_30MIN = numeric(0))
    for (subj_id in unique(lh_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(lh_wide_data, SUBJID == subj_id)[, c("AI_30min", "AI_2SESS")]
      
      # Rename dataframe
      df_name <- paste("lh_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(LH_AI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add SUBJID variable: 
    icc_df$SUBJID <- unique(lh_wide_data$SUBJID)
    
    #Format icc_df LONG
    lh_icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('LH_AI_ICC_')),
        names_to = "TIME", 
        values_to = "LH_AI_ICC"
      )
    
    lh_icc_long$TIME <- as.factor(lh_icc_long$TIME)
    lh_icc_long$LH_AI_ICC <- as.numeric(lh_icc_long$LH_AI_ICC)
    lh_icc_long$SUBJID <- as.factor(lh_icc_long$SUBJID)
    
    lh_icc_long$TIME <- gsub("^.{0,10}", "", lh_icc_long$TIME) #remove extraneous characters from $TIME
    lh_icc_long$TIME <- gsub("...$", "", lh_icc_long$TIME)
    
    
    
    
#LH FIGURE    
    #Fig. 1a: Subject RAINCLOUD OF RH AI X TIME
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(lh_icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = LH_AI_ICC, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
      ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1.0)), clip = "off")+
      labs(y="Subject LH AI ICC", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_AI_LH_Subject_ICC_Rain_230613.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
    
    
    


#---------------------------------NSD TASK EFFECTS: AI-------------------
#Load files: AI
    #REST
    AIREST_EVEN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/EVEN/avg_ai/MSHBM_LONG_AVG_AI_NSD_EVEN_REST_230221.csv")
    AIREST_ODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/ODD/avg_ai/MSHBM_LONG_AVG_AI_NSD_ODD_REST_230221.csv")
    AIREST_FIRST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/FIRST_HALF/avg_ai/MSHBM_LONG_AVG_AI_NSD_FIRST_HALF_REST_230221.csv")
    AIREST_SECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/SECOND_HALF/avg_ai/MSHBM_LONG_AVG_AI_NSD_SECOND_HALF_REST_230221.csv")
    AIREST_RAND1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/RAND1_1/avg_ai/MSHBM_LONG_AVG_AI_NSD_RAND1_1_REST_230221.csv")
    AIREST_RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/REST/RAND1_2/avg_ai/MSHBM_LONG_AVG_AI_NSD_RAND1_2_REST_230221.csv")
    #TASK
    AITASK_EVEN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/EVEN/avg_ai/MSHBM_LONG_AVG_AI_NSD_EVEN_TASK_230221.csv")
    AITASK_ODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/ODD/avg_ai/MSHBM_LONG_AVG_AI_NSD_ODD_TASK_230221.csv")
    AITASK_FIRST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/FIRST_HALF/avg_ai/MSHBM_LONG_AVG_AI_NSD_FIRST_HALF_TASK_230221.csv")
    AITASK_SECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/SECOND_HALF/avg_ai/MSHBM_LONG_AVG_AI_NSD_SECOND_HALF_TASK_230221.csv")
    AITASK_RAND1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/RAND1_1/avg_ai/MSHBM_LONG_AVG_AI_NSD_RAND1_1_TASK_230221.csv")
    AITASK_RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/ai_spec/TASK/RAND1_2/avg_ai/MSHBM_LONG_AVG_AI_NSD_RAND1_2_TASK_230221.csv")
    
    
    #AI - drop medial wall
      list_of_ai <- c("AIREST_EVEN", "AIREST_ODD", "AIREST_FIRST", "AIREST_SECOND", "AIREST_RAND1", "AIREST_RAND2", "AITASK_EVEN", "AITASK_ODD", "AITASK_FIRST", "AITASK_SECOND", "AITASK_RAND1", "AITASK_RAND2")
      # Loop over each dataframe in the list and apply the subset function
      for (df_name in list_of_ai) {
        assign(df_name, subset(get(df_name), Network != 0))
      }
      
   
    #Reorder networks to match Kong2019 network order 
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    list_of_dfs <- c("AIREST_EVEN", "AIREST_ODD", "AIREST_FIRST", "AIREST_SECOND", "AIREST_RAND1", "AIREST_RAND2", "AITASK_EVEN", "AITASK_ODD", "AITASK_FIRST", "AITASK_SECOND", "AITASK_RAND1", "AITASK_RAND2")
    for (df_name in list_of_dfs) {
      df <- get(df_name)
      df$NewNetwork <- recode(df$Network, !!!mapping)
      assign(df_name, df)
    }
    
    
    #Merge AI datasets
      #Setup dataframe
      ai_wide_data <- data.frame(
        NewNetwork = AIREST_EVEN$NewNetwork,
        SUBJID = AIREST_EVEN$SUBJID,
        REST_EVEN = AIREST_EVEN$Network_AVG_AI,
        REST_ODD = AIREST_ODD$Network_AVG_AI,
        REST_FIRST = AIREST_FIRST$Network_AVG_AI,
        REST_SECOND = AIREST_SECOND$Network_AVG_AI,
        REST_RAND1 = AIREST_RAND1$Network_AVG_AI,
        REST_RAND2 = AIREST_RAND2$Network_AVG_AI,
        TASK_EVEN = AITASK_EVEN$Network_AVG_AI, 
        TASK_ODD = AITASK_ODD$Network_AVG_AI,
        TASK_FIRST = AITASK_FIRST$Network_AVG_AI,
        TASK_SECOND = AITASK_SECOND$Network_AVG_AI,
        TASK_RAND1 = AITASK_RAND1$Network_AVG_AI,
        TASK_RAND2 = AITASK_RAND2$Network_AVG_AI
      )
      #Reshape to long
      ai_long_data <- ai_wide_data %>% 
        pivot_longer(
          cols = c("REST_EVEN", "REST_ODD", "REST_FIRST", "REST_SECOND", "REST_RAND1", "REST_RAND2", "TASK_EVEN", "TASK_ODD", "TASK_FIRST", "TASK_SECOND", "TASK_RAND1", "TASK_RAND2"), 
          names_to = "ITERATION",
          values_to = "Network_AVG_AI"
        )
      ai_long_data$ITERATION <- as.factor(ai_long_data$ITERATION)
      ai_long_data$NewNetwork <- as.factor(ai_long_data$NewNetwork)
      ai_long_data$Network_AVG_AI <- as.numeric(ai_long_data$Network_AVG_AI)
      ai_wide_data$NewNetwork <- as.factor(ai_wide_data$NewNetwork)
      
      #AI LH Dataset
      #Setup dataframe
      ai_lh_wide_data <- data.frame(
        NewNetwork = AIREST_EVEN$NewNetwork,
        SUBJID = AIREST_EVEN$SUBJID,
        REST_EVEN = AIREST_EVEN$LH_AVG_AI,
        REST_ODD = AIREST_ODD$LH_AVG_AI,
        REST_FIRST = AIREST_FIRST$LH_AVG_AI,
        REST_SECOND = AIREST_SECOND$LH_AVG_AI,
        REST_RAND1 = AIREST_RAND1$LH_AVG_AI,
        REST_RAND2 = AIREST_RAND2$LH_AVG_AI,
        TASK_EVEN = AITASK_EVEN$LH_AVG_AI, 
        TASK_ODD = AITASK_ODD$LH_AVG_AI,
        TASK_FIRST = AITASK_FIRST$LH_AVG_AI,
        TASK_SECOND = AITASK_SECOND$LH_AVG_AI,
        TASK_RAND1 = AITASK_RAND1$LH_AVG_AI,
        TASK_RAND2 = AITASK_RAND2$LH_AVG_AI
      )
      #Reshape to long
      ai_lh_long_data <- ai_lh_wide_data %>% 
        pivot_longer(
          cols = c("REST_EVEN", "REST_ODD", "REST_FIRST", "REST_SECOND", "REST_RAND1", "REST_RAND2", "TASK_EVEN", "TASK_ODD", "TASK_FIRST", "TASK_SECOND", "TASK_RAND1", "TASK_RAND2"), 
          names_to = "ITERATION",
          values_to = "LH_AVG_AI"
        )
      ai_lh_long_data$ITERATION <- as.factor(ai_lh_long_data$ITERATION)
      ai_lh_long_data$NewNetwork <- as.factor(ai_lh_long_data$NewNetwork)
      ai_lh_long_data$LH_AVG_AI <- as.numeric(ai_lh_long_data$LH_AVG_AI)
      ai_lh_long_data$NewNetwork <- as.factor(ai_lh_long_data$NewNetwork)
      
      
      #AI RH Dataset
      #Setup dataframe
      ai_rh_wide_data <- data.frame(
        NewNetwork = AIREST_EVEN$NewNetwork,
        SUBJID = AIREST_EVEN$SUBJID,
        REST_EVEN = AIREST_EVEN$RH_AVG_AI,
        REST_ODD = AIREST_ODD$RH_AVG_AI,
        REST_FIRST = AIREST_FIRST$RH_AVG_AI,
        REST_SECOND = AIREST_SECOND$RH_AVG_AI,
        REST_RAND1 = AIREST_RAND1$RH_AVG_AI,
        REST_RAND2 = AIREST_RAND2$RH_AVG_AI,
        TASK_EVEN = AITASK_EVEN$RH_AVG_AI, 
        TASK_ODD = AITASK_ODD$RH_AVG_AI,
        TASK_FIRST = AITASK_FIRST$RH_AVG_AI,
        TASK_SECOND = AITASK_SECOND$RH_AVG_AI,
        TASK_RAND1 = AITASK_RAND1$RH_AVG_AI,
        TASK_RAND2 = AITASK_RAND2$RH_AVG_AI
      )
      #Reshape to long
      ai_rh_long_data <- ai_rh_wide_data %>% 
        pivot_longer(
          cols = c("REST_EVEN", "REST_ODD", "REST_FIRST", "REST_SECOND", "REST_RAND1", "REST_RAND2", "TASK_EVEN", "TASK_ODD", "TASK_FIRST", "TASK_SECOND", "TASK_RAND1", "TASK_RAND2"), 
          names_to = "ITERATION",
          values_to = "RH_AVG_AI"
        )
      ai_rh_long_data$ITERATION <- as.factor(ai_rh_long_data$ITERATION)
      ai_rh_long_data$NewNetwork <- as.factor(ai_rh_long_data$NewNetwork)
      ai_rh_long_data$RH_AVG_AI <- as.numeric(ai_rh_long_data$RH_AVG_AI)
      ai_rh_long_data$NewNetwork <- as.factor(ai_rh_long_data$NewNetwork)
      
 
#CALCULATE ICC
   #SET6: LH AI ICC- PER SUBJECT
      
      #EVEN-ODD (LH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      eo_lh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_EVEN", "REST_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_lh_icc_df <- rbind(eo_lh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      eo_lh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      eo_lh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_EVEN", "TASK_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_lh_icc_df2 <- rbind(eo_lh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      eo_lh_icc_df <- cbind(eo_lh_icc_df, eo_lh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      eo_lh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_ODD", "REST_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_lh_icc_df2 <- rbind(eo_lh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      eo_lh_icc_df <- cbind(eo_lh_icc_df, eo_lh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      eo_lh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_EVEN", "TASK_EVEN")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_lh_icc_df2 <- rbind(eo_lh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      eo_lh_icc_df <- cbind(eo_lh_icc_df, eo_lh_icc_df2)
      
      #RESHAPE EO LH DATASET
      eo_lh_ai_icc_long <- eo_lh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "LH_AI_ICC"
        )
      eo_lh_ai_icc_long$ITERATION <- as.factor(eo_lh_ai_icc_long$ITERATION)
      eo_lh_ai_icc_long$LH_AI_ICC <- as.numeric(eo_lh_ai_icc_long$LH_AI_ICC)
      eo_lh_ai_icc_long$SUBJID <- as.factor(eo_lh_ai_icc_long$SUBJID)
      
      
      #FIRST-SECOND (LH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      fs_lh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_FIRST", "REST_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_lh_icc_df <- rbind(fs_lh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      fs_lh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      fs_lh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_FIRST", "TASK_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_lh_icc_df2 <- rbind(fs_lh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      fs_lh_icc_df <- cbind(fs_lh_icc_df, fs_lh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      fs_lh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_SECOND", "REST_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_lh_icc_df2 <- rbind(fs_lh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      fs_lh_icc_df <- cbind(fs_lh_icc_df, fs_lh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      fs_lh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_FIRST", "TASK_FIRST")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_lh_icc_df2 <- rbind(fs_lh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      fs_lh_icc_df <- cbind(fs_lh_icc_df, fs_lh_icc_df2)
      
      #RESHAPE FS DATASET
      fs_lh_ai_icc_long <- fs_lh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "LH_AI_ICC"
        )
      fs_lh_ai_icc_long$ITERATION <- as.factor(fs_lh_ai_icc_long$ITERATION)
      fs_lh_ai_icc_long$LH_AI_ICC <- as.numeric(fs_lh_ai_icc_long$LH_AI_ICC)
      fs_lh_ai_icc_long$SUBJID <- as.factor(fs_lh_ai_icc_long$SUBJID)
      
      
      #RAND1-RAND2 (LH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      rr_lh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_RAND1", "REST_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_lh_icc_df <- rbind(rr_lh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      rr_lh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      rr_lh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_RAND1", "TASK_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_lh_icc_df2 <- rbind(rr_lh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      rr_lh_icc_df <- cbind(rr_lh_icc_df, rr_lh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      rr_lh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("TASK_RAND2", "REST_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_lh_icc_df2 <- rbind(rr_lh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      rr_lh_icc_df <- cbind(rr_lh_icc_df, rr_lh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      rr_lh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_lh_wide_data, SUBJID == i)[, c("REST_RAND1", "TASK_RAND1")]
        
        # Rename dataframe
        df_name <- paste("ai_lh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_lh_icc_df2 <- rbind(rr_lh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      rr_lh_icc_df <- cbind(rr_lh_icc_df, rr_lh_icc_df2)
      
      #RESHAPE FS DATASET
      rr_lh_ai_icc_long <- rr_lh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "LH_AI_ICC"
        )
      rr_lh_ai_icc_long$ITERATION <- as.factor(rr_lh_ai_icc_long$ITERATION)
      rr_lh_ai_icc_long$LH_AI_ICC <- as.numeric(rr_lh_ai_icc_long$LH_AI_ICC)
      rr_lh_ai_icc_long$SUBJID <- as.factor(rr_lh_ai_icc_long$SUBJID)
      
      
  #SET7: RH AI ICC- PER SUBJECT
      
      #EVEN-ODD (RH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      eo_rh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_EVEN", "REST_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_rh_icc_df <- rbind(eo_rh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      eo_rh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      eo_rh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_EVEN", "TASK_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_rh_icc_df2 <- rbind(eo_rh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      eo_rh_icc_df <- cbind(eo_rh_icc_df, eo_rh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      eo_rh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_ODD", "REST_ODD")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_rh_icc_df2 <- rbind(eo_rh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      eo_rh_icc_df <- cbind(eo_rh_icc_df, eo_rh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      eo_rh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_EVEN", "TASK_EVEN")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        eo_rh_icc_df2 <- rbind(eo_rh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      eo_rh_icc_df <- cbind(eo_rh_icc_df, eo_rh_icc_df2)
      
      #RESHAPE EO LH DATASET
      eo_rh_ai_icc_long <- eo_rh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "RH_AI_ICC"
        )
      eo_rh_ai_icc_long$ITERATION <- as.factor(eo_rh_ai_icc_long$ITERATION)
      eo_rh_ai_icc_long$RH_AI_ICC <- as.numeric(eo_rh_ai_icc_long$RH_AI_ICC)
      eo_rh_ai_icc_long$SUBJID <- as.factor(eo_rh_ai_icc_long$SUBJID)
      
      #FIRST-SECOND (RH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      fs_rh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_FIRST", "REST_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_rh_icc_df <- rbind(fs_rh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      fs_rh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      fs_rh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_FIRST", "TASK_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_rh_icc_df2 <- rbind(fs_rh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      fs_rh_icc_df <- cbind(fs_rh_icc_df, fs_rh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      fs_rh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_SECOND", "REST_SECOND")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_rh_icc_df2 <- rbind(fs_rh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      fs_rh_icc_df <- cbind(fs_rh_icc_df, fs_rh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      fs_rh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_FIRST", "TASK_FIRST")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        fs_rh_icc_df2 <- rbind(fs_rh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      fs_rh_icc_df <- cbind(fs_rh_icc_df, fs_rh_icc_df2)
      
      #RESHAPE FS DATASET
      fs_rh_ai_icc_long <- fs_rh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "RH_AI_ICC"
        )
      fs_rh_ai_icc_long$ITERATION <- as.factor(fs_rh_ai_icc_long$ITERATION)
      fs_rh_ai_icc_long$RH_AI_ICC <- as.numeric(fs_rh_ai_icc_long$RH_AI_ICC)
      fs_rh_ai_icc_long$SUBJID <- as.factor(fs_rh_ai_icc_long$SUBJID)
      
      
      #RAND1-RAND2 (LH)
      #REST-REST
      # Create an empty dataframe to store ICC3 values
      rr_rh_icc_df <- data.frame(RESTREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_RAND1", "REST_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_rh_icc_df <- rbind(rr_rh_icc_df, data.frame(RESTREST = icc3_value))
      }
      
      rr_rh_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
      
      #TASK-TASK
      # Create an empty dataframe to store ICC3 values
      rr_rh_icc_df2 <- data.frame(TASKTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_RAND1", "TASK_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_rh_icc_df2 <- rbind(rr_rh_icc_df2, data.frame(TASKTASK = icc3_value))
      } 
      rr_rh_icc_df <- cbind(rr_rh_icc_df, rr_rh_icc_df2)
      
      #TASK-REST
      # Create an empty dataframe to store ICC3 values
      rr_rh_icc_df2 <- data.frame(TASKREST = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("TASK_RAND2", "REST_RAND2")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_rh_icc_df2 <- rbind(rr_rh_icc_df2, data.frame(TASKREST = icc3_value))
      } 
      rr_rh_icc_df <- cbind(rr_rh_icc_df, rr_rh_icc_df2)
      
      #REST-TASK
      # Create an empty dataframe to store ICC3 values
      rr_rh_icc_df2 <- data.frame(RESTTASK = numeric(0))
      for (i in 1:8) {
        # Create dataframe subset
        df_subset <- subset(ai_rh_wide_data, SUBJID == i)[, c("REST_RAND1", "TASK_RAND1")]
        
        # Rename dataframe
        df_name <- paste("ai_rh_wide_dataR.", i, sep = "")
        assign(df_name, df_subset)
        
        # Compute ICC and save ICC3 value to dataframe
        icc_result <- suppressWarnings(ICC(get(df_name)))
        icc3_value <- icc_result$results$ICC[3]
        rr_rh_icc_df2 <- rbind(rr_rh_icc_df2, data.frame(RESTTASK = icc3_value))
      } 
      rr_rh_icc_df <- cbind(rr_rh_icc_df, rr_rh_icc_df2)
      
      #RESHAPE FS DATASET
      rr_rh_ai_icc_long <- rr_rh_icc_df %>% 
        pivot_longer(
          cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
          names_to = "ITERATION",
          values_to = "RH_AI_ICC"
        )
      rr_rh_ai_icc_long$ITERATION <- as.factor(rr_rh_ai_icc_long$ITERATION)
      rr_rh_ai_icc_long$RH_AI_ICC <- as.numeric(rr_rh_ai_icc_long$RH_AI_ICC)
      rr_rh_ai_icc_long$SUBJID <- as.factor(rr_rh_ai_icc_long$SUBJID)
      
      
#WILCOXON SIGNED RANK TESTS
      #LEFT H.
          #EO
          eo_wide <- pivot_wider(eo_lh_ai_icc_long, names_from = ITERATION, values_from = LH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(eo_wide$ITERATION.RESTREST, eo_wide$ITERATION.RESTTASK, paired = TRUE)
          
          #FS
          fs_wide <- pivot_wider(fs_lh_ai_icc_long, names_from = ITERATION, values_from = LH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(fs_wide$ITERATION.RESTREST, fs_wide$ITERATION.RESTTASK, paired = TRUE)
          
          #R1R2
          rr_wide <- pivot_wider(rr_lh_ai_icc_long, names_from = ITERATION, values_from = LH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(rr_wide$ITERATION.RESTREST, rr_wide$ITERATION.RESTTASK, paired = TRUE)
          
          
      #RIGHT H.
          #EO
          eo_wide <- pivot_wider(eo_rh_ai_icc_long, names_from = ITERATION, values_from = RH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(eo_wide$ITERATION.RESTREST, eo_wide$ITERATION.RESTTASK, paired = TRUE)
          
          #FS
          fs_wide <- pivot_wider(fs_rh_ai_icc_long, names_from = ITERATION, values_from = RH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(fs_wide$ITERATION.RESTREST, fs_wide$ITERATION.RESTTASK, paired = TRUE)
          
          #R1R2
          rr_wide <- pivot_wider(rr_rh_ai_icc_long, names_from = ITERATION, values_from = RH_AI_ICC, names_prefix = "ITERATION.")
          #TASKTASK vs RESTREST
          wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.RESTREST, paired = TRUE)
          #TASKTASK vs TASKREST
          wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.TASKREST, paired = TRUE)
          #RESTREST vs RESTTASK
          wilcox.test(rr_wide$ITERATION.RESTREST, rr_wide$ITERATION.RESTTASK, paired = TRUE)
          
      
#FIGURES: RH and LH
  #SET 6: LH AI ICC SUBJECT-LEVEL
      #FIGURE 6.1: EVEN/ODD
      ggplot(eo_lh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=LH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'Even/Odd Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.13, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET6_1_LH_AI_ICC_ALL_EVENODD_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      #FIGURE 6.2: FIRST/SECOND
      ggplot(fs_lh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=LH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'First/Second Half of Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET6_2_LH_AI_ICC_ALL_FIRSTSECOND_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      #FIGURE 6.3: RAND1/RAND2
      ggplot(rr_lh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=LH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'Random Selection of Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET6_3_LH_AI_ICC_ALL_RAND1RAND2_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
   #SET 7: RH AI ICC SUBJECT-LEVEL
      #FIGURE 7.1: EVEN/ODD
      ggplot(eo_rh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=RH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'Even/Odd Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.26, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET7_1_RH_AI_ICC_ALL_EVENODD_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      #FIGURE 7.2: FIRST/SECOND
      ggplot(fs_rh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=RH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'First/Second Half of Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.38, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET7_2_RH_AI_ICC_ALL_FIRSTSECOND_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      #FIGURE 7.3: RAND1/RAND2
      ggplot(rr_rh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=RH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'Random Selection of Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.26, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET7_3_RH_AI_ICC_ALL_RAND1RAND2_230613.png"), width = 2.21, height = 2.21,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      

#SET 8: EVEN/ODD only, 3 columns
      #FIGURE 8.1: LH EVEN/ODD
      eo_lh_ai_icc_long <- subset(eo_lh_ai_icc_long, ITERATION!="RESTTASK")
      ggplot(eo_lh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST')), y=LH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'LH AI ICC Even/Odd Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.13, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.5, vjust=.8, size=10, angle = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET8_1_LH_AI_ICC_ALL_EVENODD_230915.png"), width = 3.35, height = 3.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
      #FIGURE 8.2: RH EVEN/ODD
      eo_rh_ai_icc_long <- subset(eo_rh_ai_icc_long, ITERATION!="RESTTASK")
      ggplot(eo_rh_ai_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST')), y=RH_AI_ICC))+
        geom_line(aes(group=SUBJID))+
        labs(x = '', y = 'RH AI ICC Even/Odd Runs')+
        #scale_colour_manual(values=CBIG_Palette)+
        #scale_fill_manual(values=CBIG_Palette)+
        geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
        scale_y_continuous(limits = c(0.26, 1.0))+
        scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
              axis.text.x = element_text(colour = "black", hjust = 0.5, vjust=.8, size=10, angle = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
              legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
      ggsave(filename = paste("NSD_SET8_2_RH_AI_ICC_ALL_EVENODD_230915.png"), width = 3.35, height = 3.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
#--------------------------------MOST SPECIALIZED NETWORKS-----------------------------
#Load ALL .csv
study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
study3$Sex_Bin <- as.factor(study3$Sex_Bin)  
study3$LH_AVG_AI <- study3$LH_AVG_AI*100
study3$RH_AVG_AI <- study3$RH_AVG_AI*100
#Multiple regressions to test for significant intercept given covariates
      #Covariates=mean-centered age, sex, handedness, mean-centered mean FD
      #Bonferroni-corrected alpha = .001 (RH=17, LH=17)


    #HCP-DISC (276) Analysis
      HCP_AI <- subset(study3, dataset=="HCP-DISC")
      #LH AVG AI MODELS
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(HCP_AI, NewNetwork == i)
          
          model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
          # Create a unique name for each model
          model_name <- paste("HCP_DISC_LH_model", i, sep = "")
          
          # Assign the model to the unique name
          assign(model_name, model)
        }
        #Access model results through: summary(HCP_DISC_LH_model1)
        
        
        #RH AVG AI MODELS
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(HCP_AI, NewNetwork == i)
          
          model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
          # Create a unique name for each model
          model_name <- paste("HCP_DISC_RH_model", i, sep = "")
          
          # Assign the model to the unique name
          assign(model_name, model)
        }
        #Access model results through: summary(HCP_DISC_RH_model1)

      
      
  #HCP-REP 277 ANALYSIS
      HCPR_AI <- subset(study3, dataset=="HCP-REP")
      #LH AVG AI MODELS
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPR_AI, NewNetwork == i)
        
        model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCP_REP_LH_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCP_REP_LH_model1)
      
      
      #RH AVG AI MODELS
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPR_AI, NewNetwork == i)
        
        model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCP_REP_RH_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCP_REP_RH_model1)
      
      

  #HCPD 343 ANALYSIS  
      HCPD_AI <- subset(study3, dataset=="HCPD")
      #LH AVG AI MODELS
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPD_AI, NewNetwork == i)
        
        model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCPD_LH_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCPD_LH_model1)
      
      
    #RH AVG AI MODELS (HCPD)
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPD_AI, NewNetwork == i)
        
        model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCPD_RH_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCPD_RH_model1)
      
 
      
#Model-adjusted AI for visual inspection (HCP, HCPD)
      #LH
      study3$LH_AVG_AI_ADJ <- NA
      lh_ci_df <- data.frame(dataset=factor(),
                          NewNetwork=factor(),
                          CI_MIN=integer(),
                          CI_MAX=integer(),
                          PERC97.5=integer(),
                          PERC2.5=integer(),
                          MEAN=integer())
      
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study3, dataset==d)
        
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(subset1, NewNetwork == i)
        
        # Fit the linear regression model
        model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
        
        #Grab lm coefficients
        BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
        BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
        BETA_FD <- model[["coefficients"]][["FD_Centered"]]
        BETA_HAND <- model[["coefficients"]][["Handedness"]]
        
        #Grab means
        MEAN_AGE <- mean(subset_data$Age_Centered)
        MEAN_FD <- mean(subset_data$FD_Centered)
        MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
        MEAN_HAND <- mean(subset_data$Handedness)
        
        #IDs of the subset
        subsetted_ids <- subset_data$SUBJID
        
        #Find matching rows
        matching_rows <- study3$SUBJID %in% subsetted_ids &
          study3$NewNetwork %in% i
        
        #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
        study3$LH_AVG_AI_ADJ[matching_rows] <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
        subset_data$LH_AVG_AI_ADJ <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
        
        #find mean
        MEAN <- mean(subset_data$LH_AVG_AI_ADJ)
        #Load DescTools
        #CI_OUT <- MeanCI(subset_data$LH_AVG_AI_ADJ, method="classic")
        #MEAN=CI_OUT[1]
        #CI_MIN=CI_OUT[2]
        #CI_MAX=CI_OUT[3]
        
        n <- length(subset_data$LH_AVG_AI_ADJ)
        std_dev <- sd(subset_data$LH_AVG_AI_ADJ)
        std_err <- std_dev / sqrt(n)
        alpha = 0.05
        degrees_of_freedom <- n - 1
        
        #t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
        z_score=1.96
        margin_error <- z_score * std_err
        
        
        #lower bound
        CI_MIN <- MEAN - margin_error
        #upper bound
        CI_MAX <- MEAN + margin_error
        
        #calculate 97.5 and 2.5 percentiles
        PERC97.5 <- quantile(subset_data$LH_AVG_AI_ADJ, probs = 0.975)
        PERC2.5 <- quantile(subset_data$LH_AVG_AI_ADJ, probs = 0.025)
        
        #Append CI data to dataframe
        row_df <- data.frame(d, i, CI_MIN, CI_MAX,PERC97.5, PERC2.5, MEAN)
        names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX","PERC97.5", "PERC2.5", "MEAN")
        lh_ci_df <- rbind(lh_ci_df, row_df)
        }
      }
      
      

    #RH
      study3$RH_AVG_AI_ADJ <- NA
      rh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             PERC97.5=integer(),
                             PERC2.5=integer(),
                             MEAN=integer())
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study3, dataset==d)
        
        for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(subset1, NewNetwork == i)
        
        # Fit the linear regression model
        model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
        
        #Grab lm coefficients
        BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
        BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
        BETA_FD <- model[["coefficients"]][["FD_Centered"]]
        BETA_HAND <- model[["coefficients"]][["Handedness"]]
        
        #Grab means
        MEAN_AGE <- mean(subset_data$Age_Centered)
        MEAN_FD <- mean(subset_data$FD_Centered)
        MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
        MEAN_HAND <- mean(subset_data$Handedness)
        
        #IDs of the subset
        subsetted_ids <- subset_data$SUBJID
        
        #Find matching rows
        matching_rows <- study3$SUBJID %in% subsetted_ids &
          study3$NewNetwork %in% i
        
        #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
        study3$RH_AVG_AI_ADJ[matching_rows] <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
        subset_data$RH_AVG_AI_ADJ <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
        
        #find mean
        MEAN <- mean(subset_data$RH_AVG_AI_ADJ)
        n <- length(subset_data$RH_AVG_AI_ADJ)
        std_dev <- sd(subset_data$RH_AVG_AI_ADJ)
        std_err <- std_dev / sqrt(n)
        alpha = 0.05
        degrees_of_freedom <- n - 1
        t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
        margin_error <- t_score * std_err
        
        #lower bound
        CI_MIN <- MEAN - margin_error
        #upper bound
        CI_MAX <- MEAN + margin_error
        
        #calculate 97.5 and 2.5 percentiles
        PERC97.5 <- quantile(subset_data$RH_AVG_AI_ADJ, probs = 0.975)
        PERC2.5 <- quantile(subset_data$RH_AVG_AI_ADJ, probs = 0.025)
        
        #Append CI data to dataframe
        row_df <- data.frame(d, i, CI_MIN, CI_MAX,PERC97.5, PERC2.5, MEAN)
        names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX","PERC97.5", "PERC2.5", "MEAN")
        rh_ci_df <- rbind(rh_ci_df, row_df)
         }
      }
      
      
#FIGURE: Model-adjusted RH and LH AVG AI for HCP
    #FIG 1: LH Vertical point and line
        #Multiply AI by 100 for %
        #lh_ci_df$MEAN <- lh_ci_df$MEAN*100
        #lh_ci_df$CI_MIN <- lh_ci_df$CI_MIN*100
        #lh_ci_df$CI_MAX <- lh_ci_df$CI_MAX*100
      # Create the point and line plot
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('14', '1', '2', '16', '12', '11', '10', '9', '8', '4', '3', '7', '15', '17', '5', '13', '6')
      lh_ci_df$NewNetwork <- factor(lh_ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCPD", "HCP-REP", "HCP-DISC")
      lh_ci_df$dataset <- factor(lh_ci_df$dataset, level=dataset_order)
      ggplot(lh_ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
        geom_hline(yintercept = seq(from=1.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .5), color="black", size=.5) +
        geom_point(position = position_dodge(width = .5), size=2, shape=21) +
        coord_cartesian(ylim= c(1.2, NA), clip = "off") +
        labs(y = "", x = "Adjusted LH Avg. AI (%)") +
        #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
        scale_y_discrete(labels = c("DEF-B", "ViS-A", "VIS-B", "LIM-A", "CTRL-C", "CTRL-B", "CTRL-A", "SAL-B", "SAL-A", "SOM-B", "SOM-A", "DAN-B", "DEF-C", "LIM-B", "LANG", "DEF-A", "DAN-A"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_SpecNetworks_LH_PointLineAdjusted_230620.png"), width = 3.5, height = 6,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
     
    #FIG 1: RH Vertical point and line
        #Multiply AI by 100 for %
        #rh_ci_df$MEAN <- rh_ci_df$MEAN*100
        #rh_ci_df$CI_MIN <- rh_ci_df$CI_MIN*100
        #rh_ci_df$CI_MAX <- rh_ci_df$CI_MAX*100
      # Create the point and line plot
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('16', '10', '6', '13', '9', '3', '5', '1', '14', '4', '7', '2', '12', '8', '15', '11', '17')
      rh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCPD", "HCP-REP", "HCP-DISC")
      rh_ci_df$dataset <- factor(rh_ci_df$dataset, level=dataset_order)
      ggplot(rh_ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
        geom_hline(yintercept = seq(from=1.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .5), color="black", size=.5) +
        geom_point(position = position_dodge(width = .5), size=2, shape=21) +
        coord_cartesian(ylim= c(1.2, NA), clip = "off") +
        labs(y = "", x = "Adjusted RH Avg. AI (%)") +
        #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
        scale_y_discrete(labels =c("LIM-A", "CTRL-A", "DAN-A", "DEF-A", "SAL-B", "SOM-A", "LANG", "VIS-A", "DEF-B", "SOM-B", "DAN-B", "VIS-B", "CTRL-C", "SAL-A", "DEF-C", "CTRL-B", "LIM-B"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_SpecNetworks_RH_PointLineAdjusted_230620.png"), width = 3.5, height = 6,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
       
      
      
      #FIG 1: LH HORZ point and line
      #Multiply AI by 100 for %
      #lh_ci_df$MEAN <- lh_ci_df$MEAN*100
      #lh_ci_df$CI_MIN <- lh_ci_df$CI_MIN*100
      #lh_ci_df$CI_MAX <- lh_ci_df$CI_MAX*100
      # Create the point and line plot
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('14', '1', '2', '16', '12', '11', '10', '9', '8', '4', '3', '7', '15', '17', '5', '13', '6')
      lh_ci_df$NewNetwork <- factor(lh_ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCPD", "HCP-REP", "HCP-DISC")
      lh_ci_df$dataset <- factor(lh_ci_df$dataset, level=dataset_order)
      ggplot(lh_ci_df, aes(x = NewNetwork, y = MEAN, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
        geom_vline(xintercept = seq(from=.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = 0,  position = position_dodge(width = .5), color="black", size=.5) +
        geom_point(position = position_dodge(width = .5), size=2, shape=21) +
        coord_cartesian(xlim= c(1.2, NA), clip = "off") +
        labs(x = "", y = "Adjusted LH Avg. AI (%)") +
        scale_x_discrete(labels = c("DEF-B", "VIS-A", "VIS-B", "LIM-A", "CTRL-C", "CTRL-B", "CTRL-A", "SAL-B", "SAL-A", "SOM-B", "SOM-A", "DAN-B", "DEF-C", "LIM-B", "LANG", "DEF-A", "DAN-A"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1, angle=25, hjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_SpecNetworks_LH_PointLineAdjusted_HORZ_230620.png"), width = 6, height = 2.25,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      #FIG 1: RH HORZ point and line
      #Multiply AI by 100 for %
      #rh_ci_df$MEAN <- rh_ci_df$MEAN*100
      #rh_ci_df$CI_MIN <- rh_ci_df$CI_MIN*100
      #rh_ci_df$CI_MAX <- rh_ci_df$CI_MAX*100
      # Create the point and line plot
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('16', '10', '6', '13', '9', '3', '5', '1', '14', '4', '7', '2', '12', '8', '15', '11', '17')
      rh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCPD", "HCP-REP", "HCP-DISC")
      rh_ci_df$dataset <- factor(rh_ci_df$dataset, level=dataset_order)
      ggplot(rh_ci_df, aes(x = NewNetwork, y = MEAN, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
        geom_vline(xintercept = seq(from=.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = 0,  position = position_dodge(width = .5), color="black", size=.5) +
        geom_point(position = position_dodge(width = .5), size=2, shape=21) +
        coord_cartesian(xlim= c(1.2, NA), clip = "off") +
        labs(x = "", y = "Adjusted RH Avg. AI (%)") +
        #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
        scale_x_discrete(labels =c("LIM-A", "CTRL-A", "DAN-A", "DEF-A", "SAL-B", "SOM-A", "LANG", "VIS-A", "DEF-B", "SOM-B", "DAN-B", "VIS-B", "CTRL-C", "SAL-A", "DEF-C", "CTRL-B", "LIM-B"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1, angle=25, hjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_SpecNetworks_RH_PointLineAdjusted_HORZ_230620.png"), width = 6, height = 2.25,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
      
    #FIG 1: LH Vertical boxplot
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('14', '1', '2', '16', '12', '11', '10', '9', '8', '4', '3', '7', '15', '17', '5', '13', '6')
      # use factor() to set the order of the factor levels
      HCP_AI$NewNetwork <- factor(HCP_AI$NewNetwork, level = network_order)
      GroupPalette <- c("#E69F00","#D55E00", "#009E73")
      # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
      ggplot(HCP_AI, aes(x = LH_AVG_AI_ADJ, y = NewNetwork, group=interaction(dataset, NewNetwork), fill = dataset)) + 
        geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = "black") +
        coord_cartesian(ylim= c(1.2, NA), clip = "off") +
        labs(y = "", x = "Adjusted LH AVG AI") +
        #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
        #scale_y_discrete(labels =c("Limbic-B", "Limbic-A", "Default-C", "Default-B", "Default-A", "Control-C", "Control-B", "Control-A", "Salience/VenAttn-B", "Salience/VenAttn-A", "Dorsal Attention-B", "Dorsal Attention-A", "Language", "Somatomotor-B", "Somatomotor-A", "Visual-B", "Visual-A"))+
        scale_y_discrete(labels = c("Default-B", "Visual-A", "Visual-B", "Limbic-A", "Control-C", "Control-B", "Control-A", "Sal/VenAttn-B", "Sal/VenAttn-A", "Somatomotor-B", "Somatomotor-A", "Dorsal Attn-B", "Default-C", "Limbic-B", "Language", "Default-A", "Dorsal Attn-A"))+
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP_AI_LH_Boxplots_230615.png"), width = 3.35, height = 6,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
    #FIG 1: RH Vertical boxplot
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('16', '10', '6', '13', '9', '3', '5', '1', '14', '4', '7', '2', '12', '8', '15', '11', '17')
      # use factor() to set the order of the factor levels
      HCP_AI$NewNetwork <- factor(HCP_AI$NewNetwork, level = network_order)
      GroupPalette <- c("#E69F00","#D55E00", "#009E73")
      # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
      ggplot(HCP_AI, aes(x = RH_AVG_AI_ADJ, y = NewNetwork, group=interaction(dataset, NewNetwork), fill = dataset)) + 
        geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = "black") +
        coord_cartesian(ylim= c(1.2, NA), clip = "off") +
        labs(y = "", x = "Adjusted RH AVG AI") +
        #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
        #scale_y_discrete(labels =c("Limbic-B", "Limbic-A", "Default-C", "Default-B", "Default-A", "Control-C", "Control-B", "Control-A", "Salience/VenAttn-B", "Salience/VenAttn-A", "Dorsal Attention-B", "Dorsal Attention-A", "Language", "Somatomotor-B", "Somatomotor-A", "Visual-B", "Visual-A"))+
        scale_y_discrete(labels =c("Limbic-A", "Control-A", "Dorsal Attn-A", "Default-A", "Sal/VenAttn-B", "Somatomotor-A", "Language", "Visual-A", "Default-B", "Somatomotor-B", "Dorsal Attn-B", "Visual-B", "Control-C", "Sal/VenAttn-A", "Default-C", "Control-B", "Limbic-B"))+
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP_AI_RH_Boxplots_230615.png"), width = 3.35, height = 6,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)

      
#HANDEDNESS X SAL-A (LH)
      subsetted_data <- subset(study3, NewNetwork==8)
      subsetted_data <- subset(subsetted_data, dataset!="REST")
      subsetted_data <- subset(subsetted_data, dataset!="TASK")
      Palette <- c("#E69F00", "#D55E00", "#0072B2")
      filename <- "Study3_AI_LH_Handedness_Plots_Network8_230626.png"
      subsetted_data$dataset <- as.factor(subsetted_data$dataset)
      subsetted_data$Handedness <- as.numeric(subsetted_data$Handedness)
      ggplot(subsetted_data, aes(x = Handedness, y = LH_AVG_AI, fill=dataset)) +
        labs(x = "Handedness (EHI)", y = "SAL-A LH Avg. AI (%)") +
        labs(fill = " ", color = " ") +
        geom_point(aes(fill = dataset),color="black", pch = 21) +
        geom_smooth(aes(color = dataset), method = "lm", size = 0.75, se = TRUE) +
        #scale_y_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
        #scale_x_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
        scale_color_manual(values = Palette) +
        scale_fill_manual(values = Palette) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0, size=10),
              axis.title = element_text(colour = "black", size = 10),
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=9),
              axis.text.x = element_text(colour = "black", size=9),
              legend.position = "none",
              legend.title = element_text(colour = "black", size = 12),
              legend.text = element_text(colour = "black", size = 12),
              legend.background = element_rect(fill = "white", size = 0.5),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
              axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
              panel.border = element_blank(),
              panel.background = element_blank())
      
      ggsave(filename = filename, width = 3.35, height = 3.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
#-----------------------------NETWORK-LEVEL AI: HCP-DISC-------------------------
#LOAD DEMOS
      #Load ALL .csv
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      
      #Formatting for Demos
      study3_demos <- subset(study3, NewNetwork=="1")
      
      #Filter to just covariates
      HCPDISC <- study3_demos[,c("SUBJID","dataset", "Sex_Bin", "Handedness", "Age_Centered", "FD_Centered")]
      
      #Constrain sex to factor
      HCPDISC$Sex_Bin <- as.factor(HCPDISC$Sex_Bin)
      
      #Filter to just HCP-DISC,
      HCPDISC <- subset(HCPDISC, dataset=="HCP-DISC")
      
      #Grab HCP-DISC IDs
      DISC_IDS <- HCPDISC$SUBJID
      
      #Load ALL HCP network-level AI and combine
        #232 set
        N1_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-1_HCP_230421.csv")
        N2_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-2_HCP_230421.csv")
        N3_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-3_HCP_230421.csv")
        N4_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-4_HCP_230421.csv")
        N5_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-5_HCP_230421.csv")
        N6_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-6_HCP_230421.csv")
        N7_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-7_HCP_230421.csv")
        N8_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-8_HCP_230421.csv")
        N9_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-9_HCP_230421.csv")
        N10_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-10_HCP_230421.csv")
        N11_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-11_HCP_230421.csv")
        N12_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-12_HCP_230421.csv")
        N13_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-13_HCP_230421.csv")
        N14_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-14_HCP_230421.csv")
        N15_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-15_HCP_230421.csv")
        N16_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-16_HCP_230421.csv")
        N17_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-17_HCP_230421.csv")
        
         #REPLICATION (487) set
         N1R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-1_HCP_REP_230421.csv")
         N2R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-2_HCP_REP_230421.csv")
         N3R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-3_HCP_REP_230421.csv")
         N4R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-4_HCP_REP_230421.csv")
         N5R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-5_HCP_REP_230421.csv")
         N6R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-6_HCP_REP_230421.csv")
         N7R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-7_HCP_REP_230421.csv")
         N8R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-8_HCP_REP_230421.csv")
         N9R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-9_HCP_REP_230421.csv")
         N10R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-10_HCP_REP_230421.csv")
         N11R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-11_HCP_REP_230421.csv")
         N12R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-12_HCP_REP_230421.csv")
         N13R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-13_HCP_REP_230421.csv")
         N14R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-14_HCP_REP_230421.csv")
         N15R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-15_HCP_REP_230421.csv")
         N16R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-16_HCP_REP_230421.csv")
         N17R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-17_HCP_REP_230421.csv")
        
         #Combine two sets
         N1_AI <- merge(N1_AI, N1R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N2_AI <- merge(N2_AI, N2R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N3_AI <- merge(N3_AI, N3R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N4_AI <- merge(N4_AI, N4R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N5_AI <- merge(N5_AI, N5R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N6_AI <- merge(N6_AI, N6R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N7_AI <- merge(N7_AI, N7R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N8_AI <- merge(N8_AI, N8R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N9_AI <- merge(N9_AI, N9R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N10_AI <- merge(N10_AI, N10R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N11_AI <- merge(N11_AI, N11R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N12_AI <- merge(N12_AI, N12R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N13_AI <- merge(N13_AI, N13R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N14_AI <- merge(N14_AI, N14R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N15_AI <- merge(N15_AI, N15R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N16_AI <- merge(N16_AI, N16R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         N17_AI <- merge(N17_AI, N17R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
         
      #Drop network 0
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame
        df <- df[df$Network != 0, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df)
      }
      
      #Convert network to numeric
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$Network <- as.numeric(as.character(df$Network))
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
      #Switch network ordering to reflect CBIG legend ordering - ordering is reflected in plots
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      list_of_dfs <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in list_of_dfs) {
        df <- get(df_name)
        df$NewNetwork <- recode(df$Network, !!!mapping)
        assign(df_name, df)
      }
      
      #Filter to IDs in demos
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame by keeping only rows where SUBJID is in TD_IDS
        df_sub <- df[df$SUBJID %in% DISC_IDS, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df_sub)
      }
      
      #Convert NewNetwork to factor
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      counter <- 0
      for (df_name in df_names) {
        counter <- counter + 1
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$NewNetwork <- as.factor(df$NewNetwork)
        #Create dataset variable
        df$dataset <- counter
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
      #Combine all HCPDISC dfs
      HCPDISC_NET <- data.frame()
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      
      for (df_name in df_names) {
        # Read the current dataframe
        current_df <- get(df_name)
        
        # Merge the current dataframe with HCPD_NET using bind_rows
        HCPDISC_NET <- bind_rows(HCPDISC_NET, current_df)
      }
      
      #Reorder datasets to conform with CBIG ordering
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCPDISC_NET$NewDataset <- as.factor(recode(HCPDISC_NET$dataset, !!!mapping))
      
      
#MODEL-ADJUSTED VALUES      
      #LH
      lh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPDISC_NET$LH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPDISC_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPDISC, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPDISC_NET$SUBJID %in% subsetted_ids &
            HCPDISC_NET$NewNetwork %in% i &
            HCPDISC_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPDISC_NET$LH_AVG_AI_ADJ[matching_rows] <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$LH_AVG_AI_ADJ <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$LH_AVG_AI_ADJ)
          n <- length(subset_data$LH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$LH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          lh_ci_df <- rbind(lh_ci_df, row_df)
        }
      }
      
      #RH
      rh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPDISC_NET$RH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPDISC_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPDISC, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPDISC_NET$SUBJID %in% subsetted_ids &
            HCPDISC_NET$NewNetwork %in% i &
            HCPDISC_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPDISC_NET$RH_AVG_AI_ADJ[matching_rows] <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$RH_AVG_AI_ADJ <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$RH_AVG_AI_ADJ)
          n <- length(subset_data$RH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$RH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          rh_ci_df <- rbind(rh_ci_df, row_df)
        }
      }

      
#WITHIN/BETWEEN PLOT
    #Organize data
      #LH
      lh_ci_df$HEMI <- "LH"
      lh_ci_df$SEED_SPEC <- ifelse(lh_ci_df$dataset=="5" | lh_ci_df$dataset=="6" | lh_ci_df$dataset=="13" | lh_ci_df$dataset=="15" | lh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$TARG_SPEC <- ifelse(lh_ci_df$NewNetwork == "5" | lh_ci_df$NewNetwork=="6" | lh_ci_df$NewNetwork=="13" | lh_ci_df$NewNetwork=="15" | lh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$dataset==lh_ci_df$NewNetwork, "WITHIN", "")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", lh_ci_df$WITH_BTWN)
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", lh_ci_df$WITH_BTWN)
      
      #RH
      rh_ci_df$HEMI <- "RH"
      rh_ci_df$SEED_SPEC <- ifelse(rh_ci_df$dataset=="8" | rh_ci_df$dataset=="11" | rh_ci_df$dataset=="12" | rh_ci_df$dataset=="15" | rh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$TARG_SPEC <- ifelse(rh_ci_df$NewNetwork == "8" | rh_ci_df$NewNetwork=="11" | rh_ci_df$NewNetwork=="12" | rh_ci_df$NewNetwork=="15" | rh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$dataset==rh_ci_df$NewNetwork, "WITHIN", "")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", rh_ci_df$WITH_BTWN)
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", rh_ci_df$WITH_BTWN)
      
      #Combine LH and RH
      all_ci_df <- bind_rows(lh_ci_df, rh_ci_df)
      
      #Multiply AI by 100 for %
      all_ci_df$MEAN <- all_ci_df$MEAN*100
      all_ci_df$CI_MIN <- all_ci_df$CI_MIN*100
      all_ci_df$CI_MAX <- all_ci_df$CI_MAX*100
      
    #FIG. 1: BARPLOT
      all_ci_df$hemi_seedspec = as.factor(paste0(all_ci_df$HEMI,all_ci_df$SEED_SPEC))
      #ci_df <- ci_df[ci_df$NewNetwork %in% network_order, ]
      #Order main grouping
      grouping_order <- c('WITHIN', 'BTWN_SPEC', 'BTWN_NOT_SPEC')
      all_ci_df$WITH_BTWN <- factor(all_ci_df$WITH_BTWN, levels=grouping_order)
      #Order subgroups manually
      subgrouping_order <- c('LHSPEC', "RHSPEC", "LHNOT_SPEC", "RHNOT_SPEC")
      all_ci_df$hemi_seedspec <- factor(all_ci_df$hemi_seedspec, levels=subgrouping_order)
      GroupPalette <- c("#44AA99","#B2E0D8", "#CC6677", "#EECAD0")
      ggplot(all_ci_df, aes(x = WITH_BTWN, y = MEAN, group=interaction(hemi_seedspec, WITH_BTWN), fill = hemi_seedspec)) +
        geom_bar(stat = "summary", fun.y = "mean_se", position=position_dodge(width = .8)) +
        geom_point(stat="identity", position=position_dodge(width=.8), size=1, shape=21, color="black", fill="black")+
        #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
        labs(x = "", y = "Mean Adj. dAI (%)") +
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        scale_y_continuous(expand=c(0,0), limits=c(-2,34))+
        #scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")) +
        scale_x_discrete(labels=c("Within", "Between Specialized", "Between Not Specialized"))+
        #theme_bw()
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_LHRH_ADJ_MEAN_WITHBTWN_BAR_230623.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
      
#HORIZONTALLY-ORIENTED PLOTS
        #Multiply AI by 100 for %
        lh_ci_df$MEAN <- lh_ci_df$MEAN*100
        lh_ci_df$CI_MIN <- lh_ci_df$CI_MIN*100
        lh_ci_df$CI_MAX <- lh_ci_df$CI_MAX*100
        
      #LH AI LOOP
      lh_ci_df$NewNetwork <- as.factor(lh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(lh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        lh_ci_df$NewNetwork <- factor(lh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
                 geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
                 #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
                 geom_point(size=2, shape=21) +
                 coord_cartesian(xlim= c(1.2, NA), clip = "off") +
                 labs(y = paste0(network_list[counter], " Adj. LH dAI"), x = "") +
                 #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_fill_manual(values = CBIG_Palette) + 
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "none",
                   axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
                   axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 )
        )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCP-DISC_INDIVIDUAL_NETWORKLEVEL_LH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
      
      
      #RH
        #Multiply AI by 100 for %
        rh_ci_df$MEAN <- rh_ci_df$MEAN*100
        rh_ci_df$CI_MIN <- rh_ci_df$CI_MIN*100
        rh_ci_df$CI_MAX <- rh_ci_df$CI_MAX*100
        
      #RH
      rh_ci_df$NewNetwork <- as.factor(rh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(rh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        rh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
                 geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
                 #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
                 geom_point(size=2, shape=21) +
                 coord_cartesian(xlim= c(1.2, NA), clip = "off") +
                 labs(y = paste0(network_list[counter], " Adj. RH dAI"), x = "") +
                 #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_fill_manual(values = CBIG_Palette) + 
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 9),
                   legend.position = "none",
                   axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
                   axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 )
        )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCP-DISC_INDIVIDUAL_NETWORKLEVEL_RH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      

#MEAN MATRICES
    #LH AI  
      #Visualize the LH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(lh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#E69F00", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)==0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean LH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_NETWORKLEVELAI_LH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
    #RH AI  
      #Visualize the RH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(rh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#E69F00", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)== 0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean RH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_NETWORKLEVELAI_RH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
      
      
      
      
#LH WITHIN_NETWORK CORRELATION MATRICES
      #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.6285/(sqrt(276-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.6285
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPDISC_sub1 <- subset(HCPDISC_NET, NewDataset==d)
        
        #Format wide
        HCPDISC_wide  <- reshape(HCPDISC_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPDISC_wide[, c("LH_AVG_AI_ADJ.1", "LH_AVG_AI_ADJ.2", "LH_AVG_AI_ADJ.3", "LH_AVG_AI_ADJ.4", "LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", "LH_AVG_AI_ADJ.7", "LH_AVG_AI_ADJ.8", "LH_AVG_AI_ADJ.9", "LH_AVG_AI_ADJ.10", "LH_AVG_AI_ADJ.11", "LH_AVG_AI_ADJ.12", "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.14", "LH_AVG_AI_ADJ.15", "LH_AVG_AI_ADJ.16", "LH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        lh_cor_matrix <- cor(selected_vars)
        
        #Fig. 1: LH Heatmap (HCP only)
        # Convert the correlation matrix to a tidy format
        cor_data <- as.data.frame(as.table(lh_cor_matrix))
        colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
        color_breaks <- seq(-1, 1, by = 0.2)
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        #lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
                 geom_tile(color = "white",
                           lwd = 1,
                           linetype = 1) +
                 scale_fill_gradient(low = "#E69F00", high = "white", breaks=color_breaks, labels=color_breaks) +
                 geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
                 labs(title = paste0("HCP-DISC ", network_list[counter], " LH AI"), x = "", y = "", fill="")+
                 #labs(title = "", x = "", y = "", fill="")+
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "right",
                   legend.key.height = unit(1, "cm"),
                   legend.key.width = unit(.1, "cm"),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 ))
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", d, "_HCP-DISC_INDIVIDUAL_NETWORKLEVEL_LH_CORRMATRIX_230622.png"), width = 5, height = 5,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      

#RH CORRELATION MATRICES
      #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.6285/(sqrt(276-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.6285
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPDISC_sub1 <- subset(HCPDISC_NET, NewDataset==d)
        
        #Format wide
        HCPDISC_wide  <- reshape(HCPDISC_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPDISC_wide[, c("RH_AVG_AI_ADJ.1", "RH_AVG_AI_ADJ.2", "RH_AVG_AI_ADJ.3", "RH_AVG_AI_ADJ.4", "RH_AVG_AI_ADJ.5", "RH_AVG_AI_ADJ.6", "RH_AVG_AI_ADJ.7", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.9", "RH_AVG_AI_ADJ.10", "RH_AVG_AI_ADJ.11", "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.13", "RH_AVG_AI_ADJ.14", "RH_AVG_AI_ADJ.15", "RH_AVG_AI_ADJ.16", "RH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        rh_cor_matrix <- cor(selected_vars)
        
        #Fig. 1: LH Heatmap (HCP only)
        # Convert the correlation matrix to a tidy format
        cor_data <- as.data.frame(as.table(rh_cor_matrix))
        colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
        color_breaks <- seq(-1, 1, by = 0.2)
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        #lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
                 geom_tile(color = "white",
                           lwd = 1,
                           linetype = 1) +
                 scale_fill_gradient(low = "#E69F00", high = "white", breaks=color_breaks, labels=color_breaks) +
                 geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
                 labs(title = paste0("HCP-DISC ", network_list[counter], " RH AI"), x = "", y = "", fill="")+
                 #labs(title = "", x = "", y = "", fill="")+
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "right",
                   legend.key.height = unit(1, "cm"),
                   legend.key.width = unit(.1, "cm"),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 ))
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", d, "_HCP-DISC_INDIVIDUAL_NETWORKLEVEL_RH_CORRMATRIX_230622.png"), width = 5, height = 5,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      

#----------------------------NETWORK-LEVEL AI: HCP-REP--------------------------      
#LOAD DEMOS
      #Load ALL .csv
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      
      #Formatting for Demos
      study3_demos <- subset(study3, NewNetwork=="1")
      
      #Filter to just covariates
      HCPREP <- study3_demos[,c("SUBJID","dataset", "Sex_Bin", "Handedness", "Age_Centered", "FD_Centered")]
      
      #Constrain sex to factor
      HCPREP$Sex_Bin <- as.factor(HCPREP$Sex_Bin)
      
      #Filter to just HCP-REP,
      HCPREP <- subset(HCPREP, dataset=="HCP-REP")
      
      #Grab HCP-REP IDs
      REP_IDS <- HCPREP$SUBJID
      
    #Load ALL HCP network-level AI and combine
      #232 set
      N1_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-1_HCP_230421.csv")
      N2_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-2_HCP_230421.csv")
      N3_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-3_HCP_230421.csv")
      N4_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-4_HCP_230421.csv")
      N5_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-5_HCP_230421.csv")
      N6_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-6_HCP_230421.csv")
      N7_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-7_HCP_230421.csv")
      N8_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-8_HCP_230421.csv")
      N9_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-9_HCP_230421.csv")
      N10_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-10_HCP_230421.csv")
      N11_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-11_HCP_230421.csv")
      N12_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-12_HCP_230421.csv")
      N13_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-13_HCP_230421.csv")
      N14_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-14_HCP_230421.csv")
      N15_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-15_HCP_230421.csv")
      N16_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-16_HCP_230421.csv")
      N17_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-17_HCP_230421.csv")
      
      #REPLICATION (487) set
      N1R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-1_HCP_REP_230421.csv")
      N2R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-2_HCP_REP_230421.csv")
      N3R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-3_HCP_REP_230421.csv")
      N4R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-4_HCP_REP_230421.csv")
      N5R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-5_HCP_REP_230421.csv")
      N6R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-6_HCP_REP_230421.csv")
      N7R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-7_HCP_REP_230421.csv")
      N8R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-8_HCP_REP_230421.csv")
      N9R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-9_HCP_REP_230421.csv")
      N10R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-10_HCP_REP_230421.csv")
      N11R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-11_HCP_REP_230421.csv")
      N12R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-12_HCP_REP_230421.csv")
      N13R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-13_HCP_REP_230421.csv")
      N14R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-14_HCP_REP_230421.csv")
      N15R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-15_HCP_REP_230421.csv")
      N16R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-16_HCP_REP_230421.csv")
      N17R_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-17_HCP_REP_230421.csv")
      
      #Combine two sets
      N1_AI <- merge(N1_AI, N1R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N2_AI <- merge(N2_AI, N2R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N3_AI <- merge(N3_AI, N3R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N4_AI <- merge(N4_AI, N4R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N5_AI <- merge(N5_AI, N5R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N6_AI <- merge(N6_AI, N6R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N7_AI <- merge(N7_AI, N7R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N8_AI <- merge(N8_AI, N8R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N9_AI <- merge(N9_AI, N9R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N10_AI <- merge(N10_AI, N10R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N11_AI <- merge(N11_AI, N11R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N12_AI <- merge(N12_AI, N12R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N13_AI <- merge(N13_AI, N13R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N14_AI <- merge(N14_AI, N14R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N15_AI <- merge(N15_AI, N15R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N16_AI <- merge(N16_AI, N16R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      N17_AI <- merge(N17_AI, N17R_AI, by=c("SUBJID", "Network", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI"), all=TRUE)
      
      #Drop network 0
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame
        df <- df[df$Network != 0, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df)
      }
      
      #Convert network to numeric
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$Network <- as.numeric(as.character(df$Network))
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
      #Switch network ordering to reflect CBIG legend ordering - ordering is reflected in plots
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      list_of_dfs <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in list_of_dfs) {
        df <- get(df_name)
        df$NewNetwork <- recode(df$Network, !!!mapping)
        assign(df_name, df)
      }
      
      #Filter to IDs in demos
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame by keeping only rows where SUBJID is in TD_IDS
        df_sub <- df[df$SUBJID %in% REP_IDS, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df_sub)
      }
      
      #Convert NewNetwork to factor
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      counter <- 0
      for (df_name in df_names) {
        counter <- counter + 1
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$NewNetwork <- as.factor(df$NewNetwork)
        #Create dataset variable
        df$dataset <- counter
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
      #Combine all HCPDISC dfs
      HCPREP_NET <- data.frame()
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      
      for (df_name in df_names) {
        # Read the current dataframe
        current_df <- get(df_name)
        
        # Merge the current dataframe with HCPD_NET using bind_rows
        HCPREP_NET <- bind_rows(HCPREP_NET, current_df)
      }
      
      #Reorder datasets to conform with CBIG ordering
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCPREP_NET$NewDataset <- as.factor(recode(HCPREP_NET$dataset, !!!mapping))
      
      
#MODEL-ADJUSTED VALUES      
      #LH
      lh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPREP_NET$LH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPREP_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPREP, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPREP_NET$SUBJID %in% subsetted_ids &
            HCPREP_NET$NewNetwork %in% i &
            HCPREP_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPREP_NET$LH_AVG_AI_ADJ[matching_rows] <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$LH_AVG_AI_ADJ <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$LH_AVG_AI_ADJ)
          n <- length(subset_data$LH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$LH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          lh_ci_df <- rbind(lh_ci_df, row_df)
        }
      }
      
      
      #RH
      rh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPREP_NET$RH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPREP_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPREP, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPREP_NET$SUBJID %in% subsetted_ids &
            HCPREP_NET$NewNetwork %in% i &
            HCPREP_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPREP_NET$RH_AVG_AI_ADJ[matching_rows] <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$RH_AVG_AI_ADJ <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$RH_AVG_AI_ADJ)
          n <- length(subset_data$RH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$RH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          rh_ci_df <- rbind(rh_ci_df, row_df)
        }
      }

      
      
            
#WITHIN/BETWEEN PLOT
      #Organize data
      #LH
      lh_ci_df$HEMI <- "LH"
      lh_ci_df$SEED_SPEC <- ifelse(lh_ci_df$dataset=="5" | lh_ci_df$dataset=="6" | lh_ci_df$dataset=="13" | lh_ci_df$dataset=="15" | lh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$TARG_SPEC <- ifelse(lh_ci_df$NewNetwork == "5" | lh_ci_df$NewNetwork=="6" | lh_ci_df$NewNetwork=="13" | lh_ci_df$NewNetwork=="15" | lh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$dataset==lh_ci_df$NewNetwork, "WITHIN", "")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", lh_ci_df$WITH_BTWN)
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", lh_ci_df$WITH_BTWN)
      
      #RH
      rh_ci_df$HEMI <- "RH"
      rh_ci_df$SEED_SPEC <- ifelse(rh_ci_df$dataset=="8" | rh_ci_df$dataset=="11" | rh_ci_df$dataset=="12" | rh_ci_df$dataset=="15" | rh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$TARG_SPEC <- ifelse(rh_ci_df$NewNetwork == "8" | rh_ci_df$NewNetwork=="11" | rh_ci_df$NewNetwork=="12" | rh_ci_df$NewNetwork=="15" | rh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$dataset==rh_ci_df$NewNetwork, "WITHIN", "")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", rh_ci_df$WITH_BTWN)
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", rh_ci_df$WITH_BTWN)
      
      #Combine LH and RH
      all_ci_df <- bind_rows(lh_ci_df, rh_ci_df)
      
      #Multiply AI by 100 for %
      all_ci_df$MEAN <- all_ci_df$MEAN*100
      all_ci_df$CI_MIN <- all_ci_df$CI_MIN*100
      all_ci_df$CI_MAX <- all_ci_df$CI_MAX*100
      
    #FIG. 1: BARPLOT
      all_ci_df$hemi_seedspec = as.factor(paste0(all_ci_df$HEMI,all_ci_df$SEED_SPEC))
      #ci_df <- ci_df[ci_df$NewNetwork %in% network_order, ]
      #Order main grouping
      grouping_order <- c('WITHIN', 'BTWN_SPEC', 'BTWN_NOT_SPEC')
      all_ci_df$WITH_BTWN <- factor(all_ci_df$WITH_BTWN, levels=grouping_order)
      #Order subgroups manually
      subgrouping_order <- c('LHSPEC', "RHSPEC", "LHNOT_SPEC", "RHNOT_SPEC")
      all_ci_df$hemi_seedspec <- factor(all_ci_df$hemi_seedspec, levels=subgrouping_order)
      GroupPalette <- c("#44AA99","#B2E0D8", "#CC6677", "#EECAD0")
      ggplot(all_ci_df, aes(x = WITH_BTWN, y = MEAN, group=interaction(hemi_seedspec, WITH_BTWN), fill = hemi_seedspec)) +
        geom_bar(stat = "summary", fun.y = "mean_se", position=position_dodge(width = .8)) +
        geom_point(stat="identity", position=position_dodge(width=.8), size=1, shape=21, color="black", fill="black")+
        #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
        labs(x = "", y = "Mean dAI (%)") +
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        scale_y_continuous(expand=c(0,0), limits=c(-2,34))+
        #scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")) +
        scale_x_discrete(labels=c("Within", "Between Specialized", "Between Not Specialized"))+
        #theme_bw()
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_LHRH_ADJ_MEAN_WITHBTWN_BAR_230623.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
      
      
#HORIZONTALLY-ORIENTED PLOTS
        #Multiply AI by 100 for %
        lh_ci_df$MEAN <- lh_ci_df$MEAN*100
        lh_ci_df$CI_MIN <- lh_ci_df$CI_MIN*100
        lh_ci_df$CI_MAX <- lh_ci_df$CI_MAX*100
        
      #LH AI LOOP
      lh_ci_df$NewNetwork <- as.factor(lh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(lh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        lh_ci_df$NewNetwork <- factor(lh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
                 geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
                 #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
                 geom_point(size=2, shape=21) +
                 coord_cartesian(xlim= c(1.2, NA), clip = "off") +
                 labs(y = paste0(network_list[counter], " Adj. LH dAI"), x = "") +
                 #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_fill_manual(values = CBIG_Palette) + 
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "none",
                   axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
                   axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 )
        )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCP-REP_INDIVIDUAL_NETWORKLEVEL_LH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
      
      #RH
        #Multiply AI by 100 for %
        rh_ci_df$MEAN <- rh_ci_df$MEAN*100
        rh_ci_df$CI_MIN <- rh_ci_df$CI_MIN*100
        rh_ci_df$CI_MAX <- rh_ci_df$CI_MAX*100
      rh_ci_df$NewNetwork <- as.factor(rh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(rh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        rh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
                 geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
                 #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
                 geom_point(size=2, shape=21) +
                 coord_cartesian(xlim= c(1.2, NA), clip = "off") +
                 labs(y = paste0(network_list[counter], " Adj. RH dAI"), x = "") +
                 #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_fill_manual(values = CBIG_Palette) + 
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 9),
                   legend.position = "none",
                   axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
                   axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 )
        )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCP-REP_INDIVIDUAL_NETWORKLEVEL_RH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      

#MEAN MATRICES
    #LH AI  
      #Visualize the LH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(lh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#D55E00", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)== 0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean LH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_NETWORKLEVELAI_LH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
    #RH AI  
      #Visualize the RH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(rh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#D55E00", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)== 0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean RH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_NETWORKLEVELAI_RH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
      
      
      
#LH CORRELATION MATRICES
      #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.7551/(sqrt(277-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.7551
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPREP_sub1 <- subset(HCPREP_NET, NewDataset==d)
        
        #Format wide
        HCPREP_wide  <- reshape(HCPREP_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPREP_wide[, c("LH_AVG_AI_ADJ.1", "LH_AVG_AI_ADJ.2", "LH_AVG_AI_ADJ.3", "LH_AVG_AI_ADJ.4", "LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", "LH_AVG_AI_ADJ.7", "LH_AVG_AI_ADJ.8", "LH_AVG_AI_ADJ.9", "LH_AVG_AI_ADJ.10", "LH_AVG_AI_ADJ.11", "LH_AVG_AI_ADJ.12", "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.14", "LH_AVG_AI_ADJ.15", "LH_AVG_AI_ADJ.16", "LH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        lh_cor_matrix <- cor(selected_vars)
        
        #Fig. 1: LH Heatmap (HCP only)
        # Convert the correlation matrix to a tidy format
        cor_data <- as.data.frame(as.table(lh_cor_matrix))
        colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
        color_breaks <- seq(-1, 1, by = 0.2)
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        #lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
                 geom_tile(color = "white",
                           lwd = 1,
                           linetype = 1) +
                 scale_fill_gradient(low = "#D55E00", high = "white", breaks=color_breaks, labels=color_breaks) +
                 geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
                 labs(title = paste0("HCP-DISC ", network_list[counter], " LH AI"), x = "", y = "", fill="")+
                 #labs(title = "", x = "", y = "", fill="")+
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "right",
                   legend.key.height = unit(1, "cm"),
                   legend.key.width = unit(.1, "cm"),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 ))
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", d, "_HCP-REP_INDIVIDUAL_NETWORKLEVEL_LH_CORRMATRIX_230622.png"), width = 5, height = 5,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
            
#RH CORRELATION MATRICES
      #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.7551/(sqrt(277-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.7551
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPREP_sub1 <- subset(HCPREP_NET, NewDataset==d)
        
        #Format wide
        HCPREP_wide  <- reshape(HCPREP_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPREP_wide[, c("RH_AVG_AI_ADJ.1", "RH_AVG_AI_ADJ.2", "RH_AVG_AI_ADJ.3", "RH_AVG_AI_ADJ.4", "RH_AVG_AI_ADJ.5", "RH_AVG_AI_ADJ.6", "RH_AVG_AI_ADJ.7", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.9", "RH_AVG_AI_ADJ.10", "RH_AVG_AI_ADJ.11", "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.13", "RH_AVG_AI_ADJ.14", "RH_AVG_AI_ADJ.15", "RH_AVG_AI_ADJ.16", "RH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        rh_cor_matrix <- cor(selected_vars)
        
        #Fig. 1: LH Heatmap (HCP only)
        # Convert the correlation matrix to a tidy format
        cor_data <- as.data.frame(as.table(rh_cor_matrix))
        colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
        color_breaks <- seq(-1, 1, by = 0.2)
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        #lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
                 geom_tile(color = "white",
                           lwd = 1,
                           linetype = 1) +
                 scale_fill_gradient(low = "#D55E00", high = "white", breaks=color_breaks, labels=color_breaks) +
                 geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
                 labs(title = paste0("HCP-DISC ", network_list[counter], " RH AI"), x = "", y = "", fill="")+
                 #labs(title = "", x = "", y = "", fill="")+
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "right",
                   legend.key.height = unit(1, "cm"),
                   legend.key.width = unit(.1, "cm"),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 ))
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", d, "_HCP-REP_INDIVIDUAL_NETWORKLEVEL_RH_CORRMATRIX_230622.png"), width = 5, height = 5,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
      
      
           
      
#----------------------------NETWORK-LEVEL AI: HCPD-----------------------------
#LOAD DEMOS
      #Load data descriptor (Box)
      HCPD_data <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_LS_2.0_subject_completeness.xlsx")
      #Load IDs of 583 participants with REST fMRI data post-preproc
      PARC_IDS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/subjids/REST_ids.txt")
      DVARS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/DVARS_avg_HCPD_ALL_230517.csv")
      FD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/FD_avg_HCPD_ALL_230517.csv")
      VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/RemainingVols_HCPD_230517.csv")
      #EHI
      EHI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_EHI_230616.csv")
      EHI <- EHI[,c("subjectkey", "hcp_handedness_score")]
      #merge with HCPD Data
      HCPD_data <- merge(HCPD_data, EHI, by="subjectkey")
      
      #Clean up motion_metrics
      names(DVARS)[1] <- "SUBJID"
      names(FD)[1] <- "SUBJID"
      names(VOLS)[1] <- "SUBJID"
      
      FD$SUBJID <- gsub("^.{0,4}", "", FD$SUBJID) #remove "sub-" string
      DVARS$SUBJID <- gsub("^.{0,4}", "", DVARS$SUBJID) #remove "sub-" string
      VOLS$SUBJID <- gsub("^.{0,4}", "", VOLS$SUBJID) #remove "sub-" string
      
      #Clean up demos dataset
      HCPD_data <- HCPD_data[,c("src_subject_id", "sex", "interview_age", "Full_MR_Compl", "hcp_handedness_score")]
      HCPD_data <- subset(HCPD_data, src_subject_id!="HCA or HCD subject id")
      names(HCPD_data)[1] <- "SUBJID"
      
      #Rename handedness for consistency
      HCPD_data$Handedness <- HCPD_data$hcp_handedness_score
      
      #Age Years (convert from months to years)
      HCPD_data$interview_age <- as.numeric(as.character(HCPD_data$interview_age))
      HCPD_data$Age_in_Yrs <- HCPD_data$interview_age/12
      
      #Create dataset marker
      HCPD_data$dataset <- "HCPD"
      
      #Merge datasets
      HCPD_data <- merge(HCPD_data, PARC_IDS, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, DVARS, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, FD, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, VOLS, by =c("SUBJID"), all=FALSE)
      
      #Exclusions
      #1. Participants with FD_avg greater than 0.2
      HCPD_data <- subset(HCPD_data, FD_avg<.2)
      #2. Participants with avg DVARS greater than 50
      HCPD_data <- subset(HCPD_data, DVARS_avg<51)
      #3. % volumes remaining is less than 50% (474 * 4 = 1896) or 253*6=1518 for ages 5-7
      HCPD_data$Percent_Vols <- ifelse(HCPD_data$Age_in_Yrs < 7, (HCPD_data$Sum_Volumes / (253*6)), (HCPD_data$Sum_Volumes / (474*4)))
      HCPD_data <- subset(HCPD_data, Percent_Vols>.5)
      HCPD_data$Percent_Vols <- HCPD_data$Percent_Vols*100
      #4. Participants missing handedness
      HCPD_data <- subset(HCPD_data, Handedness!="NA")
      
      #dataset indicator
      HCPD_data$dataset <- "HCPD"
      
      #subset to relevant variables
      HCPD_data <- HCPD_data[,c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness")]
      
      #Organize covariates
      HCPD_data$Age_Centered <- as.numeric(HCPD_data$Age_in_Yrs - mean(HCPD_data$Age_in_Yrs))
      HCPD_data$FD_Centered <- as.numeric(HCPD_data$FD_avg - mean(HCPD_data$FD_avg))
      HCPD_data$Sex_Bin <- as.factor(ifelse(HCPD_data$sex == "M", 0, 1)) #recode males as 0 and Females as 1
      HCPD_data$Handedness <- as.numeric(HCPD_data$Handedness)
      
      #Grab IDs
      IDS_343 <- HCPD_data$SUBJID
      
      #Load network-level AI
      N1_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-1_HCPD_230421.csv")
      N2_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-2_HCPD_230421.csv")
      N3_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-3_HCPD_230421.csv")
      N4_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-4_HCPD_230421.csv")
      N5_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-5_HCPD_230421.csv")
      N6_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-6_HCPD_230421.csv")
      N7_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-7_HCPD_230421.csv")
      N8_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-8_HCPD_230421.csv")
      N9_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-9_HCPD_230421.csv")
      N10_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-10_HCPD_230421.csv")
      N11_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-11_HCPD_230421.csv")
      N12_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-12_HCPD_230421.csv")
      N13_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-13_HCPD_230421.csv")
      N14_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-14_HCPD_230421.csv")
      N15_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-15_HCPD_230421.csv")
      N16_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-16_HCPD_230421.csv")
      N17_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/network_level_ai/MSHBM2019_LONG_NETWORK_LEVEL_AI_NETWORK-17_HCPD_230421.csv")
      
      #Drop network 0
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame
        df <- df[df$Network != 0, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df)
      }
      
      #Convert network to numeric
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$Network <- as.numeric(as.character(df$Network))
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
      #Switch network ordering to reflect CBIG legend ordering - ordering is reflected in plots
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      list_of_dfs <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      for (df_name in list_of_dfs) {
        df <- get(df_name)
        df$NewNetwork <- recode(df$Network, !!!mapping)
        assign(df_name, df)
      }
      
      #Filter to IDs in demos
      for (df_name in df_names) {
        # retrieve the data frame object using get()
        df <- get(df_name)
        # subset the data frame by keeping only rows where SUBJID is in TD_IDS
        df_sub <- df[df$SUBJID %in% IDS_343, ]
        # overwrite the original data frame with the subsetted data frame
        assign(df_name, df_sub)
      }
      
      #Convert NewNetwork to factor
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      counter <- 0
      for (df_name in df_names) {
        counter <- counter + 1
        # retrieve the data frame object using get()
        df <- get(df_name)
        # convert Network to numeric
        df$NewNetwork <- as.factor(df$NewNetwork)
        #Create dataset variable
        df$dataset <- counter
        # overwrite the original data frame with the modified one
        assign(df_name, df)
      }
      
    #Combine all HCPD dfs
      HCPD_NET <- data.frame()
      df_names <- c("N1_AI", "N2_AI", "N3_AI", "N4_AI", "N5_AI", "N6_AI", "N7_AI", "N8_AI", "N9_AI", "N10_AI", "N11_AI", "N12_AI", "N13_AI", "N14_AI", "N15_AI", "N16_AI", "N17_AI")
      
      for (df_name in df_names) {
        # Read the current dataframe
        current_df <- get(df_name)
        
        # Merge the current dataframe with HCPD_NET using bind_rows
        HCPD_NET <- bind_rows(HCPD_NET, current_df)
      }
    
    #Reorder datasets to conform with CBIG ordering
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCPD_NET$NewDataset <- as.factor(recode(HCPD_NET$dataset, !!!mapping))
      
      
#MODEL-ADJUSTED VALUES      
      #LH
      lh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPD_NET$LH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPD_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPD_data, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPD_NET$SUBJID %in% subsetted_ids &
            HCPD_NET$NewNetwork %in% i &
            HCPD_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPD_NET$LH_AVG_AI_ADJ[matching_rows] <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$LH_AVG_AI_ADJ <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$LH_AVG_AI_ADJ)
          n <- length(subset_data$LH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$LH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          lh_ci_df <- rbind(lh_ci_df, row_df)
        }
      }
      
      
      
      #RH
      rh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      HCPD_NET$RH_AVG_AI_ADJ <- NA
      
      for (d in 1:17) {
        subset1 <- subset(HCPD_NET, NewDataset==d)
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          #merge AI dataframe with demos
          subset_data <- merge(subset_data, HCPD_data, by="SUBJID", all=FALSE)
          
          # Fit the linear regression model
          model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPD_NET$SUBJID %in% subsetted_ids &
            HCPD_NET$NewNetwork %in% i &
            HCPD_NET$NewDataset == d
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPD_NET$RH_AVG_AI_ADJ[matching_rows] <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$RH_AVG_AI_ADJ <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$RH_AVG_AI_ADJ)
          n <- length(subset_data$RH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$RH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          rh_ci_df <- rbind(rh_ci_df, row_df)
        }
      }
 
      
#WITHIN/BETWEEN PLOT
      #Organize data
      #LH
      lh_ci_df$HEMI <- "LH"
      lh_ci_df$SEED_SPEC <- ifelse(lh_ci_df$dataset=="5" | lh_ci_df$dataset=="6" | lh_ci_df$dataset=="13" | lh_ci_df$dataset=="15" | lh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$TARG_SPEC <- ifelse(lh_ci_df$NewNetwork == "5" | lh_ci_df$NewNetwork=="6" | lh_ci_df$NewNetwork=="13" | lh_ci_df$NewNetwork=="15" | lh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$dataset==lh_ci_df$NewNetwork, "WITHIN", "")
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", lh_ci_df$WITH_BTWN)
      lh_ci_df$WITH_BTWN <- ifelse(lh_ci_df$WITH_BTWN!="WITHIN" & lh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", lh_ci_df$WITH_BTWN)
      
      #RH
      rh_ci_df$HEMI <- "RH"
      rh_ci_df$SEED_SPEC <- ifelse(rh_ci_df$dataset=="8" | rh_ci_df$dataset=="11" | rh_ci_df$dataset=="12" | rh_ci_df$dataset=="15" | rh_ci_df$dataset=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$TARG_SPEC <- ifelse(rh_ci_df$NewNetwork == "8" | rh_ci_df$NewNetwork=="11" | rh_ci_df$NewNetwork=="12" | rh_ci_df$NewNetwork=="15" | rh_ci_df$NewNetwork=="17", "SPEC", "NOT_SPEC")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$dataset==rh_ci_df$NewNetwork, "WITHIN", "")
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="SPEC", "BTWN_SPEC", rh_ci_df$WITH_BTWN)
      rh_ci_df$WITH_BTWN <- ifelse(rh_ci_df$WITH_BTWN!="WITHIN" & rh_ci_df$TARG_SPEC=="NOT_SPEC", "BTWN_NOT_SPEC", rh_ci_df$WITH_BTWN)
      
      #Combine LH and RH
      all_ci_df <- bind_rows(lh_ci_df, rh_ci_df)
      
      #Multiply AI by 100 for %
      all_ci_df$MEAN <- all_ci_df$MEAN*100
      all_ci_df$CI_MIN <- all_ci_df$CI_MIN*100
      all_ci_df$CI_MAX <- all_ci_df$CI_MAX*100
      
    #FIG. 1: BARPLOT
      all_ci_df$hemi_seedspec = as.factor(paste0(all_ci_df$HEMI,all_ci_df$SEED_SPEC))
      #ci_df <- ci_df[ci_df$NewNetwork %in% network_order, ]
      #Order main grouping
      grouping_order <- c('WITHIN', 'BTWN_SPEC', 'BTWN_NOT_SPEC')
      all_ci_df$WITH_BTWN <- factor(all_ci_df$WITH_BTWN, levels=grouping_order)
      #Order subgroups manually
      subgrouping_order <- c('LHSPEC', "RHSPEC", "LHNOT_SPEC", "RHNOT_SPEC")
      all_ci_df$hemi_seedspec <- factor(all_ci_df$hemi_seedspec, levels=subgrouping_order)
      GroupPalette <- c("#44AA99","#B2E0D8", "#CC6677", "#EECAD0")
      ggplot(all_ci_df, aes(x = WITH_BTWN, y = MEAN, group=interaction(hemi_seedspec, WITH_BTWN), fill = hemi_seedspec)) +
        geom_bar(stat = "summary", fun.y = "mean_se", position=position_dodge(width = .8)) +
        geom_point(stat="identity", position=position_dodge(width=.8), size=1, shape=21, color="black", fill="black")+
        #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
        labs(x = "", y = "Mean dAI (%)") +
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        scale_y_continuous(expand=c(0,0), limits=c(-2,34))+
        scale_x_discrete(labels=c("Within", "Between Specialized", "Between Not Specialized"))+
        #theme_bw()
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_LHRH_ADJ_MEAN_WITHBTWN_BAR_230623.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
           
      
#HORIZONTALLY-ORIENTED PLOTS
      #Multiply AI by 100 for %
      lh_ci_df$MEAN <- lh_ci_df$MEAN*100
      lh_ci_df$CI_MIN <- lh_ci_df$CI_MIN*100
      lh_ci_df$CI_MAX <- lh_ci_df$CI_MAX*100
      
      #LH AI LOOP
      lh_ci_df$NewNetwork <- as.factor(lh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(lh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        lh_ci_df$NewNetwork <- factor(lh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
          ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
          geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
          #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
          geom_point(size=2, shape=21) +
          coord_cartesian(xlim= c(1.2, NA), clip = "off") +
          labs(y = paste0(network_list[counter], " Adj. LH dAI"), x = "") +
          #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
          scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
          scale_fill_manual(values = CBIG_Palette) + 
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none",
            axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
            axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          )
         )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCPD_INDIVIDUAL_NETWORKLEVEL_LH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
      
    #RH  
      #Multiply AI by 100 for %
      rh_ci_df$MEAN <- rh_ci_df$MEAN*100
      rh_ci_df$CI_MIN <- rh_ci_df$CI_MIN*100
      rh_ci_df$CI_MAX <- rh_ci_df$CI_MAX*100
      
      #RH AI LOOP
      rh_ci_df$NewNetwork <- as.factor(rh_ci_df$NewNetwork)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      for (df_name in 1:17) {
        counter <- counter + 1 
        #subset to each seed network
        subset_data <- subset(rh_ci_df, dataset==df_name)
        # Create the point and line plot
        network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(subset_data, aes(x=NewNetwork, y=MEAN, fill=NewNetwork)) + 
                 geom_vline(xintercept = seq(from=1, to=17, by = 1), linetype="dotted", color="darkgray") +
                 #geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  color="black", size=.5) +
                 geom_point(size=2, shape=21) +
                 coord_cartesian(xlim= c(1.2, NA), clip = "off") +
                 labs(y = paste0(network_list[counter], " Adj. RH dAI"), x = "") +
                 #geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_fill_manual(values = CBIG_Palette) + 
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 9),
                   legend.position = "none",
                   axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
                   axis.text.x = element_text(colour = "black", angle=20, vjust=1, hjust=1),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 )
        )
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", df_name, "_HCPD_INDIVIDUAL_NETWORKLEVEL_RH_PointLine_230621.png"), width = 6.9, height = 1.7,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
      
 
      
#MEAN MATRICES
    #LH AI  
      #Visualize the LH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(lh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#0072B2", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)== 0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean LH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_NETWORKLEVELAI_LH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
    #RH AI  
      #Visualize the RH AI matrix using ggplot heatmap 
      color_breaks <- seq(-1, 1, by = 0.1)
      # Create the ggplot heatmap with reordered axis labels
      ggplot(rh_ci_df, aes(x = as.factor(dataset), y = as.factor(NewNetwork), fill = MEAN)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "#0072B2", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(NewNetwork, dataset, label = ifelse(round(MEAN,2)== 0, "", round(MEAN, 2))), color = "black", size = 1.75) +
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        xlab("Target Network") +
        ylab("Averaged Network") +
        labs(fill = "Mean RH dAI")+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_NETWORKLEVELAI_RH_MEAN_HEATMAP_230622.png"), width = 5.5, height = 4.2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      
      
      
           
#LH CORRELATION MATRICES
     #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.6194/(sqrt(343-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.6194
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPD_sub1 <- subset(HCPD_NET, NewDataset==d)
        
        #Format wide
        HCPD_wide  <- reshape(HCPD_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPD_wide[, c("LH_AVG_AI_ADJ.1", "LH_AVG_AI_ADJ.2", "LH_AVG_AI_ADJ.3", "LH_AVG_AI_ADJ.4", "LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", "LH_AVG_AI_ADJ.7", "LH_AVG_AI_ADJ.8", "LH_AVG_AI_ADJ.9", "LH_AVG_AI_ADJ.10", "LH_AVG_AI_ADJ.11", "LH_AVG_AI_ADJ.12", "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.14", "LH_AVG_AI_ADJ.15", "LH_AVG_AI_ADJ.16", "LH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        lh_cor_matrix <- cor(selected_vars)
      
      #Fig. 1: LH Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(lh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
      #lh_ci_df$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
      assign(paste0(df_name, "_plot"), 
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "skyblue", high = "white", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
        labs(title = paste0("HCPD ", network_list[counter], " LH AI"), x = "", y = "", fill="")+
        #labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        ))
      # save the plot under a name that includes the dataframe name
      ggsave(filename = paste0("Study3_", d, "_HCPD_INDIVIDUAL_NETWORKLEVEL_LH_CORRMATRIX_230621.png"), width = 5, height = 5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }
  
          
#RH CORRELATION MATRICES
      #Create matrices for each network
      counter <- 0
      network_list <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      #Threshold!
      r_value <- 3.6194/(sqrt(343-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.6194
      
      for (d in 1:17) {
        counter <- counter + 1 
        #Subset to seed network
        HCPD_sub1 <- subset(HCPD_NET, NewDataset==d)
        
        #Format wide
        HCPD_wide  <- reshape(HCPD_sub1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
        
        #Select networks
        selected_vars <- HCPD_wide[, c("RH_AVG_AI_ADJ.1", "RH_AVG_AI_ADJ.2", "RH_AVG_AI_ADJ.3", "RH_AVG_AI_ADJ.4", "RH_AVG_AI_ADJ.5", "RH_AVG_AI_ADJ.6", "RH_AVG_AI_ADJ.7", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.9", "RH_AVG_AI_ADJ.10", "RH_AVG_AI_ADJ.11", "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.13", "RH_AVG_AI_ADJ.14", "RH_AVG_AI_ADJ.15", "RH_AVG_AI_ADJ.16", "RH_AVG_AI_ADJ.17")] 
        
        # Compute the correlation matrix
        rh_cor_matrix <- cor(selected_vars)
        
        #Fig. 1: LH Heatmap (HCP only)
        # Convert the correlation matrix to a tidy format
        cor_data <- as.data.frame(as.table(rh_cor_matrix))
        colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
        color_breaks <- seq(-1, 1, by = 0.2)
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        #rh_cor_matrix$NewNetwork <- factor(rh_ci_df$NewNetwork, level = network_order)
        assign(paste0(df_name, "_plot"), 
               ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
                 geom_tile(color = "white",
                           lwd = 1,
                           linetype = 1) +
                 scale_fill_gradient(low = "skyblue", high = "white", breaks=color_breaks, labels=color_breaks) +
                 geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 1 | abs(Correlation) < r_value, "", round(Correlation, 2))), color = "black", size = 2) +
                 labs(title = paste0("HCPD ", network_list[counter], " RH AI"), x = "", y = "", fill="")+
                 #labs(title = "", x = "", y = "", fill="")+
                 scale_x_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 scale_y_discrete(labels = c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
                 theme(
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 10),
                   legend.position = "right",
                   legend.key.height = unit(1, "cm"),
                   legend.key.width = unit(.1, "cm"),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                 ))
        # save the plot under a name that includes the dataframe name
        ggsave(filename = paste0("Study3_", d, "_HCPD_INDIVIDUAL_NETWORKLEVEL_RH_CORRMATRIX_230621.png"), width = 5, height = 5,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures", dpi = 300)
      }       
      
      
#-----------------------------BETWEEN-NETWORK RELATIONSHIPS: CORRELATION MATRICES----------------------
#Load ALL .csv
study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
study3$Sex_Bin <- as.factor(study3$Sex_Bin)  

#Created model-adjusted values for 3 datasets
      #LH
      study3$LH_AVG_AI_ADJ <- NA
      lh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study3, dataset==d)
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          # Fit the linear regression model
          model <- lm(LH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- study3$SUBJID %in% subsetted_ids &
            study3$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          study3$LH_AVG_AI_ADJ[matching_rows] <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$LH_AVG_AI_ADJ <- subset_data$LH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean/CI
          MEAN <- mean(subset_data$LH_AVG_AI_ADJ)
          n <- length(subset_data$LH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$LH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          lh_ci_df <- rbind(lh_ci_df, row_df)
        }
      }
      
      #RH
      study3$RH_AVG_AI_ADJ <- NA
      rh_ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             MEAN=integer())
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study3, dataset==d)
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          # Fit the linear regression model
          model <- lm(RH_AVG_AI ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- study3$SUBJID %in% subsetted_ids &
            study3$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          study3$RH_AVG_AI_ADJ[matching_rows] <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$RH_AVG_AI_ADJ <- subset_data$RH_AVG_AI - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$RH_AVG_AI_ADJ)
          n <- length(subset_data$RH_AVG_AI_ADJ)
          std_dev <- sd(subset_data$RH_AVG_AI_ADJ)
          std_err <- std_dev / sqrt(n)
          alpha = 0.05
          degrees_of_freedom <- n - 1
          t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
          margin_error <- t_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          rh_ci_df <- rbind(rh_ci_df, row_df)
        }
      }

#Reshape dataframe to WIDE
      study3_wide  <- reshape(study3, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
      
#HCP-DISC CORRELATION MATRIX
      #Subset to HCP-DISC
      HCPDISC_wide <- subset(study3_wide, dataset.1=="HCP-DISC")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPDISC_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                    "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                    "LH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      lh_cor_matrix <- cor(selected_vars)
      
      lh_cor_matrix[abs(lh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 1: LH Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(lh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)

      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-DISC LH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_LH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
    #FIG. 2: RH: 17, 11, 15, 8, 12
      HCPDISC_wide <- subset(study3_wide, dataset.1=="HCP-DISC")
      selected_vars <- HCPDISC_wide[, c("RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                        "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                        "RH_AVG_AI_ADJ.17")]
      # Compute the correlation matrix
      rh_cor_matrix <- cor(selected_vars)
      rh_cor_matrix[abs(rh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 1: RH Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(rh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-DISC RH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_RH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      

      
      
    #FIG. 3: LH & RH HCP-DISC
      HCPDISC_wide <- subset(study3_wide, dataset.1=="HCP-DISC")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPDISC_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                        "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                        "LH_AVG_AI_ADJ.17", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                        "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                        "RH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      all_cor_matrix <- cor(selected_vars)
      all_cor_matrix[abs(all_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #ALL Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(all_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3) +
        #labs(title = "HCP-DISC LH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        scale_y_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 25),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-DISC_ALL_AI_Corr_Heatmap_230621.png"), width = 5, height = 4,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      

#HCP-REP CORRELATION MATRIX
      #Subset to HCP-REP
      HCPREP_wide <- subset(study3_wide, dataset.1=="HCP-REP")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPREP_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                        "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                        "LH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      lh_cor_matrix <- cor(selected_vars)
      lh_cor_matrix[abs(lh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 1: LH Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(lh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-REP LH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_LH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
         
    #FIG. 2: RH: 17, 11, 15, 8, 12
      HCPREP_wide <- subset(study3_wide, dataset.1=="HCP-REP")
      selected_vars <- HCPREP_wide[, c("RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                        "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                        "RH_AVG_AI_ADJ.17")]
      # Compute the correlation matrix
      rh_cor_matrix <- cor(selected_vars)
      rh_cor_matrix[abs(rh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 2: RH Heatmap HCP-REP
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(rh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-REP RH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_RH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
    #FIG. 3: LH & RH HCP-REP
      HCPREP_wide <- subset(study3_wide, dataset.1=="HCP-REP")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPREP_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                        "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                        "LH_AVG_AI_ADJ.17", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                        "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                        "RH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      all_cor_matrix <- cor(selected_vars)
      all_cor_matrix[abs(all_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #ALL Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(all_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3) +
        #labs(title = "HCP-REP AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        scale_y_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 25),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCP-REP_ALL_AI_Corr_Heatmap_230621.png"), width = 5, height = 4,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
      
      
         
            
#HCPD CORRELATION MATRIX
      #Subset to HCP-REP
      HCPD_wide <- subset(study3_wide, dataset.1=="HCPD")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPD_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                       "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                       "LH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      lh_cor_matrix <- cor(selected_vars)
      lh_cor_matrix[abs(lh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 1: LH Heatmap (HCP only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(lh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-REP LH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("LANG","DAN-A", "DEF-A","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_LH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
     
    #FIG. 2: RH: 17, 11, 15, 8, 12
      HCPD_wide <- subset(study3_wide, dataset.1=="HCPD")
      selected_vars <- HCPD_wide[, c("RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                       "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                       "RH_AVG_AI_ADJ.17")]
      # Compute the correlation matrix
      rh_cor_matrix <- cor(selected_vars)
      rh_cor_matrix[abs(rh_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #Fig. 2: RH Heatmap HCP-REP
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(rh_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3.5) +
        #labs(title = "HCP-REP RH AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        scale_y_discrete(labels=c("SAL-A","CTRL-B", "CTRL-C","DEF-C", "LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_RH_AI_Corr_Heatmap_230621.png"), width = 3.5, height = 3.5,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
      
     
  #FIG. 3: LH & RH HCPD
      HCPD_wide <- subset(study3_wide, dataset.1=="HCPD")
      #Just include top 5 LH and RH AI networks
      selected_vars <- HCPD_wide[, c("LH_AVG_AI_ADJ.5", "LH_AVG_AI_ADJ.6", 
                                       "LH_AVG_AI_ADJ.13", "LH_AVG_AI_ADJ.15",
                                       "LH_AVG_AI_ADJ.17", "RH_AVG_AI_ADJ.8", "RH_AVG_AI_ADJ.11", 
                                       "RH_AVG_AI_ADJ.12", "RH_AVG_AI_ADJ.15",
                                       "RH_AVG_AI_ADJ.17")]
      
      # Compute the correlation matrix
      all_cor_matrix <- cor(selected_vars)
      all_cor_matrix[abs(all_cor_matrix) ==1] <- 0 #removes values less than .25
      
      #ALL Heatmap (HCPD only)
      # Convert the correlation matrix to a tidy format
      cor_data <- as.data.frame(as.table(all_cor_matrix))
      colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
      color_breaks <- seq(-1, 1, by = 0.2)
      
      # Create the heatmap using ggplot functions
      ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
        geom_tile(color = "white",
                  lwd = 1,
                  linetype = 1) +
        scale_fill_gradient(low = "white", high = "skyblue", breaks=color_breaks, labels=color_breaks) +
        geom_text(aes(Variable2, Variable1, label = ifelse(Correlation == 0, "", round(Correlation, 2))), color = "black", size = 3) +
        #labs(title = "HCPD AI Corr. Matrix", x = "", y = "", fill="")+
        labs(title = "", x = "", y = "", fill="")+
        scale_x_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        scale_y_discrete(labels=c("LH LANG","LH DAN-A", "LH DEF-A","LH DEF-C", "LH LIM-B", "RH SAL-A","RH CTRL-B", "RH CTRL-C","RH DEF-C", "RH LIM-B")) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "right",
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(.1, "cm"),
          axis.text.y = element_text(colour = "black", hjust = 1),
          axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 25),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study3_HCPD_ALL_AI_Corr_Heatmap_230621.png"), width = 5, height = 4,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      
              
      
#-------------------------HCP232 HANDEDNESS x AI---------------------
#SETUP      
      #Load HCP232 data
      HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
      HCP_ALL_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
      HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
      FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_HCP_ALL_230515.csv")
      DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_HCP_ALL_230515.csv")
      TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_HCP_230515.csv")
      
      #Format data
      names(HCP_DEMOS)[1] <- "SUBJID"
      names(HCP_R_DEMOS)[1] <- "SUBJID"
      names(FD_AVG)[1] <- "SUBJID"
      names(DVARS_AVG)[1] <- "SUBJID"
      names(TOT_VOLS)[1] <- "SUBJID"
      
      FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
      DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
      TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
      TOT_VOLS$Percent_Vols <- TOT_VOLS$Sum_Volumes / (1196*4) #Number of volumes after skip4 = 1196 x4 runs
      
      #Merge HCP datasets
      comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
      comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
      comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
      HCP_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
      
      #Variable for sex (Male=1, Female=2; same as UT-TD)
      HCP_df$sex <- ifelse(HCP_df$Gender == "F", 2, ifelse(HCP_df$Gender == "M", 1, NA))
      
      #Just include relevant columns
      HCP_df <- HCP_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness")]
      
      #Load AI data
      HCP_AI_ALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
      
      #Drop Network 0 (medial wall)
      HCP_AI_ALL <- subset(HCP_AI_ALL, Network!=0)
      
      #Reorder CBIG Networks
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCP_AI_ALL$NewNetwork <- recode(HCP_AI_ALL$Network, !!!mapping)
      
      #Create dataset marker
      HCP_AI_ALL$dataset <- "HCP"
      
      #Merge HCP AI with demos
      HCP_AI <- merge(HCP_AI_ALL, HCP_df, by=c("SUBJID"), all=TRUE)
      
      
#FIGURES: 
      #LH
      for (n in 1:17) {
        subsetted_data <- subset(HCP_AI, NewNetwork == n)  # Subsetting the dataset based on Network
        
        Network_Names<- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
        
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
        subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
        
        filename <- paste("Study3_HCP_AI_LH_HandednessPlots_Network", n, "_230615.png", sep = "")
        plot_title <- paste(Network_Names[n])
        
        ggplot(subsetted_data, aes(x = Handedness, y = LH_AVG_AI, color = NewNetwork)) +
          labs(x = "Handedness (EHI)", y = "LH AVG AI (Raw)") +
          labs(fill = " ", color = " ") +
          ggtitle(plot_title)+
          geom_point(aes(fill = NewNetwork), pch = 21) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
          geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette[n]) +
          scale_fill_manual(values = CBIG_Palette[n]) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 2.21, height = 2.21,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      }    
      
    #RH
      for (n in 1:17) {
        subsetted_data <- subset(HCP_AI, NewNetwork == n)  # Subsetting the dataset based on Network
        
        Network_Names<- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
        
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
        subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
        
        filename <- paste("Study3_HCP_AI_RH_HandednessPlots_Network", n, "_230615.png", sep = "")
        plot_title <- paste(Network_Names[n])
        
        ggplot(subsetted_data, aes(x = Handedness, y = RH_AVG_AI, color = NewNetwork)) +
          labs(x = "Handedness (EHI)", y = "RH AVG AI (Raw)") +
          labs(fill = " ", color = " ") +
          ggtitle(plot_title)+
          geom_point(aes(fill = NewNetwork), pch = 21) +
          #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
          geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          #scale_y_continuous(limits = c(-1, 1)) +
          #scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette[n]) +
          scale_fill_manual(values = CBIG_Palette[n]) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 2.21, height = 2.21,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      } 

      
#----------------------------------tSNR GROUP MAPS-----------------------------------------
#LOAD DATA
      #Load ALL .csv
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      
      #Load tSNR
      HCP_tsnr <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/ALL/HCP_Network_AVG_SNR_230614.csv")
      HCPR_tsnr <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/REPLICATION/HCP_REP_Network_AVG_SNR_230614.csv")
      HCPD_tsnr <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/tSNR/ALL/HCPD_Network_AVG_SNR_230614.csv")
      
      tSNR <- merge(HCP_tsnr, HCPR_tsnr, by=c("SUBJID", "Network", "LH_AVG_SNR", "RH_AVG_SNR"), all=TRUE)
      tSNR <- merge(tSNR, HCPD_tsnr, by=c("SUBJID", "Network", "LH_AVG_SNR", "RH_AVG_SNR"), all=TRUE)
      
      #Merge tSNR and ALL
      group_tsnr <- merge(tSNR, study3, by=c("SUBJID", "Network"), all=TRUE)
      
      #Drop subjects missing $FD_avg
      group_tsnr <- subset(group_tsnr, FD_avg!="NA")
      
#HCP-DISC avg. tSNR per network in LH and RH
      #Filter to HCP-DISC
      group_tsnr_disc <- subset(group_tsnr, dataset=="HCP-DISC")
      
      #Avg. tSNR across network
      HCPDISC_LH_MEAN <- aggregate(LH_AVG_SNR ~ Network, data = group_tsnr_disc, FUN = mean)
      HCPDISC_RH_MEAN <- aggregate(RH_AVG_SNR ~ Network, data = group_tsnr_disc, FUN = mean)
      
      #Save out avg as .txt files
      write.csv(HCPDISC_LH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/HCPDISC/HCPDISC_tSNR_LH_230911.csv",row.names=FALSE, quote=FALSE)
      write.csv(HCPDISC_RH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/HCPDISC/HCPDISC_tSNR_RH_230911.csv",row.names=FALSE, quote=FALSE)
      

#HCP-REP avg. tSNR per network in LH and RH
      #Filter to HCP-REP
      group_tsnr_rep <- subset(group_tsnr, dataset=="HCP-REP")
      
      #Avg. tSNR across network
      HCPREP_LH_MEAN <- aggregate(LH_AVG_SNR ~ Network, data = group_tsnr_rep, FUN = mean)
      HCPREP_RH_MEAN <- aggregate(RH_AVG_SNR ~ Network, data = group_tsnr_rep, FUN = mean)
      
      #Save out avg as .txt files
      write.csv(HCPREP_LH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/HCPREP/HCPREP_tSNR_LH_230911.csv",row.names=FALSE, quote=FALSE)
      write.csv(HCPREP_RH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/tSNR/HCPREP/HCPREP_tSNR_RH_230911.csv",row.names=FALSE, quote=FALSE)

            
#HCPD avg. tSNR per network in LH and RH
      #Filter to HCP-D
      group_tsnr_d <- subset(group_tsnr, dataset=="HCPD")
      
      #Avg. tSNR across network
      HCPD_LH_MEAN <- aggregate(LH_AVG_SNR ~ Network, data = group_tsnr_d, FUN = mean)
      HCPD_RH_MEAN <- aggregate(RH_AVG_SNR ~ Network, data = group_tsnr_d, FUN = mean)
      
      #Save out avg as .txt files
      write.csv(HCPD_LH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/tSNR/ALL/HCPD_tSNR_LH_230911.csv",row.names=FALSE, quote=FALSE)
      write.csv(HCPD_RH_MEAN[2],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/tSNR/ALL/HCPD_tSNR_RH_230911.csv",row.names=FALSE, quote=FALSE)
      

#-----------------------------------------SUBJIDS----------------------------------
#OFFICIAL SUBJIDS TEXT LISTS
      #Load dataset
      study3 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv")
      study3_demos <- subset(study3, NewNetwork=="1")

      #Subset to HCPD
      hcpd <- subset(study3_demos, dataset=="HCPD")
      #Save out .txt
      write.csv(hcpd[1],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCPD_IDS_230911.csv",row.names=FALSE, quote=FALSE)
      
      
      #Subset to HCP-DISC
      hcpdisc <- subset(study3_demos, dataset=="HCP-DISC")
      #Save out .txt
      write.csv(hcpdisc[1],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCPDISC_IDS_230911.csv",row.names=FALSE, quote=FALSE)
      
      #Subset to HCP-REP
      hcprep <- subset(study3_demos, dataset=="HCP-REP")
      #Save out .txt
      write.csv(hcprep[1],"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCPREP_IDS_230911.csv",row.names=FALSE, quote=FALSE)
      
            
#---------------------------SAVE OUT .CSV AI/DEMOS-----------------------------------
#Load ALL HCP data (4 sessions)
      HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
      HCP_ALL_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
      HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
      FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_HCP_ALL_230515.csv")
      DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_HCP_ALL_230515.csv")
      TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_HCP_230515.csv")
      
      #Format data
      names(HCP_DEMOS)[1] <- "SUBJID"
      names(HCP_R_DEMOS)[1] <- "SUBJID"
      names(FD_AVG)[1] <- "SUBJID"
      names(DVARS_AVG)[1] <- "SUBJID"
      names(TOT_VOLS)[1] <- "SUBJID"
      
      FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
      DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
      TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
      TOT_VOLS$Percent_Vols <- (TOT_VOLS$Sum_Volumes / (1196*4))*100 #Number of volumes after skip4 = 1196 x4 runs
      
      #Merge HCP datasets
      comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
      comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
      comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
      HCP_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
      
      #Variable for sex, Males=0
      HCP_df$Sex_Bin <- as.factor(ifelse(HCP_df$Gender == "F", 1, ifelse(HCP_df$Gender == "M", 0, NA)))
      
      #Just include relevant columns
      HCP_df <- HCP_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Sex_Bin", "Handedness")]
      
      #Create sessions marker
      HCP_df$sessions <- "4"
      
      #Organize covariates
      HCP_df$Age_Centered <- as.numeric(HCP_df$Age_in_Yrs - mean(HCP_df$Age_in_Yrs))
      HCP_df$FD_Centered <- as.numeric(HCP_df$FD_avg - mean(HCP_df$FD_avg))
      HCP_df$Handedness <- as.numeric(HCP_df$Handedness)
      
      ##Load AI data
      HCP_AI_ALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
      
      #Drop Network 0 (medial wall)
      HCP_AI_ALL <- subset(HCP_AI_ALL, Network!=0)
      
      #Reorder CBIG Networks
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCP_AI_ALL$NewNetwork <- recode(HCP_AI_ALL$Network, !!!mapping)
      
      #Merge HCP AI with demos
      HCP_AI <- merge(HCP_AI_ALL, HCP_df, by=c("SUBJID"), all=TRUE)
      
      
    #Load HCP-REP dataset: subjects with 1-3 runs of data matched to the HCP232 dataset
      #Load Demos
      HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
      HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
      FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/FD_avg_HCP_REP_230616.csv")
      DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/DVARS_avg_HCP_REP_230616.csv")
      TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/RemainingVols_HCP_REP_230616.csv")
      
      #Format data
      names(HCP_DEMOS)[1] <- "SUBJID"
      names(HCP_R_DEMOS)[1] <- "SUBJID"
      names(FD_AVG)[1] <- "SUBJID"
      names(DVARS_AVG)[1] <- "SUBJID"
      names(TOT_VOLS)[1] <- "SUBJID"
      
      FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
      DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
      TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
      TOT_VOLS$Percent_Vols <- (TOT_VOLS$Sum_Volumes / (1196*4))*100 #Number of volumes after skip4 = 1196 x4 runs
      
      #Merge HCP datasets
      comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
      comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
      comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
      HCPR_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
      
      #Variable for sex, Males=0 
      HCPR_df$Sex_Bin <- as.factor(ifelse(HCPR_df$Gender == "F", 1, ifelse(HCPR_df$Gender == "M", 0, NA)))
      
      #Just include relevant variables
      HCPR_df <- HCPR_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Sex_Bin", "Handedness")]
      
      #Create sessions marker
      HCPR_df$sessions <- "1-3"
      
      #Organize covariates
      HCPR_df$Age_Centered <- as.numeric(HCPR_df$Age_in_Yrs - mean(HCPR_df$Age_in_Yrs))
      HCPR_df$FD_Centered <- as.numeric(HCPR_df$FD_avg - mean(HCPR_df$FD_avg))
      HCPR_df$Handedness <- as.numeric(HCPR_df$Handedness)
      
      #Load in AI
      HCPR_AI_ALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/REPLICATION/avg_ai/MSHBM_LONG_AVG_AI_HCP_REPLICATION_230221.csv")
      
      #Drop Network 0 (medial wall)
      HCPR_AI_ALL <- subset(HCPR_AI_ALL, Network!=0)
      
      #Reorder CBIG Networks
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCPR_AI_ALL$NewNetwork <- recode(HCPR_AI_ALL$Network, !!!mapping)
      
      #Merge HCP AI with demos
      HCPR_AI <- merge(HCPR_AI_ALL, HCPR_df, by=c("SUBJID"), all=FALSE)
      
  #JOIN all HCP    
      #Merge HCP and HCPR dataframes
      HCP_ALL <- merge(HCPR_AI, HCP_AI, by=c("SUBJID", "FD_avg","DVARS_avg", "Percent_Vols", "Sum_Volumes", "Handedness", "Sex_Bin", "Age_in_Yrs", "sessions", "Network", "NewNetwork", "LH_TOT", "RH_TOT", "RH_AVG_AI", "LH_AVG_AI", "Network_AVG_AI", "Age_Centered", "FD_Centered"), all=TRUE)
      
      #Apply exclusion criteria
      #1. Participants with FD_avg greater than 0.2
      HCP_ALL <- subset(HCP_ALL, FD_avg<.2)
      #2. Participants with avg DVARS greater than 50
      HCP_ALL <- subset(HCP_ALL, DVARS_avg<51)
      #3. % volumes remaining is less than 50% 
      HCP_ALL <- subset(HCP_ALL, Percent_Vols>50)
      #4. Subjects missing handedness
      HCP_ALL <- subset(HCP_ALL, Handedness!="NA")
      
      #Split HCP_ALL into discovery and replication sets
      #1. Random assignment
            subjids <- unique(HCP_ALL$SUBJID) #vector of unique IDs
            set.seed(42) #seed is set for reproducibility
            discovery_prop <- 0.5  # Proportion for discovery dataset
            discovery_subjids <- sample(subjids, size = round(length(subjids) * discovery_prop))
            HCP_ALL$dataset[HCP_ALL$SUBJID %in% discovery_subjids] <- "HCP-DISC"
            HCP_ALL$dataset[HCP_ALL$SUBJID %in% setdiff(subjids, discovery_subjids)] <- "HCP-REP"
            
      #2. Check that two datasets match on covariates, using MatchIt
            HCP_ALL$data_bin <- ifelse(HCP_ALL$dataset=="HCP-DISC", 0, 1) #binarize dataset
            #create matchit model (nearest neighbor, propensity scores)
            m.out1 <- matchit(data_bin ~ Age_in_Yrs + FD_avg + Sex_Bin + Handedness + 
                                Percent_Vols, data = HCP_ALL,
                              method = "nearest", distance = "glm")
            summary(m.out1)
            
            #QQ plots
              #Age
              plot(m.out1, type = "qq", interactive = FALSE,
                   which.xs = ~Age_in_Yrs)
              #Handedness
              plot(m.out1, type = "qq", interactive = FALSE,
                   which.xs = ~Handedness)
              #FD_avg
              plot(m.out1, type = "qq", interactive = FALSE,
                   which.xs = ~FD_avg)
              #Percent_Vols
              plot(m.out1, type = "qq", interactive = FALSE,
                   which.xs = ~Percent_Vols)
            
            #visualize covariates
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/Study3_HCPR_Matched_DensityPlots_Covs1_230620.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            plot(m.out1, type = "density", interactive = FALSE,
                 which.xs = ~Age_in_Yrs + Handedness + Percent_Vols)
            dev.off()              # Close device and save as PNG
            
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/Study3_HCPR_Matched_DensityPlots_Covs2_230620.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            plot(m.out1, type = "density", interactive = FALSE,
                 which.xs = ~FD_avg + Sex_Bin)
            dev.off()              # Close device and save as PNG
            
  #Load HCPD
      #Load data descriptor (Box)
      HCPD_data <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_LS_2.0_subject_completeness.xlsx")
      #Load IDs of 583 participants with REST fMRI data post-preproc
      PARC_IDS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/subjids/REST_ids.txt")
      DVARS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/DVARS_avg_HCPD_ALL_230517.csv")
      FD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/FD_avg_HCPD_ALL_230517.csv")
      VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/RemainingVols_HCPD_230517.csv")
      #EHI
      EHI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_EHI_230616.csv")
      EHI <- EHI[,c("subjectkey", "hcp_handedness_score")]
      #merge with HCPD Data
      HCPD_data <- merge(HCPD_data, EHI, by="subjectkey")
      
      #Clean up motion_metrics
      names(DVARS)[1] <- "SUBJID"
      names(FD)[1] <- "SUBJID"
      names(VOLS)[1] <- "SUBJID"
      
      FD$SUBJID <- gsub("^.{0,4}", "", FD$SUBJID) #remove "sub-" string
      DVARS$SUBJID <- gsub("^.{0,4}", "", DVARS$SUBJID) #remove "sub-" string
      VOLS$SUBJID <- gsub("^.{0,4}", "", VOLS$SUBJID) #remove "sub-" string
      
      #Clean up demos dataset
      HCPD_data <- HCPD_data[,c("src_subject_id", "sex", "interview_age", "Full_MR_Compl", "hcp_handedness_score")]
      HCPD_data <- subset(HCPD_data, src_subject_id!="HCA or HCD subject id")
      names(HCPD_data)[1] <- "SUBJID"
      
      #Rename handedness for consistency
      HCPD_data$Handedness <- HCPD_data$hcp_handedness_score
      
      #Age Years (convert from months to years)
      HCPD_data$interview_age <- as.numeric(as.character(HCPD_data$interview_age))
      HCPD_data$Age_in_Yrs <- HCPD_data$interview_age/12
      
      #Create dataset marker
      HCPD_data$dataset <- "HCPD"
      
      #Merge datasets
      HCPD_data <- merge(HCPD_data, PARC_IDS, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, DVARS, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, FD, by =c("SUBJID"), all=FALSE)
      HCPD_data <- merge(HCPD_data, VOLS, by =c("SUBJID"), all=FALSE)
      
      #Exclusions
      #1. Participants with FD_avg greater than 0.2
      HCPD_data <- subset(HCPD_data, FD_avg<.2)
      #2. Participants with avg DVARS greater than 50
      HCPD_data <- subset(HCPD_data, DVARS_avg<51)
      #3. % volumes remaining is less than 50% (474 * 4 = 1896) or 253*6=1518 for ages 5-7
      HCPD_data$Percent_Vols <- ifelse(HCPD_data$Age_in_Yrs < 7, (HCPD_data$Sum_Volumes / (253*6)), (HCPD_data$Sum_Volumes / (474*4)))
      HCPD_data <- subset(HCPD_data, Percent_Vols>.5)
      HCPD_data$Percent_Vols <- HCPD_data$Percent_Vols*100
      #4. Participants missing handedness
      HCPD_data <- subset(HCPD_data, Handedness!="NA")
      
      #dataset indicator
      HCPD_data$dataset <- "HCPD"
      
      #subset to relevant variables
      HCPD_data <- HCPD_data[,c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness")]
      
      #Load HCPD AI
      HCPD_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/ai_spec/REST/avg_ai/MSHBM_LONG_AVG_AI_HCPD_230314.csv")
      
      #Drop Network 0 (medial wall)
      HCPD_AI <- subset(HCPD_AI, Network!=0)
      #Reorder CBIG Networks
      mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
      HCPD_AI$NewNetwork <- recode(HCPD_AI$Network, !!!mapping)
      
      #Merge in AI, demos
      HCPD_AI <- merge(HCPD_data, HCPD_AI, by =c("SUBJID"), all=FALSE)
      
      #Organize covariates
      HCPD_AI$Age_Centered <- as.numeric(HCPD_AI$Age_in_Yrs - mean(HCPD_AI$Age_in_Yrs))
      HCPD_AI$FD_Centered <- as.numeric(HCPD_AI$FD_avg - mean(HCPD_AI$FD_avg))
      HCPD_AI$Sex_Bin <- as.factor(ifelse(HCPD_AI$sex == "M", 0, 1)) #recode males as 0 and Females as 1
      HCPD_AI$Handedness <- as.numeric(HCPD_AI$Handedness)
     
#NSD (demos only)
      #Load datasets
      DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/NSD_analysis/NSD_BIDS/participants.csv")
      FD_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/FD_avg_NSD_REST_230515.csv")
      FD_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/FD_avg_NSD_TASK_230515.csv")
      DVARS_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/DVARS_avg_NSD_REST_230515.csv")
      DVARS_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/DVARS_avg_NSD_TASK_230515.csv")
      TOTAL_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/RemainingVols_NSD_REST_230515.csv")
      TOTAL_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/RemainingVols_NSD_TASK_230515.csv")
      
      #Format data
      names(FD_REST)[1] <- "SUBJID"
      names(FD_REST)[2] <- "avg_FD_REST"
      names(FD_TASK)[1] <- "SUBJID"
      names(FD_TASK)[2] <- "avg_FD_TASK"
      names(DVARS_REST)[1] <- "SUBJID"
      names(DVARS_REST)[2] <- "avg_DVARS_REST"
      names(DVARS_TASK)[1] <- "SUBJID"
      names(DVARS_TASK)[2] <- "avg_DVARS_TASK"
      names(TOTAL_REST)[1] <- "SUBJID"
      names(TOTAL_REST)[2] <- "VOLS_REST"
      names(TOTAL_TASK)[1] <- "SUBJID"
      names(TOTAL_TASK)[2] <- "VOLS_TASK"
      
      #Merge datasets
      comb1 <- merge(DEMOS, FD_REST, by =c("SUBJID"), all=FALSE)
      comb2 <- merge(comb1, FD_TASK, by =c("SUBJID"), all=FALSE)
      comb3 <- merge(comb2, DVARS_REST, by =c("SUBJID"), all=FALSE)
      comb4 <- merge(comb3, DVARS_TASK, by =c("SUBJID"), all=FALSE)
      comb5 <- merge(comb4, TOTAL_REST, by =c("SUBJID"), all=FALSE)
      NSD_df <- merge(comb5, TOTAL_TASK, by =c("SUBJID"), all=FALSE)
      
      #Long format (for plotting)
      NSD_FD <- NSD_df %>%
        pivot_longer(cols = starts_with("avg_FD_"),
                     names_to = "dataset", 
                     values_to = "FD_avg")
      NSD_FD$dataset <- gsub("^.{0,7}", "", NSD_FD$dataset) 
      
      NSD_DVARS <- NSD_df %>%
        pivot_longer(cols = starts_with("avg_DVARS_"),
                     names_to = "dataset", 
                     values_to = "DVARS_avg")
      NSD_DVARS$dataset <- gsub("^.{0,10}", "", NSD_DVARS$dataset) 
      
      NSD_TOT <- NSD_df %>%
        pivot_longer(cols = starts_with("VOLS_"),
                     names_to = "dataset", 
                     values_to = "Sum_Volumes")
      NSD_TOT$dataset <- gsub("^.{0,5}", "", NSD_TOT$dataset) 
      
      #Merge datasets
      comb1 <- merge(NSD_FD, NSD_DVARS, by =c("SUBJID", "dataset"), all=FALSE)
      NSD_df2 <- merge(comb1, NSD_TOT, by =c("SUBJID", "dataset"), all=FALSE)
      
      #Create % vols
      NSD_df2$Percent_Vols <- (NSD_df2$Sum_Volumes / (184*12))*100 #Number of volumes after skip4 = 184 x12 runs (same for Task and Rest)
      
      #Tidy up dataset
      NSD_df2 <- NSD_df2[,c("SUBJID", "dataset", "age", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg")]
      names(NSD_df2)[3] <- "Age_in_Yrs"
 
      #Set variables not available to NA
      NSD_df2$Handedness <- NA
      NSD_df2$Sex_Bin <- NA
      NSD_df2$RH_AVG_AI <- NA
      NSD_df2$LH_AVG_AI <- NA
      NSD_df2$NewNetwork <- 1
      NSD_df2$RH_TOT <- NA
      NSD_df2$LH_TOT <- NA
      NSD_df2$Age_Centered <- NA
      NSD_df2$FD_Centered <- NA
      NSD_df2$Sex_Bin <- NA
      NSD_df2$Network_AVG_AI <- NA
      NSD_df2$Network <- NA
      
         
       
#Merge all datasets
      study3 <- merge(HCP_ALL, HCPD_AI, by=c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Handedness","Sex_Bin", "RH_AVG_AI", "LH_AVG_AI", "NewNetwork", "RH_TOT", "LH_TOT", "Age_Centered", "FD_Centered", "Sex_Bin", "Network_AVG_AI", "Network"), all=TRUE)
      study3 <- merge(study3, NSD_df2, by=c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Handedness","Sex_Bin", "RH_AVG_AI", "LH_AVG_AI", "NewNetwork", "RH_TOT", "LH_TOT", "Age_Centered", "FD_Centered", "Sex_Bin", "Network_AVG_AI", "Network"), all=TRUE)
      
      
#For replication and transparency purposes, write out entire datset used for stats
write.csv(study3,"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/csv_files/study3_HCP_AI_entirety_230620.csv",row.names=FALSE, quote=FALSE)
      