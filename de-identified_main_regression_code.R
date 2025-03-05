##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

###################### Paper Title
###################### Locally Ensconced and Globally Integrated: How Network Cohesion and Range Relate to a Language-Based Model of Organizational Identification.
###################### Authors: Lara Yang, Sarayu Anshuman, Amir Goldberg, Sameer Srivastava

#This file contains the code for the main regression models in the paper.
###############################
#Note: Only the regressions with the 'within-person' specification can be replicated using the de-idenitified data, not for the 'between-person' specification, 
#      as we have removed all identifying data to follow the data agreements.
###############################
# The code below contains the following steps -
# 1. Set the directory path and load the libraries.
# 2. Read the final processed file.
# 3. Within person models (code for between person shared, but it does not run as identifying data is needed to run this)
# 4. Robustness checks code

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# 1. set the directory path and load the libraries

#set path
print(getwd())
setwd("/Users/sarayuvyakaranam/Desktop/identification_final_analysis_codes/de-identified_data_and_code")
print(getwd())

#install.packages("ggthemes")

#rm(list=ls())
library(lfe)
library(psych)
library(corrplot)
library(DescTools)
# slide
library(DataCombine)
# bin
library(OneR)
library(stargazer)
library(margins)
library(sjPlot)
library(ggplot2)
library(dplyr)
library(xtable)  # Load the package

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# 2. Read the final processed file.
df_network = read.csv('de-identified_data_files/final_de-identified_data.csv')
names(df_network)
####checking count start
nrow(df_network)
ncol(df_network)
####checking count end
names(df_network)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#3. Within person models (code for between-person shown below, but it does not run as identifying data is needed to run the model)
df_network$company <- as.factor(df_network$company)
df_network$quarter <- as.factor(df_network$quarter)

#these models include Global Reach that is not logged and also include num_tokens, and the standard errors are clustered at the person level 
within_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
  )

######################################### TABLE 6 from the paper #########################################
stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Within-Person", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Local Clustering X Global Reach"),type = "text")

#Between person models code 
# (Note: the following code will not run for the shared de-identified data as demographic information like gender is needed to run the below models).

between_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

######################################### TABLE 5 from the paper #########################################
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Between-Person", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Local Clustering X Global Reach"),type = "text")

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# 4. Robustness checks code
# (Note: the following code will not run for the shared de-identified data as demographic information like gender is needed to run the below models).

df_network = df_network[order(df_network$user_id, df_network$quarter),]
df_network = df_network[, c("user_id", "quarter", "i_we", "log_tenure", "num_tokens", "company", "gender", "department", "job_title", "manager", "we_i_ratio", "num_we_count", "num_i_count", "unweighted_degree", "past_unweighted_degree",  "weighted_clustering", "past_weighted_clustering", 'comm_diversity_all_neighbors_unweighted', 'past_comm_diversity_all_neighbors_unweighted', 'vanilla_cf')]
names(df_network) = c("user_id", "quarter", "i_we", "log_tenure", "log_num_tokens", "company", "gender", "department", "job_title", "manager", "log_we_i", "log_num_we", "log_num_i", "log_network_size", "past_log_network_size",  "log_clustering", "past_log_clustering", 'global_reach', 'past_global_reach', 'vanilla_cf')
df_network = df_network[!(is.na(df_network$log_tenure)) & !(is.na(df_network$department)) & !(is.na(df_network$i_we)),]

####checking count start
nrow(df_network) #10971
ncol(df_network)
####checking count end

sum(is.na(df_network$vanilla_cf)) #1
na_rows <- which(is.na(df_network$vanilla_cf))
print(df_network[na_rows, ])

#now running the models. These models are Tables A1 through A8 in the appendix

within_manager_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',])
)

within_we_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

within_ratio_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

between_manager_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Company_2',])
)

between_we_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

between_ratio_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

within_cf_models = list(
  felm(i_we ~ log_tenure + log_num_tokens + vanilla_cf | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network))

between_cf_models = list(
  felm(i_we ~ log_tenure + log_num_tokens + vanilla_cf | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

#latex output with merging output tables
######################################### TABLE A1 from the paper #########################################
#We controls
stargazer(between_we_controls,within_we_controls, digits=3, header=F, star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", 
                                                          "Standard errors clustered at the person level", "Tenure, Number of Tokens, 
                                                          Network Size, Number of We-Words, and Local Clustering are logged due to right skewed distributions"), 
          notes.append=F, title="Identification on Local Clustering and Global Reach Controlling for Number of We-Words: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                           c("Person Controls", "No","No","No","Yes","Yes","Yes"),
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Number of We-Words ", "Local Clustering", "Global Reach"))

######################################### TABLE A2 from the paper #########################################
#Ratio of We to I control
stargazer(between_ratio_controls,within_ratio_controls, digits=3, header=F, star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, Ratio of Number of We-Words to I-Words, and Local Clustering are logged due to right skewed distributions"), 
          notes.append=F, title="Identification on Local Clustering and Global Reach Controlling for Ratio of Number of We-Words to Number of I-Words, 
          Between-Person and Within-Person Models", add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                                           c("Person Controls", "No","No","No","Yes","Yes","Yes"),
                                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", 
                             "Ratio of Number of We-Words to Number of I-Words", 
                             "Local Clustering", "Global Reach"))

######################################### TABLE A3 from the paper #########################################
#Managerial Status
stargazer(between_manager_controls,within_manager_controls, digits=3, 
          header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", 
                    "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local 
                    Clustering are logged due to right skewed distributions"), 
          notes.append=F, 
          title="Identification on Local Clustering and Global Reach Controlling for Managerial Status: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                           c("Person Controls", "No","No","No","Yes","Yes","Yes"), 
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Manager Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")),
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"))

######################################### TABLE A4 from the paper #########################################
#Linguistic Fit Models
stargazer(between_cf_models,within_cf_models, digits=3, header=F, 
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", 
                    "Tenure, Number of Tokens, Network Size, and Local Clustering are logged"), 
          notes.append=F, title="Identification on Linguistic Fit: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","Yes","No","No","No","No"), 
                           c("Person Controls", "No","No","No","No","Yes","Yes","Yes","Yes"), 
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Linguistic Fit", "Local Clustering", "Global Reach"))

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################