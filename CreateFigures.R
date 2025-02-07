## The use of residual blood specimens in seroprevalence studies for vaccine preventable diseases: A scoping review
## Final Figures


# Import Clean Data
library(readr)
library(tidyverse)
df <- read_csv("data/ResidualScoping_Clean_30Apr2024.csv")

## Color Palettes
library('unikn')  
library(RColorBrewer)
library(NatParksPalettes)
pal_np <- natparks.pals(name="Yellowstone",n=6,type="discrete")
pal_vol <- natparks.pals(name="Volcanoes",n=5,type="discrete")

## Figure 2 - Time Series articles by VPD group and top 6 ####
library(dplyr)
library(ggplot2)

df_tmp1 <- df %>% select(PUBYEAR) %>%
  group_by(PUBYEAR) %>%
  summarise("number of studies" = n())

p2a <- ggplot(data=df_tmp1) +
  geom_col(aes(x=PUBYEAR, y=`number of studies`), fill=as.character(pal_unikn_pref[1])) +
  labs(y="Number of Studies", x="Publication Year") +
  theme_bw() +
  ylim(c(0,120))+
  scale_x_continuous(breaks=c(seq(1990, 2020, 5))) +
  theme (strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) 


df_tmp2 <- df %>% select(PUBYEAR, starts_with("VPD_s_of")) %>%
  group_by(PUBYEAR) %>%
  summarise(`COVID-19` = sum(VPD_s_of_interest_sars_cov_2),
            `HepE` = sum(VPD_s_of_interest_hepatitis_e),
            `HepB` = sum(VPD_s_of_interest_hepatitis_b),
            `Influenza` = sum(VPD_s_of_interest_influenza),
            HPV = sum(VPD_s_of_interest_human_papillom),
            Measles = sum(VPD_s_of_interest_measles)) %>%
  tidyr::complete(PUBYEAR=1990:2022) %>%
  replace(is.na(.), 0) %>%
  tidyr::pivot_longer(-PUBYEAR, names_to="VPD", values_to="number of studies")

p2b <- ggplot(data=df_tmp2) +
  geom_line(aes(x=PUBYEAR, y=`number of studies`, col=`VPD`), size=1.2) + 
  labs(y="Number of Studies", x="Publication Year") +
  theme_bw() +
  theme (legend.position = c(0.2, 0.75),
         legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) +
  scale_color_manual(values = as.character(pal_unikn_pref)[2:7])

p2 <- cowplot::plot_grid(p2a, p2b, labels = c('A', 'B'), label_size = 18)
ggsave(filename="figs/fig2.pdf", p2, width=10, height=5)

## Figure S2 - Time Series articles by original sample and use ####

df_tmp5 <- df %>% select(PUBYEAR, starts_with("POP_")) %>%
  group_by(PUBYEAR) %>%
  summarise(`general population` = sum(POP_GENERAL),
            `blood and plasma donors` = sum(POP_BLOODPLASMA),
            `patient population` = sum(POP_PATIENT),
            `pregnant women` = sum(POP_PREGNANT),
            `occupational populations` = sum(POP_OCCUPATION),
            `vulnerable population` = sum(POP_VULNERABLE),
            students = sum(POP_STUDENTS),
            `drug users` = sum(POP_DRUGUSERS),
            `not specified` = sum(POP_NOTSPECIFIED)) %>%
  tidyr::complete(PUBYEAR=1990:2022) %>%
  replace(is.na(.), 0) %>%
  tidyr::pivot_longer(-PUBYEAR, names_to="original sample population", values_to="number of studies") %>%
  mutate(`original sample population` = factor(`original sample population`, 
                                               levels=c("not specified",
                                                        "vulnerable population", "students",
                                                        "pregnant women", 
                                                        "occupational populations",
                                                        "drug users",
                                                        "general population",
                                                        "patient population",
                                                        "blood and plasma donors")))

p3a <- ggplot(df_tmp5, aes(fill=`original sample population`, y=`number of studies`, x=PUBYEAR)) + 
  geom_bar(position="fill", stat="identity") +
  labs(y="proportion of studies", x="publication year") +
  theme_bw() +
  scale_x_continuous(breaks=c(seq(1990, 2020, 5))) +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text.x = element_text(angle =45, hjust=1),
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values = as.character(pal_unikn_pref))


df_tmp6 <- df %>% select(PUBYEAR, starts_with("Original_use_")) %>%
  group_by(PUBYEAR) %>%
  summarise(`blood or plasma donation` = sum(Original_use_for_samples_blood_d | Original_use_for_samples_plasma_),
            `serosurvey` = sum(Original_use_for_samples_seropre),
            `diagnostic specimens` = sum(Original_use_for_samples_diagnos),
            `other non-serosurvey` = sum(Original_use_for_samples_other_n),
            `other` = sum(Original_use_for_samples_other)) %>%
  tidyr::complete(PUBYEAR=1990:2022) %>%
  replace(is.na(.), 0) %>%
  tidyr::pivot_longer(-PUBYEAR, names_to="original use for specimens", values_to="number of studies") %>%
  mutate(`original use for specimens` = factor(`original use for specimens`, 
                                               levels=c("other",
                                                        "other non-serosurvey",
                                                        "serosurvey",
                                                        "diagnostic specimens",
                                                        "blood or plasma donation")))

df_tmp6a <- df %>% 
  summarise(`blood or plasma donation` = sum(Original_use_for_samples_blood_d | Original_use_for_samples_plasma_),
            `serosurvey` = sum(Original_use_for_samples_seropre),
            `diagnostic specimens` = sum(Original_use_for_samples_diagnos),
            `other non-serosurvey` = sum(Original_use_for_samples_other_n),
            `other` = sum(Original_use_for_samples_other))

p3b <- ggplot(df_tmp6, aes(fill=`original use for specimens`, y=`number of studies`, x=PUBYEAR)) + 
  geom_bar(position="fill", stat="identity") +
  labs(y="proportion of studies", x="publication year") +
  theme_bw() +
  scale_x_continuous(breaks=c(seq(1990, 2020, 5))) +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text.x = element_text(angle =45, hjust=1),
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values =  natparks.pals(name="Yellowstone",n=5,type="discrete"))


p3 <- cowplot::plot_grid(p3a, p3b, labels = c('A', 'B'), label_size = 18, nrow=2)
ggsave(filename="figs/fig.s2.pdf", p3, width=10, height=10)



## Figure 5 - Map #####
library(dplyr)
library(tidyverse)
library(janitor)
library(countrycode)
library(sf)

df_tmp3 <- 
  df %>%
  janitor::clean_names() %>%
  select(covidence_number,
         starts_with ("study_country_s")) %>%
  select(-c(study_country_s_001_yes, study_country_s_001))

df_tmp3_long <- df_tmp3 %>% 
  tidyr::pivot_longer(names_to = "NAME", #converting into a long table to simplify analysis
                      values_to = "counts",
                      cols = starts_with("study_country_s")) %>% 
  filter(counts == 1) %>%  #filtering for values 1, removing studies with country=0
  distinct() %>%
  mutate(NAME = str_remove(NAME, "study_country_s_001_")) %>% #removing first part of the countries variable names
  mutate(NAME = str_replace(NAME, "_", " ")) %>%
  mutate(NAME = str_replace(NAME, "_", " ")) %>%
  mutate(NAME = str_replace(NAME, "_", " ")) %>%
  group_by(NAME) %>% 
  summarise(`number of studies` = sum(counts)) %>% #counting number of studies per country
  ungroup()

#checking iso2c - they are all country names
df_tmp3_long$NAME <- countrycode(df_tmp3_long$NAME, origin="iso2c", destination="iso3c")

# pulling in the shapefile
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()

# Remove Antarctica and Greenland
world <- world[world$adm0_a3 != "ATA",]
world <- world[world$adm0_a3 != "GRL",]

# joining both dataframe based on country name
world <- dplyr::left_join(world, df_tmp3_long, by = c("adm0_a3"="NAME"))

## plot it
p4 <- ggplot(data = world) +
  geom_sf(aes(fill = `number of studies`)) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", na.value="lightgrey") +
  ggtitle("Published studies by country (1990-2022)") +
  theme_minimal() 
ggsave(filename="figs/fig5.pdf", p4, width=10, height=8)

# stats
tmp_world <- world %>% select(`number of studies`, iso_a3) %>% arrange(`number of studies`)
max(tmp_world$`number of studies`, na.rm=T) ## Number of US articles
max(tmp_world$`number of studies`, na.rm=T)/nrow(df)
tmp_world_group <- world %>% as_tibble() %>% select(`number of studies`, iso_a3, income_grp) %>% 
  dplyr::group_by(income_grp) %>% dplyr::summarize(tot = sum(`number of studies`, na.rm=T))
sum(tmp_world_group$tot[3:5]) #Number of LMIC articles
sum(tmp_world_group$tot[3:5])/nrow(df)

## Figure S3 - Histogram of time between collection and publication year ####

mean_year <- round(sum(tmp2$DIFF_COLLECTION_PUB[1:38]*tmp2$n[1:38]/sum(tmp2$n[1:38])),2) #mean year
ps4a <- ggplot(data=df) +
  geom_histogram(aes(x=DIFF_COLLECTION_PUB)) +
  labs(y="number of studies", x="difference in publication year and \nyear of original sample collection") +
  theme_bw() +
  theme (strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold"),
         axis.title.y = element_text(vjust=+1),
         axis.title.x = element_text(vjust=-2, margin = margin(t = 0, r = 0, b = 5, l = 0))) +
  geom_vline(xintercept=mean_year, color="red") +
  geom_text(x=mean_year, y=Inf, label=round(mean_year, 1), 
            hjust=-0.2, vjust=1.5, color="red", size=4) 

# Sample size s3
df %>% count(DIFF_COLLECTION_PUB) -> tmp2
samplesize <- nrow(df) -  as.numeric(tmp2[nrow(tmp2),"n"]) 

# Stats in the supplemental text
sum(tmp2$n[1:10])/samplesize #number articles with data collection within 10 years
sum(tmp2$n[1:20])/samplesize #number articles with data collection within 20 years

# Get plot by VPD
df_tmp_s4 <- df %>%
  select(contains("DIFF_COLLECTION_PUB"), "VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                "VPD_s_of_interest_measles") %>%
  pivot_longer(-DIFF_COLLECTION_PUB, names_to="VPD") %>%
  filter(value==1) %>% 
  select(-value) %>% 
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) 
df_tmp_s4_mean <- df_tmp_s4 %>%
  group_by(VPD) %>%
  summarise(mean=round(mean(DIFF_COLLECTION_PUB, na.rm=TRUE),2)) %>%
  arrange(mean)

ps4b <- ggplot(data=df_tmp_s4) +
  geom_histogram(aes(x=DIFF_COLLECTION_PUB)) +
  labs(y="number of studies", x="difference in publication year and \nyear of original sample collection") +
  theme_bw() +
  theme (strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold"),
         axis.title.y = element_text(vjust=+1),
         axis.title.x = element_text(vjust=-2, margin = margin(t = 0, r = 0, b = 5, l = 0))) +
  geom_vline(data=df_tmp_s4_mean, aes(xintercept=mean), color="red") +
  geom_text(data=df_tmp_s4_mean, 
            aes(x=mean, y=Inf, label=round(mean, 1)), 
            hjust=-0.2, vjust=1.5, color="red", size=4) + 
  facet_wrap(.~VPD)


ps4 <- cowplot::plot_grid(ps4a, ps4b, labels = c('A', 'B'), label_size = 18, nrow=2)
ggsave(filename="figs/fig.s3.pdf", ps4, width=8, height=10)



## Figure 3 - Bar plot of objective by top six VPDs ####

df %>% select(contains("Paper_objective_to")) %>%
  pivot_longer(contains("Paper_objective_to"), names_to="objective") %>%
  filter(value==1) %>% 
  mutate(objective = case_when(objective=="Paper_objective_to_describe_popu" ~ "Describe population seropositivity",
                               objective=="Paper_objective_to_describe_sero" ~ "Describe seroprevalence among a clinical subpopulation",
                               objective=="Paper_objective_to_evaluate_the_" ~ "Evaluate the need for blood donor screening",
                               objective=="Paper_objective_to_describe_anti" ~ "Describe antibody kinetics following vaccination",
                               objective=="Paper_objective_to_describe_ant1" ~ "Describe antibody kinetics following infection",
                               objective=="Paper_objective_to_estimate_infe" ~ "Estimate infection rates or understand transmission dynamics",
                               objective=="Paper_objective_to_compare_the_u" ~ "Compare the use of residual samples to population-based samples",
                               objective=="Paper_objective_to_identify_risk" ~ "Identify risk factors of seropositivity",
                               objective=="Paper_objective_to_evaluate_outc" ~ "Evaluate outcomes of seropositivity",
                               objective=="Paper_objective_to_evaluate_chan" ~ "Evaluate changes in seropositivity over time",
                               objective=="Paper_objective_to_describe_cros" ~ "Evaluate antibody cross-reactivity",
                               objective=="Paper_objective_to_describe_vira" ~ "Describe viral dynamics",
                               objective=="Paper_objective_to_compare_popul" ~ "Compare population seropositivity between countries or international regions")) %>%
  count(objective) %>% arrange(n)


df_tot_by_VPD <- df %>% select("VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                         "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                         "VPD_s_of_interest_measles") %>%
  pivot_longer(everything(), names_to="VPD") %>%
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) %>%
  group_by(VPD) %>%
  summarise(tot = sum(value))

df_tmp7 <- df %>% select(contains("Paper_objective_to"), "VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                         "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                         "VPD_s_of_interest_measles") %>%
  pivot_longer(contains("Paper_objective_to"), names_to="objective") %>%
  filter(value==1) %>% 
  select(-value) %>%
  pivot_longer(-objective, names_to="VPD") %>%
  filter(value==1) %>% 
  select(-value) %>% 
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) %>%
  mutate(objective = case_when(objective=="Paper_objective_to_describe_popu" ~ "Describe\npopulation\nseroprevalence",
                               objective=="Paper_objective_to_describe_sero" ~ "Describe\nseroprevalence\namong a\nclinical\nsubpopulation",
                               objective=="Paper_objective_to_evaluate_the_" ~ "Evaluate\nthe need\nfor blood\ndonor\nscreening",
                               objective=="Paper_objective_to_describe_anti" ~ "Describe\nantibody\nkinetics\nfollowing\nvaccination",
                               objective=="Paper_objective_to_describe_ant1" ~ "Describe\nantibody\nkinetics\nfollowing\ninfection",
                               objective=="Paper_objective_to_estimate_infe" ~ "Estimate\ninfection\nrates or\nunderstand\ntransmission\ndynamics",
                               objective=="Paper_objective_to_compare_the_u" ~ "Compare\nuse of\nresidual\nsamples to\npopulation-\nbased\nsamples",
                               objective=="Paper_objective_to_identify_risk" ~ "Identify\nrisk\nfactors\nof\nseropositivity",
                               objective=="Paper_objective_to_evaluate_outc" ~ "Evaluate\noutcomes\nof\nseropositivity",
                               objective=="Paper_objective_to_evaluate_chan" ~ "Evaluate\nchanges in\nseroprevalence\nover time",
                               objective=="Paper_objective_to_describe_cros" ~ "Evaluate\nantibody\ncross-\nreactivity",
                               objective=="Paper_objective_to_describe_vira" ~ "Describe\nviral\ndynamics",
                               objective=="Paper_objective_to_compare_popul" ~ "Compare\npopulation\nseroprevalence\nb/w countries\nor regions")) %>%
  count(VPD, objective) %>% 
  complete(VPD, objective) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(objective = reorder(factor(objective), n, decreasing=T)) %>%
  left_join(df_tot_by_VPD, by="VPD") %>%
  group_by(VPD) %>%
  mutate(prop = n / tot*100)


df_tmp7_all <- df %>% select(contains("Paper_objective_to")) %>%
  pivot_longer(contains("Paper_objective_to"), names_to="objective") %>%
  filter(value==1) %>% 
  select(-value) %>%
  mutate(objective = case_when(objective=="Paper_objective_to_describe_popu" ~ "Describe\npopulation\nseroprevalence",
                               objective=="Paper_objective_to_describe_sero" ~ "Describe\nseroprevalence\namong a\nclinical\nsubpopulation",
                               objective=="Paper_objective_to_evaluate_the_" ~ "Evaluate\nthe need\nfor blood\ndonor\nscreening",
                               objective=="Paper_objective_to_describe_anti" ~ "Describe\nantibody\nkinetics\nfollowing\nvaccination",
                               objective=="Paper_objective_to_describe_ant1" ~ "Describe\nantibody\nkinetics\nfollowing\ninfection",
                               objective=="Paper_objective_to_estimate_infe" ~ "Estimate\ninfection\nrates or\nunderstand\ntransmission\ndynamics",
                               objective=="Paper_objective_to_compare_the_u" ~ "Compare\nuse of\nresidual\nsamples to\npopulation-\nbased\nsamples",
                               objective=="Paper_objective_to_identify_risk" ~ "Identify\nrisk\nfactors\nof\nseropositivity",
                               objective=="Paper_objective_to_evaluate_outc" ~ "Evaluate\noutcomes\nof\nseropositivity",
                               objective=="Paper_objective_to_evaluate_chan" ~ "Evaluate\nchanges in\nseroprevalence\nover time",
                               objective=="Paper_objective_to_describe_cros" ~ "Evaluate\nantibody\ncross-\nreactivity",
                               objective=="Paper_objective_to_describe_vira" ~ "Describe\nviral\ndynamics",
                               objective=="Paper_objective_to_compare_popul" ~ "Compare\npopulation\nseroprevalence\nb/w countries\nor regions")) %>%
  count(objective) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(objective = reorder(factor(objective), n, decreasing=T)) %>%
  mutate(prop = n / nrow(df)*100) %>%
  mutate(VPD = "All") %>%
  select(VPD, objective, n, prop)

all_text <- df_tmp7_all %>% filter(prop>5.2 | objective=="Evaluate the need for blood \ndonor screening")
lim_text <- df_tmp7[c(19,38,50,61),]

df_tmp7 <- bind_rows(df_tmp7, df_tmp7_all)


ps2 <- ggplot(df_tmp7, aes(fill=VPD, y=prop, x=objective)) + 
  geom_bar(width=0.8, position=position_dodge(), stat="identity") +
  labs(y="Percent of Studies\n(for all and for top 6 VPDs)", x="Objective") +
  theme_bw() +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text.x = element_text(size=10),
         #axis.text.x = element_text(size=10, angle =35, hjust=1.1, vjust=1),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values = as.character(pal_unikn_pref))
  #ylim(c(0,85)) +
  #geom_text(data=all_text, y=82, aes(label=paste0(round(prop,1),"%")), color="#59C7EB", size=5)+
  #geom_text(data=lim_text[1,], y=78, aes(label=paste0(round(prop,1),"%")), color=pal_unikn_pref[3],size=5) +
  #geom_text(data=lim_text[2,], y=78, aes(label=paste0(round(prop,1),"%")), color=pal_unikn_pref[4],size=5) +
  #geom_text(data=lim_text[3,], y=78, aes(label=paste0(round(prop,1),"%")), color=pal_unikn_pref[5],size=5) +
  #geom_text(data=lim_text[4,], y=78, aes(label=paste0(round(prop,1),"%")), color=pal_unikn_pref[6],size=5)

ggsave(filename="figs/fig3.pdf", ps2, width=15, height=6)



## Figure S1 - Bar plot of sample population by top six VPDs ####
df %>% select(starts_with("POP_")) %>%
  pivot_longer(starts_with("POP_"), names_to="orig_sample") %>%
  filter(value==1) %>% 
  mutate(orig_sample = case_when(orig_sample=="POP_GENERAL" ~ "general population",
                                 orig_sample=="POP_BLOODPLASMA" ~ "blood and plasma donors",
                                 orig_sample=="POP_PATIENT" ~ "patient population",
                                 orig_sample=="POP_PREGNANT" ~ "pregnant women",
                                 orig_sample=="POP_OCCUPATION" ~ "occupational populations",
                                 orig_sample=="POP_VULNERABLE" ~ "vulnerable population",
                                 orig_sample=="POP_STUDENTS" ~ "students",
                                 orig_sample=="POP_DRUGUSERS" ~ "drug users",
                                 orig_sample=="POP_NOTSPECIFIED" ~ "not specified")) %>%
  count(orig_sample) %>% arrange(n)


df_tmp8 <- df %>% select(starts_with("POP_"), "VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                         "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                         "VPD_s_of_interest_measles") %>%
  pivot_longer(starts_with("POP_"), names_to="orig_sample") %>%
  filter(value==1) %>% 
  select(-value) %>%
  pivot_longer(-orig_sample, names_to="VPD") %>%
  filter(value==1) %>% 
  select(-value) %>% 
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) %>%
  mutate(orig_sample = case_when(orig_sample=="POP_GENERAL" ~ "general population",
                                 orig_sample=="POP_BLOODPLASMA" ~ "blood and plasma donors",
                                 orig_sample=="POP_PATIENT" ~ "patient population",
                                 orig_sample=="POP_PREGNANT" ~ "pregnant women",
                                 orig_sample=="POP_OCCUPATION" ~ "occupational populations",
                                 orig_sample=="POP_VULNERABLE" ~ "vulnerable population",
                                 orig_sample=="POP_STUDENTS" ~ "students",
                                 orig_sample=="POP_DRUGUSERS" ~ "drug users",
                                 orig_sample=="POP_NOTSPECIFIED" ~ "not specified")) %>%
  count(VPD, orig_sample) %>% 
  complete(VPD, orig_sample) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(orig_sample = reorder(factor(orig_sample), n, decreasing=T)) %>%
  left_join(df_tot_by_VPD, by="VPD") %>%
  group_by(VPD) %>%
  mutate(prop = n / tot*100)


df_tmp8_all <- df %>% select(starts_with("POP_")) %>%
  pivot_longer(starts_with("POP_"), names_to="orig_sample") %>%
  filter(value==1) %>% 
  select(-value) %>%
  mutate(orig_sample = case_when(orig_sample=="POP_GENERAL" ~ "general population",
                                 orig_sample=="POP_BLOODPLASMA" ~ "blood and plasma donors",
                                 orig_sample=="POP_PATIENT" ~ "patient population",
                                 orig_sample=="POP_PREGNANT" ~ "pregnant women",
                                 orig_sample=="POP_OCCUPATION" ~ "occupational populations",
                                 orig_sample=="POP_VULNERABLE" ~ "vulnerable population",
                                 orig_sample=="POP_STUDENTS" ~ "students",
                                 orig_sample=="POP_DRUGUSERS" ~ "drug users",
                                 orig_sample=="POP_NOTSPECIFIED" ~ "not specified")) %>%
  count(orig_sample) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(orig_sample = reorder(factor(orig_sample), n, decreasing=T)) %>%
  mutate(prop = n / nrow(df)*100) %>%
  mutate(VPD = "All") %>%
  select(VPD, orig_sample, n, prop)

df_tmp8 <- bind_rows(df_tmp8, df_tmp8_all)

ps3 <- ggplot(df_tmp8, aes(fill=VPD, y=prop, x=orig_sample)) + 
  geom_bar(width=0.8, position=position_dodge(), stat="identity") +
  labs(y="Percent of Studies\n(for all and for top 6 VPDs)", x="Original Sample Population") +
  theme_bw() +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         axis.text.x = element_text(angle =90, hjust=1, vjust=0.5),
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values = as.character(pal_unikn_pref)) 

ggsave(filename="figs/fig.s1.pdf", ps3, width=12, height=8)






## Figure 4 - Bar plot of original use of samples by top six VPDs ####

df %>% select(starts_with("Original_use_for_samples_")) %>%
  pivot_longer(starts_with("Original_use_for_samples_"), names_to="orig_use") %>%
  filter(value==1) %>% 
  mutate(orig_use = case_when(orig_use=="Original_use_for_samples_blood_d" ~ "Blood and\nPlasma Donations",
                                 orig_use=="Original_use_for_samples_plasma_" ~ "Blood and\nPlasma Donations",
                                 orig_use=="Original_use_for_samples_seropre" ~ "Serosurvey",
                                 orig_use=="Original_use_for_samples_diagnos" ~ "Diagnostic Specimens",
                                 orig_use=="Original_use_for_samples_other_n" ~ "Non-Serological Surveys",
                                 orig_use=="Original_use_for_samples_other" ~ "Other")) %>%
  count(orig_use) %>% arrange(n)


df_tmp8 <- df %>% select(starts_with("Original_use_for_samples_"), "VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                         "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                         "VPD_s_of_interest_measles") %>%
  pivot_longer(starts_with("Original_use_for_samples_"), names_to="orig_use") %>%
  filter(value==1) %>% 
  select(-value) %>%
  pivot_longer(-orig_use, names_to="VPD") %>%
  filter(value==1) %>% 
  select(-value) %>% 
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) %>%
  mutate(orig_use = case_when(orig_use=="Original_use_for_samples_blood_d" ~ "Blood and\nPlasma Donations",
                              orig_use=="Original_use_for_samples_plasma_" ~ "Blood and\nPlasma Donations",
                              orig_use=="Original_use_for_samples_seropre" ~ "Serosurvey",
                              orig_use=="Original_use_for_samples_diagnos" ~ "Diagnostic Specimens",
                              orig_use=="Original_use_for_samples_other_n" ~ "Non-Serological Surveys",
                              orig_use=="Original_use_for_samples_other" ~ "Other")) %>%
  count(VPD, orig_use) %>% 
  complete(VPD, orig_use) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(orig_use = reorder(factor(orig_use), n, decreasing=T)) %>%
  left_join(df_tot_by_VPD, by="VPD") %>%
  group_by(VPD) %>%
  mutate(prop = n / tot*100)


df_tmp8_all <- df %>% select(starts_with("Original_use_for_samples_")) %>%
  pivot_longer(starts_with("Original_use_for_samples_"), names_to="orig_use") %>%
  filter(value==1) %>% 
  select(-value) %>%
  mutate(orig_use = case_when(orig_use=="Original_use_for_samples_blood_d" ~ "Blood and\nPlasma Donations",
                              orig_use=="Original_use_for_samples_plasma_" ~ "Blood and\nPlasma Donations",
                              orig_use=="Original_use_for_samples_seropre" ~ "Serosurvey",
                              orig_use=="Original_use_for_samples_diagnos" ~ "Diagnostic Specimens",
                              orig_use=="Original_use_for_samples_other_n" ~ "Non-Serological Surveys",
                              orig_use=="Original_use_for_samples_other" ~ "Other")) %>%
  count(orig_use) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(orig_sample = reorder(factor(orig_use), n, decreasing=T)) %>%
  mutate(prop = n / nrow(df)*100) %>%
  mutate(VPD = "All") %>%
  select(VPD, orig_use, n, prop)

df_tmp8 <- bind_rows(df_tmp8, df_tmp8_all) %>%
  mutate(orig_use = factor(orig_use, levels=c("Diagnostic Specimens", "Blood and\nPlasma Donations","Serosurvey",
                                              "Non-Serological Surveys","Other")))

ps3 <- ggplot(df_tmp8, aes(fill=VPD, y=prop, x=orig_use)) + 
  geom_bar(width=0.8, position=position_dodge(), stat="identity") +
  labs(y="Percent of Studies\n(for all and for top 6 VPDs)", x="Original Use for Specimens") +
  theme_bw() +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         #axis.text.x = element_text(angle =45, hjust=1, vjust=1),
         axis.text.x = element_text(size =10),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values = as.character(pal_unikn_pref)) 

ggsave(filename="figs/fig4.pdf", ps3, width=10, height=4)



## Figure 6 - Bar plot of bias for top six VPDs ####

df %>% select(-BIAS_RETEST) %>%
  select(starts_with("BIAS_"), "NOTBIASED") %>%
  pivot_longer(everything(), names_to="bias") %>%
  filter(value==1) %>% 
  mutate(bias = case_when(bias=="BIAS_SUBSAMPLING" ~ "Conducted\nstratified\nsubsampling",
                          bias=="BIAS_INCEXC" ~ "Used inclusion\nor exclusion\ncriteria for\nresidual samples",
                          bias=="BIAS_STRATANALY" ~ "Conducted\nstratified\nanalysis",
                          bias=="BIAS_WEIGHTED" ~ "Weighted\nresults",
                          bias=="BIAS_COMPVALID" ~ "Compared to\nalternative\ndata source\nfor “validity",
                          bias=="BIAS_COMPPUB" ~ "Compared to\nother published\nestimates",
                          #bias=="BIAS_RETEST" ~ "Tested samples multiple times \nfor one pathogen",
                          bias=="BIAS_SENS" ~ "Conducted\nsensitivity\nanalysis",
                          bias=="BIAS_NONE" ~ "None of\nthe above",
                          bias=="NOTBIASED" ~ "Not biased")) %>%
  count(bias) %>% arrange(n)


df_tmp9 <- df %>% 
  select(-BIAS_RETEST) %>%
  select(starts_with("BIAS_"), "NOTBIASED", "VPD_s_of_interest_sars_cov_2", "VPD_s_of_interest_hepatitis_e",
                         "VPD_s_of_interest_hepatitis_b", "VPD_s_of_interest_influenza", "VPD_s_of_interest_human_papillom",
                         "VPD_s_of_interest_measles") %>%
  pivot_longer(c(starts_with("BIAS_"), "NOTBIASED"), names_to="bias") %>%
  filter(value==1) %>% 
  select(-value) %>%
  pivot_longer(-bias, names_to="VPD") %>%
  filter(value==1) %>% 
  select(-value) %>% 
  mutate(VPD = case_when(VPD=="VPD_s_of_interest_sars_cov_2" ~ "COVID-19",
                         VPD=="VPD_s_of_interest_hepatitis_e" ~ "HepE",
                         VPD=="VPD_s_of_interest_hepatitis_b" ~ "HepB",
                         VPD=="VPD_s_of_interest_influenza" ~ "Influenza",
                         VPD=="VPD_s_of_interest_human_papillom" ~ "HPV",
                         VPD=="VPD_s_of_interest_measles" ~ "Measles")) %>%
  mutate(bias = case_when(bias=="BIAS_SUBSAMPLING" ~ "Conducted\nstratified\nsubsampling",
                          bias=="BIAS_INCEXC" ~ "Used inclusion\nor exclusion\ncriteria for\nresidual samples",
                          bias=="BIAS_STRATANALY" ~ "Conducted\nstratified\nanalysis",
                          bias=="BIAS_WEIGHTED" ~ "Weighted\nresults",
                          bias=="BIAS_COMPVALID" ~ "Compared to\nalternative\ndata source\nfor “validity",
                          bias=="BIAS_COMPPUB" ~ "Compared to\nother published\nestimates",
                          #bias=="BIAS_RETEST" ~ "Tested samples multiple times \nfor one pathogen",
                          bias=="BIAS_SENS" ~ "Conducted\nsensitivity\nanalysis",
                          bias=="BIAS_NONE" ~ "None of\nthe above",
                          bias=="NOTBIASED" ~ "Not biased")) %>%
  count(VPD, bias) %>% 
  complete(VPD, bias) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(bias = reorder(factor(bias), n, decreasing=T)) %>%
  left_join(df_tot_by_VPD, by="VPD") %>%
  group_by(VPD) %>%
  mutate(prop = n / tot*100)

df_tmp9_all <- df %>% 
  select(-BIAS_RETEST) %>%
  select(starts_with("BIAS_"), "NOTBIASED") %>%
  pivot_longer(c(starts_with("BIAS_"), "NOTBIASED"), names_to="bias") %>%
  filter(value==1) %>% 
  select(-value) %>%
  mutate(bias = case_when(bias=="BIAS_SUBSAMPLING" ~ "Conducted\nstratified\nsubsampling",
                          bias=="BIAS_INCEXC" ~ "Used inclusion\nor exclusion\ncriteria for\nresidual samples",
                          bias=="BIAS_STRATANALY" ~ "Conducted\nstratified\nanalysis",
                          bias=="BIAS_WEIGHTED" ~ "Weighted\nresults",
                          bias=="BIAS_COMPVALID" ~ "Compared to\nalternative\ndata source\nfor “validity",
                          bias=="BIAS_COMPPUB" ~ "Compared to\nother published\nestimates",
                          #bias=="BIAS_RETEST" ~ "Tested samples multiple times \nfor one pathogen",
                          bias=="BIAS_SENS" ~ "Conducted\nsensitivity\nanalysis",
                          bias=="BIAS_NONE" ~ "None of\nthe above",
                          bias=="NOTBIASED" ~ "Not biased")) %>%
  count(bias) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(bias = reorder(factor(bias), n, decreasing=T)) %>%
  mutate(prop = n / nrow(df)*100) %>% 
  mutate(VPD = "All") %>%
  select(VPD, bias, n, prop)

df_tmp9 <- bind_rows(df_tmp9, df_tmp9_all)


ps5 <- ggplot(df_tmp9, aes(fill=VPD, y=prop, x=bias)) + 
  geom_bar(width=0.5, position=position_dodge(), stat="identity") +
  labs(y="Percent of Studies\n(for all and for top 6 VPDs)", x="How Addressed or Handled Bias") +
  theme_bw() +
  theme (legend.title = element_text(face="bold", size=12),
         legend.text = element_text(size=12),
         #legend.background = element_rect(colour ="black"),
         strip.background = element_rect(fill=NA),
         text=element_text(size=12, color="black"), 
         #axis.text.x = element_text(angle =45, hjust=1, vjust=1),
         axis.text=element_text(size=12, color="black"),
         axis.title=element_text(size=13, face="bold")) +
  scale_fill_manual(values = as.character(pal_unikn_pref))

ggsave(filename="figs/fig6.pdf", ps5, width=12, height=6)



## Discussion Text ####
#Metadata
df.meta <- df %>%
  mutate(basic = ifelse(META_BASICDEMOG==1 & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 & META_EXTENDED==0 | META_NONE==1, 1, 0),
         basic_or_ext = ifelse(META_BASICDEMOG==1 & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 | META_EXTENDED==1  & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 | META_NONE==1, 1, 0),
         ext = ifelse(META_EXTENDED==1, 1, 0),
         epi = ifelse(META_EPIDATA_NONVPD==1 | META_EPIDATA_VPD==1, 1, 0))
df.meta %>% count(basic)/601
df.meta %>% count(ext)/601
df.meta %>% count(epi)/601

df.meta.obj <- df %>% filter(Original_use_for_samples_seropre==1) %>%
  mutate(basic = ifelse(META_BASICDEMOG==1 & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 & META_EXTENDED==0 | META_NONE==1, 1, 0),
         basic_or_ext = ifelse(META_BASICDEMOG==1 & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 | META_EXTENDED==1  & META_EPIDATA_NONVPD==0 & META_EPIDATA_VPD==0 | META_NONE==1, 1, 0),
         ext = ifelse(META_EXTENDED==1, 1, 0),
         epi = ifelse(META_EPIDATA_NONVPD==1 | META_EPIDATA_VPD==1, 1, 0))
  
df.meta.obj %>% count(basic)/65
df.meta.obj %>% count(ext)/65
df.meta.obj %>% count(epi)/65



     