source('/data/projects/project_mferro/project_migraine/file_paths.R')

Beta_F1 <-fread(file='/home/ivm/project_migraine/output/list_ENDP_F1.csv')
Beta_F2 <-fread(file='/home/ivm/project_migraine/output/list_ENDP_F2.csv')
Beta_F3 <-fread(file='/home/ivm/project_migraine/output/list_ENDP_F3.csv')

ENDP_lists<- fread("/home/ivm/project_migraine/data/ENDP_MAPPING.csv")
ENDP_lists$HD_ICD_10 <- gsub("\\[|\\]|\\$|%|!", "", ENDP_lists$HD_ICD_10)

ENDP_lists <- ENDP_lists %>% mutate(CAT=ifelse(CAT=="", substr(NAME, 1, 1), substr(HD_ICD_10, 1, 1)))


recode_vector_ENDP <- c(
  'A' = 'Infectious and Parasitic',
  'B' = 'Infectious and Parasitic',
  'C' = 'Neoplasms',
  'D' = 'Blood Diseases',
  'E' = 'Endocrine and metabolic ',
  'F' = 'Mental and behavioral',
  'G' = 'Nervous system',
  'H' = 'Eye, ear and adnexa', 
  'I' = 'Circulatory system',
  'J' = 'Respiratory system',
  'K' = 'Digestive system',
  'L' = 'Skin',
  'M' = 'Musculoskeletal system',
  'N' = 'Genitourinary system',
  'O' = 'Pregnancy',
  'P' = 'Perinatal',
  'Q' = 'Congenital malformations',
  'R' = 'Abnormal lab. findings',
  'S' = 'Injury',
  'T' = 'External causes',
  'V' = 'External causes',
  'Y' = 'External causes',
  'Z' = 'Health services'
)

recode_vector_DRUGS <- c(
  'A01' = "Stomatological preparations",
  'A02' = "Drugs for acid related disorders",
  'A03' = "Drugs for gastrointestinal disorders",
  'A04' = "Anti-emetics and anti-nauseants",
  'A05' = "Bile and liver therapy",
  'A06' = "Laxatives",
  'A07' = "Anti-diarrheals, intestinal agents",
  'A08' = "Anti-obesity preparations",
  'A09' = "Digestives, including enzymes",
  'A10' = "Drugs used in diabetes",
  'A11' = "Vitamins",
  'A12' = "Mineral supplements",
  'A13' = "Tonics",
  'A14' = "Anabolic agents for systemic use",
  'A15' = "Appetite stimulants",
  'A16' = "Alimentary tract and metabolism products",
  'B01' = "Antithrombotic agents",
  'B02' = "Antihemorrhagics",
  'B03' = "Antianemic preparations",
  'B05' = "Blood substitutes and perfusion solutions",
  'B06' = "Other hematological agents",
  'C01' = "Cardiac therapy",
  'C02' = "Antihypertensives",
  'C03' = "Diuretics",
  'C04' = "Peripheral vasodilators",
  'C05' = "Vasoprotectives",
  'C07' = "Beta blocking agents",
  'C08' = "Calcium channel blockers",
  'C09' = "Agents acting on the renin-angiotensin system",
  'C10' = "Lipid modifying agents",
  'D01' = "Antifungals for dermatological use",
  'D02' = "Emollients and protectives",
  'D03' = "Wounds and ulcers",
  'D04' = "Antipruritics, antihistamines",
  'D05' = "Antipsoriatics",
  'D06' = "Antibiotics and chemotherapeutics for dermatological use",
  'D07' = "Corticosteroids, dermatological preparations",
  'D08' = "Antiseptics and disinfectants",
  'D09' = "Medicated dressings",
  'D10' = "Acne preparations",
  'D11' = "Other dermatological preparations",
  'G01' = "Gynecological anti-infectives and antiseptics",
  'G02' = "Other gynecologicals",
  'G03' = "Sex hormones and modulators of the genital system",
  'G04' = "Urologicals",
  'H01' = "Pituitary and hypothalamic hormones and analogues",
  'H02' = "Corticosteroids for systemic use",
  'H03' = "Thyroid therapy",
  'H04' = "Pancreatic hormones",
  'H05' = "Calcium homeostasis",
  'J01' = "Antibacterials for systemic use",
  'J02' = "Antimycotics for systemic use",
  'J04' = "Antimycobacterials",
  'J05' = "Antivirals for systemic use",
  'J06' = "Immune sera and immunoglobulins",
  'J07' = "Vaccines",
  'L01' = "Antineoplastic agents",
  'L02' = "Endocrine therapy",
  'L03' = "Immunostimulants",
  'L04' = "Immunosuppressants",
  'M01' = "Anti-inflammatory and antirheumatic products",
  'M02' = "Topical products for joint and muscular pain",
  'M03' = "Muscle relaxants",
  'M04' = "Antigout preparations",
  'M05' = "Drugs for treatment of bone diseases",
  'M09' = "Other drugs for disorders of the musculo-skeletal system",
  'N01' = "Anesthetics",
  'N02' = "Analgesics",
  'N03' = "Antiepileptics",
  'N04' = "Anti-Parkinson drugs",
  'N05' = "Psycholeptics",
  'N06' = "Psychoanaleptics",
  'N07' = "Other nervous system drugs",
  'P01' = "Antiprotozoals",
  'P02' = "Anthelmintics",
  'P03' = "Ectoparasiticides, including scabicides, insecticides, and repellents",
  'R01' = "Nasal preparations",
  'R02' = "Throat preparations",
  'R03' = "Drugs for obstructive airway diseases",
  'R05' = "Cough and cold preparations",
  'R06' = "Antihistamines for systemic use",
  'R07' = "Other respiratory system products",
  'S01' = "Ophthalmologicals",
  'S02' = "Otologicals",
  'S03' = "Ophthalmological and otological preparations",
  'V01' = "Allergens",
  'V03' = "All other therapeutic products",
  'V04' = "Diagnostic agents",
  'V06' = "General nutrients",
  'V07' = "All other non-therapeutic products",
  'V08' = "Contrast media",
  'V09' = "Diagnostic radiopharmaceuticals",
  'V10' = "Therapeutic radiopharmaceuticals",
  'V20' = "Surgical dressings"
)


ENDP_lists$macro_cat <- recode_vector_ENDP[ENDP_lists$CAT]


### PLOT BETAS by Type of Switching ###
betas <- bind_rows(Beta_F1,
                   Beta_F2,
                   Beta_F3,
                   .id="Model") %>%
  mutate( type_failure= as.factor(case_when(
    Model==1 ~ "SWITCH_1",
    Model==2 ~ "SWITCH_2",
    Model==3 ~ "SWITCH_3",
    TRUE~"None"
  ))) %>% left_join(ENDP_lists, by=c("ENDPOINT"="NAME")) 

names_DRUGS <- colnames(fread(medication_file))
names_SES <- colnames(fread(SES_file))


betas <- betas %>% mutate(macro_cat = ifelse(betas$ENDPOINT %in% names_DRUGS, "DRUGS",
                                             ifelse(betas$ENDPOINT %in% names_SES, "SES",
                                                 ifelse(is.na(macro_cat), 'Other', macro_cat))),
                          LONGNAME = ifelse(macro_cat=="DRUGS", recode_vector_DRUGS[betas$ENDPOINT], LONGNAME))


betas_final <- betas %>%  
  mutate( LONGNAME = ifelse(is.na(LONGNAME), ENDPOINT, LONGNAME))


betas_final_F1<- betas_final %>% subset(type_failure=="SWITCH_1") 
betas_final_F2<- betas_final %>% subset(type_failure=="SWITCH_2") 
betas_final_F3<- betas_final %>% subset(type_failure=="SWITCH_3") 

## if we want only macro categories with at least 1 associated factor ##
# macro_cat_sign <- betas_final_F3 %>%
#   filter(FDR_p_values < 0.05) %>%
#   distinct(macro_cat) %>%
#   pull(macro_cat)
# 
# betas_final_F3_filtered <- betas_final_F3 %>%
#   filter(macro_cat %in% macro_cat_sign)

#F3
library(ggrepel)
ggplot(betas_final_F3, aes(x = macro_cat, y = Beta, color = macro_cat)) +
  geom_point(aes(alpha = ifelse(FDR_p_values < 0.05 | ENDPOINT=="G6_MIGRAINE", 1, 0.2))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  ggrepel::geom_text_repel(data = subset(betas_final_F3, FDR_p_values < 0.05 | ENDPOINT=="G6_MIGRAINE"), aes(label = LONGNAME), max.overlaps = Inf) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
