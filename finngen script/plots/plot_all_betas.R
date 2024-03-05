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

recode_vector_SES <- c(
  'divorced' = "Whether the individual has divorced",
  'self_rated_health_moderate_or_poor_scaled_health_and_welfare_indicator' = "Self-rated health moderate or poor scaled health and welfare indicator",
  'ses_self_employed' = "Socioeconomic status: self-employed",
  'ses_upperlevel' = "Socioeconomic status: Upper-level employees",
  'ses_manual_workers' = "Socioeconomic status: manual workers",
  'ses_students' = "Socioeconomic status: students",
  'edufield_generic' = "Field of education: generic programmes and qualifications",
  'edufield_education' = "Field of education: education",
  'edufield_artshum' = "Field of education: Arts and humanities",
  'edufield_socialsciences' = "Field of education: Social sciences, journalism, and information",
  'edufield_businessadminlaw' = "Field of education: Business, administration, and law",
  'edufield_science_math_stat' = "Field of education: Natural sciences, mathematics, and statistics",
  'edufield_ict' = "Field of education: Information and communication technologies",
  'edufield_engineering' = "Field of education: Engineering, manufacturing, and construction",
  'edufield_agriculture' = "Field of education: Agriculture, forestry, fisheries, and veterinary",
  'edufield_services' = "Field of education: Services",
  'mothertongue_fi' = "Mother tongue: Finnish",
  'mothertongue_swe' = "Mother tongue: Swedish",
  'mothertongue_rus' = "Mother tongue: Russian",
  'mothertongue_other' = "Mother tongue: other than Finnish, Swedish, or Russian",
  'received_study_allowance' = "Received study allowance",
  'received_basic_unemployment_allowance' = "Received basic daily unemployment allowance",
  'total_benefits' = "Sum of social benefits received, indexed",
  'emigrated' = "Emigrated",
  'miscarriages' = "Number of miscarriages",
  'terminated_pregnancies' = "Number of terminated pregnancies",
  'ectopic_pregnancies' = "Number of ectopic pregnancies",
  'stillborns' = "Number of births with at least one stillborn infant",
  'invitro_fertilization' = "In-vitro fertilization",
  'thrombosis_prophylaxis' = "Thrombosis prophylaxis",
  'anemia' = "Anemia during pregnancy",
  'no_analgesics' = "No analgesics during labor",
  'analgesics_info_missing' = "Information about analgesics during labor missing",
  'extraction_of_placenta' = "Manual extraction of placenta",
  'prophylaxis' = "Intrapartum antibiotic Group B Streptococcal prophylaxis",
  'mother_antibiotics' = "Antibiotic treatment of the mother during labor",
  'blood_transfusion' = "Blood transfusion during labor",
  'circumcision' = "Opening of circumcision during labor",
  'hysterectomy' = "Hysterectomy",
  'embolisation' = "Embolisation",
  'vaginal_delivery_breech' = "Vaginal delivery, breech",
  'forceps_delivery' = "Forceps-assisted delivery",
  'emergency_c_section' = "Delivery by emergency C-section",
  'mode_of_delivery_NA' = "Mode of delivery missing or unknown",
  'placenta_praevia' = "Placenta praevia",
  'ablatio_placentae' = "Ablatio placentae",
  'eclampsia' = "Eclampsia",
  'shoulder_dystocia' = "Shoulder dystocia",
  'asphyxia' = "Asphyxia",
  'live_born' = "Child born live",
  'stillborn_before_delivery' = "Child stillborn, died before delivery",
  'stillborn_during_delivery' = "Child stillborn, died during delivery",
  'stillborn_unknown' = "Child stillborn, unknown if died before or during delivery",
  'care_in_elderly_home' = "Type of long-term care: Care in an elderly home",
  'assisted_living_elderly' = "Type of long-term care: Round-the-clock assisted living for the elderly",
  'institutional_care_demented' = "Type of long-term care: Institutional care for the demented",
  'enhanced_care_demented' = "Type of long-term care: Enhanced residential care for the demented",
  'institutionalized_intellectual_disability' = "Type of long-term care: Intellectual disability services, institutionalized care",
  'assisted_intellectual_disability' = "Type of long-term care: Intellectual disability services, assisted living",
  'instructed_intellectual_disability' = "Type of long-term care: Intellectual disability services, instructed living",
  'supported_intellectual_disability' = "Type of long-term care: Intellectual disability services, supported living",
  'services_fos_substance_abusers' = "Type of long-term care: Services for substance abusers",
  'rehabilitation' = "Type of long-term care: Institutional care, rehabilitation",
  'residential_care_housing' = "Type of long-term care: Residential care housing",
  'psychiatric_residential_care_housing' = "Type of long-term care: Psychiatric residential care housing unit",
  '247_residential_care_housing_under_65yo' = "Type of long-term care: Round-the-clock residential care housing (for less than 65 years old)",
  '247_psychiatric_residential_care' = "Type of long-term care: Round-the-clock care in a psychiatric residential care housing unit",
  'unknown_long_term_care' = "Type of long-term care: unknown code",
  'mr_deficient_locomotion' = "Main reason for treatment in social welfare services: deficient locomotion",
  'mr_nervous_system' = "Main reason for treatment in social welfare services: nervous system related reasons",
  'mr_mental_confusion' = "Main reason for treatment in social welfare services: mental confusion",
  'mr_deficiencies_in_communication' = "Main reason for treatment in social welfare services: deficiencies in communication (speech, hearing, eyesight)",
  'mr_depression' = "Main reason for treatment in social welfare services: depression",
  'mr_other_psychiatric' = "Main reason for treatment in social welfare services: other psychiatric disease/symptom",
  'mr_loneliness_insecurity' = "Main reason for treatment in social welfare services: loneliness/insecurity",
  'mr_difficulties_with_housing' = "Main reason for treatment in social welfare services: difficulties with housing",
  'mr_lack_of_help_from_family' = "Main reason for treatment in social welfare services: lack of help from the family",
  'mr_lack_of_services' = "Main reason for treatment in social welfare services: lack of services for those living at home",
  'mr_lack_of_place_of_care' = "Main reason for treatment in social welfare services: lack of appropriate place of care",
  'mr_rehabilitation' = "Main reason for treatment in social welfare services: rehabilitation",
  'mr_medical_rehabilitation' = "Main reason for treatment in social welfare services: medical rehabilitation",
  'mr_accident' = "Main reason for treatment in social welfare services: accident",
  'mr_somatic' = "Main reason for treatment in social welfare services: investigation and treatment of a somatic disease",
  'mr_medical_abuse' = "Main reason for treatment in social welfare services: abuse of medications",
  'mr_polysubstance_abuse' = "Main reason for treatment in social welfare services: polysubstance use issues",
  'mr_other_addiction' = "Main reason for treatment in social welfare services: other addiction",
  'mr_substance_use_family' = "Main reason for treatment in social welfare services: substance use issues of a family member or something similar",
  'mr_NA' = "Main reason for treatment in social welfare services: missing",
  'long_term_care_duration' = "Total treatment days for a calendar year"
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
                          LONGNAME = ifelse(macro_cat=="DRUGS", recode_vector_DRUGS[betas$ENDPOINT], LONGNAME),
                          LONGNAME = ifelse(macro_cat=="SES", recode_vector_SES[betas$ENDPOINT], LONGNAME))


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
