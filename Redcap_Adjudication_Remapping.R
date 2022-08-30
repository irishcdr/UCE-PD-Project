#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load  library
library(tidyverse) #collection of pckgs  
#readr - updated data import functions like read_csv()
#dplyr - subsetting, sorting, transforming variables, grouping
#tidyr - restructuring rows and columns
#magrittr - piping a chain of commands
#lubridate - date and time variable processing
#stringr - string variable manipulation
library(readr) #updated data import functions like read_csv()
library(Hmisc) #tools for data analysis and summary function
library(xlsx) 
library(lubridate)
library(anytime) #transform dates


#set working directory
setwd("[workingdirectory]")

#upload redcap data files for entry 1 and 2
entry1 <- read_csv("UCEPDEntry1_DATA_2022-07-27_0311.csv", col_types = cols(cohort = col_character(), 
                                                                            abstractor = col_character(), abstractor_other = col_character(), 
                                                                            abstraction_date = col_date(format = "%Y-%m-%d"), 
                                                                            start_time = col_time(format = "%H:%M"), deceased_date = col_date(format = "%Y-%m-%d"), 
                                                                            recent_chart_date = col_date(format = "%Y-%m-%d"), 
                                                                            parkinsonism_year = col_double(), parkinsonism_month = col_double(), 
                                                                            dx_primary = col_character(), dx_other = col_character(),dx_source_other = col_character(),
                                                                            dx_certainty = col_character(), dx_source_other = col_double(), 
                                                                            dx_alt2_other = col_character(), dx_alt3 = col_double(), dx_alt3_other = col_character(), 
                                                                            dx_alt3_year = col_double(), dx_alt3_month = col_double(), 
                                                                            chart_problems___1 = col_logical(), chart_problems___2 = col_logical(), chart_problems___3 = col_logical(), 
                                                                            chart_problems___4 = col_logical(), chart_problems___5 = col_logical(), 
                                                                            missing_info___1 = col_logical(), missing_info___2 = col_logical(), missing_info___3 = col_logical(), 
                                                                            missing_info___4 = col_logical(), missing_info___5 = col_logical(),
                                                                            poss_pd_reason___1 = col_logical(), poss_pd_reason___2 = col_logical(), poss_pd_reason___3 = col_logical(),
                                                                            poss_pd_reason___4 = col_logical(), poss_pd_reason___5 = col_logical(),
                                                                            notes = col_character(), time_comp = col_time(format = "%H:%M")))


entry2 <- read_csv("UCEPDEntry2_DATA_2022-07-27_0312.csv", col_types = cols(cohort = col_character(), abstractor = col_character(), abstractor_other = col_character(), 
                                                                            abstraction_date = col_date(format = "%Y-%m-%d"),  start_time = col_time(format = "%H:%M"), deceased_date = col_date(format = "%Y-%m-%d"), 
                                                                            recent_chart_date = col_date(format = "%Y-%m-%d"), parkinsonism_year = col_double(), parkinsonism_month = col_double(), 
                                                                            dx_primary = col_character(), dx_other = col_character(),dx_source_other = col_character(),
                                                                            dx_certainty = col_character(), dx_source_other = col_double(), dx_alt2_other = col_character(), 
                                                                            dx_alt3 = col_double(), dx_alt3_other = col_character(), dx_alt3_year = col_double(), dx_alt3_month = col_double(), 
                                                                            chart_problems___1 = col_logical(), chart_problems___2 = col_logical(), chart_problems___3 = col_logical(), 
                                                                            chart_problems___4 = col_logical(), chart_problems___5 = col_logical(), 
                                                                            missing_info___1 = col_logical(), missing_info___2 = col_logical(), missing_info___3 = col_logical(), 
                                                                            missing_info___4 = col_logical(), missing_info___5 = col_logical(),
                                                                            poss_pd_reason___1 = col_logical(), poss_pd_reason___2 = col_logical(), poss_pd_reason___3 = col_logical(),
                                                                            poss_pd_reason___4 = col_logical(), poss_pd_reason___5 = col_logical(), notes = col_character(), time_comp = col_time(format = "%H:%M")))


#add column for entry 1 and entry 2
entry1 <- entry1%>%
  add_column(entry_write = "Entry 1", .after = "record_id")
entry2 <- entry2%>%
  add_column(entry_write = "Entry 2", .after = "record_id")


#add columns record_id_RC
entry1$record_id_RC <- gsub(" ", "", paste(entry1$record_id, "e1"))
entry2$record_id_RC <- gsub(" ", "", paste(entry2$record_id, "e2"))

#use describe() function from Hmisc to get summary statistics for all variables in dataframe including finding missing
describe(entry1)
describe(entry2)

#exclude incomplete records with missing abstraction date
entry1 <- entry1 %>% drop_na(abstraction_date)
entry2 <- entry2 %>% drop_na(abstraction_date)

describe(entry1)
describe(entry2)

plot(describe(entry1))
plot(describe(entry2))



#combine data frames vertically
head(entry1)
head(entry2)
#relabel mismatched columns
names(entry1)[names(entry1) == 'abstraction_records_entry_1_v3_complete'] <- "abstraction_records_complete"
names(entry2)[names(entry2) == 'abstraction_records_entry_2_v3_complete'] <- "abstraction_records_complete"
allentry <- rbind(entry1, entry2)

##Relabel Data 
#Read Data and relabel for Entry 1
allentry_adj=read.csv('allentry_20220624.csv')


#Setting Labels
label(allentry_adj$record_id)="Record ID"
label(allentry_adj$cohort)="1.1 Cohort"
label(allentry_adj$abstractor)="1.2 Abstractor"
label(allentry_adj$abstractor_other)="1.2.1 Abstractor Other"
label(allentry_adj$abstraction_date)="1.3 Date of Abstraction"
label(allentry_adj$start_time)="1.4 Start Time"
label(allentry_adj$alive)="1.5 At the time of abstraction, is the patient alive?"
label(allentry_adj$deceased_date)="1.5.1 Date deceased"
label(allentry_adj$recent_chart_date)="2.1 Most recent encounter date found in charts:"
label(allentry_adj$parkinsonism_year)="2.2 Date of earliest parkinsonism: YEAR Find the earliest date where parkinsonism was mentioned as first suspected or possible. Record YEAR in this field. Find the note or encounter in the medical record where any term parkinsonism or parkinsonian or parkinson or Parkinsons Disease (PD) or idiopathic Parkinsons Disease (iPD) is FIRST documented diagnosis, assessment, or impression of possible parkinsonism. CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$parkinsonism_month)="2.2.1 Date of earliest parkinsonism: MONTH"
label(allentry_adj$dx_primary)="2.3 Primary Diagnosis: Best documented diagnosis related to parkinsonism in the medical record Choose the most specific diagnosis documented in the medical record. If disagreement or uncertainty exists about documented diagnosis, impression, or assessment put more weight on more recent notes and on more specific notes by movement disorders neurologists and other neurologists.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_other)="2.3.1 Other Neurological Disorder Specify:"
label(allentry_adj$dx_certainty)="2.4 Primary clinical diagnosis - certainty Choose the certainty category that best describes the documented certainty for the diagnosis above. Guideline:  Certain: diagnosis clearly documented Somewhat certain: diagnosis is listed first or most likely or possible Somewhat uncertain: diagnosis is listed among multiple diagnoses without being most likely ORphysicians consistently disagree on the diagnosis Uncertain: no supportive information  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_source)="2.5 Primary parkinsonism diagnosis - Source The most specific specialty who documented diagnosis CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_source_other)="2.5.1 Other Source specify: "
label(allentry_adj$dx_year)="2.6 Date of primary diagnosis: YEAR Find the earliest note or encounter in the medical record where the clinical diagnosis selected above is FIRST documented as suspected, possible, or coded as a billing or encounter diagnosis. Make sure to check outside records/scanned documents for words that are not found by a text search. CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_month)="2.6.1 Date of primary diagnosis: MONTH Find the earliest note or encounter in the medical record where the clinical diagnosis selected above is FIRST documented as suspected, possible, or coded as a billing or encounter diagnosis. Make sure to check outside records/scanned documents for words that are not found by a text search. "
label(allentry_adj$dx_date_certainty)="2.7 Date of Diagnosis Certainty   Certain - date of diagnosis made and confirmed in physician note Somewhat certain - physician documents a year or history/patient reports month/year Somewhat uncertain - relative time frame (i.e. 2 years ago) or history/patient reports year Uncertain - no supportive information to suggest a year of diagnosis   CLICK HERE FOR ADDITIONAL GUIDANCE "
label(allentry_adj$dx_alt1)="2.8 Clinical diagnosis alternate 1 - documented Enter the next most likely parkinsonism or neurologic diagnosis documented in the chart. This is typically used when there is more than one type of parkinsonism or confounding neurologic disorder listed as a diagnosis.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_alt1_other)="2.8.1 Other Alternate 1 Diagnosis"
label(allentry_adj$dx_alt1_year)="2.9 Date of Alternate Diagnosis: YEAR"
label(allentry_adj$dx_alt1_month)="2.9.1 Date of Alternate Diagnosis: MONTH"
label(allentry_adj$dx_alt2)="2.10 Clinical diagnosis alternate 2 - documentedOption - next most likely diagnosis documented in chart CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_alt2_other)="2.10.1 Other Alternate 2 Diagnosis"
label(allentry_adj$dx_alt2_year)="2.11 Date of Alternate 2 Diagnosis: YEAR Find the note or encounter in the medical record where alternate diagnosis 2 is FIRST documented as suspected, possible, or coded as a billing or encounter diagnosis: It is worth emphasizing that this alternate 2 diagnosis date of diagnosis is determined by when this particular diagnosis is documented as being suspected or a possibility. This is because the diagnosis is usually suspected before being later confirmed."
label(allentry_adj$dx_alt2_month)="2.11.1 Date of Alternate 2 Diagnosis: MONTH"
label(allentry_adj$dx_alt3)="2.12 Clinical Diagnosis Alternate 3 - documented option - next most likely diagnosis documented in chart (usually will be same source as above, but may not be for 2nd opinions) CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dx_alt3_other)="2.12.1 Other Alternate 3 Diagnosis "
label(allentry_adj$dx_alt3_year)="2.13 Date of Alternative 3 Diagnosis: YEARFind the note or encounter in the medical record where alternate diagnosis 3 is FIRST documented as suspected, possible, or coded as a billing or encounter diagnosis:It is worth emphasizing that this alternate 3 diagnosis date of diagnosis is determined by when this particular diagnosis is documented as being suspected or a possibility. This is because when the diagnosis is usually suspected before being later confirmed."
label(allentry_adj$dx_alt3_month)="2.13.1 Date of Alternative 3 Diagnosis: MONTHFind the note or encounter in the medical record where alternate diagnosis 3 is FIRST documented as suspected, possible, or coded as a billing or encounter diagnosis: It is worth emphasizing that this alternate 3 diagnosis month of diagnosis is determined by when this particular diagnosis is documented as being suspected or a possibility. This is because when the diagnosis is usually suspected before being later confirmed."
label(allentry_adj$brady_hx)="2A.1 Bradykinesia in History: slowness or small movements, small handwriting or micrographia in history as described by patient/care partner (not exam)"
label(allentry_adj$rigidity_hx)="2A.1 Rigidity in History: stiffness, rigidity in history as described by patient/care partner (not exam)"
label(allentry_adj$tremor_hx)="2A.1 Tremor in History: tremor or shaking of upper (including hands) or lower extremities as described by patient/care partner (not exam)"
label(allentry_adj$gait_hx)="2A.1 Gait/Posture in History:  any description of falls, balance trouble, shuffling gait, stooped/bent posture, feet getting stuck on ground when walking, freezing/magnetic gait, hard to start walking as described by patient/care partner (not exam)"
label(allentry_adj$brady_ex)="2B.1 Bradykinesia in exam: slow movements, decreased amplitude, speed, or fatiguability of rapid alternating movements, fine finger movements, finger tapping, hand grips, or slow gait"
label(allentry_adj$rigidity_ex)="2B.2 Rigidity in exam: stiffness, increased tone, rigidity"
label(allentry_adj$tremor_ex)="2B.3 Tremor in exam: tremor or shaking (not dyskinesia or chorea) in upper or lower extremities"
label(allentry_adj$gait_ex)="2B.4 Gait/stance instability in exam: stooped or kyphotic posture, bent spine/bent posture, shuffling gait, small steps, festination, start hesitation, magnetic gait, freezing of gait, pull test with more than 2 step recovery"
label(allentry_adj$asymmetry)="2C.1 Asymmetry at onset (history or exam) Asymmetry at onset (history or exam): bradykinesia or tremor (or other parkinson symptoms) are asymmetric in initial evaluations.  Can infer if hx & exam findings are all consistently documented as worse on one side compared to the other in initial evaluations.Please note in comments that UPDRS (or a rating scale) was documented and used for this question!"
label(allentry_adj$rest_tremor)="2C.2 Tremor rest (history or exam) tremor specifically mentioned as occurring at rest or resting or described as rest tremor, pill-rolling tremor, or resting tremor."
label(allentry_adj$pd_progression)="2C.3 Progession of parkinsonism: Since the parkinsonism date of diagnosis, is there documentation that parkinsonism has progressed over time Progression of PD may be documented in the chart as  Symptoms are getting worse New symptoms are emerging over time Documented increases in PD medications over time   CLICK HERE FOR ADDITIONAL GUIDANCE  "
label(allentry_adj$pd_prog_notes)="2C.3.1 Progression of parkinsonism comments:Comments are strongly encouraged for the above question. Please note any context you used for your response (avoid any PHI)"
label(allentry_adj$dat_scan)="2C.4 DAT scan DAT scan results may be referenced in notes, imaging reports, or scanned into a Media/PDF tab.  CLICK HERE FOR ADDITIONAL GUIDANCE "
label(allentry_adj$dopamed_hx)="3.1 Did the patient take dopaminergic medication (DA meds)?  For this question, limit medication search to just dopaminergic medications (levodopa meds and dopamine agonists). Refer to dopaminerigic medication list (see tab) list.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dopamed_start_year)="3.2 Earliest use of dopaminergic PD Medication: YEAR Find the year of earliest encounter or note where dopaminergic medication is either prescribed or mentioned as being prescribed (even if outside the local medical group).  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dopamed_start_month)="3.2.1 Earliest use of dopaminergic PD Medication: MONTH Record the month of earliest encounter or note where dopaminergic medication is either prescribed or mentioned as being prescribed (even if outside the local medical group)."
label(allentry_adj$dopamed_last_year)="3.3 Last documented use of dopaminergic PD medication: YEAR  Enter the year of the most recent encounter where any dopaminergic medication was listed as being taken (does not have to be prescribed; med can simply be listed as being present at that encounter). Encounter may be any type (including office visit, refill, historical, or telephone calls).  "
label(allentry_adj$dopamed_last_month)="3.3.1 Last documented use of dopaminergic PD medication: MONTHEnter the month of the most recent encounter where any dopaminergic medication was listed as being taken (does not have to be prescribed; med can simply be listed as being present at that encounter). Encounter may be any type (including office visit, refill, historical, or telephone calls)."
label(allentry_adj$dopa_response)="3.4 Documented response to dopaminergic medication:   Look for documentation that there was an improvement in parkinsons symptoms after starting dopaminergic medication.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$dopa_response_uncertain)="3.4.1 If an uncertain or questionable response to dopaminergic medication, please provide context for your response above:"
label(allentry_adj$dopa_dose1000mg)="3.4.2 If no response to dopaminergic medications, did the patient ever take 1000 mg at any time? "
label(allentry_adj$chorea_dyskinesia)="3.5 Dopaminergic medication induced chorea or dyskinesia:  Dyskinesia or chorea are disordered movements that cannot be suppressed and may arise when taking dopaminergic medications. Do not confuse it with tremor which is rhythmic movements that often are suppressed. Search for dyskinesia and chorea in the search bar to scan the chart for these terms.  Yesrefers to documentation of chorea or dyskinesia and the patient is on levodopa or dopamine agonists. No refers to explicit documentation that there is no chorea or no dyskinesia. Questionable refers to documentation that the presence of chorea or dyskinesia is questionable or uncertain but terms are mentioned. Not documented refers to no mention of chorea or dyskinesia in the documentation. Not applicable is when the patient is not on any dopaminergic medications. "
label(allentry_adj$neuroleptic)="3.6 Neuroleptic treatment at onset of symptoms and diagnosis (within 24 months) Use the date of diagnosis for parkinsonism entered above to answer this question and review notes that are from 12 months BEFORE to 12 months AFTER (24 month window) of the date of diagnosis. Look through notes and medication prescriptions/medication lists in this timeframe for neuroleptic medication documentation. If a patient was seen in an emergency room or hospitalized, be sure to check the medication list in those settings since neuroleptics are often used in those settings.  Search the chart for neuroleptic medications as guided below. Search for neuroleptic class medications. Be sure to read and review outside records. Also, list any medication that the clinicians seem to have concerns over contributing to neurologic condition (any class).   CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$neuroleptic_meds1)="3.6.1 Neuroleptics or other concerning medications 1: "
label(allentry_adj$neuroleptic_meds2)="3.6.2 Neuroleptics or other concerning medications 2: "
label(allentry_adj$neuroleptic_meds3)="3.6.3 Neuroleptics or other concerning medications 3: "
label(allentry_adj$neuroleptic_meds4)="3.6.4 Neuroleptics or other concerning medications 4: "
label(allentry_adj$dementia)="Early dementia within FIRST YEAR after the earliest date of diagnosis above:  Look for documented diagnosis of dementia within one year of the date of diagnosis above. Check Problem List (when dementia was entered/noted) and clinical notes. Comments encouraged if Yes or Questionable.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$hallucinations)="Hallucinations not due to levodopa or dopamine agonist medications in first 3 years of diagnosis or symptoms:  Refers to documentation of any hallucinations or visions in the first 3 years of diagnosis or symptoms. Of note, the hallucinations must be present in the absence of levodopa or other dopaminergic agonist medication. "
label(allentry_adj$early_falls)="Early falls Early falls is defined as having had more than 3 documented falls or any 1 fall with injury within the first 3 years after date of diagnosis listed above"
label(allentry_adj$orthostatic_hypotension)="Orthostatic hypotension within the first 3 years after the date of diagnosis:  YES - Documented orthostatic hypotension or 3+ fainting episodes are present NO - Documentation states that there is no fainting or no orthostatic hypotension QUESTIONABLE - Uncertain if there is orthostatic hypotension or more than 3 fainting spells upon standing up  NOT DOCUMENTED - There is no mention of orthostatic hypotension or fainting episodes.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$orthostatic_hypo_meds)="Was the patient on any of the following orthostatic hypotension medication within the first 3 years of diagnosis?  Midodrine (Proamatine, Orvaten) Fludrocortisone (Florinef) Droxidopa (Northera) "
label(allentry_adj$incontinence)="Bladder incontinence documented within first 3 years of diagnosis:  Review notes and problem list for bladder incontinence urinary incontinence urine incontinence overflow incontinence, stress incontinence or urge incontinence as long as the word incontinence is being used. If incontinence by itself is used; and context usually will strongly suggest that it is urinary incontinence that is being referred to.  Other urinary symptoms that do NOT count are urinary frequency, urgency, nocturia, or dysuria. CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$head_injury)="History of traumatic brain injury or repeated (2 or more) concussions:  Look for documented traumatic brain injury or TBI at any time in history. Look for documented concussions at least 2 times in the history or in the actual notes. Post-concussion syndrome is not sufficient for this question.  History of recurrent brain impacts (ie boxing, football, soccer) is not sufficient unless documentation states there are at least 2 concussions present in the past.   "
label(allentry_adj$strokes)="History of confluent or multiple (2 or more) strokes:  Look for documentation of two or more strokes occurring at any time OR brain imaging evidence for either multiple (2 more strokes - also known as infarcts or lacunes) or confluent or extensive white matter changes. Reference to multi-infarct dementia would count as this means multiple strokes/infarcts have occurred. Imaging results that say - microvascular ischemic disease, nonspecific white matter changes, periventricular white matter changes do not count. This information may be found in the following places: brain imaging reports of head CT or brain MRI in Imaging/Radiology sections, notes that mention strokes, or multiple stroke entries in the Problem List.  CLICK HERE FOR ADDITIONAL GUIDANCE"
label(allentry_adj$hydrocephalus_braintumor)="Presence of brain tumor or hydrocephalus on imaging:  Look for Notes and/or Problem List entries that mention brain tumor, brain mass, or hydrocephalus, particularly confirmed normal pressure hydrocephalus or NPH.   If not confirmed but mentioned or of uncertain relevance, choose Questionable. Please write comments to provide context to whether the clinical notes document if the brain tumor or hydrocephalus seems to contribute to the parkinsonism."
label(allentry_adj$encephalitis)="History of definite encephalitis Refers to inflammation of the brain. Look for the term in the Notes or Problem List. This may be present in the past medical history. If present, in the comment field, write the approximate year and/or age of the patient when the encephalitis occurred."
label(allentry_adj$cerebellar)="Unequivocal abnormal cerebellar exam  Refers to findings of abnormal ataxia, dysmetria, past pointing or dysdiadochokinesia.  These findings must be listed within the Physical Exam/ Exam portion of the note, often within the neurologic section of the exam.  This can be documented by either a neurologist or non-neurologist. However, if there is conflicting documentation on cerebellar findings, the exam findings of the neurologist will overrule non-neurologist exam findings."
label(allentry_adj$supranuclear_gaze_palsy)="Trouble looking down during exam (supranuclear gaze palsy) Defined as difficulty looking down when testing eye movements on the exam. This suggests a Parkinson-plus syndrome. This can be documented by either a neurologist or a non-neurologist. However, if there is conflicting documentation on supranuclear gaze palsy, abstract based more recent notes and prioritize neurologist note(s).  Look in the notes section of the chart, specifically at the physical examination. Eye movements are usually mentioned in the neurologic exam or in the HEENT (head, eyes, ears, nose, and throat) exam Any documentation of trouble looking down mark as Yes Documentation of EOMI or extraocular movements intact suggests there is no trouble looking down and hence, markNo "
label(allentry_adj$chart_problems___1)="Chart Abstraction Difficulties Check ALL that apply and please consider adding notes  (choice=There was not much information in the chart to answer most questions )"
label(allentry_adj$chart_problems___2)="Chart Abstraction Difficulties Check ALL that apply and please consider adding notes  (choice=There was less than 3 years of follow-up to review after the Parkinson-related date of diagnoses )"
label(allentry_adj$chart_problems___3)="Chart Abstraction Difficulties Check ALL that apply and please consider adding notes  (choice=Important information was found in scanned documents )"
label(allentry_adj$chart_problems___4)="Chart Abstraction Difficulties Check ALL that apply and please consider adding notes  (choice=Important information was found from electronic outside records (i.e. from a health information exchange) )"
label(allentry_adj$chart_problems___5)="Chart Abstraction Difficulties Check ALL that apply and please consider adding notes  (choice=Chart review was complex or confusing or there were conflicting diagnoses)"
label(allentry_adj$missing_info___1)="What kind of neurology evaluations did you find? Check all that apply, comments welcome. This is to capture your overall impression of the abstraction. There is no need to re-review the chart. (choice=Most were relevant neurology outpatient visits and notes )"
label(allentry_adj$missing_info___2)="What kind of neurology evaluations did you find? Check all that apply, comments welcome. This is to capture your overall impression of the abstraction. There is no need to re-review the chart. (choice=Most were neurology outside records or scanned documents )"
label(allentry_adj$missing_info___3)="What kind of neurology evaluations did you find? Check all that apply, comments welcome. This is to capture your overall impression of the abstraction. There is no need to re-review the chart. (choice=Most did not include any useful information about parkinsonism )"
label(allentry_adj$missing_info___4)="What kind of neurology evaluations did you find? Check all that apply, comments welcome. This is to capture your overall impression of the abstraction. There is no need to re-review the chart. (choice=Most neurology evaluations were in the past (> 5 years)  )"
label(allentry_adj$missing_info___5)="What kind of neurology evaluations did you find? Check all that apply, comments welcome. This is to capture your overall impression of the abstraction. There is no need to re-review the chart. (choice=There were large gaps missing (no neuro evals) among available neurology evaluations )"
label(allentry_adj$poss_pd_reason___1)="Based on your chart review (including impressions and notes), do any of the following apply to this patients diagnosis? Please write detailed comments for reason. (choice=Too early to determine)"
label(allentry_adj$poss_pd_reason___2)="Based on your chart review (including impressions and notes), do any of the following apply to this patients diagnosis? Please write detailed comments for reason. (choice=Missing or insufficient information to review)"
label(allentry_adj$poss_pd_reason___3)="Based on your chart review (including impressions and notes), do any of the following apply to this patients diagnosis? Please write detailed comments for reason. (choice=Multiple medical co-morbidities (non-parkinsonian ones) to make it difficult to be certain)"
label(allentry_adj$poss_pd_reason___4)="Based on your chart review (including impressions and notes), do any of the following apply to this patients diagnosis? Please write detailed comments for reason. (choice=Neurodegenerative or other parkinsonisms are in the differential diagnosis)"
label(allentry_adj$poss_pd_reason___5)="Based on your chart review (including impressions and notes), do any of the following apply to this patients diagnosis? Please write detailed comments for reason. (choice=Not PD, specific that the patient ONLY has non-neurodegenerative parkinsonism and does not have any neurodegenerative parkinsonism.)"
label(allentry_adj$notes)="Comments Please provide any information about this abstraction that can provide context or explanation not provided above that may help reviewers better understand or classify the likelihood of PD or verifying the date of diagnosis (no PHI)."
label(allentry_adj$time_comp)="Time completed: "
label(allentry_adj$abstraction_records_entry_1_v3_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
allentry_adj$cohort.factor = factor(allentry_adj$cohort,levels=c("20184","20203","20204","20211","20212","20213","20214","20221","20222","20223","20224"))
allentry_adj$abstractor.factor = factor(allentry_adj$abstractor,levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","99"))
allentry_adj$alive.factor = factor(allentry_adj$alive,levels=c("1","0","2"))
allentry_adj$parkinsonism_year.factor = factor(allentry_adj$parkinsonism_year,levels=c("999","998","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$parkinsonism_month.factor = factor(allentry_adj$parkinsonism_month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","999"))
allentry_adj$dx_primary.factor = factor(allentry_adj$dx_primary,levels=c("1","16","12","14","4","5","11","13","19","17"))
allentry_adj$dx_certainty.factor = factor(allentry_adj$dx_certainty,levels=c("1","2","3","4"))
allentry_adj$dx_source.factor = factor(allentry_adj$dx_source,levels=c("1","2","3","4","5","6","7"))
allentry_adj$dx_year.factor = factor(allentry_adj$dx_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dx_month.factor = factor(allentry_adj$dx_month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","999"))
allentry_adj$dx_date_certainty.factor = factor(allentry_adj$dx_date_certainty,levels=c("1","2","3","4"))
allentry_adj$dx_alt1.factor = factor(allentry_adj$dx_alt1,levels=c("1","16","12","14","4","5","11","13","19","98","10","18","2","3","20","97","7","6","15","21","96","17"))
allentry_adj$dx_alt1_year.factor = factor(allentry_adj$dx_alt1_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dx_alt1_month.factor = factor(allentry_adj$dx_alt1_month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","999"))
allentry_adj$dx_alt2.factor = factor(allentry_adj$dx_alt2,levels=c("1","16","12","14","4","5","11","13","19","98","10","18","2","3","20","97","7","6","15","21","96","17"))
allentry_adj$dx_alt2_year.factor = factor(allentry_adj$dx_alt2_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dx_alt2_month.factor = factor(allentry_adj$dx_alt2_month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","999"))
allentry_adj$dx_alt3.factor = factor(allentry_adj$dx_alt3,levels=c("1","16","12","14","4","5","11","13","19","98","10","18","2","3","20","97","7","6","15","21","96","17"))
allentry_adj$dx_alt3_year.factor = factor(allentry_adj$dx_alt3_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dx_alt3_month.factor = factor(allentry_adj$dx_alt3_month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","999"))
allentry_adj$brady_hx.factor = factor(allentry_adj$brady_hx,levels=c("1","2","3","4"))
allentry_adj$rigidity_hx.factor = factor(allentry_adj$rigidity_hx,levels=c("1","2","3","4"))
allentry_adj$tremor_hx.factor = factor(allentry_adj$tremor_hx,levels=c("1","2","3","4"))
allentry_adj$gait_hx.factor = factor(allentry_adj$gait_hx,levels=c("1","2","3","4"))
allentry_adj$brady_ex.factor = factor(allentry_adj$brady_ex,levels=c("1","2","3","4"))
allentry_adj$rigidity_ex.factor = factor(allentry_adj$rigidity_ex,levels=c("1","2","3","4"))
allentry_adj$tremor_ex.factor = factor(allentry_adj$tremor_ex,levels=c("1","2","3","4"))
allentry_adj$gait_ex.factor = factor(allentry_adj$gait_ex,levels=c("1","2","3","4"))
allentry_adj$asymmetry.factor = factor(allentry_adj$asymmetry,levels=c("1","2","3","4"))
allentry_adj$rest_tremor.factor = factor(allentry_adj$rest_tremor,levels=c("1","2","3","4"))
allentry_adj$pd_progression.factor = factor(allentry_adj$pd_progression,levels=c("1","2","3","4"))
allentry_adj$dat_scan.factor = factor(allentry_adj$dat_scan,levels=c("1","2","3","4","0"))
allentry_adj$dopamed_hx.factor = factor(allentry_adj$dopamed_hx,levels=c("1","2","0","3","4"))
allentry_adj$dopamed_start_year.factor = factor(allentry_adj$dopamed_start_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dopamed_start_month.factor = factor(allentry_adj$dopamed_start_month,levels=c("999","1","2","3","4","5","6","7","8","9","10","11","12"))
allentry_adj$dopamed_last_year.factor = factor(allentry_adj$dopamed_last_year,levels=c("999","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980"))
allentry_adj$dopamed_last_month.factor = factor(allentry_adj$dopamed_last_month,levels=c("999","1","2","3","4","5","6","7","8","9","10","11","12"))
allentry_adj$dopa_response.factor = factor(allentry_adj$dopa_response,levels=c("1","2","3"))
allentry_adj$dopa_dose1000mg.factor = factor(allentry_adj$dopa_dose1000mg,levels=c("1","2","3"))
allentry_adj$chorea_dyskinesia.factor = factor(allentry_adj$chorea_dyskinesia,levels=c("1","2","3","4","5"))
allentry_adj$neuroleptic.factor = factor(allentry_adj$neuroleptic,levels=c("1","2","3","4"))
allentry_adj$dementia.factor = factor(allentry_adj$dementia,levels=c("1","2","3","4"))
allentry_adj$hallucinations.factor = factor(allentry_adj$hallucinations,levels=c("1","2","3","4"))
allentry_adj$early_falls.factor = factor(allentry_adj$early_falls,levels=c("1","2","3","4"))
allentry_adj$orthostatic_hypotension.factor = factor(allentry_adj$orthostatic_hypotension,levels=c("1","2","3","4"))
allentry_adj$orthostatic_hypo_meds.factor = factor(allentry_adj$orthostatic_hypo_meds,levels=c("1","2","3","4"))
allentry_adj$incontinence.factor = factor(allentry_adj$incontinence,levels=c("1","2","3","4"))
allentry_adj$head_injury.factor = factor(allentry_adj$head_injury,levels=c("1","2","3","4"))
allentry_adj$strokes.factor = factor(allentry_adj$strokes,levels=c("1","2","3","4"))
allentry_adj$hydrocephalus_braintumor.factor = factor(allentry_adj$hydrocephalus_braintumor,levels=c("1","2","3","4","5"))
allentry_adj$encephalitis.factor = factor(allentry_adj$encephalitis,levels=c("1","2","3","4"))
allentry_adj$cerebellar.factor = factor(allentry_adj$cerebellar,levels=c("1","2","3","4"))
allentry_adj$supranuclear_gaze_palsy.factor = factor(allentry_adj$supranuclear_gaze_palsy,levels=c("1","2","3","4"))
allentry_adj$chart_problems___1.factor = factor(allentry_adj$chart_problems___1,levels=c("0","1"))
allentry_adj$chart_problems___2.factor = factor(allentry_adj$chart_problems___2,levels=c("0","1"))
allentry_adj$chart_problems___3.factor = factor(allentry_adj$chart_problems___3,levels=c("0","1"))
allentry_adj$chart_problems___4.factor = factor(allentry_adj$chart_problems___4,levels=c("0","1"))
allentry_adj$chart_problems___5.factor = factor(allentry_adj$chart_problems___5,levels=c("0","1"))
allentry_adj$missing_info___1.factor = factor(allentry_adj$missing_info___1,levels=c("0","1"))
allentry_adj$missing_info___2.factor = factor(allentry_adj$missing_info___2,levels=c("0","1"))
allentry_adj$missing_info___3.factor = factor(allentry_adj$missing_info___3,levels=c("0","1"))
allentry_adj$missing_info___4.factor = factor(allentry_adj$missing_info___4,levels=c("0","1"))
allentry_adj$missing_info___5.factor = factor(allentry_adj$missing_info___5,levels=c("0","1"))
allentry_adj$poss_pd_reason___1.factor = factor(allentry_adj$poss_pd_reason___1,levels=c("0","1"))
allentry_adj$poss_pd_reason___2.factor = factor(allentry_adj$poss_pd_reason___2,levels=c("0","1"))
allentry_adj$poss_pd_reason___3.factor = factor(allentry_adj$poss_pd_reason___3,levels=c("0","1"))
allentry_adj$poss_pd_reason___4.factor = factor(allentry_adj$poss_pd_reason___4,levels=c("0","1"))
allentry_adj$poss_pd_reason___5.factor = factor(allentry_adj$poss_pd_reason___5,levels=c("0","1"))
allentry_adj$abstraction_records_entry_1_v3_complete.factor = factor(allentry_adj$abstraction_records_entry_1_v3_complete,levels=c("0","1","2"))

levels(allentry_adj$cohort.factor)=c("2018 Q4","2020 Q3","2020 Q4","2021 Q1","2021 Q2","2021 Q3","2021 Q4","2022 Q1","2022 Q2","2022 Q3","2022 Q4")
levels(allentry_adj$abstractor.factor)=c("Allan Wu","Andrew Wilson","Wesley Wu","Emily Laltoo","Cooper Baddley","Lauren Uhr","Lauren Byrne","Aline Duarte Folle","Irish del Rosario","Miguel Cuevas","EJ Choi","Jennifer Decopain","Saba Khan","Margaret Lee","Other")
levels(allentry_adj$alive.factor)=c("Yes","No...","Unknown")
levels(allentry_adj$parkinsonism_year.factor)=c("0000","No Parkinsonism found","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
#set unknown month to january
levels(allentry_adj$parkinsonism_month.factor)=c("January","February","March","April","May","June","July","August","September","October","November","December","January")
levels(allentry_adj$dx_primary.factor)=c("Parkinsons Disease (PD) or idiopathic Parkinsons Disease (iPD)","Parkinsonism, vascular","Parkinsonism, neuroleptic/drug-induced","Progressive supranuclear palsy (PSP)","Corticobasal degeneration (CBD)","Dementia with Lewy Bodies (LBD)","Multiple system atrophy (MSA)","Normal pressure hydrocephalus (NPH)","Parkinsonism, other (not specified above)","No parkinsonism disorder identified")
levels(allentry_adj$dx_certainty.factor)=c("Certain","Somewhat certain ","Somewhat uncertain ","Uncertain")
levels(allentry_adj$dx_source.factor)=c("Movement disorders specialist","Neurologist","Primary care physician","Other Physician","Patient - self reported","Other Source","Not Applicable")
levels(allentry_adj$dx_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
#set unknown month to january
levels(allentry_adj$dx_month.factor)=c("January","February","March","April","May","June","July","August","September","October","November","December","January")
levels(allentry_adj$dx_date_certainty.factor)=c("Certain ","Somewhat certain ","Somewhat uncertain ","Uncertain")
levels(allentry_adj$dx_alt1.factor)=c("Parkinsons disease","Parkinsonism, vascular","Parkinsonism, Neuroleptic/Drug-induced","Progressive supranuclear palsy (PSP)","Corticobasal degeneration (CBD)","Dementia with Lewy Bodies (DLB)","Multiple system atrophy (MSA)","Normal pressure hydrocephalus (NPH)","Parkinsonism, other (not listed above)","-----------------------------------------------------------------","Motor neuron disease (ALS)","Spinocerebellar ataxia (SCA)","Alzheimers Disease (AD)","Frontotemporal dementia (FTD)","Dementia (not listed above)","---------------------------------------------------------","Essential Tremor (ET)","Dopa-responsive dystonia (DRD)","Functional neurologic disorder (FND)","Other neurological disorders (not listed above)","--------------------------------------------------------------------------------","No neurological disorder was identified")
levels(allentry_adj$dx_alt1_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
#set unknown month to january
levels(allentry_adj$dx_alt1_month.factor)=c("January","February","March","April","May","June","July","August","September","October","November","December","January")
levels(allentry_adj$dx_alt2.factor)=c("Parkinsons disease","Parkinsonism, vascular","Parkinsonism, Neuroleptic/Drug-induced","Progressive supranuclear palsy (PSP)","Corticobasal degeneration (CBD)","Dementia with Lewy Bodies (DLB)","Multiple system atrophy (MSA)","Normal pressure hydrocephalus (NPH)","Parkinsonism, other (not listed above)","-----------------------------------------------------------------","Motor neuron disease (ALS)","Spinocerebellar ataxia (SCA)","Alzheimers Disease (AD)","Frontotemporal dementia (FTD)","Dementia (not listed above)","---------------------------------------------------------","Essential Tremor (ET)","Dopa-responsive dystonia (DRD)","Functional neurologic disorder (FND)","Other neurological disorders (not listed above)","--------------------------------------------------------------------------------","No neurological disorder was identified")
levels(allentry_adj$dx_alt2_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
levels(allentry_adj$dx_alt2_month.factor)=c("January","February","March","April","May","June","July","August","September","October","November","December","January")
levels(allentry_adj$dx_alt3.factor)=c("Parkinsons disease","Parkinsonism, vascular","Parkinsonism, Neuroleptic/Drug-induced","Progressive supranuclear palsy (PSP)","Corticobasal degeneration (CBD)","Dementia with Lewy Bodies (DLB)","Multiple system atrophy (MSA)","Normal pressure hydrocephalus (NPH)","Parkinsonism, other (not listed above)","-----------------------------------------------------------------","Motor neuron disease (ALS)","Spinocerebellar ataxia (SCA)","Alzheimers Disease (AD)","Frontotemporal dementia (FTD)","Dementia (not listed above)","---------------------------------------------------------","Essential Tremor (ET)","Dopa-responsive dystonia (DRD)","Functional neurologic disorder (FND)","Other neurological disorders (not listed above)","--------------------------------------------------------------------------------","No neurological disorder was identified")
levels(allentry_adj$dx_alt3_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
levels(allentry_adj$dx_alt3_month.factor)=c("January","February","March","April","May","June","July","August","September","October","November","December","January")
levels(allentry_adj$brady_hx.factor)=c("bradykinesia "," ","bradykinesia_q "," ")
levels(allentry_adj$rigidity_hx.factor)=c("rigidity "," ","rigidity_q "," ")
levels(allentry_adj$tremor_hx.factor)=c("tremor "," ","tremor_q "," ")
levels(allentry_adj$gait_hx.factor)=c("gait "," ","gait_q "," ")
levels(allentry_adj$brady_ex.factor)=c("bradykinesia "," ","bradykinesia_q "," ")
levels(allentry_adj$rigidity_ex.factor)=c("rigidity "," ","rigidity_q "," ")
levels(allentry_adj$tremor_ex.factor)=c("tremor "," ","tremor_q "," ")
levels(allentry_adj$gait_ex.factor)=c("gait "," ","gait_q "," ")
levels(allentry_adj$asymmetry.factor)=c("asymmetry "," ","asymmetry_q "," ")
levels(allentry_adj$rest_tremor.factor)=c("rest_tremor "," ","rest_tremor_q "," ")
levels(allentry_adj$pd_progression.factor)=c("Yes ","No (records document that patients parkinsonism is not progressing)","Questionable","Not documented")
levels(allentry_adj$dat_scan.factor)=c("DAT scan performed and shows dopaminergic deficit ","DAT scan performed and normal ","DAT scan performed and questionable or uncertain or not documented result ","Uncertain if DAT scan performed (DAT scan ordered and uncertain if done) ","No DAT Scan documented ")
levels(allentry_adj$dopamed_hx.factor)=c("YES, at least 1 year ","YES, LESS than 1 year ","NO, patient has not been on DA meds ","QUESTIONABLE - DA meds prescribed but uncertain or inconsistently taken or not sure duration","NOT DOCUMENTED - no information about DA meds  ")
levels(allentry_adj$dopamed_start_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
levels(allentry_adj$dopamed_start_month.factor)=c("January","January","February","March","April","May","June","July","August","September","October","November","December")
levels(allentry_adj$dopamed_last_year.factor)=c("0000","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1983","1982","1981","1980")
levels(allentry_adj$dopamed_last_month.factor)=c("January","January","February","March","April","May","June","July","August","September","October","November","December")
levels(allentry_adj$dopa_response.factor)=c("Documented benefit or response to dopaminergic medications","No response to dopaminergic medications...","Uncertain or questionable response documented...")
levels(allentry_adj$dopa_dose1000mg.factor)=c("History of 1000 mg of levodopa","No history of 1000 mg of l-dopa","Unknown and questionable history of 1000 mg of l-dopa")
levels(allentry_adj$chorea_dyskinesia.factor)=c("Yes","No","Questionable","Not Documented","Not applicable")
levels(allentry_adj$neuroleptic.factor)=c("YES, exposure to neuroleptic medications or medications that clinicians have concerns over contributing to neurologic condition","NO, There is documentation that states there was no exposure to neuroleptic medications ","QUESTIONABLE, Possible or uncertain neuroleptic exposure (add comments) ","NOT DOCUMENTED, No mention of neuroleptics in the medical record")
levels(allentry_adj$dementia.factor)=c("dementia ","  ","dementia_q ","  ")
levels(allentry_adj$hallucinations.factor)=c("hallucinations "," ","hallucinations_q "," ")
levels(allentry_adj$early_falls.factor)=c("early_falls "," ","early_falls_q "," ")
levels(allentry_adj$orthostatic_hypotension.factor)=c("orthostatic hypotension ","  ","orthostatic_hypotension_q "," ")
levels(allentry_adj$orthostatic_hypo_meds.factor)=c("orthostatic_hypo_meds ","  ","orthostatic_hypo_q "," ")
levels(allentry_adj$incontinence.factor)=c("incontinence "," ","incontinence_q ","  ")
levels(allentry_adj$head_injury.factor)=c("heady_injury "," ","head_injury_q "," ")
levels(allentry_adj$strokes.factor)=c("strokes "," ","strokes_q ","  ")
levels(allentry_adj$hydrocephalus_braintumor.factor)=c("hydrocephalus_braintumor ","   ","hydrocephalus_braintumor_q "," "," ")
levels(allentry_adj$encephalitis.factor)=c("encephalitis "," ","encephalitis_q "," ")
levels(allentry_adj$cerebellar.factor)=c("cerebellar "," ","cerebellar_q "," ")
levels(allentry_adj$supranuclear_gaze_palsy.factor)=c("supranuclear_gaze_palsy "," ","supranuclear_gaze_palsy_q "," ")
levels(allentry_adj$chart_problems___1.factor)=c(" ","There was not much information in the chart to answer most questions | ")
levels(allentry_adj$chart_problems___2.factor)=c(" ","There was less than 3 years of follow-up to review after the PD related Dx | ")
levels(allentry_adj$chart_problems___3.factor)=c(" ","Important information was found in scanned documents | ")
levels(allentry_adj$chart_problems___4.factor)=c(" ","Important information was found from electronic outside records | ")
levels(allentry_adj$chart_problems___5.factor)=c(" ","Chart review was complex or confusing or there were conflicting dx | ")
levels(allentry_adj$missing_info___1.factor)=c(" ","Most were relevant neurology outpatient visit & notes | ")
levels(allentry_adj$missing_info___2.factor)=c(" ","Most were neurology outside records or scanned documents | ")
levels(allentry_adj$missing_info___3.factor)=c(" ","Most did not include any useful information about parkinsonism | ")
levels(allentry_adj$missing_info___4.factor)=c(" ","Most neurology evals were >5 years ago | ")
levels(allentry_adj$missing_info___5.factor)=c(" ","There were large gaps missing (no neuro evals) among available neuro evaluations | ")
levels(allentry_adj$poss_pd_reason___1.factor)=c(" ","Too early to determine | ")
levels(allentry_adj$poss_pd_reason___2.factor)=c(" ","Missing or insufficient information to review | ")
levels(allentry_adj$poss_pd_reason___3.factor)=c(" ","Multiple medical co-morbidities (non-parkinsonian ones) to make it difficult to be certain |")
levels(allentry_adj$poss_pd_reason___4.factor)=c(" ","Neurodegenerative or other parkinsonisms are in the differential diagnosis | ")
levels(allentry_adj$poss_pd_reason___5.factor)=c(" ","Not PD, specific that the patient ONLY has non-neurodegenerative parkinsonism and does not have any neurodegenerative parkinsonism | ")
levels(allentry_adj$abstraction_records_complete.factor)=c("Incomplete","Unverified","Complete")

#combine fields
allentry_adj$symptoms_hx <- gsub(" ", " ", paste(allentry_adj$brady_hx.factor, allentry_adj$rigidity_hx.factor, allentry_adj$tremor_hx.factor, allentry_adj$gait_hx.factor))
allentry_adj$symptoms_ex <- gsub(" ", " ", paste(allentry_adj$brady_ex.factor, allentry_adj$rigidity_ex.factor, allentry_adj$tremor_ex.factor, allentry_adj$gait_ex.factor))
allentry_adj$symptoms_any <- gsub(" ", " ", paste(allentry_adj$asymmetry.factor, allentry_adj$rest_tremor.factor))
allentry_adj$redflags_list <- gsub(" ", " ", paste(allentry_adj$dementia.factor, allentry_adj$hallucinations.factor, allentry_adj$early_falls.factor, allentry_adj$orthostatic_hypotension.factor, allentry_adj$orthostatic_hypo_meds.factor, allentry_adj$incontinence.factor,
                                                   allentry_adj$head_injury.factor, allentry_adj$encephalitis.factor, allentry_adj$cerebellar.factor, allentry_adj$supranuclear_gaze_palsy.factor))
allentry_adj$chart_problems_list <- gsub(" ", " ", paste(allentry_adj$chart_problems___1.factor, allentry_adj$chart_problems___2.factor, allentry_adj$chart_problems___3.factor, allentry_adj$chart_problems___4.factor, allentry_adj$chart_problems___5.factor))
allentry_adj$missing_info_list <- gsub(" ", " ", paste(allentry_adj$missing_info___1.factor, allentry_adj$missing_info___2.factor, allentry_adj$missing_info___3.factor, allentry_adj$missing_info___4.factor, allentry_adj$missing_info___5.factor))
allentry_adj$poss_pd_reason_list <- gsub(" ", " ", paste(allentry_adj$poss_pd_reason___1.factor, allentry_adj$poss_pd_reason___2.factor, allentry_adj$poss_pd_reason___3.factor, allentry_adj$poss_pd_reason___4.factor, allentry_adj$poss_pd_reason___5.factor))

#calculate duration 
pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio)        # data import/export
class(allentry_adj$abstraction_date_1)
allentry_adj <- allentry_adj %>%
  mutate(abstraction_date_1 = as.Date(abstraction_date, format = "%Y"))
allentry_adj <- allentry_adj %>%
  mutate(dx_year.factor = as.Date(dx_year.factor, format = "%Y"))
allentry_adj$dx_duration <- allentry_adj$abstraction_date_1 - allentry_adj$dx_year.factor
allentry_adj$dopamed_duration <- allentry_adj$dopamed_last_year - allentry_adj$dopamed_start_year

#Reorder columns
allentry_adj <- allentry_adj %>% select(unique_id, record_id, cohort.factor, entry, abstraction_date, abstractor.factor, parkinsonism_year.factor, parkinsonism_month.factor, dx_primary.factor, 
                            dx_certainty.factor, dx_source.factor, dx_date, dx_date_certainty.factor, dx_alt1.factor, alt1_date, dx_alt2.factor, alt2_date, dx_alt3.factor, alt3_date,
                            symptoms_hx, symptoms_ex, symptoms_any, redflags_list, chart_problems_list, missing_info_list, poss_pd_reason_list, dx_duration, dopamed_duration, 
                            pd_prog_notes, dat_scan.factor, dopamed_hx.factor, dopa_response.factor, dopa_response_uncertain, 
                            dopa_dose1000mg.factor, chorea_dyskinesia.factor, neuroleptic.factor, neuroleptic_meds1, neuroleptic_meds2, neuroleptic_meds3, neuroleptic_meds4, dementia.factor, notes, everything())


#export files to csv format

write.csv(allentry_adj, file = "allentry_for_adjudication_20220628.csv")
