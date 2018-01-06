options(java.parameters = "-Xmx8000m")

library(ggplot2)
library(ggthemes)
library(scales)
library(dygraphs)
library(xts)
library(magrittr)
library(highcharter)
library(dplyr)
library(DT)
library(koboloadeR)
library(data.table)
library(RColorBrewer)
library(rdrop2)
library(rio)
library(fst)

token <- drop_auth()
saveRDS(token, file = "token.rds")
drop_acc() %>% data.frame()
drop_dir()
abc1 <- drop_download('hwc_1450_28112017.csv', overwrite = TRUE)

library(devtools, quietly=TRUE)
source_gist("https://gist.github.com/dfalster/5589956")
source(addNewData.R)

setwd("/home/girdhari/HWC Dashboard")



source("http://news.mrdwab.com/install_github.R")
devtools::install_github("mrdwab/koboloadeR")
library(koboloadeR)

kobo_datasets(user = c("tattva","tattva"), api = "https://kc.humanitarianresponse.info/api/v1/")

hwc_main <- kobo_data_downloader("153088",user = c("tattva","tattva"),
                                 api = "https://kc.humanitarianresponse.info/api/v1/",check = FALSE)

head(hwc_main)


# write.csv(hwc_main,"hwc_data_dump.csv")
hwc_main <- read.csv("hwc_main_04jan2018.csv", check.names = FALSE)
###FST read write
write.fst(hwc_main, "hwc_main.fst", 100)  
test_df <- read.fst("./Data/hwc_main.fst")

###FST read write


saveRDS(hwc_main, file = "hwc_main.rds", compress = FALSE)
hwc_main_for_testing <- readRDS("hwc_main2.rds")

export(hwc_main,"hwc_main1.rds")



# currentDate <- Sys.Date()
# csvFileName <- paste("hwc_data_dump_",currentDate,".csv",sep="")
# fwrite(hwc_main, file=csvFileName)
# hwc_main <- fread("hwc_data_dump_2017-08-12.csv", header = TRUE)

# datatable(hwc_main)
# typeof(hwc_main)

hwc_dt <- data.table(hwc_main)
class(hwc_dt)


# hh_tab <- hwc_dt[,.(subcentre, asha, household_number, total_members)]

sc_hh_summary <-  hwc_dt[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre)] ## remove numbers and underscore to show

sc_asha_hh_summary <-  hwc_dt[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)] ## remove numbers and underscore to show

asha_hh_summary <-  hwc_dt[, .(asha, village_name ,household_number, `group_hh_family_members[1]/name`,total_members )] ## we dont need ASHA name 

hwc_dt_temp <- hwc_dt

member_details <- function( row_num) {
  
  family_details <- hwc_dt_temp[row_num]
  hh_member_n <- data.table() # Create a blank data.table
  
  for (i in seq(family_details[,total_family_members])) {
    
    # print(i)
    
    field_1 <- paste0("group_hh_family_members","[",i,"]","/","name")
    field_2 <- paste0("group_hh_family_members","[",i,"]","/","relationship_with_hof")
    field_3 <- paste0("group_hh_family_members","[",i,"]","/","age")
    field_4 <- paste0("group_hh_family_members","[",i,"]","/","sex")
    field_5 <- paste0("group_hh_family_members","[",i,"]","/","marital_status")
    
    field_all <- c(field_1,field_2,field_3,field_4,field_5) 
    hh_member_temp <- family_details[, field_all, with=FALSE]
    
    # print(length(hh_member_temp))
    
    hh_member_n <- rbind(hh_member_n,  setnames(hh_member_temp, old = field_all, 
                                                new = c("Name","Relationship","Age","Sex","Marital Status"))) ## we dont need ASHA name 
  }
  return(hh_member_n)  
  
}


members_coded <- member_details(5)

allowedVars <- c("Relation","Gender","Marital.Status")

members_decoded <- addNewData("dataNew.csv", members_coded,allowedVars)
members_decoded$Age <- as.integer(members_coded$Age)



##################for group_bh conversion

class(group_bh)
allowedVars <- c("sex_child","twin_child")
members_decoded <- addNewData("dataNew1.csv",group_bh,allowedVars)


################################



## Look up value function START
## Look up value function START


# allowedVars <- c("Relation","Gender","Marital.Status")
# members_decoded <- addNewData("dataNew.csv", members_coded,allowedVars)

addNewData <- function(newDataFileName, data, allowedVars){
  
      import <- readNewData(newDataFileName, allowedVars)
    
        if( !is.null(import)){    
            for(i in seq_len(nrow(import))){  #Make replacements
                col.to <- import$newVariable[i] 
                col.from <- import$lookupVariable[i]
                if(is.na(col.from)){ # apply to whole column
                    data[col.to] <- import$newValue[i]
                  } else { # apply to subset
                      rows <- data[[col.from]] == import$lookupValue[i]
                      data[rows,col.to] <- import$newValue[i]
                    }
              }   
          }      
      data
    }

readNewData <- function(newDataFileName, allowedVars){
    
    if( file.exists(newDataFileName)){
          import <- read.csv(newDataFileName, header=TRUE, stringsAsFactors=FALSE,
                                                     strip.white=TRUE)
          if( nrow(import)> 0 ){
              
                #Check columns names for import are right
                expectedColumns<- c("lookupVariable","lookupValue","newVariable","newValue")
                nameIsOK <-  expectedColumns %in% names(import)
                if(any(!nameIsOK))
                    stop("Incorrect name in lookup table for ",
                                       newDataFileName, "--> ", paste(expectedColumns[!nameIsOK],collapse=", "))
                
                  #Check values of newVariable are in list of allowed variables
                  import$lookupVariable[import$lookupVariable == ""] <- NA
                  nameIsOK <- import$newVariable %in% allowedVars
                  if(any(!nameIsOK))
                      stop("Incorrect name(s) in newVariable column of ",
                                         newDataFileName, "--> ", paste(import$newVariable[!nameIsOK],collapse=", "))
                } else {
                    import <- NULL
                  }
        } else {
            import <- NULL
          }
    import
  }

## Look up value function END
## Look up value function END



member_details <- function() {
  
  hwc_temp <- hwc_main
 
  db_members <- setNames(data.table(matrix(nrow = 0, ncol = 63)),

  c("familyID","fam_member_id","subCentre","asha","village","hhNumber","totalMembers","name","relationship_with_hof","age","sex","marital_status","read_or_write","in_school_college",
    "years_education",

    "ncd_family_history",
    "smoke_tobacco",
    "start_age_smoke_tobacco",
    "drink_alcohol",
    "start_age_alcohol",
    "physical_activity",
    "physical_activity_7_days",
    "waist_size",

    # "ncd_diagnosed",
    "cataract","tb","high_bp","heart_disease","diabetes","leprosy","cancer","asthma","polio","paralysis","epilepsy","lung_disease","mental_illness","std_aids",
    "treatment_12_months","govt_facility","pvt_clinic","medicine_shop","quacks",
    "hospitalized_12_months","days_hospitalized","treatment_expense","tests_included","tests_expenditure","treatment_travel_expenses","insurance_covered_expenses",
    # "insurance_paid",
    # "ncd_symptoms_all",
    "shortness_of_breath","coughing_2_weeks","blood_in_sputum","history_of_fits","difficulty_in_opening_mouth","ulcers_2_weeks","change_tone_voice",
    "ncd_symptoms_women","lump_in_breast","blood_nipple_discharge","bleeding_between_periods","bleeding_after_menopause","bleeding_after_intercourse","foul_smelling_discharge"
    ))
  db_members1 <- db_members
  db_members_full <- db_members
  
  db_family <- setNames(data.table(matrix(nrow = 0, ncol = 42)),
                         
                         c("familyID","subCentre","asha","village","hhNumber","totalMembers",
                           
                           "religion","caste","drinking_water_source","toilet_type","house_type","electricity","bw_tv","colour_TV",
                           "refrigerator","mobile","landline","ac_cooler","bicycle","motor_cycle_scooter","car","water_pump","tractor",
                           
                           "any_health_insurance","rsby_card","any_death","any_u5_death","any_maternal_death",
                           
                           "q3_1a","q3_1b","q3_1c","q3_1d","q3_1e","q3_1f","q3_1g",
                           "q3_2a","q3_2b","q3_2c","q3_2d","q3_2e","q3_2f","q3_2g"
                         ))
  
  db_family1 <- db_family
  db_family_full <- db_family
  
  
  db_birth_history <- setNames(data.table(matrix(nrow = 0, ncol = 16)),
                        
                        c("familyID","fam_member_id","childID","subCentre","asha","village","hhNumber","motherName","totalMembers",
                          "child_name","sex_child","twin_child","dob_child","child_location","age_child_at_death","days_month_year"
                        ))
  
  db_birth_history1 <- db_birth_history
  db_birth_history_full <- db_birth_history
  
  
for(i in 1:nrow(hwc_temp))
     
 {
  
  print("MAIN LOOP######################")
  print(i)
  db_members <- db_members1
  
    for (j in 1:as.numeric(hwc_temp[i,total_family_members])) {
      
      tryCatch({

      print("2nd LOOP######################")
      print(j)

      blank_row <- data.table(matrix(NA_real_, nrow = 1, ncol = 63))
      db_members <- rbindlist(list(db_members,blank_row))

      db_members[j,]$familyID <- as.integer(i)
      db_members[j,]$fam_member_id <-as.integer(j)

      db_members[, subCentre:=as.character(subCentre)]
      db_members[j,]$subCentre <- as.character(hwc_temp[i, "subcentre", with=FALSE])

      db_members[, asha:=as.character(asha)]
      db_members[j,]$asha <- as.character(hwc_temp[i, "asha", with=FALSE])

      db_members[, village:=as.character(village)]
      db_members[j,]$village <- as.character(hwc_temp[i, "village_name", with=FALSE])

      # db_members[, village:=as.character(village)]
      db_members[j,]$hhNumber <- as.integer(hwc_temp[i, "household_number", with=FALSE])

      # db_members[, village:=as.character(village)]
      db_members[j,]$totalMembers <- as.integer(hwc_temp[i, "total_family_members", with=FALSE])

      field_1 <- paste0("group_hh_family_members","[",j,"]","/","name")
      db_members[, name:=as.character(name)]
      db_members[j,]$name <- as.character(hwc_temp[i, field_1, with=FALSE])

      field_2 <- paste0("group_hh_family_members","[",j,"]","/","relationship_with_hof")
      db_members[j,]$relationship_with_hof <- as.integer(hwc_temp[i, field_2, with=FALSE])

      field_3 <- paste0("group_hh_family_members","[",j,"]","/","age")
      db_members[j,]$age <- as.integer(hwc_temp[i, field_3, with=FALSE])

      field_4 <- paste0("group_hh_family_members","[",j,"]","/","sex")
      db_members[j,]$sex <- as.integer(hwc_temp[i, field_4, with=FALSE])

      # tryCatch({

      field_5 <- paste0("group_hh_family_members","[",j,"]","/","marital_status")
      db_members[j,]$marital_status <- as.integer(hwc_temp[i, field_5, with=FALSE])

      field_6 <- paste0("group_hh_family_members","[",j,"]","/","read_or_write")
      db_members[j,]$read_or_write <- as.integer(hwc_temp[i, field_6, with=FALSE])

      field_7 <- paste0("group_hh_family_members","[",j,"]","/","in_school_college")
      db_members[j,]$in_school_college <- as.integer(hwc_temp[i, field_7, with=FALSE])

      field_8 <- paste0("group_hh_family_members","[",j,"]","/","years_education")
      db_members[j,]$years_education <- as.integer(hwc_temp[i, field_8, with=FALSE])


      field_52 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","smoke_tobacco")
      db_members[j,]$smoke_tobacco <- as.integer(hwc_temp[i, field_52, with=FALSE])

      field_53 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","start_age_smoke_tobacco")
      db_members[j,]$start_age_smoke_tobacco <- as.integer(hwc_temp[i, field_53, with=FALSE])

      field_54 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","drink_alcohol")
      db_members[j,]$drink_alcohol <- as.integer(hwc_temp[i, field_54, with=FALSE])

      field_55 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","start_age_alcohol")
      db_members[j,]$start_age_alcohol <- as.integer(hwc_temp[i, field_55, with=FALSE])

      field_56 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","physical_activity")
      db_members[j,]$physical_activity <- as.integer(hwc_temp[i, field_56, with=FALSE])

      field_57 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","physical_activity_7_days")
      db_members[j,]$physical_activity_7_days <- as.integer(hwc_temp[i, field_57, with=FALSE])

      field_58 <- paste0("group_lifestyle","[",j,"]","/","group_lifestyle_001","/","waist_size")
      db_members[j,]$waist_size <- as.integer(hwc_temp[i, field_58, with=FALSE])



      field_9 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","ncd_family_history")
      db_members[j,]$ncd_family_history <- as.integer(hwc_temp[i, field_9, with=FALSE])
      
      field_10 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","cataract")
      db_members[j,]$cataract <- as.integer(hwc_temp[i, field_10, with=FALSE])

      field_11 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","tb")
      db_members[j,]$tb <- as.integer(hwc_temp[i, field_11, with=FALSE])

      field_12 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","high_bp")
      db_members[j,]$high_bp <- as.integer(hwc_temp[i, field_12, with=FALSE])

      field_13 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","heart_disease")
      db_members[j,]$heart_disease <- as.integer(hwc_temp[i, field_13, with=FALSE])

      field_14 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","diabetes")
      db_members[j,]$diabetes <- as.integer(hwc_temp[i, field_14, with=FALSE])

      field_15 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","leprosy")
      db_members[j,]$leprosy <- as.integer(hwc_temp[i, field_15, with=FALSE])

      field_16 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","cancer")
      db_members[j,]$cancer <- as.integer(hwc_temp[i, field_16, with=FALSE])

      field_17 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","asthma")
      db_members[j,]$asthma <- as.integer(hwc_temp[i, field_17, with=FALSE])

      field_18 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","polio")
      db_members[j,]$polio <- as.integer(hwc_temp[i, field_18, with=FALSE])

      field_19 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","paralysis")
      db_members[j,]$paralysis <- as.integer(hwc_temp[i, field_19, with=FALSE])

      field_20 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","epilepsy")
      db_members[j,]$epilepsy <- as.integer(hwc_temp[i, field_20, with=FALSE])

      field_21 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","lung_disease")
      db_members[j,]$lung_disease <- as.integer(hwc_temp[i, field_21, with=FALSE])

      field_22 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","mental_illness")
      db_members[j,]$mental_illness <- as.integer(hwc_temp[i, field_22, with=FALSE])

      field_23 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_major_morbidities_past","/","std_aids")
      db_members[j,]$std_aids <- as.integer(hwc_temp[i, field_23, with=FALSE])

      # tryCatch({
      field_24 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_12_months")
      db_members[j,]$treatment_12_months <- as.integer(hwc_temp[i, field_24, with=FALSE])


      # if(!is.na(db_members[j,]$treatment_12_months))
      # {
      # if ( db_members[j,]$treatment_12_months == 1  )
      # {
      field_25 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_from_where","/","govt_facility")
      db_members[j,]$govt_facility <- as.integer(hwc_temp[i, field_25, with=FALSE])

      field_26 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_from_where","/","pvt_clinic")
      db_members[j,]$pvt_clinic <- as.integer(hwc_temp[i, field_26, with=FALSE])

      field_27 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_from_where","/","medicine_shop")
      db_members[j,]$medicine_shop <- as.integer(hwc_temp[i, field_27, with=FALSE])

      field_28 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_from_where","/","quacks")
      db_members[j,]$pvt_clinic <- as.integer(hwc_temp[i, field_26, with=FALSE])
      # }


      field_29 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","hospitalized_12_months")
      db_members[j,]$hospitalized_12_months <- as.integer(hwc_temp[i, field_29, with=FALSE])

      # if(!is.na(db_members[j,]$treatment_12_months))

      field_30 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","days_hospitalized")
      db_members[j,]$days_hospitalized <- as.integer(hwc_temp[i, field_30, with=FALSE])

      field_31 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_expense")
      db_members[j,]$treatment_expense <- as.integer(hwc_temp[i, field_31, with=FALSE])

      field_32 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","tests_included")
      db_members[j,]$tests_included <- as.integer(hwc_temp[i, field_32, with=FALSE])

      field_33 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","tests_expenditure")
      db_members[j,]$tests_expenditure <- as.integer(hwc_temp[i, field_33, with=FALSE])

      field_34 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","treatment_travel_expenses")
      db_members[j,]$treatment_travel_expenses <- as.integer(hwc_temp[i, field_34, with=FALSE])

      field_35 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","insurance_covered_expenses")
      db_members[j,]$insurance_covered_expenses <- as.integer(hwc_temp[i, field_35, with=FALSE])

      # field_36 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_history","/","group_treatment_costs","/","insurance_paid")
      # db_members[j,]$insurance_paid <- as.integer(hwc_temp[i, field_36, with=FALSE])

      # }


      # field_37 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","ncd_symptoms_all")
      field_38 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","shortness_of_breath")
      db_members[j,]$shortness_of_breath <- as.integer(hwc_temp[i, field_38, with=FALSE])

      field_39 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","coughing_2_weeks")
      db_members[j,]$coughing_2_weeks <- as.integer(hwc_temp[i, field_39, with=FALSE])

      field_40 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","blood_in_sputum")
      db_members[j,]$blood_in_sputum <- as.integer(hwc_temp[i, field_40, with=FALSE])

      field_41 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","history_of_fits")
      db_members[j,]$history_of_fits <- as.integer(hwc_temp[i, field_41, with=FALSE])

      field_42 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","difficulty_in_opening_mouth")
      db_members[j,]$difficulty_in_opening_mouth <- as.integer(hwc_temp[i, field_42, with=FALSE])

      field_43 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","ulcers_2_weeks")
      db_members[j,]$ulcers_2_weeks <- as.integer(hwc_temp[i, field_43, with=FALSE])

      field_44 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_all","/","change_tone_voice")
      db_members[j,]$change_tone_voice <- as.integer(hwc_temp[i, field_44, with=FALSE])


      # field_45 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","ncd_symptoms_women")
      # db_members[j,]$ncd_symptoms_women <- as.integer(hwc_temp[i, field_45, with=FALSE])

      field_46 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","lump_in_breast")
      db_members[j,]$lump_in_breast <- as.integer(hwc_temp[i, field_46, with=FALSE])

      field_47 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","blood_nipple_discharge")
      db_members[j,]$blood_nipple_discharge <- as.integer(hwc_temp[i, field_47, with=FALSE])

      field_48 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","bleeding_between_periods")
      db_members[j,]$bleeding_between_periods <- as.integer(hwc_temp[i, field_48, with=FALSE])

      field_49 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","bleeding_after_menopause")
      db_members[j,]$bleeding_after_menopause <- as.integer(hwc_temp[i, field_49, with=FALSE])

      field_50 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","bleeding_after_intercourse")
      db_members[j,]$bleeding_after_intercourse <- as.integer(hwc_temp[i, field_50, with=FALSE])

      field_51 <- paste0("group_lifestyle","[",j,"]","/","group_ncd_symptoms_women","/","foul_smelling_discharge")
      db_members[j,]$foul_smelling_discharge <- as.integer(hwc_temp[i, field_51, with=FALSE])


     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    
    if (hwc_temp[i, "selected_woman_name", with=FALSE] == db_members[j,]$name)
          
    {
      print(hwc_temp[i, "selected_woman_name", with=FALSE] )
      print(db_members[j,]$name)
      
      col1 <- colnames(hwc_temp[i,])
      tt <- col1[grepl("group_bh", col1)]
      temp2 <- gregexpr("[0-9]+", tt) 
      max_k <- max(as.numeric(unique(unlist(regmatches(tt, temp2)))))
      
      db_birth_history <- db_birth_history1
      
      
        
      for (k in 1:max_k)
        
      {
        print("3rd LOOP######################")
        print(k)
        
        cfield_0 <- paste0("group_bh","[",k,"]","/","child_count")
        check <- as.integer(hwc_temp[i, cfield_0, with=FALSE])
        
        print("CHILD COUNT ######################")
        print(check)
        
        if ( !is.na(check) )
        {
          
        tryCatch({
          
        blank_row <- data.table(matrix(NA_real_, nrow = 1, ncol = 16))
        db_birth_history <- rbindlist(list(db_birth_history,blank_row))
      
        db_birth_history[k,]$familyID <- as.integer(i)
        db_birth_history[k,]$fam_member_id <- as.integer(j)
        db_birth_history[k,]$childID <- as.integer(k)
      
        db_birth_history[, subCentre:=as.character(subCentre)]
        db_birth_history[k,]$subCentre <- as.character(hwc_temp[i, "subcentre", with=FALSE])
      
        db_birth_history[, asha:=as.character(asha)]
        db_birth_history[k,]$asha <- as.character(hwc_temp[i, "asha", with=FALSE])
      
        db_birth_history[, village:=as.character(village)]
        db_birth_history[k,]$village <- as.character(hwc_temp[i, "village_name", with=FALSE])
        
        db_birth_history[, motherName:=as.character(motherName)]
        db_birth_history[k,]$motherName <- as.character(hwc_temp[i, "selected_woman_name", with=FALSE])
        
        # db_members[, village:=as.character(village)]
        db_birth_history[k,]$hhNumber <- as.integer(hwc_temp[i, "household_number", with=FALSE])
      
        # db_members[, village:=as.character(village)]
        db_birth_history[k,]$totalMembers <- as.integer(hwc_temp[i, "total_family_members", with=FALSE])
      
        cfield_1 <- paste0("group_bh","[",k,"]","/","child_name")
        db_birth_history[, child_name:=as.character(child_name)]
        db_birth_history[k,]$child_name <- as.character(hwc_temp[i, cfield_1, with=FALSE])
      
        cfield_2 <- paste0("group_bh","[",k,"]","/","sex_child")
        db_birth_history[k,]$sex_child <- as.integer(hwc_temp[i, cfield_2, with=FALSE])
      
        cfield_3 <- paste0("group_bh","[",k,"]","/","sex_child")
        db_birth_history[k,]$sex_child <- as.integer(hwc_temp[i, cfield_3, with=FALSE])
      
        cfield_4 <- paste0("group_bh","[",k,"]","/","sex_child")
        db_birth_history[k,]$sex_child <- as.integer(hwc_temp[i, cfield_4, with=FALSE])
      
        cfield_5 <- paste0("group_bh","[",k,"]","/","twin_child")
        db_birth_history[k,]$twin_child <- as.integer(hwc_temp[i, cfield_5, with=FALSE])
      
        cfield_6 <- paste0("group_bh","[",k,"]","/","dob_child")
        db_birth_history[, dob_child:=as.character(dob_child)]
        db_birth_history[k,]$dob_child <- as.character(hwc_temp[i, cfield_6, with=FALSE])
      
        cfield_7 <- paste0("group_bh","[",k,"]","/","child_location")
        db_birth_history[k,]$child_location <- as.integer(hwc_temp[i, cfield_7, with=FALSE])
      
        cfield_8 <- paste0("group_bh","[",k,"]","/","age_child_at_death")
        db_birth_history[k,]$age_child_at_death <- as.integer(hwc_temp[i, cfield_8, with=FALSE])
      
        cfield_9 <- paste0("group_bh","[",k,"]","/","days_month_year")
        db_birth_history[k,]$days_month_year <- as.integer(hwc_temp[i, cfield_9, with=FALSE])
        
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

        }
        
        else {break}
        
      }
        
    
      
      db_birth_history_full <- rbindlist(list(db_birth_history_full,db_birth_history))
    } 
      
  }
  
   db_members_full <- rbindlist(list(db_members_full,db_members))

   ##############################################################################################
    tryCatch({

    blank_row_f <- data.table(matrix(NA_real_, nrow = 1, ncol = 42))
    db_family <- rbindlist(list(db_family,blank_row_f))
   
   db_family[i,]$familyID <- as.integer(i)
   
    db_family[, subCentre:=as.character(subCentre)]
    db_family[i,]$subCentre <- as.character(hwc_temp[i, "subcentre", with=FALSE])
   
    db_family[, asha:=as.character(asha)]
    db_family[i,]$asha <- as.character(hwc_temp[i, "asha", with=FALSE])
   
    db_family[, village:=as.character(village)]
    db_family[i,]$village <- as.character(hwc_temp[i, "village_name", with=FALSE])
   
    db_family[i,]$hhNumber <- as.integer(hwc_temp[i, "household_number", with=FALSE])
   
    db_family[i,]$totalMembers <- as.integer(hwc_temp[i, "total_family_members", with=FALSE])
   
    f_field_1 <- paste0("group_hh_family","/","religion")
   db_family[i,]$religion <- as.integer(hwc_temp[i, f_field_1, with=FALSE])
   
    f_field_2 <- paste0("group_hh_family","/","caste")
    db_family[i,]$caste <- as.integer(hwc_temp[i, f_field_2, with=FALSE])
   
    f_field_3 <- paste0("group_hh_family","/","drinking_water_source")
    db_family[i,]$drinking_water_source <- as.integer(hwc_temp[i, f_field_3, with=FALSE])
   
    f_field_4 <- paste0("group_hh_family","/","toilet_type")
    db_family[i,]$toilet_type <- as.integer(hwc_temp[i, f_field_4, with=FALSE])
   
    f_field_5 <- paste0("group_hh_family","/","house_type")
    db_family[i,]$house_type <- as.integer(hwc_temp[i, f_field_5, with=FALSE])
   
    f_field_6 <- paste0("group_hh_family","/","group_household_goods_list","/","electricity")
    db_family[i,]$electricity <- as.integer(hwc_temp[i, f_field_6, with=FALSE])
   
    f_field_7 <- paste0("group_hh_family","/","group_household_goods_list","/","bw_tv")
    db_family[i,]$bw_tv <- as.integer(hwc_temp[i, f_field_7, with=FALSE])
   
    f_field_8 <- paste0("group_hh_family","/","group_household_goods_list","/","colour_TV")
    db_family[i,]$colour_TV <- as.integer(hwc_temp[i, f_field_8, with=FALSE])
   
    f_field_9 <- paste0("group_hh_family","/","group_household_goods_list","/","refrigerator")
    db_family[i,]$refrigerator <- as.integer(hwc_temp[i, f_field_9, with=FALSE])
   
    f_field_10 <- paste0("group_hh_family","/","group_household_goods_list","/","mobile")
    db_family[i,]$mobile <- as.integer(hwc_temp[i, f_field_10, with=FALSE])
   
   
    f_field_11 <- paste0("group_hh_family","/","group_household_goods_list","/","landline")
    db_family[i,]$landline <- as.integer(hwc_temp[i, f_field_11, with=FALSE])
   
    f_field_12 <- paste0("group_hh_family","/","group_household_goods_list","/","ac_cooler")
    db_family[i,]$ac_cooler <- as.integer(hwc_temp[i, f_field_12, with=FALSE])
   
    f_field_13 <- paste0("group_hh_family","/","group_household_goods_list","/","bicycle")
    db_family[i,]$bicycle <- as.integer(hwc_temp[i, f_field_13, with=FALSE])
   
    f_field_14 <- paste0("group_hh_family","/","group_household_goods_list","/","motor_cycle_scooter")
    db_family[i,]$motor_cycle_scooter <- as.integer(hwc_temp[i, f_field_14, with=FALSE])
   
    f_field_15 <- paste0("group_hh_family","/","group_household_goods_list","/","car")
    db_family[i,]$car <- as.integer(hwc_temp[i, f_field_15, with=FALSE])
   
    f_field_16 <- paste0("group_hh_family","/","group_household_goods_list","/","water_pump")
    db_family[i,]$water_pump <- as.integer(hwc_temp[i, f_field_16, with=FALSE])
   
    f_field_17 <- paste0("group_hh_family","/","group_household_goods_list","/","tractor")
    db_family[i,]$tractor <- as.integer(hwc_temp[i, f_field_17, with=FALSE])
   
   
    f_field_18 <- paste0("group_hh_family","/","any_health_insurance")
    db_family[i,]$any_health_insurance <- as.integer(hwc_temp[i, f_field_18, with=FALSE])
   
    f_field_19 <- paste0("group_hh_family","/","rsby_card")
    db_family[i,]$rsby_card <- as.integer(hwc_temp[i, f_field_19, with=FALSE])
   
    f_field_20 <- paste0("group_hh_family","/","any_death")
    db_family[i,]$any_death <- as.integer(hwc_temp[i, f_field_20, with=FALSE])
   
    f_field_21 <- paste0("group_hh_family","/","any_u5_death")
    db_family[i,]$any_u5_death <- as.integer(hwc_temp[i, f_field_21, with=FALSE])
   
    f_field_22 <- paste0("group_hh_family","/","any_maternal_death")
    db_family[i,]$any_maternal_death <- as.integer(hwc_temp[i, f_field_22, with=FALSE])
   
   
    f_field_23 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1a")
    db_family[i,]$q3_1a <- as.integer(hwc_temp[i, f_field_23, with=FALSE])
   
    f_field_24 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1b")
    db_family[i,]$q3_1b <- as.integer(hwc_temp[i, f_field_24, with=FALSE])
   
    f_field_25 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1c")
    db_family[i,]$q3_1c <- as.integer(hwc_temp[i, f_field_25, with=FALSE])
   
    f_field_26 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1d")
    db_family[i,]$q3_1d <- as.integer(hwc_temp[i, f_field_26, with=FALSE])
   
    f_field_27 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1e")
    db_family[i,]$q3_1e <- as.integer(hwc_temp[i, f_field_27, with=FALSE])
   
    f_field_28 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1f")
    db_family[i,]$q3_1f <- as.integer(hwc_temp[i, f_field_28, with=FALSE])
   
    f_field_29 <- paste0("group_hh_family","/","group_hh_exp_30_days","/","q3_1g")
    db_family[i,]$q3_1g <- as.integer(hwc_temp[i, f_field_29, with=FALSE])
   
    f_field_30 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2a")
    db_family[i,]$q3_2a <- as.integer(hwc_temp[i, f_field_30, with=FALSE])
   
    f_field_31 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2b")
    db_family[i,]$q3_2b <- as.integer(hwc_temp[i, f_field_31, with=FALSE])
   
    f_field_32 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2c")
    db_family[i,]$q3_2c <- as.integer(hwc_temp[i, f_field_32, with=FALSE])
   
    f_field_33 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2d")
    db_family[i,]$q3_2d <- as.integer(hwc_temp[i, f_field_33, with=FALSE])
   
    f_field_34 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2e")
    db_family[i,]$q3_2e <- as.integer(hwc_temp[i, f_field_34, with=FALSE])
   
    f_field_35 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2f")
    db_family[i,]$q3_2f <- as.integer(hwc_temp[i, f_field_35, with=FALSE])
   
    f_field_36 <- paste0("group_hh_family","/","group_hh_exp_12_months","/","q3_2g")
    db_family[i,]$q3_2g <- as.integer(hwc_temp[i, f_field_36, with=FALSE])
   
     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
   
  ##########################################################################################################
   
   
}
  all_db <- list(db_members_full, db_family,db_birth_history_full)
  return(all_db)

}
     

all_hwc_records <- member_details()

write.csv(all_hwc_records[1],"group_hh_members_details.csv")
write.csv(all_hwc_records[2],"group_hh_details.csv")
write.csv(all_hwc_records[3],"group_bh.csv")



bb.1 <- as.data.frame(all_hwc_records[1]) 
bb.2 <- as.data.frame(all_hwc_records[2]) 
bb.3 <- as.data.frame(all_hwc_records[3]) 
# 
# group_bh <- read.csv("./Data/group_bh.csv", check.names = FALSE, stringsAsFactors = FALSE)




col1 <- colnames(hwc_main)

abc <- grepl("group_bh.*child_count" , col1)

tt <- col1[grepl("group_bh", col1)]

temp2 <- gregexpr("[0-9]+", tt) 

as.numeric(unique(unlist(regmatches(tt, temp2))))





################################# TIME BASED FILTERING

dt <- as.POSIXct(hwc_main$start)

start_date_time.df = data.frame(
  date=dt,
  time=format(dt, "%H:%M")
)

reference_date <- as.POSIXct("2017-08-16 00:00:00 IST")

hwc_temp1 <- hwc_main

hwc_temp1$start <- as.POSIXct(hwc_temp1$start)
hwc_temp1$end <- as.POSIXct(hwc_temp1$end)

hwc_temp2 <- filter(hwc_temp1, start > reference_date)

timediff <-  as.numeric(difftime(hwc_temp2$end,hwc_temp2$start,units="mins"))


hist(timediff)
mean(timediff)

fivenum(timediff)
median(timediff)

################################# TIME BASED FILTERING



hh_member_details <- read.csv("./Data/group_hh_members_details.csv")
hh_details <- read.csv("./Data/group_hh_details.csv")
birth_history <- read.csv(all_hwc_records[3],"group_bh.csv")



################################################## HIGH RISK CASES CHARTING ############################

hh_details <- read.csv("./Data/group_hh_details.csv")
hh_member_details <- read.csv("./Data/group_hh_members_details.csv")

member_gt_30years <- filter(hh_member_details, age >= 30 )

member_gt_30years_score <- mutate(member_gt_30years, 
                                  age_score = ifelse((age>=30 & age <=39),0, ifelse((age>=40 & age <=49),1,2)),
                                  smoke_tobacco_score = ifelse(smoke_tobacco == 1,0, ifelse((smoke_tobacco == 2 | smoke_tobacco == 3),1,2)),
                                  alcohol_score = ifelse( drink_alcohol == 4, 1,0),
                                  waist_score = ifelse( (waist_size <= 79 & sex == 2),0,
                                                        ifelse((waist_size >= 80 & waist_size <= 90 & sex == 2), 1,
                                                               ifelse((waist_size >90 & sex == 2),2,
                                                                      ifelse( (waist_size < 90 & sex == 1),0,
                                                                              ifelse((waist_size >= 90 & waist_size <= 100 & sex == 1), 1,2))))),
                                  physical_activity_score = ifelse(physical_activity == 1, 0,1),
                                  ncd_history_score = ifelse( ncd_family_history == 1,2,0 ),
                                  ncd_total_score = age_score + smoke_tobacco_score + alcohol_score + waist_score + physical_activity_score 
                                  + ncd_history_score
                                  
)

ncd_high_risk_cases <- filter( member_gt_30years_score, ncd_total_score >4 )

ncd_referral_cases <- filter(ncd_high_risk_cases, shortness_of_breath == 1|coughing_2_weeks == 1 | blood_in_sputum ==1 |
                               history_of_fits ==1 | difficulty_in_opening_mouth == 1 | ulcers_2_weeks ==1 | 
                               change_tone_voice ==1 | lump_in_breast == 1 | blood_nipple_discharge == 1 |
                               bleeding_between_periods == 1 | bleeding_after_menopause == 1 | bleeding_after_intercourse == 1 |
                               foul_smelling_discharge == 1
)

################################################## HIGH RISK CASES CHARTING ############################

bar_ncd_high_risk <- ncd_high_risk_cases %>%
                      # order_by(subCentre,asha,hhNumber,name) %>%
                          group_by(subCentre)  %>%
                            # order_by(subCentre)
                              summarise(N_sc=n()) %>%
                                     arrange(desc(N_sc)) %>%
                                        mutate(drilldown = tolower(subCentre))

colnames(bar_ncd_high_risk)[1] <- "name"
colnames(bar_ncd_high_risk)[2] <- "y"
bar_ncd_high_risk$name <- as.character(bar_ncd_high_risk$name)


bar_ncd_high_risk_asha <- ncd_high_risk_cases %>%
                             group_by(asha)  %>%
                                   summarise(N_asha = n())

ncd_high_risk_asha_sc <- select(ncd_high_risk_cases, subCentre, asha, village) 
                            
asha_sc_mapping <- left_join(bar_ncd_high_risk_asha,ncd_high_risk_asha_sc, by = "asha" )

colnames(asha_sc_mapping)[1] <- "name"
colnames(asha_sc_mapping)[2] <- "value"
asha_sc_mapping$name <- as.character(asha_sc_mapping$name)
asha_sc_mapping$subCentre <- as.character(asha_sc_mapping$subCentre)
asha_sc_mapping$village <- as.character(asha_sc_mapping$village)

# chart.yAxis[0].axisTitle.attr({
#   text: 'new title'
# });



hc <- highchart() %>% 
    hc_chart(type = "bar",
             events = list(
               drilldown = 
                 JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name});
             } '))
             ) %>%
    hc_title(text = "High Risk Cases - NCDs") %>%
    hc_xAxis(type = "category",
             title = list(text = "Health & Wellness Centres")
             ) %>%
    hc_yAxis(title = list(text = "No. of Cases")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE),
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(bar_ncd_high_risk)))
      )
    ) %>%  
      
    hc_add_series(
      data = list_parse(bar_ncd_high_risk),
      name = "High Risk Cases - HWC wise",
      colorByPoint = TRUE
    )


  series_list <- vector("list",0)
  
  for (i in 1:nrow(bar_ncd_high_risk))
  {
    series_list[[i]] <- list( id = bar_ncd_high_risk$drilldown[i] , 
                              name = "High Risk Cases",
                                data = list_parse
                                (data_frame(
                                  name = unique(filter(asha_sc_mapping, tolower(subCentre) == bar_ncd_high_risk$drilldown[i]) %>% select(name))$name,
                                  y = unique(filter(asha_sc_mapping, tolower(subCentre) == bar_ncd_high_risk$drilldown[i]) %>% select(value))$value)
                                  )
                                )
  }

  
hc <- hc %>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list
       )
  

hc

  

################################################## DRILL-DOWN .... HIGH RISK CASES CHARTING ############################  
  
  
#   
# observeEvent(input$canvasClicked, {
#   
#   index1 <- input$canvasClicked[1]
#   index2 <- as.numeric(input$canvasClicked[2]) + 1
#   # index3 <- input$canvasClicked[3]
#   
#   print(index1)
#   print(index2)
#   # print(index3)
#   
#   ncd_df()
#   
# })
# 
# 
# ncd_df <- reactive({
#   
#   
#   index1 <- input$canvasClicked[1]
#   index2 <- as.numeric(input$canvasClicked[2]) + 1
#   
#   
#   df_subset <- ncd_high_risk_cases
#   
#   if(!is.null(index1))
#   {
#     if(index1 == "High Risk Cases:HWC wise")
#     { 
#       df_subset <- filter(ncd_high_risk_cases, subCentre == bar_ncd_high_risk[index2,]$name)
#       df_subset_temp <<- df_subset
#       
#       return (df_subset)
#       
#     }
#     
#     else if (index1 == "High Risk Cases:ASHA wise")
#     {
#       df_asha_names <- df_subset_temp %>%
#         group_by(asha)  %>%
#         summarise(N_asha = n()) %>% 
#         arrange(desc(N_asha))
#       
#       df_subset <- filter(df_subset_temp, asha == as.character(df_asha_names$asha[index2]))
#       return (df_subset)
#     }
#   }
#   else return (df_subset)
#   
# })
# 
# 
# 
# output$highriskCases = DT::renderDataTable(
#   
#   ncd_df()
#   
# )
  
  
  
summary(hwc_main)








