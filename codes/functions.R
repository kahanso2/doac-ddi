

# About
# In this script, I will define functions to be used for our work

# Function to merge overlapping enrollment periods efficiently using data.table
merge_intervals_dt <- function(enrollment_data) {
  setDT(enrollment_data)
  enrollment_data <- enrollment_data[order(ENROLID, DTSTART)]
  enrollment_data[, `:=`(start = shift(DTEND, 1, type = "lag") + 1, end = DTSTART), by = ENROLID]
  enrollment_data[, group := cumsum(start > end), by = ENROLID]
  merged <- enrollment_data[, .(DTSTART = min(DTSTART), DTEND = max(DTEND)), by = .(ENROLID, group)]
  return(merged)
}

# Optimized function to evaluate continuous enrollment
evaluate_continuous_enrollment <- function(enrollment_data, index_dates, months_prior) {
  setDT(enrollment_data)
  setDT(index_dates)
  
  enrollment_data[, `:=`(DTSTART = as.Date(DTSTART), DTEND = as.Date(DTEND))]
  index_dates[, `:=`(index_date = as.Date(index_date), start_date = index_date %m-% months(months_prior))]
  
  # Merge overlapping intervals
  merged_enrollment <- merge_intervals_dt(enrollment_data)
  
  # Initialize the progress bar
  pb <- progress_bar$new(
    total = nrow(index_dates),
    format = "  [:bar] :current/:total (:percent) in :elapsed eta: :eta"
  )
  
  # Check for continuous enrollment
  continuous_enrollment <- index_dates[, {
    pb$tick()
    
    # Filter merged_enrollment for current ENROLID
    enroll_periods <- merged_enrollment[ENROLID == .BY$ENROLID]
    
    # Check if all dates between start_date and index_date are covered by enrollment periods
    is_enrolled <- TRUE
    for (i in seq_len(.N)) {
      period_covered <- any(enroll_periods$DTSTART <= start_date[i] & enroll_periods$DTEND >= index_date[i])
      if (!period_covered) {
        is_enrolled <- FALSE
        break
      }
    }
    
    list(is_continuously_enrolled = is_enrolled)
  }, by = .(ENROLID)]
  
  return(continuous_enrollment)
}







# Continuous enrollment function
GetContinuousEnrollmentCohort2 <- function(data, days_before, days_after, max_allowed_days_gap){
  
  # Stage 1
  data |> 
    filter(!is.na(ENROLID)) |> 
    pivot_longer(cols = MEMDAY1:MEMDAY12) |> 
    filter(value != 0) |> 
    mutate(month = parse_number(name), 
           enrollment_date = my(paste(month, YEAR, sep = "-"))) |> 
    
    # Stage 2
    compute.treatment.episodes(
      ID.colname = "ENROLID",
      event.date.colname = "enrollment_date",
      event.duration.colname = "value",
      followup.window.start = 0,
      followup.window.duration = 11*365,
      maximum.permissible.gap = max_allowed_days_gap,
      medication.change.means.new.treatment.episode = F
    ) |> 
    
    # Stage 3
    left_join(data |> distinct(ENROLID, index_date)) |> 
    mutate(
      begin = index_date - days(days_before),
      end = index_date + days(days_after)
    )|> 
    mutate(test = 
             ifelse((interval(begin, index_date) %within% interval(episode.start, episode.end)) & episode.duration >= days_before,1, 0
             ),
           test2 = ifelse((interval(index_date, end) %within% interval(episode.start, episode.end)) & episode.duration >= days_after,1, 0
           )
    ) |> 
    filter(test ==1, test2 == 1)
}  


###Outcome variables
#Rules: 
  #"Indicating" Dx code required to be principle discharge diagnosis
  #"Potential"  Diagnosis code required to be principal discharge diagnosis 
    #with either 1) a diagnosis code indicating bleeding as a secondary diagnosis or 
    #2) a revenue code indicating a transfusion. Transfusion not sufficient if 
  #specific rules continued in supplement of manuscript. Add later. 

##Bleeds

###ICD-9
####Diagnosis codes "indicating" bleeding - Dx code required to be principal discharge diagnosis - QCd 6.14.24
gib_icd9_ind <- c("^5310.*", "^5312.*", "^5314.*", "^5316.*", "^5320.*", "^5322.*", "^5324.*", "^5326.*", "^5330.*", "^5332.*", "^5334.*",
                  "^5336.*", "^5340.*", "^5342.*", "^5344.*", "^5346.*", "^53501", "^53511", "^53521", "^53531", "^53541",
                  "^53551", "^53561", "^53783", "^4560", "^45620", "^5307", "^53021", "^53082", "^5780", "^4552", "^4555",
                  "^4558", "^56202", "^56203", "^56212", "^56213", "^56881", "^5693", "^56985", "^5781", "^5789")

gu_icd9_ind <- c("^59381", "^5997.*", "^6238", "^6266")

cerebral_icd9_ind <- c("^430", "^431", "^4320", "^4321", "^4329")

other_icd9_ind <- c("^4230", "^4590", "^56881", "^7191.*", "^7847", "^7848", "^7863.*")

all_icd9_bleeds_ind <- c(gib_icd9_ind, gu_icd9_ind, cerebral_icd9_ind, other_icd9_ind)
all_icd9_bleeds_ind <- paste(all_icd9_bleeds_ind, collapse = "|")

####Diagnosis Codes "Suggestive" of Possible Bleeding
gib_icd9_possible <- c("^5311.*", "^5313.*", "^5315.*", "^5317.*", "^5319.*", "^5321.*", "^5313.*",
                       "^5315.*", "^5317.*", "^5329.*", "^5331.*", "^5333.*", "^5335.*", "^5337.*",
                       "^5339.*", "^5341.*", "^5343.*", "^5345.*", "^5347.*", "^5349.*", "^53500",
                       "^53510", "^53520", "^53530", "^53540", "^53550", "^53560", "^56200",
                       "^56201", "^56210", "^56211", "^4550", "^4551", "^4553", "^4554", "^4556",
                       "^4557", "^4559", "^5301.*", "^53020")
all_gib_icd9_possible <- paste(gib_icd9_possible, collapse = "|")

unspec_icd9_possible <- c("^2851", "^2800", "^2859", "^79092", "^28749", "^2875")
all_unspec_icd9_possible <- paste(unspec_icd9_possible, collapse = "|")

gu_icd9_possible <- "^6262"

all_icd9_bleeds_possible <- c(gib_icd9_possible, unspec_icd9_possible, gu_icd9_possible)
all_icd9_bleeds_possible <- paste(all_icd9_bleeds_possible, collapse = "|")

##Additional dx codes for rule in diagnoses
anemia <- c("^2880" ,"^2851", "^2859")
orthostasis <- "^4580"
syncope <- "^7802"
comb_sec <- c(anemia, orthostasis, syncope)
all_comb_sec <- paste(comb_sec, collapse = "|")

#Trauma Exclusions
trauma_icd9 <- c("^800.*", "^801.*", "^802.*", "^803.*", "^804.*", "^805.*", "^806.*", "^807.*", "^808.*",
            "^809.*", "^810.*", "^811.*", "^812.*", "^813.*", "^818.*", "^819.*", "^820.*", "^821.*",
            "^822.*", "^823.*", "^824.*", "^827.*", "^828.*", "^829.*", "^860.*", "^8620.*", 
            "^8621.*", "^8628.*", "^8629.*", "^8630.*", "^8631.*", "^8632.*", "^8633.*", "^8634.*", "^8635.*",
            "^8638.*", "^8639.*", "^864.1.*","^865.1.*", "^866.*", "^867.*", "^8730.*", "^8731.*", "^8750.*", 
            "^8751.*", "^9024.*", "^90255", "^90256", "^90281", "^90282", "^925.*", "^926.*", "^927.*",
            "^928.*", "^929.*", "^9584.*", "^9585.*", "^9587.*", "^9967.*", "^99811", "^99812", "^9982.*",
            "^E805.*", "^E870.*", "^E881.*", "^E882.*", "^E883.*", "^E922.*", "^E923.*", "^E955.*", "^E960.*",
            "^E965.*", "^E970.*", "^E985.*" )

trauma_hcpcs <- c("^62000", "^62005", "^62010")
trauma_check_icd9 <- paste(trauma_icd9, collapse = "|")





###############   ICD-10   ##################

#Dx codes indicating bleeding
gib_icd10_ind <- c("^K250", "^K252", "^K254", "^K256", "^K260", "^K262", "^K264", "^K266",
               "^K270", "^K272", "^K274", "^K276","^K280", "^K282", "^K284", "^K286", 
               "^K2901", "^K2921", "^K2931", "^K2941", "^K2951", "^K2961", "^K2971", 
               "^K2981", "^K2991", "^K31811", "^I8501", "^I8511","^K2211", "^K226", "^K920",
               "^K5701", "^K5711", "^K5713", "^K5721", "^K5731", "^K5733", "^K5741", 
               "^K5751", "^K5753", "^K5781", "^K5791", "^K5793", "^K661", "^K625", "^K5521", "^K921", "^K922")

gu_icd10_ind <- c("^N280", "^R310", "^R311", "^R3121", "^R3129", "^R319", "^N898", "^N921")

cerebral_icd10_ind <- c("^I6000", "^I6001", "^I6002", "^I6010", "^I6011", "^I6012", "^I602", "^I6030", 
                    "^I6031", "^I6032", "^I604", "^I6050", "^I6051", "^I606", "^I607", "^I608", 
                    "^I609", "^I610", "^I611", "^I612", "^I613", "^I614", "^I615", "^I616", "^I618", 
                    "^I619", "^I621", "^I6200", "^I6201", "^I6202", "^I6203", "^I629")

other_icd10_ind <- c("^I312", "^R58", "^K661", "^M2500", "^M25011", "^M25012", "^M25019", "^M25021", "^M25022", "^M25029", 
                 "^M25031", "^M25032", "^M25039", "^M25041", "^M25042", "^M25049", "^M25051", 
                 "^M25052", "^M25059", "^M25061", "^M25062", "^M25069", "^M25071", "^M25072", 
                 "^M25073", "^M25074", "^M25075", "^M25076", "^M2508", "^R040", "^R041", "^R042", 
                 "^R0481", "^R0489", "^R049")

all_icd10_bleeds_ind <- c(gib_icd10_ind, gu_icd10_ind, cerebral_icd10_ind, other_icd10_ind)
all_icd10_bleeds_ind <- paste(all_icd10_bleeds_ind, collapse = "|")

###Diagnosis Codes Suggestive of Possible Bleeding
gib_icd10_possible <- c("^K251", "^K253", "^K255", "^K257", "^K259","^K261", "^K263",
                       "^K265", "^K267", "^K269", "^K271", "^K273", "^K275", "^K277",
                       "^K279", "^K281", "^K283", "^K285", "^K287", "^K289", "^K2900",
                       "^K2920", "^K2930", "^K2940", "^K2950", "^K2960", "^K2970",
                       "^K2980", "^K2990", "^K5700", "^K5710", "^K5712", "^K5720",
                       "^K5730", "^K5732", "^K5740", "^K5750", "^K5752", "^K5780",
                       "^K5792", "^K640", "^K641", "^K642", "^K643", "^K644", "^K645",
                       "^K648", "^K649", "^K200", "^K208", "^K209", "^K210", "^K2210")
all_gib_icd10_possible <- paste(gib_icd10_possible, collapse = "|")

unspec_icd10_possible <- c("^D62", "^D500", "^D649", "^R791", "^D6959", "^D696")
all_unspec_icd10_possible <- paste(unspec_icd10_possible, collapse = "|")

all_icd10_bleeds_possible <- c(gib_icd10_possible, unspec_icd10_possible)
all_icd10_bleeds_possible <- paste(all_icd10_bleeds_possible, collapse = "|")


###Excluded outcomes: 
#Major trauma code recorded on day prior to admission through day after admission

#Map (forward/backward) ICD 9 to ICD-10 using CMS GEM
# Load the GEM files and set column names
forward_gem <- read.table("C:/Users/kahanso2/Documents/doac-ddi/data/2018_I9gem.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(forward_gem) <- c("ICD9", "ICD10", "FLAG")

backward_gem <- read.table("C:/Users/kahanso2/Documents/doac-ddi/data/2018_I10gem.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(backward_gem) <- c("ICD10", "ICD9", "FLAG")

# Function to map ICD-9 to ICD-10 using the forward GEM
map_icd9_to_icd10 <- function(icd9_codes, gem) {
  icd10_codes <- unique(unlist(lapply(icd9_codes, function(code) {
    exact_match <- grep(code, gem$ICD9, value = TRUE)
    if (length(exact_match) > 0) {
      mapped_codes <- gem[gem$ICD9 %in% exact_match, "ICD10"]
      return(mapped_codes)
    }
    return(NULL)
  })))
  return(icd10_codes)
}

# Function to map ICD-10 to ICD-9 using the backward GEM
map_icd10_to_icd9 <- function(icd10_codes, gem) {
  icd9_codes <- unique(unlist(lapply(icd10_codes, function(code) {
    exact_match <- grep(code, gem$ICD10, value = TRUE)
    if (length(exact_match) > 0) {
      mapped_codes <- gem[gem$ICD10 %in% exact_match, "ICD9"]
      return(mapped_codes)
    }
    return(NULL)
  })))
  return(icd9_codes)
}

# Function to filter mapped codes to ensure they are part of the initial trauma list
filter_mapped_codes <- function(mapped_codes, initial_list) {
  filtered_codes <- mapped_codes[sapply(mapped_codes, function(code) any(grepl(code, initial_list)))]
  return(filtered_codes)
}

# Function to add ^ prefix to each code
add_prefix <- function(codes) {
  prefixed_codes <- sapply(codes, function(code) {
    if (!startsWith(code, "^")) {
      return(paste0("^", code))
    } else {
      return(code)
    }
  })
  return(prefixed_codes)
}

# Function to iteratively map codes until no new codes are found
iterative_mapping <- function(initial_icd9, forward_gem, backward_gem) {
  current_icd9 <- initial_icd9
  final_icd10 <- c()
  
  iteration <- 1
  repeat {
    # Forward mapping
    current_icd9 <- add_prefix(current_icd9)  # Ensure patterns have ^ prefix
    new_icd10 <- map_icd9_to_icd10(current_icd9, forward_gem)
    
    # Backward mapping
    new_icd9 <- map_icd10_to_icd9(new_icd10, backward_gem)
    
    # Filter new_icd9 to ensure they are part of the initial trauma list
    new_icd9 <- filter_mapped_codes(new_icd9, initial_icd9)
    
    # Print intermediate results
    cat("Iteration:", iteration, "\n")
    cat("New ICD-10 codes:\n")
    print(new_icd10)
    cat("New ICD-9 codes:\n")
    print(new_icd9)
    
    # Check if any new ICD-10 codes are found
    if (length(setdiff(new_icd10, final_icd10)) == 0) break
    
    final_icd10 <- unique(c(final_icd10, new_icd10))
    current_icd9 <- new_icd9
    iteration <- iteration + 1
  }
  
  return(final_icd10)
}

# # Example usage with your trauma codes
# trauma_icd9 <- c("^800.*", "^801.*", "^802.*", "^803.*", "^804.*", "^805.*", "^806.*", "^807.*", "^808.*",
#                  "^809.*", "^810.*", "^811.*", "^812.*", "^813.*", "^818.*", "^819.*", "^820.*", "^821.*",
#                  "^822.*", "^823.*", "^824.*", "^827.*", "^828.*", "^829.*", "^860.*", "^8620.*", 
#                  "^8621.*", "^8628.*", "^8629.*", "^8630.*", "^8631.*", "^8632.*", "^8633.*", "^8634.*", "^8635.*",
#                  "^8638.*", "^8639.*", "^864.1.*","^865.1.*", "^866.*", "^867.*", "^8730.*", "^8731.*", "^8750.*", 
#                  "^8751.*", "^9024.*", "^90255", "^90256", "^90281", "^90282", "^925.*", "^926.*", "^927.*",
#                  "^928.*", "^929.*", "^9584.*", "^9585.*", "^9587.*", "^9967.*", "^99811", "^99812", "^9982.*",
#                  "^E805.*", "^E870.*", "^E881.*", "^E882.*", "^E883.*", "E922.*", "E923.*", "^E955.*", "^E960.*",
#                  "^E965.*", "^E970.*", "^E985.*")

# Perform the iterative mapping to get the final list of ICD-10 codes
final_icd10 <- iterative_mapping(trauma_icd9, forward_gem, backward_gem)
final_icd10 <- paste0("^", final_icd10)
trauma_check_icd10<- paste(final_icd10, collapse = "|")

trauma_hcpcs <- c("^62000", "^62005", "^62010") #Assume these werent changed
trauma_hcpcs_all <- paste(trauma_hcpcs, collapse = "|")

##Additional dx codes for rule in diagnoses
anemia_icd10 <- c("^D500" ,"^D62", "^D649")
orthostasis_icd10 <- "^I951"
syncope_icd10 <- "^R55"
comb_sec_icd10 <- c(anemia_icd10, orthostasis_icd10, syncope_icd10)
all_comb_sec_icd10 <- paste(comb_sec_icd10, collapse = "|")



###Myopathy
#ICD Codes - myopathy (Bykov 2020)
outcome <- c(72888, 72889, 72887, 7289, 7291, 72881, 7913, 7104, 35989, 3599,
             "M6282", "M6289", "M6281", "M629", "M791", "M609", "M601", "M608", "R821", "M332", "G7289", "G729", "G720")
myopathy_pattern <- paste(outcome, collapse = "|")


#ICD Codes - stroke, ischemic stroke, TIA (Zhou 2020) 
#Thromboembolism	Stroke Ischemic stroke and TIA	Principal and non-principal inpatient diagnosis	433.x1, 434(excluding 434.x0), 435.x, 436.x
#Excluded if 1) any traumatic brain injury (800.x, 801.x, 802.x, 803.x, 804.x, 850.x, 851.x, 852.x, 853.x, 854.x) was present 
#Or 2) rehabilitation care (v57) was present as the primary diagnosis	
stroke_ischemicstroke_tia <- c(43301, 43311, 43321, 43331, 43381, 43391, 43401, 43411, 43491, 4350, 4351, 4352, 4353, 4358, 4359, 436, 
                               "G450", "G451", "G452", "G458", "G459", "G460", "G461", "G462", "I6300", "I6301x", "I6302",
                               "I6303x", "I6309", "I6310", "I6311x", "I6312", "I6313x", "I6319", "I6320", "I6321x", "I6322",
                               "I6323x", "I6329", "I6330", "I6331x", "I6332x", "I6333x", "I6434x", "I6339", "I6340", "I6341x",
                               "I6342x", "I6343x", "I6344x", "I6349", "I6350", "I6351x", "I6352x", "I6353x", "I6354x", "I6359",
                               "I636", "I638", "I639", "I660x", "I661x", "I662x", "I663", "I668", "I669", "I67841", "I67848", "I6789")
stroke_pattern <- paste(stroke_ischemicstroke_tia, collapse = "|")

excl_traum_brain_inj <- c(80000, 80001, 80002, 80003, 80004, 80005, 80006, 80009, 80010, 80011, 80012, 80013, 80014, 80015, 80016, 80019, 80020, 
                          80021, 80022, 80023, 80024, 80025, 80026, 80029, 80030, 80031, 80032, 80033, 80034, 80035, 80036, 80039, 80040, 80041, 
                          80042, 80043, 80044, 80045, 80046, 80049, 80050, 80051, 80052, 80053, 80054, 80055, 80056, 80059, 80060, 80061, 80062, 
                          80063, 80064, 80065, 80066, 80069, 80070, 80071, 80072, 80073, 80074, 80075, 80076, 80079, 80080, 80081, 80082, 80083, 
                          80084, 80085, 80086, 80089, 80090, 80091, 80092, 80093, 80094, 80095, 80096, 80099, 80100, 80101, 80102, 80103, 80104, 
                          80105, 80106, 80109, 80110, 80111, 80112, 80113, 80114, 80115, 80116, 80119, 80120, 80121, 80122, 80123, 80124, 80125, 
                          80126, 80129, 80130, 80131, 80132, 80133, 80134, 80135, 80136, 80139, 80140, 80141, 80142, 80143, 80144, 80145, 80146, 
                          80149, 80150, 80151, 80152, 80153, 80154, 80155, 80156, 80159, 80160, 80161, 80162, 80163, 80164, 80165, 80166, 80169, 
                          80170, 80171, 80172, 80173, 80174, 80175, 80176, 80179, 80180, 80181, 80182, 80183, 80184, 80185, 80186, 80189, 80190, 
                          80191, 80192, 80193, 80194, 80195, 80196, 80199, 8020, 8021, 80220, 80221, 80222, 80223, 80224, 80225, 80226, 80227, 80228, 
                          85100, 85101, 85102, 85103, 85104, 85105, 85106, 85109, 85110, 85111, 85112, 85113, 85114, 85115, 85116, 85119, 85120, 85121,
                          85122, 85123, 85124, 85125, 85126, 85129, 85130, 85131, 85132, 85133, 85134, 85135, 85136, 85139, 85140, 85141, 85142, 85143,
                          85144, 85145, 85146, 85149, 85150, 85151, 85152, 85153, 85154, 85155, 85156, 85159, 85160, 85161, 85162, 85163, 85164, 85165,
                          85166, 85169, 85170, 85171, 85172, 85173, 85174, 85175, 85176, 85179, 85180, 85181, 85182, 85183, 85184, 85185, 85186, 85189,
                          85190, 85191, 85192, 85193, 85194, 85195, 85196, 85199, 85200, 85201, 85202, 85203, 85204, 85205, 85206, 85209, 85210, 85211,
                          85212, 85213, 85214, 85215, 85216, 85219, 85220, 85221, 85222, 85223, 85224, 85225, 85226, 85229, 85230, 85231, 85232, 85233,
                          85234, 85235, 85236, 85239, 85240, 85241, 85242, 85243, 85244, 85245, 85246, 85249, 85250, 85251, 85252, 85253, 85254, 85255,
                          85256, 85259, 85300, 85301, 85302, 85303, 85304, 85305, 85306, 85309, 85310, 85311, 85312, 85313, 85314, 85315, 85316, 85319,
                          85400, 85401, 85402, 85403, 85404, 85405, 85406, 85409, 85410, 85411, 85412, 85413, 85414, 85415, 85416, 85419) #need to add icd-10
traum_brain_inj_pattern <- paste(excl_traum_brain_inj, collapse = "|")

excl_rehab_primarydx <- "v57" #need to add icd-10 - where would this code be???

#ICD Codes - VTE/PE
vte_pe_primary_icd9 <- c("415.11", "415.19", "451.11", "451.19", "451.2", "451.9", "453.1", "453.2", "453.40", "453.41", "453.42", "453.8", "453.9")
vte_pe_secondary_icd9 <- c("415.11", "415.19", "453.41", "453.42")
vte_pe_icd10 <- c("I2609", "I2690", "I2699", "I801x", "I8020x", "I8022x", "I8023x", "I8029x", "I803", "I809", "I821", "I82210", "I8222x", "I8240x", 
                  "I8241x", "I8242x", "I8243x", "I8244x", "I8249x", "I824Yx", "I824Zx", "I8260x", "I8261x", "I8262x", "I8290", "I8291", "I82A1x", 
                  "I82B1x", "I82C1x", "T800XXA", "T81718A", "T8172XA", "T82817A", "T82818A", "I2609", "I2690", "I2699", "I8241x", "I8242x", "I8243x", 
                  "I8244x", "I8249x", "I824Yx", "I824Zx", "T800XXA", "T81718A", "T8172XA", "T82817A", "T82818A")


#Object vector
object_oac <- c("Dabigatran Etexilate Mesylate", "Apixaban", "Rivaroxaban", "Warfarin Sodium", "Edoxaban") #Add in pravastatin when doing a full evaluation. 
object_test <- "Apixaban"

oac <- c("Dabigatran Etexilate Mesylate", "Apixaban", "Rivaroxaban", "Warfarin Sodium", "Edoxaban")

#Precipitant(s) vector
precipitant <- c("Ibuprofen", "Fluconazole", "Lisinopril", "Gemfibrozil", "Clarithromycin", "Levothyroxin", "Metformin", "Quetiapine")

#cont_enroll_req day requirement
cont_enroll_req <- 365

#New user requirement
new_user_req <- '2009-12-31'

#Object grace period (7d grace (Leonard))
obj_grace <- 7

#Precipitant grace period
precip_grace <- 0




# Create exclusion lists for drugs/devices that shouldn't cause DDI. Also remove vitamins
excluded_mastfrm <- c("DEV", "CRE", "OIN", "LOT", "GEL", "EMU", "WAX", "TIN", "EMO", "FOA", "PAD", "PAS")
excluded_thrdtds <- c("Bulk Compounding Ingredient", "Vitamins, Prenatal, Misc Preps", 
                      "Vitamins W/Iron, Misc Preps.", "Vitamins W/Minerals, Misc Prep", 
                      "Vitamins, Plain, Misc Preps.", "Vitamins, Prenatal", "Amino Acid Supplements & Comb.", "Sterile Water", "Vitamin C & Combinations", "Polysaccharide-Iron Cmplx&Comb", "Lactobacillus & Comb.", "B-Complex/Iron/Vit. C & Comb.", "Skin/Mucous Membrane, Misc.", "Dietary Supplements, Misc.", "Iron Carbonyl & Comb.", "Iron Heme Polypeptide & Comb.", "Nutritional Supplements", "Opium Preparations", "Calcium Phosphate Dibasic", "Acidophilus & Comb.", "Hormones, Misc.", "Vitamin B Complex & Comb.", "B-Complex/Vitamin C", "Fish Oil & Comb.", "Omega-3 Fatty Acids", "Psyllium & Comb.", "Artificial Saliva, EENT")
excluded_gennme <- c("Antibacterial/Analgesic Combination", "Cough/Cold Combination", "Cyclobenzaprine HCl;Cream, Multi Ingredient")

# List of drugs to control for
nsaids <- c("Celecoxib", "Diclofenac", "Diflunisal", "Etodolac", "Fenoprofen Calcium", "Flurbiprofen", "Ibuprofen", 
            "Indomethacin", "Ketoprofen", "Ketorolac", "Meclofenamate Sodium", "Mefenamic Acid", "Meloxicam", "Nabumetone", 
            "Naproxen", "Oxaprozin", "Piroxicam", "Salsalate", "Sulindac", "Tolmetin Sodium")

antiplatelet <- c("Abciximab", "Anagrelide Hydrochloride", "Aspirin", "Cilostazol", "Clopidogrel", "Dipyridamole", "Eptifibatide", 
                  "Prasugrel Hydrochloride", "Ticagrelor", "Ticlopidine Hydrochloride", "Tirofiban Hydrochloride")

other_anticoag <- c("Fondaparinux Sodium", "Enoxaparin Sodium", "Argatroban", "Dalteparin", "Tinzaparin", "Heparin", "Bivalirudin", "Desirudin", "Phenprocoumon")

ssri_snri <- c("Citalopram", "Escitalopram", "Fluoxetine", "Fluvoxamine", "Milnacipran", "Paroxetine", "Sertraline")

