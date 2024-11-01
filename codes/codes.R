
# About
# In this script, I will define codes to be used in our work


###Outcome variables

##Bleeds

###############   ICD-9   ##################

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


#Object vector
object_oac <- c("Dabigatran Etexilate Mesylate", "Apixaban", "Rivaroxaban", "Warfarin Sodium", "Edoxaban")

#cont_enroll_req day requirement
cont_enroll_req <- 365


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

giprotect <- c("Sucralfate", "Cimetidine", "Dexlansoprazole", "Esomeprazole", "Famotidine", "Lansoprazole", "Misoprostol", "Nizatidine", "Omeprazole", "Pantoprazole", "Rabeprazole", "Ranitidine")

anticoagulant <- c("Dabigatran Etexilate Mesylate", "Apixaban", "Rivaroxaban", "Warfarin Sodium", "Edoxaban")
