
library(readr)
library(dplyr)

#Loading all relevant tsv files right now

net_tv <- read_tsv("/users/dkim221/copied_ad_2019_select/Occurrences/NetworkTV.tsv")

brand <- read_tsv("/users/dkim221/copied_ad_2019_select/References/Brand.tsv")

adver <- read_tsv("/users/dkim221/copied_ad_2019_select/References/Advertiser.tsv")

#distr <- read_tsv("/users/dkim221/copied_ad_2019_select/References/Distributor.tsv")

prod_cat <- read_tsv("/users/dkim221/copied_ad_2019_select/References/ProductCategories.tsv")

#tv_prog <- read_tsv("/users/dkim221/copied_ad_2019_select/References/TV Program.tsv")

#View(c(net_tv,brand,adver,distr,prod_cat,tv_prog))

#sample_brand <- brand_code[sample(nrow(brand_code), 200), ]

net_columns <- net_tv %>% dplyr::distinct(MediaTypeID) #we discovered that there are entries with 5 distinct codes in NetworkTV.tsv
View(net_columns)

engl_net_data <- filter(net_tv, MediaTypeID %in% c(1,2,3)) #took out spanish language

sheets_list <- list(net_tv, brand, adver, distr, prod_cat, tv_prog)

View(net_tv)
View(brand)
View(adver)
View(distr)
View(prod_cat)
View(tv_prog)
#####pre-meeting 2, some preliminary grepl-ing below 
adv_subsid_ma <- dplyr::filter(adver, grepl("MEDICARE ADV",AdvSubsidDesc)) ; 
View(adv_subsid_ma)

adv_par_med <- dplyr::filter(adver, grepl("MEDICARE",AdvSubsidDesc)) ; 
View(adv_par_med)
print(nrow(adv_par_med))

adv_par_ma <- dplyr::filter(adver, grepl("MEDICARE ADVA",AdvSubsidDesc)) ; adv_subsid_ma
View(adv_par_ma)
####
prod_desc_ma <- dplyr::filter(prod_cat, grepl("MEDICARE ADVA",ProdDesc)) ; adv_subsid_ma
View(prod_desc_ma)

pcc_sub_ma <- dplyr::filter(prod_cat, grepl("MEDICARE ADVA",PCCSubCode)) ; adv_subsid_ma
View(prod_desc_ma)
#This filter was unsuccessful !
###

#####

#Analysis 2: Filter down prod category to health insurance, start with one known
#advertiser of medicare advantage and explore what brands are associated with it, 
#look at the brand descriptions

#B210 = PCCMajCode for Life & medical insurance 
#B212 = PCCSubDesc for Medical & Dental Insurance

med_insur_prod_code <- prod_cat %>% filter(PCCSubCode == "B212") #filtering by the PCCSubDesc code for Meical & Dental Insurance

#We can see that PCCMajorCode and PCCIndusCode are the same, but suppose we want to focus on the ProductIDs, which are different:
hinsur_code_desc <- prod_cat %>% 
  filter(PCCSubCode == "B212") %>% #filtering by the PCCSubDesc code for Meical & Dental Insurance
  select(ProductID, ProductDesc)
View(hinsur_code_desc) #these are the ProductIDs of Medical & Dental Insurance

prod_ids_health_insur <- med_insur_prod_code[['ProductID']] #vector of ProductID codes

#Matching those productIDs with brands in Brand.tsv that have health insurance productIDs associated with it

health_insur_brands <- brand %>% filter(ProductID %in% prod_ids_health_insur)
View(health_insur_brands) #df of only and all brands with productIDs related to Medical & Dental Health Insurance

##Now let's find all the advertisers associated with one known case of Medicare Advantage advertisers: Aetna

adv_aetna <- dplyr::filter(adver, grepl("AETNA",AdvSubsidDesc) | grepl("AETNA",AdvParentDesc)) 

ma_adv_l <- c("AETNA", "CIGNA", "HUMANA")

ma_df_l <- list()
for (i in 1:length(ma_adv_l))  {
  df<- filter(adver, grepl(ma_adv_l[i],AdvSubsidDesc) | grepl(ma_adv_l[i],AdvParentDesc))
  #names(df)<-unlist(json_data$resultSets[1,"headers"])
  ma_df_l[[i]] <- df # save your dataframes into the list
}

view_ma_adv_lists <- lapply(ma_df_l, View)


View(adv_aetna) #Note that this is not clean -- there are non-HI companies that are filtered in with Aetna

##Extracting the unique AdvParentCodes and AdvSubsidCodes of things coded with Aetna
adv_parent_aetna <- adv_aetna %>% 
  distinct(AdvParentCode) %>% 
  select(AdvParentCode)
adv_subsid_aetna <- adv_aetna %>% 
  distinct(AdvSubsidCode) %>% 
  select(AdvSubsidCode)
View(adv_parent_aetna)

view_adv_cds <- lapply(list(adv_parent_aetna,adv_subsid_aetna), View)

not_both <- adv_aetna %>% filter(!(grepl("AETNA",AdvSubsidDesc) & grepl("AETNA",AdvParentDesc))) 
#entries that don't have aetna in both the parent and subsid advertiser ("AETNA" in AdvParentDesc OR in AdvSubsidDesc exclusive)
View(not_both)

# Let's find all the brands that have Aetna in the Advertiser Subsid or Advertiser
# Parent description AND have the Medical/Health Insurance codes 

# health_insur_brands %>% rename(AdvParentCode_b = AdvParentCode)
# View(health_insur_brands)

# rename(all_of(name_vec)); can_vs_ncan

rename_brand_cols = c(AdvParentCode_b = "AdvParentCode", AdvSubsidCode_b = "AdvSubsidCode")

hi_brands_rename <- health_insur_brands %>% 
  rename(all_of(rename_brand_cols)) #renaming the AdvParentCode and AdvSubsidCode columns to avoid confusion in the future
  

aetna_hi_brands <- hi_brands_rename %>%
  dplyr::filter(AdvSubsidCode_b %in% adv_subsid_aetna$AdvSubsidCode | 
                  AdvParentCode_b %in% adv_parent_aetna$AdvParentCode) #filter brands 
View(arrange(aetna_hi_brands,AdvParentCode_b)) 
View(aetna_hi_brands) #df of brands whose parent or subsid adv code belong to "Aetna", 
# Above filtering should theoretically clean the non-HI related aetna subsid and parent advertisers from the brands, 
# because hi_brands_rename is only health insurance codes


# Majority (all but one) belong to this subset of advertisers and subsidiary advertisers:
View(filter(adv_aetna, AdvParentCode == 1359687)) #CVS Health Corp (Code = 1359687) as major advertisers associated with Aetna subsidiaries


#####
#Error checking for filter below:

# aetna_hi_brands1 <- aetna_hi_brands_rename %>%
#   dplyr::filter(AdvParentCode_b %in% c(10082,492152,1141734,1951742, 1550111))
# 
# aetna_hi_brands2 <- aetna_hi_brands_rename %>%
#   dplyr::filter(AdvParentCode_b %in% c(1420187, 2546267,1081,104900,218277))
# 
# aetna_hi_brands3 <- aetna_hi_brands_rename %>%
#   dplyr::filter(AdvParentCode_b %in% c(2092237,2765219,1359687,2947671,2982520))
# 
# aetna_hi_brands <- bind_rows(aetna_hi_brands1,aetna_hi_brands2,aetna_hi_brands3)
# View(aetna_hi_brands)
# 
# aetna_hi_brands_par <- aetna_hi_brands_rename %>%
#   dplyr::filter(AdvParentCode_b %in% adv_parent_aetna$AdvParentCode)
# 
# 
# aetna_hi_brands_sub <- aetna_hi_brands_rename %>%
#   dplyr::filter(AdvSubsidCode_b %in% adv_subsid_aetna$AdvSubsidCode)
# View(aetna_hi_brands_sub)


# 1420187, 2546267,1081,104900,218277,
# 291140,441737,1177458,1203965,1784715,
# 2092237,2765219,1359687,2947671,2982520
#filter_seed <- filter(aetna_hi_brands_rename, AdvParentCode_b == adv_parent_aetna$AdvParentCode[1])
# for (i in 2:length(adv_parent_aetna$AdvParentCode)) {
#   filter_add <- filter(aetna_hi_brands_rename, AdvParentCode_b == adv_parent_aetna$AdvParentCode[i])
#   filter_seed <- bind_rows(filter_seed, filter_add)
# }
# 
# View(filtered)


# tmp = unique(filtered_data2$AdvParentCode)
# data_list <- vector("list", length(tmp))
# 
# for (i in 1:length(tmp)){
#   count = dim(adv[adv$AdvParentCode == tmp[i],])[1]
#   data_list[[i]] <- list(AdvParentCode = tmp[i], Count = count)
# }




#health_insur_brands %>% filter(grepl())






