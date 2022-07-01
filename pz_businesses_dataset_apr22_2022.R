#title: promise zone businesses dataset
#name: nissim lebovits
#last_edit: 6/7/2022

#summary: The goal of this dataset is to create a comprehensive dataset of businesses in the promise zone.
#It starts with businesses license data from l&i and appends usda snap data to it using address columns.
#It then adds the following data:
    #opa market value
    #zoning codes https://www.opendataphilly.org/dataset/zoning-base-districts
    #local business registration https://www.opendataphilly.org/dataset/city-registered-local-businesses
    #minority-owned business registration https://www.opendataphilly.org/dataset/office-of-economic-opportunity-registration
    #nearest septa route
    #tobacco retailer permits https://www.opendataphilly.org/dataset/tobacco-retailer-permits
    #liquor licenses https://plcbplus.pa.gov/pub/Default.aspx?PossePresentation=LicenseSearch
    #sugary drinks https://www.opendataphilly.org/dataset/philadelphia-beverage-tax-registered-distributors-and-dealers
    #naics codes

##############################SETUP##########################################
library(tidyverse, quietly = T)
library(reshape2, quietly = T)
library(sf, quietly = T)
library(mapview, quietly = T)
library(lubridate, quietly = T)
library(tidygeocoder, quietly = T)
library(gtools, quietly = T) #for smartbind()
library(geojsonio, quietly = T) #for opa properties geojson

setwd("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets")

########################IMPORTS##########################################

pz <- read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/General Boundaries/Shapefiles/PZ_Shapefile",
              "PZ_Boundaries",
              stringsAsFactors = FALSE) |>
  st_transform(crs = st_crs("EPSG:4326"))


phl_licenses = read.csv("./Business Licenses/business_licenses.csv") |>
  filter(licensestatus == "Active",
         !is.na(lng),
         !is.na(lat),
         licensetype != "Rental") |> #remove rental licenses
  dplyr::select(-c(the_geom, the_geom_webmercator,
                   unit_type, unit_num,
                   numberofunits, owneroccupied,
                   geocode_x, geocode_y, council_district))

#filter for only in the PZ
phl_licenses_sf = st_as_sf(phl_licenses, coords = c("lng", "lat"), crs = st_crs("EPSG:4326"))

pz_licenses_sf = phl_licenses_sf[pz, ]

pz_licenses_clean = pz_licenses_sf |>
  group_by(opa_account_num) |>
  mutate(all_licenses = paste(licensetype, collapse = " | "),
            license_duration = if (is.na(expirationdate) | expirationdate == "" | expirationdate == " ") {
              as.duration(interval(initialissuedate, today()))
            } else {
              as.duration(interval(initialissuedate, expirationdate))
            }) |>
  dplyr::select(business_name, legalname, address, zip, censustract, opa_account_num,
                initialissuedate, expirationdate, all_licenses, license_duration, posse_jobid)
            
pz_licenses_clean$address = pz_licenses_clean$address |>
  tolower()|>
  str_replace_all("street", "st") |>
  str_replace_all("avenue", "ave") |>
  str_replace_all("  ", " ") |>
  str_remove_all("[[:punct:]]")

pz_licenses_clean = pz_licenses_clean[!duplicated(pz_licenses_clean$opa_account_num),]


##############################CLEAN SNAP##########################################

#now import SNAP data, paste() addresses
phl_snap_points = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/SNAP/SNAP_Data.csv") |>
  filter(State == "PA",
         City == "Philadelphia") 

#create single address column for matching
phl_snap_points = phl_snap_points |>
  mutate(full_address = tolower(paste(phl_snap_points$Street.Number, phl_snap_points$Street.Name, sep = " ")))

#convert to sf for filtering for pz
phl_snap_points_clean = phl_snap_points |>
  filter(!is.na(Latitude),
         !is.na(Longitude))

phl_snap_points_sf = st_as_sf(phl_snap_points_clean,
                              coords = c("Longitude", "Latitude"),
                              crs = st_crs("EPSG:4326"))

#filter for pz
pz_snap_points = phl_snap_points_sf[pz, ]|>
  as.data.frame() |>
  dplyr::select(Store.Name,
                End.Date,
                full_address,
                -geometry) |>
  rename(snap_business_name = Store.Name,
         snap_end_date = End.Date,
         snap_address = full_address)

pz_snap_points$snap_address = pz_snap_points$snap_address |>
  tolower() |>
  str_replace_all("street", "st") |>
  str_replace_all("avenue", "ave") |>
  str_replace_all("  ", " ") |>
  str_remove_all("[[:punct:]]")

pz_snap_points$snap_end_date[pz_snap_points$snap_end_date == " "] = "active"

pz_snap_points = pz_snap_points |>
  group_by(snap_address) |>
  summarize(historic_snap_names = paste(snap_business_name, collapse = " | "),
            historic_snap_dates = paste(snap_end_date, collapse = " | "))

###########################JOIN SNAP TO BUSINESS LICENSES###########################
joined = pz_licenses_clean |>
  full_join(pz_snap_points, 
            by = c(address = "snap_address"))

joined$snap_status = case_when(
  is.na(joined$historic_snap_dates) ~ "no snap ever",
  str_detect(joined$historic_snap_dates, "active") ~ "active snap",
  TRUE ~ "inactive snap"
)

joined$business_status = case_when(
  is.na(joined$business_name) & joined$snap_status == "inactive snap" ~ "closed",
  is.na(joined$business_name) & joined$snap_status == "active snap" ~ "mismatch?",
  TRUE ~ "open"
)

joined$state = "PA"
joined$city = "Philadelphia"

#still need to geocode any snap sites with no business license attached to them
#should be a simple ifelse statement

for_geocode = joined[is.na(joined$business_name), ]

for_geocode = geocode(for_geocode,
               street = "address",
              city = "city",
              state = "state")

for_geocode = for_geocode |>
                dplyr::select(-geometry) |>
                st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

joined = rbind(joined[!is.na(joined$business_name), ], for_geocode)

#mapview(joined[1:25, ], zcol = "snap_status", legend = T)

##########################add zoning base districts#####################
#guide to philadelphia zoning: https://www.phila.gov/media/20200213115058/NEW-ZONING-GUIDE_2020.pdf

zbds = read_sf("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/General Boundaries/Shapefiles/phl_zoningbasedistricts",
        "Zoning_BaseDistricts",
        stringsAsFactors = FALSE) |>
  st_transform(crs = st_crs("EPSG:4326"))

joined_by_zoning = st_join(joined, zbds)

mapview(joined_by_zoning, zcol = "CODE", legend = T)

possible_home_businesses = joined_by_zoning |>
                              filter(CODE %in% c("RM1", "RM2", "RM4", "RSA3", "RSA5", "RTA1") &
                                      all_licenses != "Vacant Residential Property / Lot")

mapview(possible_home_businesses, zcol = "CODE", legend = T)
#################write csv

###########################NAICS#####################################
#read in our naics data (will need to custom make)

#join by address to joined_by_zoning
pz_naics_codes = read_excel("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Transition Documents/Data/R Scripts and Datasets/WEO/PZ Business Master List.xlsx") |>
                    clean_names()

pz_naics_codes$address = pz_naics_codes$address |>
  tolower() |>
  str_replace_all("street", "st") |>
  str_replace_all("avenue", "ave") |>
  str_replace_all("  ", " ") |>
  str_remove_all("[[:punct:]]")

joined_by_naics = left_join(joined_by_zoning, pz_naics_codes, by = "address")

mapview(joined_by_naics, zcol = "primary_naics_description", legend = T)





















opa = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/OPA Properties/opa_properties_public.csv")

opa = opa |>
        filter(!is.na(lat),
               !is.na(lng))

opa_properties = st_as_sf(opa, coords = c("lng", "lat"), crs = st_crs("EPSG:4326"))

joined_by_opa = st_join(joined, opa_properties)

joined_for_csv = as.data.frame(joined_by_zoning) |>
                    dplyr::select(-geometry)

write.csv(joined_for_csv, "promise_zone_businesses_5.4.2022.csv")

###################################FOR OUTREACH#################################

active = joined |>
  filter(snap_status == "active snap") |>
  as.data.frame() |>
  dplyr::select(-geometry)

renew = joined |>
          filter(snap_status == "inactive snap" &
                   business_status == "open") |>
  as.data.frame() |>
  dplyr::select(-geometry)

establish = joined |>
              filter(str_detect(all_licenses, "(?i)food") &
                       snap_status == "no snap ever") |>
  as.data.frame() |>
  dplyr::select(-geometry)

#active
write.csv(active, "active_snap_sites_5.13.2022.csv")
#renew
write.csv(renew, "snap_sites_to_renew_5.13.2022.csv")