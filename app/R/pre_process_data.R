# Script to preprocess uploaded csv data

# Author: Peter Boshe
# Version: 2023-05-12

# Packages
library(tidyverse)

# Parameters

# ============================================================================


#############################################################################
#  Pre-processing data                                                      #
#############################################################################

pre_process_data <- function(path) {
  df <- read_csv(path) |>
    janitor::clean_names() |>
    # change each column to the prefered datatype
    transmute(
      line = as.numeric(line_number),
      facility = as.factor(facility),
      # facility_type = as.factor(facility_type),
      district = as.factor(district),
      zone = as.factor(zone),
      period = as.character(period),
      product = product_name,
      product_code = as.factor(product_code_name),
      amc = as.numeric(amc),
      closing_balance = as.numeric(physical_count),
      mos = as.numeric(mos),
      order_quantity = as.numeric(order_quantity),
      status = as.factor(status),
      group = as.factor(group),
      year = as.factor(year)
    ) |>
    separate(period, into = c("month_range","ext_year"), sep = "\\s(?=[^\\s]+$)") |>
    separate(month_range, into = c("start_month", "end_month"), sep = "-") |>
    mutate(
      start_month = trimws(tolower(start_month)),
      end_month = trimws(tolower(end_month)),
      start_month = case_when(
        start_month == "jan" ~ "january",
        start_month == "feb" ~ "february",
        start_month == "mar" ~ "march",
        start_month == "apr" ~ "april",
        start_month == "may" ~ "may",
        start_month == "jun" ~ "june",
        start_month == "jul" ~ "july",
        start_month == "aug" ~ "august",
        start_month == "sept" ~ "september",
        start_month == "oct" ~ "october",
        start_month == "nov" ~ "november",
        start_month == "dec" ~ "december",
        TRUE ~ start_month
      ),
      end_month = case_when(
        end_month == "jan" ~ "january",
        end_month == "feb" ~ "february",
        end_month == "mar" ~ "march",
        end_month == "apr" ~ "april",
        end_month == "may" ~ "may",
        end_month == "jun" ~ "june",
        end_month == "jul" ~ "july",
        end_month == "aug" ~ "august",
        end_month == "sept" ~ "september",
        end_month == "oct" ~ "october",
        end_month == "nov" ~ "november",
        end_month == "dec" ~ "december",
        TRUE ~ end_month
      )
    ) |>
    unite("month_range", c(start_month, end_month), sep = " - ")

  # introduce factor ordering for proper arrangement of the reporting periods
  #
  ## Reordering df$month_range
  df$month_range <- df$month_range %>%
    fct_relevel(
      "january - february", "february - march", "march - april",
      "april - may", "may - june", "june - july", "july - august",
      "august - september", "september - october", "october - november",
      "november - december", "december - january"
    )
  # rearranging the dataset
  #
  df <- df |>
    arrange(year, facility, month_range) |>
    mutate(product_cleaned = case_when(
      product_code == "10010022AB" ~ "Tenofovir Disoproxil Fumarate +Lamivudine+Doltegravir (TLD)..B/30 Tablet (300/300/50) mg",
      product_code == "10011380AB" ~ "Tenofovir Disoproxil Fumarate +Lamivudine+Doltegravir (TLD)..B/90 Tablet (300/300/50) mg",
      product_code ==	"10010044MD" ~ "Paracetamol Tablet 500 mg",
      product_code == "10010176MD" ~ "Paracetamol Tablet 500 mg",
      product_code == "10040012MD" ~ "Paracetamol Syrup 120mg/5ml,100mls",
      product_code == "10011084MD" ~ "Diclofenac + Paracetamol Tablet 50/500 mg",
      product_code == "10010790SP" ~ "Diclofenac + Paracetamol + Chlorxazone Tablet 50/200/200 mg/mg/mg ",
      product_code == "10010468SP" ~ "Diclofenac + Paracetamol + Chlorzoxane Tablet 50/325/250 mg/mg/mg ",
      product_code == "10010002BE" ~ "Artemether/Lumefantrine (Kahawia 3 x 6) Tablet 20/120 mg ",
      product_code == "10010171BE" ~ "Artemether/Lumefantrine  (Kijani 4 x 6) Tablet 20/120 mg ",
      product_code == "10010171MD" ~ "Artemether/Lumefantrine  (Kijani 4 x 6) Tablet 20/120 mg ",
      product_code == "10010170BE" ~ "Artemether/Lumefantrine (Bluu 2 x 6) Tablet 20/120 mg",
      product_code == "10010170MD" ~ "Artemether/Lumefantrine (Bluu 2 x 6) Tablet 20/120 mg",
      product_code == "10010169BE" ~ "Artemether/Lumefantrine  (Njano 1 x 6) Tablet 20/120 mg",
      product_code == "10010169MD" ~ "Artemether/Lumefantrine  (Njano 1 x 6) Tablet 20/120 mg",
      product_code == "10010169AC" ~ "Artemether/Lumefantrine  (Njano 1 x 6) Tablet 20/120 mg",
      product_code == "20030043MD" ~ "Syringe Auto Disable 2cc each",
      product_code == "20030034MD" ~ "Syringe Auto Disable each 5 cc",
      product_code == "20030033MD" ~ "Syringe Auto Disable with Needle 10cc each",
      product_code == "20030073AC" ~ "Syringe Auto Disable 1 ml 22 GG*1 each 1 ml",
      product_code == "10010076MD" ~ "Amoxicillin Capsule 250 mg",
      product_code == "10010007MD" ~ "Amoxicillin Capsule 250 mg",
      product_code == "10010541AC" ~ "Amoxicillin Capsule 250 mg",
      product_code == "10010183MD" ~ "Amoxicillin Capsule 250 mg",
      product_code == "10010616MD" ~ "Amoxicillin Trihydrate+ Clavulanic Potassium Tablet 500/125 mg/mg",
      product_code == "10010127MD" ~ "Amoxicillin Trihydrate+ Clavulanic Potassium capsule Capsule 500 + 125",
      product_code == "20090063BE" ~ "Mosquito Net Size 180(L) X 160 (W) X 180 (H) With Long Lasting Insecticide Treatment (LLIN) each",
      product_code == "40070030BE" ~ "Malaria Rapid Diagnostic Test Kit (MRDT) strip ",
      product_code == "10010206MD" ~ "Vitamin B complex Tablet",
      product_code == "10010057MD" ~ "Vitamin B complex Tablet",
      product_code == "10040009MD" ~ "Vitamin B complex Syrup 100 ml",
      product_code == "20030016MD" ~ "I.V. Canula 18G each",
      product_code == "20030012MD" ~ "I.V. giving set each",
      product_code == "20030013MD" ~ "I.V. Giving Set For Paediatric - Measured Volume(Burrette) each 1 cc",
      product_code == "20030014MD" ~ "I.V. Canula 14G each",
      product_code == "20030015MD" ~ "I.V. Canula 16G each",
      product_code == "20030017MD" ~ "I.V. canula 20G each",
      product_code == "20030018MD" ~ "I.V. Canula 22G each",
      product_code == "20030019MD" ~ "I.V. Canula 24G each",
      product_code == "20020001MD" ~ "Gloves Examination Latex Non-Sterile Disposable Large Other",
      product_code == "20020002MD" ~ "Gloves Examination Latex Non-Sterile Disposable Medium each",
      product_code == "20020002AB" ~ "Gloves Examination Latex Non-Sterile Disposable Medium each" ,
      product_code == "20020005MD" ~ "Gloves Surgical Latex Rubber Sterile each size 7",
      product_code == "20020006MD" ~ "Gloves Surgical Latex Rubber Sterile Size 7.5 each",
      product_code == "20020008MD" ~ "loves Gynaecological Disposable (Small) Size 7",
      product_code == "20020009MD" ~ "Gloves Gynaecological Disposable (Medium) Size 7.5 each",
      product_code == "20020010MD" ~ "Gloves Gynaecological Disposable (Large) Size 8 each",
      product_code == "20020012MD" ~ "Orthopedic Surgical Gloves Size 7.5 (Medium) each Size 7.5 (M)",
      product_code == "10040030MD" ~ "Cetrizine syrup 5mg/5ml, 30mls Solution",
      product_code == "10010081MD" ~ "Cetrizine Tablet 10 mg",
      product_code == "10010833MD" ~ "Ampicillin + Cloxacillin Capsule 250/250 mg",
      product_code == "10060011MD" ~ "Ampicillin Injection 500 mg",
      product_code == "10010182MD" ~ "Ampicillin + Cloxacillin Capsule 250/250 mg",
      product_code == "10040035MD" ~ "Ampicillin + Cloxacilin Syrup 250 mg/ml",
      product_code == "10040035SP" ~ "Ampicillin + Cloxacilin Syrup 250 mg/ml",
      product_code == "10060334MD" ~ "Ampicillin + Cloxacilin Powder For Injection Powder 250/250 mg/mg",
      product_code == "10040124MD" ~ "Ampicillin + Cloxacillin drop each"
    )) |>
    relocate(product_cleaned, .after = product)

  return(df)
}





