library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)


### read in archive log prepped in `archive prototype`
archived <- read_csv('../archive-prototype/ia-uploaded.csv') %>% 
  mutate(agency = str_extract(identifier, ".+(?=_)")) %>% 
  select(identifier, title, agency, pinpoint, internet_archive) %>% 
  rename(request_number = title) %>% 
  select(-pinpoint)

### read in full summary data from Lucas' github
ati_summaries.read <- read_csv("https://raw.githubusercontent.com/lchski/gc-ati-summaries-data/main/ati-summaries.csv") %>% select(-umd_number) %>%
  mutate(agency = if_else(str_detect(owner_org, "-"), #create agency variable for join
                          str_to_upper(str_extract(owner_org, ".+(?=-)")),
                          str_to_upper(owner_org)),
         agency = if_else(agency == "JUS", "DOJ", agency),#rename JUS to DOJ
         request_number = case_when(str_detect(request_number, "/ A-") ~ 
                                      str_extract(request_number, "A-\\d{4}-\\d{5}"),
                                    TRUE ~ request_number))

#read in pinpoint files
pinpoint <- tibble(read_delim('on_pinpoint.txt', delim = "\n", col_names = FALSE)) %>% 
  mutate(identifier = str_remove(X1, "\\..+$"),
         pinpoint = TRUE) %>% 
  select(-X1)
  

### join archive log with full summary data
ati_summaries.join <- ati_summaries.read %>%
  full_join(archived, by = c('request_number', 'agency')) %>%
  mutate(identifier = case_when(is.na(identifier) ~ paste(agency, request_number, sep = "_"),
                                TRUE ~ identifier)) %>% 
  full_join(pinpoint, by = 'identifier') %>% 
  mutate(internet_archive = if_else(is.na(internet_archive), FALSE, internet_archive),
         pinpoint = if_else(is.na(pinpoint), FALSE, pinpoint))

# merge year and month and convert to 'date' format
ati_summaries <- ati_summaries.join %>%
  mutate(date = paste(year, month, "01", sep = "-")) %>%
  mutate(date = lubridate::ymd(date)) %>%
  select(-year, -month)

# separate fr and en org names
ati_summaries <- ati_summaries %>%
  separate(owner_org_title, into = c("org_en", "org_fr"), sep = "\\|") %>%
  mutate_at(vars(org_en, org_fr), str_squish)

# separate fr and en org acronyms 
ati_summaries <- ati_summaries %>%
  separate(owner_org, into = c("org_ac_en", "org_ac_fr"), sep = "-") %>%
  mutate(org_ac_fr = case_when(
    is.na(org_ac_fr) ~ org_ac_en,
    TRUE ~ org_ac_fr
  )) %>%
  mutate_at(vars(org_ac_en, org_ac_fr), str_to_upper) %>% 
  select(-agency)

# relocate some vars
ati_summaries <- ati_summaries %>%
  relocate(date, .before = request_number) %>%
  relocate(org_ac_en, .after = org_en) %>%
  relocate(org_ac_fr, .after = org_fr)

# make pages disclosed chr to kill slider filter in DT
ati_summaries <- ati_summaries %>%
  mutate(pages = as.character(pages))

# add html code to make live link to pinpoint
ati_summaries <- ati_summaries %>% 
  mutate(pinpoint = case_when(
    pinpoint == TRUE ~ paste0("https://journaliststudio.google.com/pinpoint/search?collection=ce02e69445f4c620&q=%22", identifier)
  )) %>%
  mutate(pinpoint = case_when(
    !is.na(pinpoint) ~ paste0("<b><a href='", pinpoint, "'>", "link", "</a></b>")))

# add html to make live link to internet archive
ati_summaries <- ati_summaries %>% 
  mutate(internet_archive = case_when(
    internet_archive == TRUE ~ paste0('https://archive.org/', identifier)
  )) %>% 
  mutate(internet_archive = case_when(
    !is.na(internet_archive) ~ paste0("<b><a href='", internet_archive, "'>", "link", "</a></b>")))

ati_summaries <- ati_summaries %>% 
  mutate(internet_archive = if_else(is.na(internet_archive), "not archived", internet_archive),
         pinpoint = if_else(is.na(pinpoint), "not archived", pinpoint))

# make org names and acronym and request_number factors
ati_summaries <- ati_summaries %>%
  mutate_at(vars(org_ac_en, org_ac_fr, org_en, org_fr, disposition), as.factor)

# split file in two two (fr and en)
ati_summaries_en <- ati_summaries %>%
  select(date, request_number, summary_en, disposition, pages, org_ac_en, org_en, pinpoint, internet_archive)

ati_summaries_fr <- ati_summaries %>%
  select(date, request_number, summary_fr, disposition, pages, org_ac_fr, org_fr, pinpoint, internet_archive
  )
