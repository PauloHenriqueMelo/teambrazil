# Load the dataset from data-raw
load(file = "data-raw/HospitalCity.rda")
tabMun$munResNome <- stringi::stri_escape_unicode(str = HospitalCity$Hospital_City)
tabMun$munResUf <- stringi::stri_escape_unicode(str = HospitalCity$Hospital_State)
usethis::use_data(HospitalCity, overwrite = TRUE)
