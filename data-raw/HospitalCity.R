# Load the dataset from the 'data-raw' directory
load(file = "data-raw/HospitalCity.rData")


# Modify the 'HospitalCity' dataset as needed
# For example, if you need to escape Unicode characters in the dataset:
HospitalCity$Hospital_City <- stringi::stri_escape_unicode(HospitalCity$Hospital_City)
HospitalCity$Hospital_State <- stringi::stri_escape_unicode(HospitalCity$Hospital_State)

usethis::use_data(HospitalCity, compress = "bzip2", overwrite = TRUE)
