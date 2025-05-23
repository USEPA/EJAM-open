# datacreate_testinput_program_name.R

testinput_program_name <- read_csv_or_xl(testdata("program_name_only_3.xlsx", quiet = T))$program
testinput_program_name <- metadata_add(testinput_program_name)
usethis::use_data(testinput_program_name, overwrite = TRUE)
dataset_documenter(
  "testinput_program_name",
  description = "EPA program name codes vector as example of input, e.g., latlon_from_program(testinput_program_name)"
)

# latlon_from_program(testinput_program_name)
