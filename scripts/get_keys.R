# set up keys ----
clean_name <- function(string) {
  str_split(string, "\\.")[[1]][1]
}

keys_here <- here::here("keys")

key_locs <- list.files(keys_here,
                       full.names = TRUE)

key_names <- map(list.files(keys_here), clean_name)

keys <- map(key_locs,
            read_csv,
            col_types = cols())

names(keys) <- key_names

keys$vlce <- keys$vlce %>%
  filter(class_val != 0)

rm(keys_here, key_locs, key_names, clean_name)
