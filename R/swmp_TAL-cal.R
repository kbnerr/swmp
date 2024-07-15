pre = c(20.5, 20.5, 20.7, 20.2, 20.0, 20.0, 19.6, # H3 2023
19.6, 18.5, 18.5, 19.1, 19.2, # H3 2024
20.4, 20.7, 20.6, 20.2, 19.7, 20.1, 19.8, # SS 2023
19.5, 18.5, 19.3, 19.1) # SS 2024

post = c(18.4, 20.1, 19.8, 19.5, 19.3, 18.9, # H3 2023
19.1, 19.1, 19.2, # H3 2024
20.6, 18.5, 20.1, 20.0, 19.5, 19.1, 19.0, # SS 2023
19.0, 19.1, 19.3) # SS 2024

pre = tibble("temp" = pre,
       "when" = "pre",
       "x" = c(rep("H3 2023", 7),
               rep("H3 2024", 5),
               rep("SS 2023", 7),
               rep("SS 2024", 4))) %>%
  separate_wider_delim(cols = x, delim = " ", names = c("site", "year"), cols_remove = TRUE)

post = tibble("temp" = post,
       "when" = "post",
       "x" = c(rep("H3 2023", 6),
               rep("H3 2024", 3),
               rep("SS 2023", 7),
               rep("SS 2024", 3))) %>%
  separate_wider_delim(cols = x, delim = " ", names = c("site", "year"), cols_remove = TRUE)

dat = rbind(pre, post)

skimr::skim(dat)

filter(dat, year == "2023") %>%
  skimr::skim()

filter(dat, year == "2024") %>%
  skimr::skim()

filter(dat, when == "pre") %>%
  skimr::skim()

filter(dat, when == "post") %>%
  skimr::skim()
