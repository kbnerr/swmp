## by linear regression
exo_std_calc = function(std_temp = 20, param = NULL) {
  requireNamespace("tidyverse")
  file_path = file.path(getwd(), "data", "TAL_fDOM_TempAdjCalc.xlsx")
  names = c("temp",
            "chla_rfu",
            "chla_ugl",
            "phycocyanin_rfu",
            "phycocyanin_ugl",
            "phycoerythrin_rfu",
            "phycoerythrin_ugl",
            "fdom_rfu",
            "fdom_qsu")
  stopifnot(is.numeric(std_temp))
  if (is.null(param)) names_select = names
  else names_select = param
  readxl::read_xlsx(path = file_path,
                    range = "B3:J14",
                    col_names = names,
                    col_types = "numeric") %>%
    select(temp, contains(names_select) | matches(names_select)) %>%
    pivot_longer(cols = -temp, names_to = "param", values_to = "value") %>%
    nest_by(param) %>%
    mutate(model = list(lm(value ~ temp, data = data)),
           std_value = predict(model, newdata = data.frame(temp = std_temp)),
           std_temp = std_temp) %>%
    select(param, std_temp, std_value) %>%
    unnest(everything())
}

## by linear interpolation
exo_std_calc = function(std_temp = 20, param = NULL) {
  requireNamespace("tidyverse")
  file_path = file.path(getwd(), "data", "TAL_fDOM_TempAdjCalc.xlsx")
  names = c("temp",
            "chla_rfu",
            "chla_ugl",
            "phycocyanin_rfu",
            "phycocyanin_ugl",
            "phycoerythrin_rfu",
            "phycoerythrin_ugl",
            "fdom_rfu",
            "fdom_qsu")
  stopifnot(is.numeric(std_temp))
  if (is.null(param)) names_select = names
  else names_select = param
  readxl::read_xlsx(path = file_path,
                    range = "B3:J14",
                    col_names = names,
                    col_types = "numeric") %>%
    select(temp, contains(names_select) | matches(names_select)) %>%
    pivot_longer(cols = -temp, names_to = "param", values_to = "value") %>%
    nest_by(param) %>%
    mutate(interp = list(approx(data$temp, data$value, xout = std_temp)),
           std_temp = interp$x,
           std_value = interp$y) %>%
    select(param, std_temp, std_value) %>%
    unnest(everything())
}

## Example code

# function will calculate standard values for all parameters
# by default, std_temp = 20
exo_std_calc()

# function looks for a number in the first argument to use as standard temp
exo_std_calc(8.005)

# note that the interpolation function will generate NA
# if the std_temp < 8 or > 30
exo_std_calc(7.999)

# function will throw an error if a non-numeric argument is provided
exo_std_calc(TRUE)

# you can specify subsets of parameters with the param argument:

# with character vectors
x = c("chla_rfu", "chla_ugl")
exo_std_calc(20.68, param = x)

# case does not matter
exo_std_calc(param = "RFU")

# function will look for any matches including single characters
exo_std_calc(param = c("q", "fdom_RFU"))

# but will throw an error if provided string is not in the param list
exo_std_calc(param = "")
