######## setup ########

library(dplyr)
library(lubridate)
if (!require("dplyr")) install.packages("dplyr")
if (!require("lares")) install.packages("lares")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("lmtest")) install.packages("lmtest")
if (!require("car")) install.packages("car")
if (!require("forcats")) install.packages("forcats")
if (!require("patchwork")) install.packages("patchwork")

library(dplyr)
library(lares)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(lmtest)
library(car)
library(forcats)
library(patchwork)

library(readxl)
MMM_Workshop_Data <- read_excel("E:/Downloads/MMM_Workshop_Data.xlsx", 
                                sheet = "Data")
MMM_Workshop_Data$Sales_Revenue_Total

setwd("E:/Workshop 2025/Jan8") # set working directory

config_file_path <- "config_file.csv" # Path to the config file
data_file_path <- "MMM_Workshop_Data.xlsx" # Path to the data file
dir.create(file.path("Model Files"), recursive = TRUE, showWarnings = FALSE)
save_path <- "Model Files/"
dir.create(file.path(save_path, "Cor Files"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(save_path, "Transformed Data"), recursive = TRUE, showWarnings = FALSE)

# Load Data
config <- read.csv(config_file_path)
data <- switch(
  tools::file_ext(data_file_path),
  "csv" = read.csv(data_file_path),
  "xlsx" = readxl::read_excel(data_file_path, sheet = "Data"),
  stop("Unsupported file type")
) %>% as.data.frame()


# Function to Extract Config Variables
extract_vars <- function(var_name) {
  if (all(!is.na(config[[var_name]]))) {
    unlist(strsplit(config[[var_name]], ","))
  } else {
    NULL
  }
}

# Extract Config Variables
ignore_col <- config$time_column[1]
dependent_var <- config$dependent_var[1]
paid_media_spends <- extract_vars("paid_media_spends") %>% intersect(names(data))
competition_spend_vars <- extract_vars("competition_spend_vars")
untransformed_vars <- extract_vars("untransformed_vars")
tv_vars <- extract_vars("tv_vars")
traditional_vars <- extract_vars("traditional_vars")
atl_vars <- extract_vars("atl_vars")
digital_vars <- setdiff(paid_media_spends, c(tv_vars, traditional_vars, atl_vars))

X_untransformed <- data[, untransformed_vars, drop = FALSE]

# Geometric Adstock Function
adstock_geometric <- function(x, theta) {
  stopifnot(length(theta) == 1)
  if (length(x) > 1) {
    x_decayed <- c(x[1], rep(0, length(x) - 1))
    for (xi in 2:length(x_decayed)) {
      x_decayed[xi] <- x[xi] + theta * x_decayed[xi - 1]
    }
    thetaVecCum <- theta
    for (t in 2:length(x)) {
      thetaVecCum[t] <- thetaVecCum[t - 1] * theta
    } # plot(thetaVecCum)
  } else {
    x_decayed <- x
    thetaVecCum <- theta
  }
  inflation_total <- sum(x_decayed) / sum(x)
  return(list(x = x, x_decayed = x_decayed, thetaVecCum = thetaVecCum, inflation_total = inflation_total))
}

#Hill Transformation
saturation_hill <- function(x, alpha, gamma, x_marginal = NULL) {
  stopifnot(length(alpha) == 1)
  stopifnot(length(gamma) == 1)
  inflexion <- c(range(x) %*% c(1 - gamma, gamma)) # linear interpolation by dot product
  if (is.null(x_marginal)) {
    x_scurve <- x**alpha / (x**alpha + inflexion**alpha) # plot(x_scurve) summary(x_scurve)
  } else {
    x_scurve <- x_marginal**alpha / (x_marginal**alpha + inflexion**alpha)
  }
  return(x_scurve)
}

# Adstock Transformation function for TV
theta_range_tv <- seq(0.3, 0.8, by = 0.1)  
gamma_range_tv <- seq(0.3, 1, by = 0.1) 
alpha_range_tv <- seq(0.5, 3, by = 0.1)

geometric_hill_transform_tv <- function(x, varname) {
  len_x <- length(x)
  
  # Prepare output structure
  transformed_data <- data.frame(x = x)
  names(transformed_data)[1] <- varname  
  
  # Iterate over theta and gamma ranges
  for (theta in theta_range_tv) {
    adstocked_x <- adstock_geometric(x, theta)$x_decayed  # Apply geometric adstock transformation
    
    for (alpha in alpha_range_tv) {
      for (gamma in gamma_range_tv){
        hill_transformed_x <- saturation_hill(adstocked_x, alpha, gamma)
        
        # Add transformed series to output
        transformed_data[[paste0(varname, "_", theta, "_", alpha,"_", gamma)]] <- hill_transformed_x
      }
    }
  }
  
  return(transformed_data)
}

# Adstock Transformation function for digital variables like FB, Instagram etc
theta_range_digital <- seq(0, 0.3, by = 0.1)  
gamma_range_digital <- seq(0.3, 1, by = 0.1) 
alpha_range_digital <- seq(0.5, 3, by = 0.1)


geometric_hill_transform_digital <- function(x, varname) {
  len_x <- length(x)
  
  # Prepare output structure
  transformed_data <- data.frame(x = x)
  names(transformed_data)[1] <- varname  
  
  # Iterate over theta and gamma ranges
  for (theta in theta_range_digital) {
    adstocked_x <- adstock_geometric(x, theta)$x_decayed  # Apply geometric adstock transformation
    
    for (alpha in alpha_range_digital) {
      for (gamma in gamma_range_digital){
        hill_transformed_x <- saturation_hill(adstocked_x, alpha, gamma)
        
        # Add transformed series to output
        transformed_data[[paste0(varname, "_", theta, "_", alpha,"_", gamma)]] <- hill_transformed_x
      }
    }
  }
  
  return(transformed_data)
}

# Adstock Transformation function for organic variables like newsletter

theta_range_organic <- seq(0.1, 0.4, by = 0.1)  
gamma_range_organic <- seq(0.3, 1, by = 0.1) 
alpha_range_organic <- seq(0.5, 3, by = 0.1)


geometric_hill_transform_organic <- function(x, varname) {
  len_x <- length(x)
  
  # Prepare output structure
  transformed_data <- data.frame(x = x)
  names(transformed_data)[1] <- varname  
  
  # Iterate over theta and gamma ranges
  for (theta in theta_range_organic) {
    adstocked_x <- adstock_geometric(x, theta)$x_decayed  # Apply geometric adstock transformation
    
    for (alpha in alpha_range_organic) {
      for (gamma in gamma_range_organic){
        hill_transformed_x <- saturation_hill(adstocked_x, alpha, gamma)
        
        # Add transformed series to output
        transformed_data[[paste0(varname, "_", theta, "_", alpha,"_", gamma)]] <- hill_transformed_x
      }
    }
  }
  
  return(transformed_data)
}


# Adstock Transformation function for Traditional Media Variables like Print, Radio, OOH etc

theta_range_traditional <- seq(0.1, 0.4, by = 0.1)  
gamma_range_traditional <- seq(0.3, 1, by = 0.1) 
alpha_range_traditional <- seq(0.5, 3, by = 0.1)


geometric_hill_transform_traditional <- function(x, varname) {
  len_x <- length(x)
  
  # Prepare output structure
  transformed_data <- data.frame(x = x)
  names(transformed_data)[1] <- varname  
  
  # Iterate over theta and gamma ranges
  for (theta in theta_range_traditional) {
    adstocked_x <- adstock_geometric(x, theta)$x_decayed  # Apply geometric adstock transformation
    
    for (alpha in alpha_range_traditional) {
      for (gamma in gamma_range_traditional){
        hill_transformed_x <- saturation_hill(adstocked_x, alpha, gamma)
        
        # Add transformed series to output
        transformed_data[[paste0(varname, "_", theta, "_", alpha,"_", gamma)]] <- hill_transformed_x
      }
    }
  }
  
  return(transformed_data)
}


# Adstock Transformation function for Competition ATL Spends

theta_range_ATL <- seq(0.1, 0.8, by = 0.1)  
gamma_range_ATL <- seq(0.3, 1, by = 0.1) 
alpha_range_ATL <- seq(0.5, 3, by = 0.1)


geometric_hill_transform_ATL <- function(x, varname) {
  len_x <- length(x)
  
  # Prepare output structure
  transformed_data <- data.frame(x = x)
  names(transformed_data)[1] <- varname  
  
  # Iterate over theta and gamma ranges
  for (theta in theta_range_ATL) {
    adstocked_x <- adstock_geometric(x, theta)$x_decayed  # Apply geometric adstock transformation
    
    for (alpha in alpha_range_ATL) {
      for (gamma in gamma_range_ATL){
        hill_transformed_x <- saturation_hill(adstocked_x, alpha, gamma)
        
        # Add transformed series to output
        transformed_data[[paste0(varname, "_", theta, "_", alpha,"_", gamma)]] <- hill_transformed_x
      }
    }
  }
  
  return(transformed_data)
}

# Applying the adstock function to the media variables

tv_transformed <- lapply(tv_vars, function(tv) geometric_hill_transform_tv(data[[tv]],tv))
traditional_transformed <- lapply(traditional_vars, function(tr) geometric_hill_transform_traditional(data[[tr]],tr))
digital_transformed <- lapply(digital_vars, function(mv) geometric_hill_transform_digital(data[[mv]],mv))
atl_transformed <- lapply(competition_spend_vars, function(cv) geometric_hill_transform_ATL(data[[cv]],cv))

transformed_series_vars = c(tv_transformed, traditional_transformed, digital_transformed, atl_transformed)
paid_media_spends <- c(tv_vars, traditional_vars, digital_vars, competition_spend_vars)

# Specify the dependent variable
y <- data[[dependent_var]]

# Correlation of the transformed series of each media with the dependent variable 
cor_list <- list()
for (i in seq_along(paid_media_spends)) {
  cor_values <- c()
  p_values <- c()
  for (j in 1:length(transformed_series_vars[[i]])) {
    if(length(transformed_series_vars[[i]][,j])==length(y)){
      summary <- cor.test(transformed_series_vars[[i]][, j], y)
      cor_values[j] <- summary$estimate
      p_values[j] <- summary$p.value
    }else{
      cor_values[j] <- NA
      p_values[j] <- NA
    }
  }
  independent_var <- paste0(colnames(transformed_series_vars[[i]]))
  corr_df <- data.frame(
    Adstock = independent_var,
    Correlation = cor_values,
    pvalue = p_values
  )
  cor_list[[i]] <- corr_df
}

# Extracting the correlations of the transformed series with the dependent variable for each media 
# in separate CSV files

for (i in seq_along(paid_media_spends)) {
  cor_df <- data.frame(cor_list[[i]])
  colnames(cor_df) <- c("independent_variable","Correlation","pvalue")
  
  if(!(paid_media_spends[i] %in% competition_spend_vars)){
    # For own brand media variables
    cor_df = cor_df %>% arrange(.,desc(Correlation))
  }else{
    # For competition spend variables
    cor_df = cor_df %>% arrange(.,Correlation)
  }
  write.csv(cor_df,paste0(save_path,"Cor Files/",paid_media_spends[i],"_cor_df.csv"))
}

# Correlation of untransformed variables
cor_df_untransformed <- do.call(rbind, lapply(untransformed_vars, function(var) {
  test <- cor.test(X_untransformed[[var]], y)
  data.frame(Variable = var, Correlation = test$estimate, p_value = test$p.value)
}))

write.csv(cor_df_untransformed, paste0(save_path, "Cor Files/untransformed_cor_df.csv"), row.names = FALSE)

# Creating the data for model building. 
# model_data contains the raw data along with the transformed series of all media
model_data <- data

for(i in seq_along(paid_media_spends)){
  transformed_data_df <- data.frame(transformed_series_vars[[i]][,])
  model_data <- cbind(model_data,transformed_data_df)
  # Exporting the transformed data of each media variable
  write.csv(transformed_data_df,paste0(save_path,"Transformed Data/",paid_media_spends[i],"_transformed_data.csv"))
  
}



########### OLS Model Building ###########
attach(model_data)

# ─── 1. ASSUME `model_data` ALREADY EXISTS ───────────────────────────────────────────
# (From our previous steps, you have something like:)
#    model_data <- data
#    model_data$adjusted_sales <- adjusted_df$adjusted_sales

# ─── 2. CREATE “Dec_Peak_Dummy” AND “Feb_Dip_Dummy” ON model_data ───────────────────
# Automatically make column names unique
names(model_data) <- make.names(names(model_data), unique = TRUE)
model_data <- model_data %>%
  # 2a) extract month number, then build two dummy columns
  mutate(
    .month = month(Month),
    Dec_Peak_Dummy = ifelse(.month == 12, 1L, 0L),
    Feb_Dip_Dummy  = ifelse(.month ==  2, 1L, 0L)
  ) %>%
  select(-.month)  # remove the temporary `.month` column

# ─── 3. CREATE “Radio_Spends_transofrmed” ON model_data ────────────────────────────────────
model_data <- model_data %>%
  arrange(Month) %>% 
  mutate(
    Radio_Spends_transofrmed = lag(Radio_Spends, n = 1, default = NA)
  )
# If you prefer the first-row NA → 0, uncomment the next line:
model_data$Radio_Spends_transofrmed[is.na(model_data$Radio_Spends_transofrmed)] <- 0

# ─── 4. ADD “adjusted_sales” IF NOT ALREADY THERE ───────────────────────────────────
# smear_sales_jfm <- function(df, date_col, sales_col , pct_oct = 0,
#                                     pct_nov = 0, pct_dec = 0) {
#   df <- df %>%
#     mutate(
#       year = lubridate::year(!!sym(date_col)),
#       month = lubridate::month(!!sym(date_col)),
#       adjusted_sales = !!sym(sales_col)
#     )
  
#   for (year in unique(df$year)[1:2]) {
#     next_year <- year + 1
    
#     # Indices for source months (Nov of `year`, Dec of `year`, Jan of `next_year`)
#     idx_nov <- which(df$year == year & df$month == 11)
#     idx_dec <- which(df$year == year & df$month == 12)
#     idx_oct <- which(df$year == year & df$month == 10)
    
#     removed_amt <- 0
    
#     if (length(idx_nov)) {
#       removed_amt <- removed_amt + df[[sales_col]][idx_nov] * pct_nov
#       df$adjusted_sales[idx_nov] <- df[[sales_col]][idx_nov] * (1 - pct_nov)
#     }
    
#     if (length(idx_dec)) {
#       removed_amt <- removed_amt + df[[sales_col]][idx_dec] * pct_dec
#       df$adjusted_sales[idx_dec] <- df[[sales_col]][idx_dec] * (1 - pct_dec)
#     }
    
#     if (length(idx_oct)) {
#       removed_amt <- removed_amt + df[[sales_col]][idx_oct] * pct_oct
#       df$adjusted_sales[idx_oct] <- df[[sales_col]][idx_oct] * (1 - pct_oct)
#     }
    
#     # Indices of all months in next year
#     idx_next_year_months <- which(df$year == next_year)
    
#     if (length(idx_next_year_months) > 0) {
#       smear_value <- removed_amt / length(idx_next_year_months)
#       df$adjusted_sales[idx_next_year_months] <- df$adjusted_sales[idx_next_year_months] + smear_value
#     }
#   }
  
#   return(df)
# }

# ─────────────────────────────────────────────────────────────────────────────────────
# The function we are going to define is smear_sales_jfm(): 
#    Shift a percentage of Sales from Oct/Nov/Dec of year N into Jan/Feb/Mar of year N+1.
#
# Args:
#   df          - data frame with at least:
#                   * a date column (named by date_col, class POSIXct)
#                   * a numeric sales column (named by sales_col)
#   date_col    - string, name of the POSIXct column (e.g. "Month")
#   sales_col   - string, name of the sales column (e.g. "Sales_Volume_Total")
#   pct_oct     - fraction to remove from October sales of year N (0 ≤ pct_oct ≤ 1)
#   pct_nov     - fraction to remove from November sales of year N (0 ≤ pct_nov ≤ 1)
#   pct_dec     - fraction to remove from December sales of year N (0 ≤ pct_dec ≤ 1)
#   jan_share   - (optional) numeric weight for January (next year). If NULL, defaults to 1.
#   feb_share   - (optional) numeric weight for February (next year). If NULL, defaults to 1.
#   mar_share   - (optional) numeric weight for March (next year). If NULL, defaults to 1.
#
# Behavior:
#   • Finds year N as each unique year in date_col except the last year (since N+1 must exist).
#   • Removes pct_oct * sales from Oct of year N, pct_nov * sales from Nov N, pct_dec * sales from Dec N.
#   • Sums these removed amounts into total_shifted.
#   • If jan_share, feb_share, mar_share are all NULL, sets them to (1,1,1) → equal 1/3 shares.
#   • Otherwise, normalizes: sum_shares = jan_share+feb_share+mar_share, allocates:
#       Jan (N+1)   += total_shifted * (jan_share / sum_shares)
#       Feb (N+1)   += total_shifted * (feb_share / sum_shares)
#       Mar (N+1)   += total_shifted * (mar_share / sum_shares)
#   • Leaves all other months of year N+1 unchanged.
#
# Returns:
#   A tibble/data.frame identical to df, with a new column `adjusted_sales` alongside the original `sales_col`.
# ─────────────────────────────────────────────────────────────────────────────────────
smear_sales_jfm <- function(
  df,
  date_col,
  sales_col,
  pct_oct = 0,
  pct_nov = 0,
  pct_dec = 0,
  jan_share = NULL,
  feb_share = NULL,
  mar_share = NULL
) {
  # 1) Ensure input columns exist
  stopifnot(date_col %in% names(df))
  stopifnot(sales_col %in% names(df))
  
  # 2) Convert to tibble + extract year/month
  df2 <- df %>%
    mutate(
      .date  = as.Date(!!sym(date_col)),
      year   = year(.date),
      month  = month(.date),
      adjusted_sales = !!sym(sales_col)
    )
  
  # 3) Prepare the JFM weighting vector
  if (is.null(jan_share) || is.null(feb_share) || is.null(mar_share)) {
    # default equal shares = 1,1,1
    jan_w <- feb_w <- mar_w <- 1
  } else {
    jan_w <- jan_share
    feb_w <- feb_share
    mar_w <- mar_share
  }
  total_w <- jan_w + feb_w + mar_w
  # Avoid dividing by zero
  if (total_w <= 0) {
    stop("Sum of jan_share + feb_share + mar_share must be > 0.")
  }
  
  # 4) Loop over each year (except the last one, since it has no "next year")
  all_years <- sort(unique(df2$year))
  # We only process years where (year+1) also appears in df2
  process_years <- all_years[all_years %in% (all_years - 1)]
  
  for (yr in process_years) {
    next_year <- yr + 1
    
    # 4A) Identify indices for Oct, Nov, Dec of year = yr
    idx_oct <- which(df2$year == yr & df2$month == 10)
    idx_nov <- which(df2$year == yr & df2$month == 11)
    idx_dec <- which(df2$year == yr & df2$month == 12)
    
    removed_amt <- 0
    
    # 4B) Remove from October
    if (length(idx_oct) == 1) {
      amt_oct <- df2[[sales_col]][idx_oct] * pct_oct
      removed_amt <- removed_amt + amt_oct
      df2$adjusted_sales[idx_oct] <- df2[[sales_col]][idx_oct] * (1 - pct_oct)
    }
    # 4C) Remove from November
    if (length(idx_nov) == 1) {
      amt_nov <- df2[[sales_col]][idx_nov] * pct_nov
      removed_amt <- removed_amt + amt_nov
      df2$adjusted_sales[idx_nov] <- df2[[sales_col]][idx_nov] * (1 - pct_nov)
    }
    # 4D) Remove from December
    if (length(idx_dec) == 1) {
      amt_dec <- df2[[sales_col]][idx_dec] * pct_dec
      removed_amt <- removed_amt + amt_dec
      df2$adjusted_sales[idx_dec] <- df2[[sales_col]][idx_dec] * (1 - pct_dec)
    }
    
    if (removed_amt == 0) {
      # Nothing to shift for this year; skip
      next
    }
    
    # 4E) Identify Jan, Feb, Mar of next year
    idx_jan <- which(df2$year == next_year & df2$month == 1)
    idx_feb <- which(df2$year == next_year & df2$month == 2)
    idx_mar <- which(df2$year == next_year & df2$month == 3)
    
    # 4F) Allocate removed_amt to JFM of next_year, using normalized weights
    if (length(idx_jan) == 1) {
      df2$adjusted_sales[idx_jan] <- df2$adjusted_sales[idx_jan] +
        removed_amt * (jan_w / total_w)
    }
    if (length(idx_feb) == 1) {
      df2$adjusted_sales[idx_feb] <- df2$adjusted_sales[idx_feb] +
        removed_amt * (feb_w / total_w)
    }
    if (length(idx_mar) == 1) {
      df2$adjusted_sales[idx_mar] <- df2$adjusted_sales[idx_mar] +
        removed_amt * (mar_w / total_w)
    }
  }
  
  # 5) Return the original columns + adjusted_sales
  out <- df2 %>%
    select(-.date, -year, -month)
  return(out)
}


adjusted_df <- smear_sales_jfm(
  MMM_Workshop_Data,
  date_col = "Month",
  sales_col = "Sales_Volume_Total",
  pct_nov = 0.1,
  pct_dec = 0.15,
  pct_oct = 0.05
)# Just in case, make sure it is present:
model_data$adjusted_sales <- adjusted_df$adjusted_sales

# ─── 5. REBUILD OUR PREDICTORS VECTOR TO INCLUDE THE NEW COLUMNS ───────────────────
predictors <- c(
  "TV_GRP_transformed",
  "Outdoor_Spends_transformed",
  "Radio_Spends_transofrmed",           # ← now included
  "Direct_Display_Spend_transformed",
  "Meta1_Spends_transformed",
  "Meta2_Spends_transformed",
  "Youtube_Spends_transformed",
  "Brand_P_ATL_Spends",
  "Dec_Peak_Dummy",              
  "Feb_Dip_Dummy",               
  "Brand_PH_ATL_Spends_transformed",
  "Programmatic_Video_Spends_transformed"
)

# ─── 6. BUILD THE FORMULA AND FIT THE SINGLE LINEAR MODEL ───────────────────────────
formula <- as.formula(
  paste0("adjusted_sales ~ ", paste(predictors, collapse = " + "))
)

lm_model <- lm(formula, data = model_data)

# ─── 7. CHECK THE RESULT ────────────────────────────────────────────────────────────
summary(lm_model)

sign_check <- function(model, negative_prefixes, positive_prefixes) {
  coefs <- coef(model)
  coefs <- coefs[names(coefs) != "(Intercept)"]
  
  result <- list()
  
  for (var_name in names(coefs)) {
    matched_negative <- any(sapply(negative_prefixes, function(prefix) startsWith(var_name, prefix)))
    matched_positive <- any(sapply(positive_prefixes, function(prefix) startsWith(var_name, prefix)))
    
    if (matched_negative && matched_positive) {
      result[[var_name]] <- "CONFLICT: Matches both positive and negative"
    } else if (matched_negative) {
      result[[var_name]] <- if (coefs[[var_name]] < 0) "PASS (negative)" else "FAIL (expected negative)"
    } else if (matched_positive) {
      result[[var_name]] <- if (coefs[[var_name]] > 0) "PASS (positive)" else "FAIL (expected positive)"
    } else {
      result[[var_name]] <- "SKIPPED (no prefix match)"
    }
  }
  
  for (var in names(result)) {
    cat(sprintf("%-50s : %s\n", var, result[[var]]))
  }
  
  invisible(result)
}
# Define expected sign **prefixes**
negative_prefixes <- c("Feb_Dip_Dummy", "Brand_PH_ATL_Spends", "Brand_P_ATL_Spends")
positive_prefixes <- c("TV_GRP", "Outdoor_Spends", "Radio_Spends", 
                       "Direct_Display_Spend", "Meta1_Spends", "Meta2_Spends", 
                       "Youtube_Spends", "Dec_Peak_Dummy", "Programmatic_Video_Spends")

# Run the sign check
sign_check(lm_model, negative_prefixes, positive_prefixes)
# the sign check was unsuccessful.

############################### Change only December ###############################
##HOw about we slowly chnge only the december from 0 to 40%
# and see r squared, sign proportions, adjusted r suqared, and possibley, the coefficients

# ─────────────────────────────────────────────────────────────────────────────────────
# ASSUMPTIONS BEFORE YOU START:
#   • `model_data` already exists inour workspace, and has these columns:
#       – Month               (POSIXct)
#       – Radio_Spends        (original radio spend)
#       – (all other raw‐media columns: TV_GRP_transformed, Outdoor_Spends_transformed, etc.)
#       – Dec_Peak_Dummy, Feb_Dip_Dummy, Radio_Spends_transofrmed (already created)
#       – adjusted_sales      (may be present from a previous run, but we will overwrite it)
#   • The `smear_sales_jfm()` function exists exactly as you posted.
#   • The single linear model formula you want to use is:
#         adjusted_sales ~ TV_GRP_transformed 
#                        + Outdoor_Spends_transformed
#                        + Radio_Spends_transofrmed
#                        + Direct_Display_Spend_transformed
#                        + Meta1_Spends_transformed
#                        + Meta2_Spends_transformed
#                        + Youtube_Spends_transformed
#                        + Brand_P_ATL_Spends
#                        + Dec_Peak_Dummy
#                        + Feb_Dip_Dummy
#                        + Brand_PH_ATL_Spends_transformed
#                        + Programmatic_Video_Spends_transformed
#
#   • We already have defined `sign_check()` (which returns a named vector of PASS/FAIL/skipped).
#     We will rely on it to count how many “FAIL” statuses appear.
#
#   • Libraries used below: dplyr, lubridate, broom, ggplot2, tidyr.
# ─────────────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(lubridate)
library(broom)
library(ggplot2)
library(tidyr)

# ─────────────────────────────────────────────────────────────────────────────────────
# 1) REDEFINE sign_check() to RETURN a named character vector (so we can count FAILs).
#    We keep the prefixes exactly as you gave them.
# ─────────────────────────────────────────────────────────────────────────────────────
sign_check <- function(model, negative_prefixes, positive_prefixes) {
  coefs <- coef(model)
  coefs <- coefs[names(coefs) != "(Intercept)"]        # drop intercept
  
  result <- c()
  for (var_name in names(coefs)) {
    matched_negative <- any(sapply(negative_prefixes, function(pref) startsWith(var_name, pref)))
    matched_positive <- any(sapply(positive_prefixes, function(pref) startsWith(var_name, pref)))
    
    if (matched_negative && matched_positive) {
      result[var_name] <- "FAIL (conflict: both)" 
    } else if (matched_negative) {
      if (coefs[[var_name]] < 0) {
        result[var_name] <- "PASS (neg)"
      } else {
        result[var_name] <- "FAIL (expected neg)"
      }
    } else if (matched_positive) {
      if (coefs[[var_name]] > 0) {
        result[var_name] <- "PASS (pos)"
      } else {
        result[var_name] <- "FAIL (expected pos)"
      }
    } else {
      result[var_name] <- "SKIPPED"
    }
  }
  return(result)
}

# ─────────────────────────────────────────────────────────────────────────────────────
# 2) SPECIFY “expected sign” PREFIXES
#    (copy exactly what you used earlier)
# ─────────────────────────────────────────────────────────────────────────────────────
negative_prefixes <- c(
  "Feb_Dip_Dummy",
  "Brand_PH_ATL_Spends",
  "Brand_P_ATL_Spends"
)
positive_prefixes <- c(
  "TV_GRP",
  "Outdoor_Spends",
  "Radio_Spends",
  "Direct_Display_Spend",
  "Meta1_Spends",
  "Meta2_Spends",
  "Youtube_Spends",
  "Dec_Peak_Dummy",
  "Programmatic_Video_Spends"
)

# ─────────────────────────────────────────────────────────────────────────────────────
# 3) DEFINE THE PREDICTOR NAMES AND FORMULA ONCE
# ─────────────────────────────────────────────────────────────────────────────────────
predictors <- c(
  "TV_GRP_transformed",
  "Outdoor_Spends_transformed",
  "Radio_Spends_transofrmed",           
  "Direct_Display_Spend_transformed",
  "Meta1_Spends_transformed",
  "Meta2_Spends_transformed",
  "Youtube_Spends_transformed",
  "Brand_P_ATL_Spends",
  "Dec_Peak_Dummy",              
  "Feb_Dip_Dummy",               
  "Brand_PH_ATL_Spends_transformed",
  "Programmatic_Video_Spends_transformed"
)

formula_string <- paste0("adjusted_sales ~ ", paste(predictors, collapse = " + "))
model_formula  <- as.formula(formula_string)

# ─────────────────────────────────────────────────────────────────────────────────────
# 4) SET UP THE GRID FOR pct_dec (0% to 40% in 1% increments)
#    We store percentages as decimals: 0.00, 0.01, 0.02, …, 0.40
# ─────────────────────────────────────────────────────────────────────────────────────
pct_dec_seq <- seq(0, 40, by = 1) / 100    # 0.00, 0.01, …, 0.40

# PREALLOCATE DATA.FRAMES to collect metrics and coefficients
metrics_list <- list()
coef_list    <- list()

# ─────────────────────────────────────────────────────────────────────────────────────
# 5) MAIN SENSITIVITY LOOP
# ─────────────────────────────────────────────────────────────────────────────────────
for (i in seq_along(pct_dec_seq)) {
  pct_dec_value <- pct_dec_seq[i]
  
  # 5A) RE‐COMPUTE adjusted_sales ON THE ORIGINAL MMM_WORKSHOP_DATA
  #     (NOT ON “model_data” directly, because we smear on the raw data frame MMM_Workshop_Data)
  temp_adjusted_df <- smear_sales_jfm(
    MMM_Workshop_Data,
    date_col  = "Month",
    sales_col = "Sales_Volume_Total",
    pct_oct   = 0,
    pct_nov   = 0,
    pct_dec   = pct_dec_value
  )
  
  # 5B) COPY THAT adjusted_sales BACK INTO model_data
  #     (overwrite the existing column at each iteration)
  model_data$adjusted_sales <- temp_adjusted_df$adjusted_sales
  
  # 5C) FIT THE LINEAR MODEL
  lm_mod <- lm(model_formula, data = model_data)
  
  # 5D) EXTRACT R‐SQUARED AND ADJ R‐SQUARED
  lm_sum <- summary(lm_mod)
  this_r2   <- lm_sum$r.squared
  this_adjr <- lm_sum$adj.r.squared
  
  # 5E) RUN sign_check() TO COUNT HOW MANY “FAIL”
  #     sniff out any entry in sign_check() result that contains “FAIL”
  sc       <- sign_check(lm_mod, negative_prefixes, positive_prefixes)
  fail_cnt <- sum(grepl("^FAIL", sc))
  
  # 5F) STORE METRICS IN A TIDY LIST
  metrics_list[[i]] <- data.frame(
    pct_dec       = pct_dec_value,
    n_sign_fails  = fail_cnt,
    R_squared     = this_r2,
    Adj_R_squared = this_adjr
  )
  
  # 5G) EXTRACT ALL COEFFICIENTS (DROP INTERCEPT)
  coefs_named <- coef(lm_mod)
  coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
  
  # Put into a data.frame long‐form:
  coef_list[[i]] <- data.frame(
    pct_dec    = pct_dec_value,
    variable   = names(coefs_named),
    coefficient = as.numeric(coefs_named),
    stringsAsFactors = FALSE
  )
}

# ─────────────────────────────────────────────────────────────────────────────────────
# 6) BIND ALL THE METRICS AND COEFFICIENTS INTO TWO DATA.FRAMES
# ─────────────────────────────────────────────────────────────────────────────────────
metrics_df <- do.call(rbind, metrics_list)
coef_df    <- do.call(rbind, coef_list)

# 6A) For convenience, let’s also pull out the “baseline” coefficients at pct_dec = 0:
baseline_coefs <- coef_df %>%
  filter(pct_dec == 0) %>%
  select(variable, coefficient) %>%
  rename(baseline = coefficient)

# ─────────────────────────────────────────────────────────────────────────────────────
# 7) VIEW / SAVE THE METRICS TABLE
#    This has four columns: pct_dec, n_sign_fails, R_squared, Adj_R_squared
# ─────────────────────────────────────────────────────────────────────────────────────
print(metrics_df)
# → if you want to save:
# write.csv(metrics_df, "sensitivity_metrics.csv", row.names = FALSE)

# ─────────────────────────────────────────────────────────────────────────────────────
# 8) CREATE HISTOGRAMS OF EVERY PREDICTOR’S COEFFICIENT ACROSS pct_dec
#     We facet‐wrap so there’s one panel per predictor, and overlay a vertical
#     line at the “baseline” value (pct_dec = 0).
# ─────────────────────────────────────────────────────────────────────────────────────
# First, join in the baseline values so we can draw a vertical line:
coef_df_plot <- coef_df %>%
  left_join(baseline_coefs, by = "variable")

# Plot:
ggplot(coef_df_plot, aes(x = coefficient)) +
  geom_histogram(bins = 20, color = "black", fill = "lightblue") +
  geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(
    title = "Sensitivity of Each Coefficient as pct_dec Goes from 0%→40%",
    x = "Estimated Coefficient",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# If you want to save the histogram as a PNG:
# ggsave("coef_sensitivity_histograms.png", width = 12, height = 10)

# ─────────────────────────────────────────────────────────────────────────────────────
# 9) (OPTIONAL) PLOT METRICS OVER pct_dec
#    e.g. how does # of sign‐fails / R² move as pct_dec changes?
# ─────────────────────────────────────────────────────────────────────────────────────
# Example: sign‐fails vs pct_dec
ggplot(metrics_df, aes(x = pct_dec * 100, y = n_sign_fails)) +
  geom_line(size = 1, color = "darkgreen") +
  geom_point(size = 2) +
  labs(
    title = "Number of Sign‐Fails vs. December Shift % (0→40)",
    x = "December Shift (%)",
    y = "Count of Sign_Fails"
  ) +
  theme_minimal()

# Example: R² and Adj R² vs pct_dec
metrics_long <- metrics_df %>%
  pivot_longer(cols = c(R_squared, Adj_R_squared),
               names_to = "metric", values_to = "value")

ggplot(metrics_long, aes(x = pct_dec * 100, y = value, color = metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "R² and Adjusted R² vs. December Shift % (0→40)",
    x = "December Shift (%)",
    y = "Metric Value",
    color = ""
  ) +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────────────────────────
# That’s it! 
#
# • `metrics_df` holds: pct_dec (0→0.40), n_sign_fails, R_squared, Adj_R_squared  
# • `coef_df`   holds: pct_dec, variable, coefficient (for each predictor)  
# • The faceted histogram (one panel per variable) shows how that variable’s coefficient
#   “moves” as you shift December from 0%→40%, with the baseline (pct_dec = 0) in red.
# • You can tweak the number of bins, colors, or facet layout as you wish.
# ─────────────────────────────────────────────────────────────────────────────────────

################ Change only one month at a time################


# Try october and november now and focus only on one month
# ─────────────────────────────────────────────────────────────────────────────────────
# PREREQUISITES:
# • MMM_Workshop_Data: data.frame with at least columns Month (POSIXct) and Sales_Volume_Total.
# • model_data:       data.frame which already contains:
#      – Month (POSIXct)
#      – Radio_Spends          (original radio spend)
#      – Dec_Peak_Dummy, Feb_Dip_Dummy, Radio_Spends_transofrmed (as you built earlier)
#      – All other raw predictors: 
#         TV_GRP_transformed, Outdoor_Spends_transformed, 
#         Direct_Display_Spend_transformed, Meta1_Spends_transformed,
#         Meta2_Spends_transformed, Youtube_Spends_transformed, 
#         Brand_P_ATL_Spends, Brand_PH_ATL_Spends_transformed, 
#         Programmatic_Video_Spends_transformed, etc.
#
# • smear_sales_jfm(): our function that takes (df, date_col, sales_col, pct_oct, pct_nov, pct_dec)
#   and returns df with an updated adjusted_sales column.  
#   (We assume you have the version that shifts Oct/Nov/Dec.)
#
# • The single‐model formula you want to fit is exactly:
#      adjusted_sales ~ TV_GRP_transformed 
#                     + Outdoor_Spends_transformed
#                     + Radio_Spends_transofrmed
#                     + Direct_Display_Spend_transformed
#                     + Meta1_Spends_transformed
#                     + Meta2_Spends_transformed
#                     + Youtube_Spends_transformed
#                     + Brand_P_ATL_Spends
#                     + Dec_Peak_Dummy
#                     + Feb_Dip_Dummy
#                     + Brand_PH_ATL_Spends_transformed
#                     + Programmatic_Video_Spends_transformed
#
# • Required libraries:
library(dplyr)
library(lubridate)
library(broom)
library(ggplot2)
library(tidyr)

# ─────────────────────────────────────────────────────────────────────────────────────
# 1) DEFINE sign_check(), exactly as before; now it returns a named vector of "PASS"/"FAIL"/"SKIPPED".
# ─────────────────────────────────────────────────────────────────────────────────────
sign_check <- function(model, negative_prefixes, positive_prefixes) {
  coefs <- coef(model)
  coefs <- coefs[names(coefs) != "(Intercept)"]
  
  result <- c()
  for (var_name in names(coefs)) {
    matched_negative <- any(sapply(negative_prefixes, function(pref) startsWith(var_name, pref)))
    matched_positive <- any(sapply(positive_prefixes, function(pref) startsWith(var_name, pref)))
    
    if (matched_negative && matched_positive) {
      result[var_name] <- "FAIL (conflict: both)"
    } else if (matched_negative) {
      result[var_name] <- if (coefs[[var_name]] < 0) "PASS (neg)" else "FAIL (expected neg)"
    } else if (matched_positive) {
      result[var_name] <- if (coefs[[var_name]] > 0) "PASS (pos)" else "FAIL (expected pos)"
    } else {
      result[var_name] <- "SKIPPED"
    }
  }
  return(result)
}

# ─────────────────────────────────────────────────────────────────────────────────────
# 2) SPECIFY which prefixes mean “negative” vs. “positive” (exactly as you had them):
# ─────────────────────────────────────────────────────────────────────────────────────
negative_prefixes <- c(
  "Feb_Dip_Dummy",
  "Brand_PH_ATL_Spends",
  "Brand_P_ATL_Spends"
)
positive_prefixes <- c(
  "TV_GRP",
  "Outdoor_Spends",
  "Radio_Spends",
  "Direct_Display_Spend",
  "Meta1_Spends",
  "Meta2_Spends",
  "Youtube_Spends",
  "Dec_Peak_Dummy",
  "Programmatic_Video_Spends"
)

# ─────────────────────────────────────────────────────────────────────────────────────
# 3) BUILD the fixed predictor list and model formula one time (we reuse inside the loop):
# ─────────────────────────────────────────────────────────────────────────────────────
predictors <- c(
  "TV_GRP_transformed",
  "Outdoor_Spends_transformed",
  "Radio_Spends_transofrmed",
  "Direct_Display_Spend_transformed",
  "Meta1_Spends_transformed",
  "Meta2_Spends_transformed",
  "Youtube_Spends_transformed",
  "Brand_P_ATL_Spends",
  "Dec_Peak_Dummy",
  "Feb_Dip_Dummy",
  "Brand_PH_ATL_Spends_transformed",
  "Programmatic_Video_Spends_transformed"
)
formula_string <- paste0("adjusted_sales ~ ", paste(predictors, collapse = " + "))
model_formula  <- as.formula(formula_string)

# ─────────────────────────────────────────────────────────────────────────────────────
# 4) WRAPPER FUNCTION: run_month_sensitivity()
#    - month_to_vary: one of "oct", "nov", or "dec"
#    - max_pct:       maximum percentage (as a decimal, e.g. 0.40 for 40%)
#    - n_steps:       (optional) number of steps between 0 and max_pct; defaults to 41 (i.e. 0%,1%,2%...40%)
#    - returns a list with:
#       • metrics_df: data.frame of (pct_varied, n_sign_fails, R_squared, Adj_R_squared)
#       • coef_df:    data.frame of (pct_varied, variable, coefficient)
#    - also automatically plots:  
#       (a) faceted histogram of each coefficient  
#       (b) sign‐fails vs. pct_varied  
#       (c) R² vs. pct_varied and Adj R² vs. pct_varied  
# ─────────────────────────────────────────────────────────────────────────────────────
run_month_sensitivity <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
) {
  # Validate month_to_vary
  month_to_vary <- tolower(month_to_vary)
  if (!month_to_vary %in% c("oct", "nov", "dec")) {
    stop("`month_to_vary` must be one of: 'oct', 'nov', 'dec'.")
  }
  
  # Build the sequence 0, 1/n_steps-1, 2/n_steps-1, ..., max_pct
  pct_seq <- seq(0, max_pct, length.out = n_steps)
  
  # Pre‐allocate lists to collect metrics & coefficients
  metrics_list <- vector("list", length(pct_seq))
  coef_list    <- vector("list", length(pct_seq))
  
  for (i in seq_along(pct_seq)) {
    pct_val <- pct_seq[i]
    
    # Decide which argument to pass to smear_sales_jfm()
    pct_oct <- if (month_to_vary == "oct") pct_val else 0
    pct_nov <- if (month_to_vary == "nov") pct_val else 0
    pct_dec <- if (month_to_vary == "dec") pct_val else 0
    
    # 4A) Recompute adjusted_sales on the RAW MMM_Workshop_Data
    temp_adj <- smear_sales_jfm(
      MMM_Workshop_Data,
      date_col  = "Month",
      sales_col = "Sales_Volume_Total",
      pct_oct   = pct_oct,
      pct_nov   = pct_nov,
      pct_dec   = pct_dec
    )
    
    # 4B) Copy adjusted_sales back into model_data
    model_data$adjusted_sales <- temp_adj$adjusted_sales
    
    # 4C) Fit the single linear model
    lm_mod <- lm(model_formula, data = model_data)
    lm_sum <- summary(lm_mod)
    
    # 4D) Extract R² and Adjusted R²
    this_r2   <- lm_sum$r.squared
    this_adjr <- lm_sum$adj.r.squared
    
    # 4E) Count sign‐fails
    sc       <- sign_check(lm_mod, negative_prefixes, positive_prefixes)
    fail_cnt <- sum(grepl("^FAIL", sc))
    
    # 4F) Store metrics
    metrics_list[[i]] <- data.frame(
      pct_varied    = pct_val,
      n_sign_fails  = fail_cnt,
      R_squared     = this_r2,
      Adj_R_squared = this_adjr
    )
    
    # 4G) Extract all coefficients (drop intercept)
    coefs_named <- coef(lm_mod)
    coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
    coef_list[[i]] <- data.frame(
      pct_varied  = pct_val,
      variable    = names(coefs_named),
      coefficient = as.numeric(coefs_named),
      stringsAsFactors = FALSE
    )
  }
  
  # 5) Bind lists into data.frames
  metrics_df <- do.call(rbind, metrics_list)
  coef_df    <- do.call(rbind, coef_list)
  
  # 6) Baseline coefficients (pct_varied == 0)
  baseline_coefs <- coef_df %>%
    filter(pct_varied == 0) %>%
    select(variable, coefficient) %>%
    rename(baseline = coefficient)
  
  # 7) Join baseline into coef_df for plotting
  coef_df_plot <- coef_df %>%
    left_join(baseline_coefs, by = "variable")
  
  # 8) FACETED HISTOGRAM: each predictor’s coefficient across all pct_varied,
  #    with a red vertical line at the baseline (pct_varied = 0) value.
  hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
    geom_histogram(bins = 20, color = "black", fill = "lightblue") +
    geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    labs(
      title = paste0(
        "Coefficient‐Distribution: varying ",
        toupper(month_to_vary), " from 0→", max_pct*100, "%"
      ),
      x = "Estimated Coefficient",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  # 9) LINE PLOT: # of sign‐fails vs. pct_varied
  sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
    geom_line(size = 1, color = "darkgreen") +
    geom_point(size = 2) +
    labs(
      title = paste0("Sign‐Fails vs. ", toupper(month_to_vary), " Shift (%)"),
      x = paste0(toupper(month_to_vary), " Shift (%)"),
      y = "Number of Sign‐Fails"
    ) +
    theme_minimal()
  
  # 10) LINE PLOT: R² and Adj R² vs. pct_varied
  metrics_long <- metrics_df %>%
    pivot_longer(
      cols      = c(R_squared, Adj_R_squared),
      names_to  = "metric",
      values_to = "value"
    )
  r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste0("R² & Adj R² vs. ", toupper(month_to_vary), " Shift (%)"),
      x = paste0(toupper(month_to_vary), " Shift (%)"),
      y = "Metric",
      color = ""
    ) +
    theme_minimal()
  
  # 11) PRINT / RETURN
  print(hist_plot)
  print(sign_fail_plot)
  print(r2_plot)
  
  return(list(
    metrics_df = metrics_df,
    coef_df    = coef_df,
    hist_plot  = hist_plot,
    sign_fail_plot = sign_fail_plot,
    r2_plot    = r2_plot
  ))
}
# ─────────────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────────────
# USAGE EXAMPLES:
#  
# 1) Vary DECEMBER from 0% → 40% in 1% steps:
   result_dec <- run_month_sensitivity(month_to_vary = "dec", max_pct = 0.40)
#  
# 2) Vary NOVEMBER from 0% → 25% in 1% steps:
   result_nov <- run_month_sensitivity(month_to_vary = "nov", max_pct = 0.4)
#  
# 3) Vary OCTOBER from 0% → 10% in 1% steps:
   result_oct <- run_month_sensitivity(month_to_vary = "oct", max_pct = 0.4)
#
# After each call, you automatically see three plots:
#   • A faceted histogram of coefficients (with baseline in red)  
#   • “# of sign‐fails vs. %”  
#   • “R² & Adj R² vs. %”  
#
# You also get back a list containing:
#   – result_$metrics_df  (a data.frame of pct_varied, n_sign_fails, R², Adj R²)  
#   – result_$coef_df     (a data.frame of pct_varied, variable, coefficient)  
#   – result_$hist_plot   (the ggplot object for the histogram)  
#   – result_$sign_fail_plot (the ggplot object for sign‐fails vs. %)  
#   – result_$r2_plot     (the ggplot object for R² vs. %)
# ─────────────────────────────────────────────────────────────────────────────────────

################### Change the modelling using adstock values only for ###################
   # Now, we try to go down the list of adstcoks by correlation values to see how they are. IF it diesnt help at all, I
   # plan to just drop the variable.
   # ─────────────────────────────────────────────────────────────────────────────────────
   # 1.1) SIGN‐CHECK FUNCTION (if not already defined)
   #     It returns a named character vector, “PASS”/“FAIL”/“SKIPPED” for each non‐intercept coef.
   # ─────────────────────────────────────────────────────────────────────────────────────
   sign_check <- function(model, negative_prefixes, positive_prefixes) {
     coefs <- coef(model)
     coefs <- coefs[names(coefs) != "(Intercept)"]
     
     result <- setNames(character(length(coefs)), names(coefs))
     for (var_name in names(coefs)) {
       matched_negative <- any(sapply(negative_prefixes, function(pref) startsWith(var_name, pref)))
       matched_positive <- any(sapply(positive_prefixes, function(pref) startsWith(var_name, pref)))
       
       if (matched_negative && matched_positive) {
         result[var_name] <- "FAIL (conflict: both)"
       } else if (matched_negative) {
         result[var_name] <- if (coefs[[var_name]] < 0) "PASS (neg)" else "FAIL (expected neg)"
       } else if (matched_positive) {
         result[var_name] <- if (coefs[[var_name]] > 0) "PASS (pos)" else "FAIL (expected pos)"
       } else {
         result[var_name] <- "SKIPPED"
       }
     }
     return(result)
   }
   
   # ─────────────────────────────────────────────────────────────────────────────────────
   # 1.2) attempt_replace_and_check()
   #
   #     Given a single predictor (fail_var) whose sign just failed, search its brand/competition’s
   #     correlation‐ranked hyperparam list, refit with the first hyperparam that yields a correct sign.
   #
   #     Inputs:
   #       • fail_var:           character, the single predictor that failed sign (e.g. “TV_Spends_transformed”)
   #       • current_predictors: character vector of column names currently in the model
   #       • media_list:         same as `paid_media_spends` (vector of “base media” names)
   #       • cor_list:           list of data.frames, each cor_list[[i]] has columns
   #                              independent_variable (e.g. “TV_Spends_transformed”), Correlation, pvalue,
   #                              sorted so row 1 is highest‐corr for that base media
   #       • model_data:         the data.frame (with all transformed columns already present)
   #       • negative_prefixes, positive_prefixes: for sign_check()
   #
   #     Returns:
   #       • A new character vector of predictors (with fail_var replaced by the first “correct‐sign” hyperparam),
   #         or NULL if no hyperparam fixes the sign.
   # ─────────────────────────────────────────────────────────────────────────────────────
   attempt_replace_and_check <- function(
    fail_var,
    current_predictors,
    media_list,
    cor_list,
    model_data,
    negative_prefixes,
    positive_prefixes
   ) {
     # 1) which “base media” does fail_var start with?
     base_media <- NULL
     for (m in media_list) {
       if (startsWith(fail_var, m)) {
         base_media <- m
         break
       }
     }
     if (is.null(base_media)) {
       # If fail_var is not a paid‐media (e.g. a dummy or lag), skip.
       return(NULL)
     }
     
     # 2) find its index in media_list
     idx_media <- which(media_list == base_media)
     if (length(idx_media) != 1) {
       return(NULL)
     }
     
     # 3) grab that media’s correlation table (already sorted externally)
     corr_df <- cor_list[[idx_media]]
     
     # 4) iterate through each candidate hyperparam column in descending‐corr order
     for (candidate in corr_df$independent_variable) {
       # skip if model_data does not actually have this column
       if (!candidate %in% names(model_data)) next
       
       # Build a new predictor vector: replace fail_var with candidate
       new_preds <- current_predictors
       new_preds[new_preds == fail_var] <- candidate
       
       # Fit new model
       formula_str <- paste0("adjusted_sales ~ ", paste(new_preds, collapse = " + "))
       lm_candidate <- lm(as.formula(formula_str), data = model_data)
       
       # Check the sign of candidate
       sc_cand <- sign_check(lm_candidate, negative_prefixes, positive_prefixes)
       this_status <- sc_cand[[candidate]]
       if (grepl("^PASS", this_status)) {
         # Found first hyperparam that fixes the sign
         return(new_preds)
       }
       # else, continue to next candidate
     }
     
     # 5) if nobody fixed it, return NULL
     return(NULL)
   }
   
   # ─────────────────────────────────────────────────────────────────────────────────────
   # 1.3) fit_with_sign_correction()
   #
   #     Fits the OLS with base_predictors. If zero or >1 variables fail, returns the original. 
   #     If exactly one fails, calls attempt_replace_and_check() to try hyperparam replacements.
   #
   #     Inputs:
   #       • model_data:        data frame with all columns (including adjusted_sales)
   #       • base_predictors:   character vector of column names you want by default
   #       • paid_media_spends: vector of “base media” names (same as media_list above)
   #       • cor_list:          list of correlation tables, one per media
   #       • negative_prefixes, positive_prefixes: for sign_check()
   #
   #     Returns:
   #       A list with:
   #         final_model     = the lm object (original or re‐fitted)
   #         used_predictors = character vector of predictors actually used
   #         sign_checks     = named PASS/FAIL/SKIPPED for the final model
   # ─────────────────────────────────────────────────────────────────────────────────────
   fit_with_sign_correction <- function(
    model_data,
    base_predictors,
    paid_media_spends,
    cor_list,
    negative_prefixes,
    positive_prefixes
   ) {
     # 1) Fit original
     formula_orig <- as.formula(paste0("adjusted_sales ~ ", paste(base_predictors, collapse = " + ")))
     lm_orig      <- lm(formula_orig, data = model_data)
     
     # 2) Sign‐check original
     sc_orig <- sign_check(lm_orig, negative_prefixes, positive_prefixes)
     fail_vars <- names(sc_orig)[grepl("^FAIL", sc_orig)]
     
     # 3) If zero or more than one fail, STOP and return original
     if (length(fail_vars) == 0 || length(fail_vars) > 1) {
       return(list(
         final_model     = lm_orig,
         used_predictors = base_predictors,
         sign_checks     = sc_orig
       ))
     }
     
     # 4) Exactly one variable failed
     single_fail <- fail_vars[1]
     
     # 5) Attempt to replace it
     new_predictors <- attempt_replace_and_check(
       fail_var           = single_fail,
       current_predictors = base_predictors,
       media_list         = paid_media_spends,
       cor_list           = cor_list,
       model_data         = model_data,
       negative_prefixes  = negative_prefixes,
       positive_prefixes  = positive_prefixes
     )
     
     # 6) If NULL (no fix), return original
     if (is.null(new_predictors)) {
       return(list(
         final_model     = lm_orig,
         used_predictors = base_predictors,
         sign_checks     = sc_orig
       ))
     }
     
     # 7) Otherwise, re‐fit with new_predictors
     formula_new <- as.formula(paste0("adjusted_sales ~ ", paste(new_predictors, collapse = " + ")))
     lm_new      <- lm(formula_new, data = model_data)
     sc_new      <- sign_check(lm_new, negative_prefixes, positive_prefixes)
     
     return(list(
       final_model     = lm_new,
       used_predictors = new_predictors,
       sign_checks     = sc_new
     ))
   }
   # ─────────────────────────────────────────────────────────────────────────────────────
   # … after writing all transformed data into model_data …
   
   # Example ofour current fixed‐formula approach (you might have multiple models)
   model1 <- lm(
     y ~ TV_Spends_transformed + Radio_Spends_transformed + Outdoor_Spends_transformed +
       Direct_Display_Spend_transformed + Meta1_Spends_transformed + 
       Meta2_Spends_transformed + Youtube_Spends_0.3_0.5_0.3 + Google_Display_Spend_0.1_0.5_0.8,
     data = model_data
   )
   # ─────────────────────────────────────────────────────────────────────────────────────
   # 2.1) DEFINE base_predictors (the 12 columns you want initially)
   base_predictors <- c(
     "TV_GRP_transformed",
     "Outdoor_Spends_transformed",
     "Radio_Spends_transofrmed",           
     "Direct_Display_Spend_transformed",
     "Meta1_Spends_transformed",
     "Meta2_Spends_transformed",
     "Youtube_Spends_transformed",
     "Brand_P_ATL_Spends",
     "Dec_Peak_Dummy",
     "Feb_Dip_Dummy",
     "Brand_PH_ATL_Spends_transformed",
     "Programmatic_Video_Spends_transformed"
   )
   
   # 2.2) CALL fit_with_sign_correction() ON model_data
   #      (Assumes model_data$adjusted_sales exists already)
   result_fit <- fit_with_sign_correction(
     model_data         = model_data,
     base_predictors    = base_predictors,
     paid_media_spends  = paid_media_spends,
     cor_list           = cor_list,
     negative_prefixes  = negative_prefixes,
     positive_prefixes  = positive_prefixes
   )
   
   # 2.3) EXTRACT the final lm object and predictor list
   lm_final      <- result_fit$final_model
   used_predicts <- result_fit$used_predictors
   sign_status   <- result_fit$sign_checks
   
   # 2.4) LOG WHAT HAPPENED
   cat("\n── Used predictors: ─────────────────────────\n")
   print(used_predicts)
   cat("\n── Sign status for each predictor: ─────────────────────────\n")
   print(sign_status)
   
   # 2.5) EXTRACT METRICS (R², Adj R², # fails, coefficients) for downstream storage
   lm_summary <- summary(lm_final)
   R2_val    <- lm_summary$r.squared
   AdjR_val  <- lm_summary$adj.r.squared
   n_fails   <- sum(grepl("^FAIL", sign_status))
   
   cat("\nR²:", round(R2_val, 5), 
       " / Adj R²:", round(AdjR_val, 5),
       " / # sign‐fails:", n_fails, "\n")
   
   # 2.6) EXTRACT COEFFICIENTS (drop intercept)
   coefs_named <- coef(lm_final)
   coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
   
   # Now you can proceed to store (R2_val, AdjR_val, n_fails) inour metrics data.frame
   # and store coefs_named inour coefficient table, exactly as you did before.
   # ─────────────────────────────────────────────────────────────────────────────────────
   
################ Now, monthly sensitity with corrections  ################ 
   run_month_sensitivity_corrected <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
   ) {
     # Validate input
     month_to_vary <- tolower(month_to_vary)
     if (!month_to_vary %in% c("oct", "nov", "dec")) {
       stop("`month_to_vary` must be one of: 'oct', 'nov', or 'dec'.")
     }
     
     # 1) Build the sequence of percentages: 0, (max_pct/(n_steps-1)), …, max_pct
     pct_seq <- seq(0, max_pct, length.out = n_steps)
     
     # 2) Define base_predictors (the 12 columns you always start with):
     base_predictors <- c(
       "TV_GRP_transformed",
       "Outdoor_Spends_transformed",
       "Radio_Spends_transofrmed",
       "Direct_Display_Spend_transformed",
       "Meta1_Spends_transformed",
       "Meta2_Spends_transformed",
       "Youtube_Spends_transformed",
       "Brand_P_ATL_Spends",
       "Dec_Peak_Dummy",
       "Feb_Dip_Dummy",
       "Brand_PH_ATL_Spends_transformed",
       "Programmatic_Video_Spends_transformed"
     )
     
     # 3) Pre‐allocate lists for metrics and coefficients
     metrics_list <- vector("list", length(pct_seq))
     coef_list    <- vector("list", length(pct_seq))
     
     # 4) Loop over each percentage in pct_seq
     for (i in seq_along(pct_seq)) {
       pct_val <- pct_seq[i]
       
       # 4A) Decide which month‐percentage to pass and call smear_sales_jfm()
       pct_oct <- if (month_to_vary == "oct") pct_val else 0
       pct_nov <- if (month_to_vary == "nov") pct_val else 0
       pct_dec <- if (month_to_vary == "dec") pct_val else 0
       
       temp_adj <- smear_sales_jfm(
         MMM_Workshop_Data,
         date_col  = "Month",
         sales_col = "Sales_Volume_Total",
         pct_oct   = pct_oct,
         pct_nov   = pct_nov,
         pct_dec   = pct_dec
       )
       
       # 4B) Copy adjusted_sales into model_data
       model_data$adjusted_sales <- temp_adj$adjusted_sales
       
       # 4C) Now call fit_with_sign_correction() (instead of a plain lm())
       result_fit <- fit_with_sign_correction(
         model_data         = model_data,
         base_predictors    = base_predictors,
         paid_media_spends  = paid_media_spends,
         cor_list           = cor_list,
         negative_prefixes  = negative_prefixes,
         positive_prefixes  = positive_prefixes
       )
       
       if (month_to_vary == 'dec'){
         ####################################
       }
       
       lm_final      <- result_fit$final_model
       used_preds    <- result_fit$used_predictors
       sign_status   <- result_fit$sign_checks
       
       # 4D) Extract R², Adj R²
       lm_sum <- summary(lm_final)
       R2_val   <- lm_sum$r.squared
       AdjR_val <- lm_sum$adj.r.squared
       
       # 4E) Count how many sign‐fails remain
       n_fails  <- sum(grepl("^FAIL", sign_status))
       
       # 4F) Store metrics
       metrics_list[[i]] <- data.frame(
         pct_varied    = pct_val,
         n_sign_fails  = n_fails,
         R_squared     = R2_val,
         Adj_R_squared = AdjR_val,
         stringsAsFactors = FALSE
       )
       
       # 4G) Extract final coefficients (drop intercept)
       coefs_named <- coef(lm_final)
       coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
       
       coef_list[[i]] <- data.frame(
         pct_varied  = pct_val,
         variable    = names(coefs_named),
         coefficient = as.numeric(coefs_named),
         stringsAsFactors = FALSE
       )
     }
     
     # 5) Bind the lists into data.frames
     metrics_df <- do.call(rbind, metrics_list)
     coef_df    <- do.call(rbind, coef_list)
     
     # 6) Extract the “baseline” (pct_varied == 0) coefficients for vertical‐line reference
     baseline_coefs <- coef_df %>%
       filter(pct_varied == 0) %>%
       select(variable, coefficient) %>%
       rename(baseline = coefficient)
     
     # 7) Join baseline_coefs into coef_df for plotting
     coef_df_plot <- coef_df %>%
       left_join(baseline_coefs, by = "variable")
     
     # 8) FACETED HISTOGRAM of each predictor’s coefficient across pct_varied,
     #    with a red vertical line at the baseline (pct_varied = 0)
     hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
       geom_histogram(bins = 20, color = "black", fill = "lightblue") +
       geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
       facet_wrap(~ variable, scales = "free", ncol = 3) +
       labs(
         title = paste0("Coeff-Distributions: Varying ", toupper(month_to_vary), " 0→", max_pct*100, "%"),
         x     = "Estimated Coefficient",
         y     = "Count"
       ) +
       theme_minimal() +
       theme(
         strip.text = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold")
       )
     
     # 9) LINE PLOT: # of sign‐fails vs. pct_varied
     sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
       geom_line(size = 1, color = "darkgreen") +
       geom_point(size = 2) +
       labs(
         title = paste0("Sign-Fails vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Count of Sign-Fails"
       ) +
       theme_minimal()
     
     # 10) LINE PLOT: R² and Adj R² vs. pct_varied
     metrics_long <- metrics_df %>%
       pivot_longer(
         cols      = c(R_squared, Adj_R_squared),
         names_to  = "metric",
         values_to = "value"
       )
     r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
       geom_line(size = 1) +
       geom_point(size = 2) +
       labs(
         title = paste0("R² & Adj R² vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Metric",
         color = ""
       ) +
       theme_minimal()
     
     # 11) Print the three plots
     print(hist_plot)
     print(sign_fail_plot)
     print(r2_plot)
     
     # 12) Return a list of data.frames and plots
     return(list(
       metrics_df     = metrics_df,
       coef_df        = coef_df,
       hist_plot      = hist_plot,
       sign_fail_plot = sign_fail_plot,
       r2_plot        = r2_plot
     ))
   }
   
   # Vary December from 0% to 40%
   result_dec <- run_month_sensitivity_corrected(month_to_vary = "dec", max_pct = 0.40)
   
   # Vary November from 0% to 25%
   result_nov_cor <- run_month_sensitivity_corrected(month_to_vary = "nov", max_pct = 0.4)
   result_nov <- run_month_sensitivity(month_to_vary = "nov", max_pct = 0.4)
   
   # Vary October from 0% to 10%
   result_oct_cor <- run_month_sensitivity_corrected(month_to_vary = "oct", max_pct = 0.4)
   result_oct <- run_month_sensitivity_corrected(month_to_vary = "oct", max_pct = 0.4)
   
########## Start dropping radio spends in December #########
   run_month_sensitivity_corrected_dec <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
   ) {
     # Validate input
     month_to_vary <- tolower(month_to_vary)
     if (!month_to_vary %in% c("oct", "nov", "dec")) {
       stop("`month_to_vary` must be one of: 'oct', 'nov', or 'dec'.")
     }
     
     # 1) Build the sequence of percentages: 0, (max_pct/(n_steps-1)), …, max_pct
     pct_seq <- seq(0, max_pct, length.out = n_steps)
     
     # 2) Define base_predictors (the 12 columns you always start with):
     base_predictors <- c(
       "TV_GRP_transformed",
       "Outdoor_Spends_transformed",
       "Radio_Spends_transofrmed",
       "Direct_Display_Spend_transformed",
       "Meta1_Spends_transformed",
       "Meta2_Spends_transformed",
       "Youtube_Spends_transformed",
       "Brand_P_ATL_Spends",
       "Dec_Peak_Dummy",
       "Feb_Dip_Dummy",
       "Brand_PH_ATL_Spends_transformed",
       "Programmatic_Video_Spends_transformed"
     )
     
     # 3) Pre‐allocate lists for metrics and coefficients
     metrics_list <- vector("list", length(pct_seq))
     coef_list    <- vector("list", length(pct_seq))
     
     # 4) Loop over each percentage in pct_seq
     for (i in seq_along(pct_seq)) {
       pct_val <- pct_seq[i]
       
       # 4A) Decide which month‐percentage to pass and call smear_sales_jfm()
       pct_oct <- if (month_to_vary == "oct") pct_val else 0
       pct_nov <- if (month_to_vary == "nov") pct_val else 0
       pct_dec <- if (month_to_vary == "dec") pct_val else 0
       
       temp_adj <- smear_sales_jfm(
         MMM_Workshop_Data,
         date_col  = "Month",
         sales_col = "Sales_Volume_Total",
         pct_oct   = pct_oct,
         pct_nov   = pct_nov,
         pct_dec   = pct_dec
       )
       
       # 4B) Copy adjusted_sales into model_data
       model_data$adjusted_sales <- temp_adj$adjusted_sales
       
       # 4C) First attempt: call fit_with_sign_correction() with the full base_predictors
       result_fit <- fit_with_sign_correction(
         model_data         = model_data,
         base_predictors    = base_predictors,
         paid_media_spends  = paid_media_spends,
         cor_list           = cor_list,
         negative_prefixes  = negative_prefixes,
         positive_prefixes  = positive_prefixes
       )
       
       lm_final      <- result_fit$final_model
       used_preds    <- result_fit$used_predictors
       sign_status   <- result_fit$sign_checks
       
       # 4D) Extract R², Adj R² (from the first attempt)
       lm_sum   <- summary(lm_final)
       R2_val   <- lm_sum$r.squared
       AdjR_val <- lm_sum$adj.r.squared
       
       # 4E) Count how many sign‐fails remain (after first attempt)
       n_fails  <- sum(grepl("^FAIL", sign_status))
       
       # 4F) If month_to_vary == "dec" AND there is at least one sign‐fail,
       #      try a second attempt with "Radio_Spends_transofrmed" removed.
       if (month_to_vary == "dec" && n_fails > 0) {
         # Build a reduced predictor set: remove "Radio_Spends_transofrmed" from the original base set
         alt_base_predictors <- setdiff(base_predictors, c("Radio_Spends_transofrmed", "DEC_Peak"))         
         # Call fit_with_sign_correction() again
         result_alt <- fit_with_sign_correction(
           model_data         = model_data,
           base_predictors    = alt_base_predictors,
           paid_media_spends  = paid_media_spends,
           cor_list           = cor_list,
           negative_prefixes  = negative_prefixes,
           positive_prefixes  = positive_prefixes
         )
         
         lm_alt      <- result_alt$final_model
         used_preds_alt <- result_alt$used_predictors
         sign_status_alt <- result_alt$sign_checks
         
         # Check if this second attempt fixed all sign‐fails
         n_fails_alt <- sum(grepl("^FAIL", sign_status_alt))
         
         if (n_fails_alt == 0) {
           # If the alternate fit has zero sign‐fails, override: use lm_alt and its stats
           lm_final    <- lm_alt
           used_preds  <- used_preds_alt
           sign_status <- sign_status_alt
           
           lm_sum     <- summary(lm_final)
           R2_val     <- lm_sum$r.squared
           AdjR_val   <- lm_sum$adj.r.squared
           n_fails    <- 0
         }
         # Otherwise, keep the original first attempt (lm_final, used_preds, n_fails) unchanged
       }
       
       # 4G) Store metrics (after any possible second attempt)
       metrics_list[[i]] <- data.frame(
         pct_varied    = pct_val,
         n_sign_fails  = n_fails,
         R_squared     = R2_val,
         Adj_R_squared = AdjR_val,
         stringsAsFactors = FALSE
       )
       
       # 4H) Extract final coefficients from whichever lm_final was kept
       coefs_named <- coef(lm_final)
       coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
       
       coef_list[[i]] <- data.frame(
         pct_varied  = pct_val,
         variable    = names(coefs_named),
         coefficient = as.numeric(coefs_named),
         stringsAsFactors = FALSE
       )
     }
     
     # 5) Bind the lists into data.frames
     metrics_df <- do.call(rbind, metrics_list)
     coef_df    <- do.call(rbind, coef_list)
     
     # 6) Extract the “baseline” (pct_varied == 0) coefficients for vertical‐line reference
     baseline_coefs <- coef_df %>%
       filter(pct_varied == 0) %>%
       select(variable, coefficient) %>%
       rename(baseline = coefficient)
     
     # 7) Join baseline_coefs into coef_df for plotting
     coef_df_plot <- coef_df %>%
       left_join(baseline_coefs, by = "variable")
     
     # 8) FACETED HISTOGRAM of each predictor’s coefficient across pct_varied,
     #    with a red vertical line at the baseline (pct_varied = 0)
     hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
       geom_histogram(bins = 20, color = "black", fill = "lightblue") +
       geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
       facet_wrap(~ variable, scales = "free", ncol = 3) +
       labs(
         title = paste0("Coeff-Distributions: Varying ", toupper(month_to_vary), " 0→", max_pct*100, "%"),
         x     = "Estimated Coefficient",
         y     = "Count"
       ) +
       theme_minimal() +
       theme(
         strip.text = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold")
       )
     
     # 9) LINE PLOT: # of sign‐fails vs. pct_varied
     sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
       geom_line(size = 1, color = "darkgreen") +
       geom_point(size = 2) +
       labs(
         title = paste0("Sign-Fails vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Count of Sign-Fails"
       ) +
       theme_minimal()
     
     # 10) LINE PLOT: R² and Adj R² vs. pct_varied
     metrics_long <- metrics_df %>%
       pivot_longer(
         cols      = c(R_squared, Adj_R_squared),
         names_to  = "metric",
         values_to = "value"
       )
     r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
       geom_line(size = 1) +
       geom_point(size = 2) +
       labs(
         title = paste0("R² & Adj R² vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Metric",
         color = ""
       ) +
       theme_minimal()
     
     # 11) Print the three plots
     print(hist_plot)
     print(sign_fail_plot)
     print(r2_plot)
     
     # 12) Return a list of data.frames and plots
     return(list(
       metrics_df     = metrics_df,
       coef_df        = coef_df,
       hist_plot      = hist_plot,
       sign_fail_plot = sign_fail_plot,
       r2_plot        = r2_plot
     ))
   }
   # Vary December from 0% to 40%, but now including the “radio‐drop” fallback:
   result_dec_cor_dec <- run_month_sensitivity_corrected_dec(month_to_vary = "dec", max_pct = 0.40)
   result_dec <- run_month_sensitivity(month_to_vary = "dec", max_pct = 0.40)
   
   # Vary November from 0% to 25% (no radio‐drop logic here because month_to_vary != "dec"):
   result_nov <- run_month_sensitivity_corrected_dec(month_to_vary = "nov", max_pct = 0.4)
   
   # Vary October from 0% to 10% (same as before):
   result_oct <- run_month_sensitivity_corrected_dec(month_to_vary = "oct", max_pct = 0.4)
   
###### Ahaa, I will now have equal split of the percentage ####
   
   run_month_sensitivity_corrected_equal_split <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
   ) {
     # Validate input
     month_to_vary <- tolower(month_to_vary)
     if (!month_to_vary %in% c("oct", "nov", "dec")) {
       stop("`month_to_vary` must be one of: 'oct', 'nov', or 'dec'.")
     }
     
     # 1) Build the sequence of percentages: 0, (max_pct/(n_steps-1)), …, max_pct
     pct_seq <- seq(0, max_pct, length.out = n_steps)
     
     # 2) Define base_predictors (the 12 columns you always start with):
     base_predictors <- c(
       "TV_GRP_transformed",
       "Outdoor_Spends_transformed",
       "Radio_Spends_transofrmed",
       "Direct_Display_Spend_transformed",
       "Meta1_Spends_transformed",
       "Meta2_Spends_transformed",
       "Youtube_Spends_transformed",
       "Brand_P_ATL_Spends",
       "Dec_Peak_Dummy",
       "Feb_Dip_Dummy",
       "Brand_PH_ATL_Spends_transformed",
       "Programmatic_Video_Spends_transformed"
     )
     
     # 3) Pre‐allocate lists for metrics and coefficients
     metrics_list <- vector("list", length(pct_seq))
     coef_list    <- vector("list", length(pct_seq))
     
     # 4) Loop over each percentage in pct_seq
     for (i in seq_along(pct_seq)) {
       pct_val <- pct_seq[i]
       
       # 4A) Decide which month‐percentage to pass and call smear_sales_jfm()
       # pct_oct <- if (month_to_vary == "oct") pct_val else 0
       # pct_nov <- if (month_to_vary == "nov") pct_val else 0
       # pct_dec <- if (month_to_vary == "dec") pct_val else 0
       pct_oct <- pct_val / 3
       pct_nov <- pct_val #if (month_to_vary == "nov") pct_val else 0
       pct_dec <- pct_val #if (month_to_vary == "dec") pct_val else 0
       
       temp_adj <- smear_sales_jfm(
         MMM_Workshop_Data,
         date_col  = "Month",
         sales_col = "Sales_Volume_Total",
         pct_oct   = pct_oct,
         pct_nov   = pct_nov,
         pct_dec   = pct_dec
       )
       
       # 4B) Copy adjusted_sales into model_data
       model_data$adjusted_sales <- temp_adj$adjusted_sales
       
       # 4C) First attempt: call fit_with_sign_correction() with the full base_predictors
       result_fit <- fit_with_sign_correction(
         model_data         = model_data,
         base_predictors    = base_predictors,
         paid_media_spends  = paid_media_spends,
         cor_list           = cor_list,
         negative_prefixes  = negative_prefixes,
         positive_prefixes  = positive_prefixes
       )
       
       lm_final      <- result_fit$final_model
       used_preds    <- result_fit$used_predictors
       sign_status   <- result_fit$sign_checks
       
       # 4D) Extract R², Adj R² (from the first attempt)
       lm_sum   <- summary(lm_final)
       R2_val   <- lm_sum$r.squared
       AdjR_val <- lm_sum$adj.r.squared
       
       # 4E) Count how many sign‐fails remain (after first attempt)
       n_fails  <- sum(grepl("^FAIL", sign_status))
       
       # 4F) If month_to_vary == "dec" AND there is at least one sign‐fail,
       #      try a second attempt with "Radio_Spends_transofrmed" removed.
       if (month_to_vary == "dec" && n_fails > 0) {
         # Build a reduced predictor set: remove "Radio_Spends_transofrmed" from the original base set
         alt_base_predictors <- setdiff(base_predictors, c("Radio_Spends_transofrmed", "DEC_Peak"))         
         # Call fit_with_sign_correction() again
         result_alt <- fit_with_sign_correction(
           model_data         = model_data,
           base_predictors    = alt_base_predictors,
           paid_media_spends  = paid_media_spends,
           cor_list           = cor_list,
           negative_prefixes  = negative_prefixes,
           positive_prefixes  = positive_prefixes
         )
         
         lm_alt      <- result_alt$final_model
         used_preds_alt <- result_alt$used_predictors
         sign_status_alt <- result_alt$sign_checks
         
         # Check if this second attempt fixed all sign‐fails
         n_fails_alt <- sum(grepl("^FAIL", sign_status_alt))
         
         if (n_fails_alt == 0) {
           # If the alternate fit has zero sign‐fails, override: use lm_alt and its stats
           lm_final    <- lm_alt
           used_preds  <- used_preds_alt
           sign_status <- sign_status_alt
           
           lm_sum     <- summary(lm_final)
           R2_val     <- lm_sum$r.squared
           AdjR_val   <- lm_sum$adj.r.squared
           n_fails    <- 0
         }
         # Otherwise, keep the original first attempt (lm_final, used_preds, n_fails) unchanged
       }
       
       # 4G) Store metrics (after any possible second attempt)
       metrics_list[[i]] <- data.frame(
         pct_varied    = pct_val,
         n_sign_fails  = n_fails,
         R_squared     = R2_val,
         Adj_R_squared = AdjR_val,
         stringsAsFactors = FALSE
       )
       
       # 4H) Extract final coefficients from whichever lm_final was kept
       coefs_named <- coef(lm_final)
       coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
       
       coef_list[[i]] <- data.frame(
         pct_varied  = pct_val,
         variable    = names(coefs_named),
         coefficient = as.numeric(coefs_named),
         stringsAsFactors = FALSE
       )
     }
     
     # 5) Bind the lists into data.frames
     metrics_df <- do.call(rbind, metrics_list)
     coef_df    <- do.call(rbind, coef_list)
     
     # 6) Extract the “baseline” (pct_varied == 0) coefficients for vertical‐line reference
     baseline_coefs <- coef_df %>%
       filter(pct_varied == 0) %>%
       select(variable, coefficient) %>%
       rename(baseline = coefficient)
     
     # 7) Join baseline_coefs into coef_df for plotting
     coef_df_plot <- coef_df %>%
       left_join(baseline_coefs, by = "variable")
     
     # 8) FACETED HISTOGRAM of each predictor’s coefficient across pct_varied,
     #    with a red vertical line at the baseline (pct_varied = 0)
     hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
       geom_histogram(bins = 20, color = "black", fill = "lightblue") +
       geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
       facet_wrap(~ variable, scales = "free", ncol = 3) +
       labs(
         title = paste0("Coeff-Distributions: Varying Equal  0→", max_pct*100, "%"),
         x     = "Estimated Coefficient",
         y     = "Count"
       ) +
       theme_minimal() +
       theme(
         strip.text = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold")
       )
     
     # 9) LINE PLOT: # of sign‐fails vs. pct_varied
     sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
       geom_line(size = 1, color = "darkgreen") +
       geom_point(size = 2) +
       labs(
         title = paste0("Sign-Fails vs. Equal Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Count of Sign-Fails"
       ) +
       theme_minimal()
     
     # 10) LINE PLOT: R² and Adj R² vs. pct_varied
     metrics_long <- metrics_df %>%
       pivot_longer(
         cols      = c(R_squared, Adj_R_squared),
         names_to  = "metric",
         values_to = "value"
       )
     r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
       geom_line(size = 1) +
       geom_point(size = 2) +
       labs(
         title = paste0("R² & Adj R² vs. Equal Shift (%)"),
         x     = "Equal Shift (%)",
         y     = "Metric",
         color = ""
       ) +
       theme_minimal()
     
     # 11) Print the three plots
     print(hist_plot)
     print(sign_fail_plot)
     print(r2_plot)
     
     # 12) Return a list of data.frames and plots
     return(list(
       metrics_df     = metrics_df,
       coef_df        = coef_df,
       hist_plot      = hist_plot,
       sign_fail_plot = sign_fail_plot,
       r2_plot        = r2_plot
     ))
   }
   # Vary December from 0% to 40%, but now including the “radio‐drop” fallback:
   result_dec_cor_dec_equal <- run_month_sensitivity_corrected_equal_split(month_to_vary = "dec", max_pct = 0.40)
   result_dec <- run_month_sensitivity(month_to_vary = "dec", max_pct = 0.40)
   
   # # Vary November from 0% to 25% (no radio‐drop logic here because month_to_vary != "dec"):
   # result_nov <- run_month_sensitivity_corrected_equal_split(month_to_vary = "nov", max_pct = 0.4)
   # 
   # # Vary October from 0% to 10% (same as before):
   # result_oct <- run_month_sensitivity_corrected_equal_split(month_to_vary = "oct", max_pct = 0.4)
   # 
##### What if I start drpping the wrong sign variables #######
   run_month_sensitivity_dropvars_dec <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
   ) {
     # ────────────────────────────────────────────────────────────────────────────────────
     # 1) Validate input
     month_to_vary <- tolower(month_to_vary)
     if (!month_to_vary %in% c("oct", "nov", "dec")) {
       stop("`month_to_vary` must be one of: 'oct', 'nov', or 'dec'.")
     }
     
     # 2) Build the sequence of percentages: 0, (max_pct/(n_steps-1)), …, max_pct
     pct_seq <- seq(0, max_pct, length.out = n_steps)
     
     # 3) Define base_predictors (the 12 columns you always start with):
     base_predictors <- c(
       "TV_GRP_transformed",
       "Outdoor_Spends_transformed",
       "Radio_Spends_transofrmed",
       "Direct_Display_Spend_transformed",
       "Meta1_Spends_transformed",
       "Meta2_Spends_transformed",
       "Youtube_Spends_transformed",
       "Brand_P_ATL_Spends",
       "Dec_Peak_Dummy",
       "Feb_Dip_Dummy",
       "Brand_PH_ATL_Spends_transformed",
       "Programmatic_Video_Spends_transformed"
     )
     
     # 4) Pre‐allocate lists for metrics and coefficients
     metrics_list <- vector("list", length(pct_seq))
     coef_list    <- vector("list", length(pct_seq))
     
     # 5) Loop over each percentage in pct_seq
     for (i in seq_along(pct_seq)) {
       pct_val <- pct_seq[i]
       
       # 5A) Decide which month‐percentage to pass and call smear_sales_jfm()
       pct_oct <- if (month_to_vary == "oct") pct_val else 0
       pct_nov <- if (month_to_vary == "nov") pct_val else 0
       pct_dec <- if (month_to_vary == "dec") pct_val else 0
       
       temp_adj <- smear_sales_jfm(
         MMM_Workshop_Data,
         date_col    = "Month",
         sales_col   = "Sales_Volume_Total",
         pct_oct     = pct_oct,
         pct_nov     = pct_nov,
         pct_dec     = pct_dec
         # (No jan_share/feb_share/mar_share → defaults to equal 1:1:1)
       )
       
       # 5B) Copy adjusted_sales into model_data
       model_data$adjusted_sales <- temp_adj$adjusted_sales
       
       # 5C) Iteratively drop any wrong‐sign predictors until all signs are correct or none remain
       current_predictors <- base_predictors
       final_model       <- NULL
       final_signs       <- NULL
       
       repeat {
         # (i) If no predictors remain, break
         if (length(current_predictors) == 0) {
           break
         }
         
         # (ii) Fit OLS with current_predictors
         formula_str <- paste0("adjusted_sales ~ ", paste(current_predictors, collapse = " + "))
         lm_fit <- lm(as.formula(formula_str), data = model_data)
         
         # (iii) Check signs
         sc <- sign_check(lm_fit, negative_prefixes, positive_prefixes)
         wrong_vars <- names(sc)[grepl("^FAIL", sc)]
         
         # (iv) If zero wrong_vars, we have our final model
         if (length(wrong_vars) == 0) {
           final_model <- lm_fit
           final_signs <- sc
           break
         }
         
         # (v) Otherwise drop all wrong_vars and repeat
         current_predictors <- setdiff(current_predictors, wrong_vars)
         # continue until either no wrong_vars remain or no predictors left
       }
       
       # 5D) At this point, final_model is either the last fit with zero wrong_vars, or NULL
       if (is.null(final_model)) {
         # if we dropped everything, just fit with intercept‐only
         final_model <- lm(adjusted_sales ~ 1, data = model_data)
         final_signs <- list()  # no predictors to check
       }
       
       # 5E) Extract R², Adj R² from final_model
       sumlm  <- summary(final_model)
       R2_val <- sumlm$r.squared
       AdjR   <- sumlm$adj.r.squared
       
       # 5F) Count how many sign‐fails remain in final_signs
       n_fails <- sum(grepl("^FAIL", final_signs))
       
       # 5G) Store metrics
       metrics_list[[i]] <- data.frame(
         pct_varied    = pct_val,
         n_sign_fails  = n_fails,
         R_squared     = R2_val,
         Adj_R_squared = AdjR,
         stringsAsFactors = FALSE
       )
       
       # 5H) Extract final coefficients (drop intercept)
       coefs_named <- coef(final_model)
       coefs_named <- coefs_named[names(coefs_named) != "(Intercept)"]
       
       coef_list[[i]] <- data.frame(
         pct_varied  = pct_val,
         variable    = names(coefs_named),
         coefficient = as.numeric(coefs_named),
         stringsAsFactors = FALSE
       )
     }
     
     # 6) Bind the lists into data.frames
     metrics_df <- do.call(rbind, metrics_list)
     coef_df    <- do.call(rbind, coef_list)
     
     # 7) Extract the “baseline” (pct_varied == 0) coefficients for vertical‐line reference
     baseline_coefs <- coef_df %>%
       filter(pct_varied == 0) %>%
       select(variable, coefficient) %>%
       rename(baseline = coefficient)
     
     # 8) Join baseline_coefs into coef_df for plotting
     coef_df_plot <- coef_df %>%
       left_join(baseline_coefs, by = "variable")
     
     # 9) FACETED HISTOGRAM of each predictor’s coefficient across pct_varied,
     #    with a red vertical line at the baseline (pct_varied = 0)
     hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
       geom_histogram(bins = 20, color = "black", fill = "lightblue") +
       geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
       facet_wrap(~ variable, scales = "free", ncol = 3) +
       labs(
         title = paste0("Coeff-Distributions: Varying ", toupper(month_to_vary), " 0→", max_pct*100, "%"),
         x     = "Estimated Coefficient",
         y     = "Count"
       ) +
       theme_minimal() +
       theme(
         strip.text = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold")
       )
     
     # 10) LINE PLOT: # of sign-fails vs. pct_varied
     sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
       geom_line(size = 1, color = "darkgreen") +
       geom_point(size = 2) +
       labs(
         title = paste0("Sign-Fails vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Count of Sign-Fails"
       ) +
       theme_minimal()
     
     # 11) LINE PLOT: R² and Adj R² vs. pct_varied
     metrics_long <- metrics_df %>%
       tidyr::pivot_longer(
         cols      = c(R_squared, Adj_R_squared),
         names_to  = "metric",
         values_to = "value"
       )
     r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
       geom_line(size = 1) +
       geom_point(size = 2) +
       labs(
         title = paste0("R² & Adj R² vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Metric",
         color = ""
       ) +
       theme_minimal()
     
     # 12) Print the three plots
     print(hist_plot)
     print(sign_fail_plot)
     print(r2_plot)
     
     # 13) Return a list of data.frames and plot objects
     return(list(
       metrics_df     = metrics_df,
       coef_df        = coef_df,
       hist_plot      = hist_plot,
       # sign_fail_plot = sign_fail_plot,
       r2_plot        = r2_plot
     ))
   }
   # 1) Vary December from 0% → 40%, dropping any wrong‐sign covariates at each step:
   result_drop_dec <- run_month_sensitivity_dropvars_dec(month_to_vary = "dec", max_pct = 0.40)
   
   # 2) Vary November (no “drop vars” fallback needed since month_to_vary != "dec"):
   result_drop_nov <- run_month_sensitivity_dropvars_dec(month_to_vary = "nov", max_pct = 0.4)
   
   # 3) Vary October similarly:
   result_drop_oct <- run_month_sensitivity_dropvars_dec(month_to_vary = "oct", max_pct = 0.4)
   
   
###### How about I go through all the adstock combs #####
   # ─────────────────────────────────────────────────────────────────────────────────────
   # fit_with_hyperparam_correction_all():
   #
   #   • Starts from a base_predictors vector.  
   #   • Fits lm(adjusted_sales ~ base_predictors).  
   #   • sign_check() on that model.  
   #   • While there exists at least one wrong‐sign predictor:
   #       – For each wrong‐sign predictor (in turn):
   #           • Identify its “base media” by matching prefixes in paid_media_spends.  
   #           • Look up that media’s correlation‐ranked table in cor_list.  
   #           • Scan each hyperparam variant in descending‐corr order,  
   #             temporarily replacing the failed predictor with that hyperparam:  
   #               — refit → run sign_check → if that one variable’s sign is now PASS,  
   #                 permanently replace it and break out of the inner loop.  
   #       – After attempting all wrong‐sign variables once, if at least one was fixed,  
   #         repeat the outer “wrong‐sign exists?” loop from the top.  
   #       – If no wrong‐sign variable could be fixed in a full pass, stop.  
   #
   #   • Returns a list with:
   #       final_model      = lm object (best we could do)
   #       used_predictors  = final vector of predictors
   #       sign_checks      = named result of sign_check() on final_model
   # ─────────────────────────────────────────────────────────────────────────────────────
   fit_with_hyperparam_correction_all <- function(
    model_data,
    base_predictors,
    paid_media_spends,
    cor_list,
    negative_prefixes,
    positive_prefixes
   ) {
     # 1) Start with the base vector
     current_preds <- base_predictors
     final_model   <- NULL
     final_signs   <- NULL
     
     repeat {
       # (a) If no predictors left, break
       if (length(current_preds) == 0) {
         final_model <- lm(adjusted_sales ~ 1, data = model_data)
         final_signs <- character(0)
         break
       }
       
       # (b) Fit with current_preds
       formula_str <- paste0("adjusted_sales ~ ", paste(current_preds, collapse = " + "))
       lm_fit      <- lm(as.formula(formula_str), data = model_data)
       
       # (c) Check signs
       sc <- sign_check(lm_fit, negative_prefixes, positive_prefixes)
       wrong_vars <- names(sc)[grepl("^FAIL", sc)]
       
       # (d) If no wrong_vars, we’re done
       if (length(wrong_vars) == 0) {
         final_model <- lm_fit
         final_signs <- sc
         break
       }
       
       # (e) Attempt to fix each wrong_var, in sequence
       fixed_any <- FALSE
       for (fail_var in wrong_vars) {
         # Identify base media:
         base_media <- NULL
         for (m in paid_media_spends) {
           if (startsWith(fail_var, m)) {
             base_media <- m
             break
           }
         }
         if (is.null(base_media)) next  # skip if not a paid‐media
         idx_media <- which(paid_media_spends == base_media)
         if (length(idx_media) != 1) next
         
         # Grab that media’s correlation table
         corr_df <- cor_list[[idx_media]]
         
         # Try each hyperparam variant
         for (candidate in corr_df$independent_variable) {
           if (!candidate %in% names(model_data)) next
           
           # Build new predictor vector
           new_preds <- current_preds
           new_preds[new_preds == fail_var] <- candidate
           
           # Refit
           formula_cand <- paste0("adjusted_sales ~ ", paste(new_preds, collapse = " + "))
           lm_cand      <- lm(as.formula(formula_cand), data = model_data)
           
           # Check sign of candidate
           sc_cand <- sign_check(lm_cand, negative_prefixes, positive_prefixes)
           if (grepl("^PASS", sc_cand[[candidate]])) {
             # Success: we fix fail_var → candidate
             current_preds <- new_preds
             fixed_any     <- TRUE
             break
           }
         }
         if (fixed_any) break
         # Otherwise, move on to next fail_var
       }
       
       # (f) If we fixed at least one this pass, loop again
       if (fixed_any) {
         next
       }
       
       # (g) Otherwise, no further fixes possible: keep current lm_fit
       final_model <- lm_fit
       final_signs <- sc
       break
     }
     
     return(list(
       final_model     = final_model,
       used_predictors = current_preds,
       sign_checks     = final_signs
     ))
   }
   run_month_sensitivity_hyper_corrected <- function(
    month_to_vary,
    max_pct,
    n_steps = 41
   ) {
     # 1) Validate
     month_to_vary <- tolower(month_to_vary)
     if (!month_to_vary %in% c("oct", "nov", "dec")) {
       stop("`month_to_vary` must be one of: 'oct', 'nov', or 'dec'.")
     }
     
     # 2) Build percentage grid
     pct_seq <- seq(0, max_pct, length.out = n_steps)
     
     # 3) Base predictors
     base_predictors <- c(
       "TV_GRP_transformed",
       "Outdoor_Spends_transformed",
       "Radio_Spends_transofrmed",
       "Direct_Display_Spend_transformed",
       "Meta1_Spends_transformed",
       "Meta2_Spends_transformed",
       "Youtube_Spends_transformed",
       "Brand_P_ATL_Spends",
       "Dec_Peak_Dummy",
       "Feb_Dip_Dummy",
       "Brand_PH_ATL_Spends_transformed",
       "Programmatic_Video_Spends_transformed"
     )
     
     # 4) Prepare storage
     metrics_list <- vector("list", length(pct_seq))
     coef_list    <- vector("list", length(pct_seq))
     
     # 5) Loop
     for (i in seq_along(pct_seq)) {
       pct_val <- pct_seq[i]
       
       # 5A) Decide smear percentages
       pct_oct <- if (month_to_vary == "oct") pct_val else 0
       pct_nov <- if (month_to_vary == "nov") pct_val else 0
       pct_dec <- if (month_to_vary == "dec") pct_val else 0
       
       temp_adj <- smear_sales_jfm(
         MMM_Workshop_Data,
         date_col  = "Month",
         sales_col = "Sales_Volume_Total",
         pct_oct   = pct_oct,
         pct_nov   = pct_nov,
         pct_dec   = pct_dec
       )
       model_data$adjusted_sales <- temp_adj$adjusted_sales
       
       # 5C) Fit + hyperparam correction on all wrong‐sign vars
       result_fit <- fit_with_hyperparam_correction_all(
         model_data         = model_data,
         base_predictors    = base_predictors,
         paid_media_spends  = paid_media_spends,
         cor_list           = cor_list,
         negative_prefixes  = negative_prefixes,
         positive_prefixes  = positive_prefixes
       )
       
       lm_final    <- result_fit$final_model
       sign_status <- result_fit$sign_checks
       used_preds  <- result_fit$used_predictors
       
       # 5D) Extract R², Adj R²
       lm_sum  <- summary(lm_final)
       R2_val  <- lm_sum$r.squared
       AdjR    <- lm_sum$adj.r.squared
       
       # 5E) Count # wrong signs remaining
       n_fails <- sum(grepl("^FAIL", sign_status))
       
       # 5F) Store metrics
       metrics_list[[i]] <- data.frame(
         pct_varied    = pct_val,
         n_sign_fails  = n_fails,
         R_squared     = R2_val,
         Adj_R_squared = AdjR,
         stringsAsFactors = FALSE
       )
       
       # 5G) Extract final coefs
       final_coefs <- coef(lm_final)
       final_coefs <- final_coefs[names(final_coefs) != "(Intercept)"]
       
       coef_list[[i]] <- data.frame(
         pct_varied  = pct_val,
         variable    = names(final_coefs),
         coefficient = as.numeric(final_coefs),
         stringsAsFactors = FALSE
       )
     }
     
     # 6) Bind results
     metrics_df <- do.call(rbind, metrics_list)
     coef_df    <- do.call(rbind, coef_list)
     
     # 7) Baseline for plotting
     baseline_coefs <- coef_df %>%
       filter(pct_varied == 0) %>%
       select(variable, coefficient) %>%
       rename(baseline = coefficient)
     
     coef_df_plot <- coef_df %>%
       left_join(baseline_coefs, by = "variable")
     
     # 8) Faceted histogram
     hist_plot <- ggplot(coef_df_plot, aes(x = coefficient)) +
       geom_histogram(bins = 20, color = "black", fill = "lightblue") +
       geom_vline(aes(xintercept = baseline), color = "red", size = 1) +
       facet_wrap(~ variable, scales = "free", ncol = 3) +
       labs(
         title = paste0("Coeff‐Distributions: Varying ", toupper(month_to_vary), " 0→", max_pct*100, "%"),
         x     = "Estimated Coefficient",
         y     = "Count"
       ) +
       theme_minimal() +
       theme(
         strip.text = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold")
       )
     
     # 9) Sign‐fails vs. %
     sign_fail_plot <- ggplot(metrics_df, aes(x = pct_varied * 100, y = n_sign_fails)) +
       geom_line(size = 1, color = "darkgreen") +
       geom_point(size = 2) +
       labs(
         title = paste0("Sign‐Fails vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Count of Sign‐Fails"
       ) +
       theme_minimal()
     
     # 10) R² & Adj R² vs. %
     metrics_long <- metrics_df %>%
       tidyr::pivot_longer(
         cols      = c(R_squared, Adj_R_squared),
         names_to  = "metric",
         values_to = "value"
       )
     r2_plot <- ggplot(metrics_long, aes(x = pct_varied * 100, y = value, color = metric)) +
       geom_line(size = 1) +
       geom_point(size = 2) +
       labs(
         title = paste0("R² & Adj R² vs. ", toupper(month_to_vary), " Shift (%)"),
         x     = paste0(toupper(month_to_vary), " Shift (%)"),
         y     = "Metric",
         color = ""
       ) +
       theme_minimal()
     
     # 11) Print plots
     print(hist_plot)
     print(sign_fail_plot)
     print(r2_plot)
     
     # 12) Return
     return(list(
       metrics_df     = metrics_df,
       coef_df        = coef_df,
       hist_plot      = hist_plot,
       sign_fail_plot = sign_fail_plot,
       r2_plot        = r2_plot
     ))
   }
   # 1) Vary December from 0 → 40%, correcting any wrong‐sign predictors via hyperparam search:
   result_hyper_dec <- run_month_sensitivity_hyper_corrected(month_to_vary = "dec", max_pct = 0.40)
   
   # 2) Vary November:
   result_hyper_nov <- run_month_sensitivity_hyper_corrected(month_to_vary = "nov", max_pct = 0.40)
   
   # 3) Vary October:
   result_hyper_oct <- run_month_sensitivity_hyper_corrected(month_to_vary = "oct", max_pct = 0.40)

   
