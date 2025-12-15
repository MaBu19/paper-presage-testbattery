getData = function(filePath) {
  
  # define the read_sheet function to use col_types
  read_sheet = function(filePath, sheet, col_types) {
    # read the sheet with the third row as the header
    df <- read_excel(filePath, sheet = sheet, skip = 2, na = c(" ", "NA", ""), col_types = col_types)
    # convert to data.table
    setDT(df)
    # --> ensure the 'id' & 'centre' columns are of type character
    df[, id := as.character(id)]
    df[, centre := as.character(centre)]
    return(df)
  }
  
  # define column types for each sheet
  sheet_col_types = list(
    sheet1 = c("text", "text", "text", "numeric", "text", "date",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",
               "numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	"numeric",	
               "numeric",	"numeric",	"text",	"numeric",	"numeric",	"numeric",	"text",	"numeric",	"numeric",	"numeric",
               "numeric",	"numeric",	"numeric",	"numeric",	"text",	"numeric",	"numeric",	"numeric",	"text",	"numeric",
               "numeric",	"numeric",	"numeric",	"text",	"text",	"numeric",	"text"),
    sheet2 = c("text", "text","date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"),
    sheet3 = c("text", "text","date","numeric", "text", "numeric", "numeric", "text", "numeric", "numeric", "text", "text"),
    sheet4 = c("text", "text", "date", "numeric", "text", "numeric", "numeric", "numeric", "numeric","text", "numeric", 
               "numeric", "numeric", "numeric", "text", "text"),
    sheet5 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "text", "text"),
    sheet6 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
               "text"),
    sheet7 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "text"),
    sheet8 = c("text", "text", "date", "numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric","numeric", "numeric", "text"),
    sheet9 = c("text", "text", "date", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric","numeric", "numeric", "text"),
    sheet10 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "text"),
    sheet11 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric","numeric", "text", "text", "text"),
    sheet12 = c("text", "text", "date", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
                "text", "text"),
    sheet13 = c("text", "text", "date", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"),
    sheet14 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric",
                "numeric","numeric","numeric","numeric","numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric",
                "numeric","numeric","numeric","numeric","numeric",
                "text", "text"),
    sheet15 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"),
    sheet16 = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "text", "text"))
  
  # browser()
  # read all sheets into a list of data tables
  sheet_names = excel_sheets(filePath)
  sheet_col_types_list = rep(list(NULL), length(sheet_names))
  sheet_col_types_list = sheet_col_types
  
  data_list = mapply(function(sheet, col_types) read_sheet(filePath, sheet, col_types), 
                      sheet_names, sheet_col_types_list, SIMPLIFY = FALSE)
  names(data_list) = sheet_names
  
  # merge the data tables by the 'id' column
  df_w = Reduce(function(x, y) merge(x, y, by = c("id", "centre"), all = TRUE), data_list)
  
  ## -- identify and properly mark NAs -----------------------------------------
  empty_cell_example <- df_w$ct_flag[38]
  
  # replace specific empty strings and NA with NA
  replace_empty_with_na <- function(x) {
    empty_strings <- c("", " ", empty_cell_example)
    if (is.character(x)) {
      x <- ifelse(trimws(x) %in% empty_strings, NA, x)
      return(x)
    } else {
      return(x)
    }
  }
  
  # apply the function to each column using mutate(across(...))
  df_w <- df_w %>%
    mutate(across(everything(), ~ replace_empty_with_na(.)))
  
  ## -- get additional parameters ----------------------------------------------
  
  # aided vs. unaided
  df_w$ha <- ifelse(
    rowSums(cbind(df_w$inclu_ha_r, df_w$inclu_ha_l), na.rm = TRUE) > 0,
    "aided",
    ifelse(
      rowSums(cbind(df_w$inclu_ha_r, df_w$inclu_ha_l), na.rm = TRUE) == 0,
      "unaided",
      NA
    )
  )
  
  # Get PTA4 (0.5 + 1 + 2 + 4 kHz /4):
  df_w <- df_w %>% group_by(id) %>%
    dplyr::mutate(
      PTA4_R = round(mean(c(ac_thres_500_r,ac_thres_1000_r,ac_thres_2000_r,ac_thres_4000_r),na.rm=TRUE),2),
      PTA4_L = round(mean(c(ac_thres_500_l,ac_thres_1000_l,ac_thres_2000_l,ac_thres_4000_l),na.rm=TRUE),2),
      PTA4_BE = ifelse(!is.na(matrix_testear) & matrix_testear != "",
                       ifelse(str_detect(matrix_testear, "R"), PTA4_R,
                              ifelse(str_detect(matrix_testear, "L"), PTA4_L, NA)),
                       NA)
    ) %>%
    ungroup()
  
  # make sure df_w is a data frame
  df_w = df_w %>% as.data.frame()
  
  
  return(df_w)
}