suppressMessages({
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  #library(zipcode)
  library(utils)
  library(scales)
  library(chron)
  library(bupaR)
  library(shiny)
  library(DT)
  library(DiagrammeR)
  library(shinyalert)
  library(edeaR)
  library(processmapR)
  library(processmonitR)
  library(processanimateR)
  library(tidyr)
  library(lubridate)
  library(RColorBrewer)
  library(DiagrammeR)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(feather)
  library(zipcodeR)
  library(formattable)
  library(shinyjs)
  library(janitor)
  library(patchwork)
  library(pryr)
  library(DBI)
  library(odbc)
})

memory.limit(size = 8000000)

process_data <- function(access_data){
  ### (3) Pre-process data ----------------------------------------------------------------------------------
  # SCheduling Data Pre-processing
  data.raw <- access_data # Assign scheduling Data

  # Dummy columns until they are added to Clarity table: SEX, FPA
  data.raw$SEX <- "Male"
  data.raw$VITALS_TAKEN_TM <- ""
  data.raw$Provider_Leave_DTTM <- ""
  
    # Data fields incldued for analysis 
  original.cols <- c("DEP_RPT_GRP_SEVENTEEN","DEPT_SPECIALTY_NAME","DEPARTMENT_NAME","PROV_NAME_WID", "DEPARTMENT_ID",
                     "MRN","PAT_NAME","ZIP_CODE","SEX","BIRTH_DATE","FINCLASS",
                     "APPT_MADE_DTTM","APPT_DTTM","PRC_NAME","APPT_LENGTH","DERIVED_STATUS_DESC",
                     "APPT_CANC_DTTM", "CANCEL_REASON_NAME",
                     "SIGNIN_DTTM","PAGED_DTTM","CHECKIN_DTTM","ARVL_LIST_REMOVE_DTTM",
                     "ROOMED_DTTM","FIRST_ROOM_ASSIGN_DTTM","VITALS_TAKEN_TM",
                     "PHYS_ENTER_DTTM","Provider_Leave_DTTM",
                     "VISIT_END_DTTM","CHECKOUT_DTTM",
                     "TIME_IN_ROOM_MINUTES","CYCLE_TIME_MINUTES","VISIT_GROUP_NUM","LOS_NAME", "DEP_RPT_GRP_THIRTYONE", 
                     "APPT_ENTRY_USER_NAME_WID", "ACCESS_CENTER_SCHEDULED_YN", "VISIT_METHOD", "VISIT_PROV_STAFF_RESOURCE_C",
                     "PRIMARY_DX_CODE", "ENC_CLOSED_CHARGE_STATUS", "Y_ENC_COSIGN_TIME", "Y_ENC_CLOSE_TIME", "Y_ENC_OPEN_TIME", "NPI", "PAT_ENC_CSN_ID", "VISITPLAN","ATTRIB_BILL_AREA")
  
  # Subset raw data 
  data.subset <- data.raw[original.cols]
  
  # Rename data fields (columns) 
  new.cols <- c("Campus","Campus.Specialty","Department","Provider", "DepartmentId",
                "MRN","Patient.Name","Zip.Code","Sex","Birth.Date","Coverage",
                "Appt.Made.DTTM","Appt.DTTM","Appt.Type","Appt.Dur","Appt.Status",
                "Appt.Cancel.DTTM", "Cancel.Reason",
                "Signin.DTTM","Paged.DTTM","Checkin.DTTM","Arrival.remove.DTTM",
                "Roomin.DTTM","Room.assigned.DTTM","Vitals.DTTM",
                "Providerin_DTTM","Providerout_DTTM",
                "Visitend.DTTM","Checkout.DTTM",
                "Time.in.room","Cycle.time","New.PT","Class.PT","Cadence",
                "Appt.Source","Access.Center","Visit.Method","Resource",
                "PRIMARY_DX_CODE", "ENC_CLOSED_CHARGE_STATUS", "Y_ENC_COSIGN_TIME", "Y_ENC_CLOSE_TIME", "Y_ENC_OPEN_TIME", "NPI", "PAT_ENC_CSN_ID", "VISITPLAN","ATTRIB_BILL_AREA")
  
  colnames(data.subset) <- new.cols
  
  # Format Date and Time Columns
  dttm.cols <- c("Birth.Date","Appt.Made.DTTM","Appt.DTTM","Appt.Cancel.DTTM",
                 "Checkin.DTTM","Arrival.remove.DTTM","Roomin.DTTM","Room.assigned.DTTM",
                 "Vitals.DTTM","Providerin_DTTM","Providerout_DTTM",
                 "Visitend.DTTM","Checkout.DTTM")
  
  # Clean up department names (X_..._DEACTIVATED)
  #data.subset$Department <- as.character(data.subset$Department)
  #data.subset <- data.subset %>%
  #  mutate(Department = ifelse(str_detect(Department, "DEACTIVATED"),
  #                             gsub('^.{2}|.{12}$', '', Department), 
  #                             ifelse(startsWith(Department,"X_"),
  #                                    gsub('^.{2}', '', Department), Department)))
  
  dttm <- function(x) {
    as.POSIXct(x,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone(),origin = "1970-01-01")
  }
  
  data.subset$Birth.Date <- dttm(data.subset$Birth.Date)
  data.subset$Appt.Made.DTTM <- dttm(data.subset$Appt.Made.DTTM)
  data.subset$Appt.DTTM <- dttm(data.subset$Appt.DTTM)
  data.subset$Appt.Cancel.DTTM <- dttm(data.subset$Appt.Cancel.DTTM)
  data.subset$Checkin.DTTM <- dttm(data.subset$Checkin.DTTM)
  data.subset$Arrival.remove.DTTM <- dttm(data.subset$Arrival.remove.DTTM)
  data.subset$Roomin.DTTM <- dttm(data.subset$Roomin.DTTM)
  data.subset$Room.assigned.DTTM <- dttm(data.subset$Room.assigned.DTTM)
  data.subset$Vitals.DTTM <- dttm(data.subset$Vitals.DTTM)
  data.subset$Providerin_DTTM <- dttm(data.subset$Providerin_DTTM)
  data.subset$Providerout_DTTM <- dttm(data.subset$Providerout_DTTM)
  data.subset$Visitend.DTTM <- dttm(data.subset$Visitend.DTTM)
  data.subset$Checkout.DTTM <- dttm(data.subset$Checkout.DTTM)
  
  # Remove Provider ID from Provider Name column
  data.subset$Provider <- trimws(gsub("\\[.*?\\]", "", data.subset$Provider))
  
  # New Patient Classification based on Visit Group Num
  data.subset$New.PT2 <- ifelse(is.na(data.subset$New.PT),"Established", "New")

  # New Patient Classification based on level of care ("LOS_NAME") 
  data.subset$New.PT3 <- ifelse(is.na(data.subset$Class.PT), "",grepl("NEW", data.subset$Class.PT, fixed = TRUE))
  
  
  # Pre-process Appointment Source: access center, entry person, zocdoc, mychart, staywell
  data.subset$Appt.Source.New <- ifelse(data.subset$Access.Center == "Y", "Access Center","")
  data.subset$Appt.Source.New <- ifelse(data.subset$Appt.Source.New == "",
                                        ifelse(grepl("ZOCDOC", data.subset$Appt.Source, fixed = TRUE)==TRUE, "Zocdoc",
                                               ifelse(grepl("MYCHART", data.subset$Appt.Source, fixed = TRUE)==TRUE, "MyChart",
                                                      ifelse(grepl("STAYWELL", data.subset$Appt.Source, fixed = TRUE)==TRUE, "StayWell","Other"))),data.subset$Appt.Source.New)
  
  
  # Notify and remove duplicates in data 
  # data.duplicates <- data.subset %>% duplicated()
  # data.duplicates <- length(data.duplicates[data.duplicates == TRUE]) ## Count of duplicated records
  # data.subset.new <- data.subset %>% distinct() ## New data set with duplicates removed
  data.subset.new <- data.subset
  
  # Create additional columns for analysis 
  data.subset.new$Appt.DateYear <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d") ## Create date-year column
  data.subset.new$Appt.MonthYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y-%m") ## Create month - year column
  data.subset.new$Appt.Date <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%m-%d") ## Create date column
  data.subset.new$Appt.Year <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y") ## Create year column
  data.subset.new$Appt.Month <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b") ## Create month colunm
  data.subset.new$Appt.Quarter <- quarters(as.Date(data.subset.new$Appt.DTTM)) ## Create quarter column 
  data.subset.new$Appt.Week <- floor_date(as.Date(data.subset.new$Appt.DateYear, "%Y-%m-%d"), unit="week", week_start = 1) # Create week column
  data.subset.new$Appt.Day <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%a") ## Create day of week colunm
  data.subset.new$Time <- format(as.POSIXct(as.ITime(data.subset.new$Appt.DTTM, format = "%H:%M")), "%H:%M") ## Create Time column
  data.subset.new$Appt.TM.Hr <- format(strptime(as.ITime(floor_date(data.subset.new$Appt.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by hour 
  # data.subset.new$Appt.TM.30m <- format(strptime(as.ITime(round_date(data.subset.new$Appt.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by 30-min
  data.subset.new$Checkin.Hr <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by hour 
  # data.subset.new$Checkin.30m <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by 30-min
  # data.subset.new$Lead.Days <- as.numeric((difftime(as.Date(data.subset.new$data.subset.new$Appt.DTTM, format="%Y-%m-%d"), as.Date(data.subset.new$data.subset.new$Appt.Cancel.DTTM, format="%Y-%m-%d"),  units = "days"))) ## Lead days for appt cancellation 
  data.subset.new$Lead.Days <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d")-as.Date(data.subset.new$Appt.Cancel.DTTM, format="%Y-%m-%d") ## Lead days for appt cancellation
  data.subset.new$Wait.Time <- as.Date(data.subset.new$Appt.DTTM, format="%Y-%m-%d")-as.Date(data.subset.new$Appt.Made.DTTM, format="%Y-%m-%d")
  data.subset.new$uniqueId <- paste(data.subset.new$Department,data.subset.new$Provider,data.subset.new$MRN,data.subset.new$Appt.DTTM) ## Unique ID 
  
  
  #Update cycltime to as.numeric(round(difftime(min(data.subset.new$Visitend.DTTM,Checkout.DTTM),data.subset.new$Checkin.DTTM,units="mins"),1))
  data.subset.new <-  data.subset.new  %>% mutate(min_of_checkout_visit = pmin(Visitend.DTTM,Checkout.DTTM , na.rm = T))
  data.subset.new$cycleTime <- as.numeric(round(difftime(data.subset.new$min_of_checkout_visit,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Visitend (min)
  
  
  
  data.subset.new$checkinToRoomin <- as.numeric(round(difftime(data.subset.new$Roomin.DTTM,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Roomin (min)
  data.subset.new$providerinToOut <- as.numeric(round(difftime(data.subset.new$Providerout_DTTM,data.subset.new$Providerin_DTTM,units="mins"),1)) ## Provider in to out (min)
  data.subset.new$visitEndToCheckout <- as.numeric(round(difftime(data.subset.new$Checkout.DTTM,data.subset.new$Visitend.DTTM,units="mins"),1)) ## Visitend to Checkout (min)
  data.subset.new$Resource <- ifelse(data.subset.new$Resource == 1, "Provider", "Resource")
  
  ## Identify US Holidays in Data 
  hld <- holidaysBetween(min(data.subset.new$Appt.DTTM, na.rm=TRUE), max(data.subset.new$Appt.DTTM, na.rm=TRUE))
  holid <- as.Date(as.character(hld), format = "%Y%m%d")
  names(holid) <- names(hld)
  holid <- as.data.frame(holid)
  holid <- cbind(names(hld), holid)
  rownames(holid) <- NULL
  colnames(holid) <- c("holiday","date")
  holid$holiday <- as.character(holid$holiday)
  holid$date <- as.character(holid$date)
  holid$date <- as.Date(holid$date, format="%Y-%m-%d")
  
  data.subset.new$holiday <- holid$holiday[match(data.subset.new$Appt.DateYear, holid$date)]
  
  # Pre-processed Scheduling Dataframe
  data.subset.new <- as.data.frame(data.subset.new)
  
  reuturn_list <- list(data.subset.new,holid)
  return(reuturn_list)
}



con <- dbConnect(odbc(), Driver = "Oracle",
                 Host = "msx01-scan.mountsinai.org",
                 Port = 1521,
                 SVC = "PRD_MSX_TAF.msnyuhealth.org",
                 UID = "villea04",
                 PWD = "villea04123$"
)


#access_date_1 <- Sys.Date()-2
#access_date_2 <- Sys.Date()-2

 access_date_1 <- "2021-01-01"
 access_date_2 <- "2022-08-21"

access_sql <- paste0("SELECT DEP_RPT_GRP_SEVENTEEN,DEPT_SPECIALTY_NAME,DEPARTMENT_NAME,PROV_NAME_WID,DEPARTMENT_ID,REFERRING_PROV_NAME_WID,
                     MRN,PAT_NAME,ZIP_CODE,BIRTH_DATE,FINCLASS,
                     APPT_MADE_DTTM,APPT_DTTM,PRC_NAME,APPT_LENGTH,DERIVED_STATUS_DESC,
                     APPT_CANC_DTTM, CANCEL_REASON_NAME,
                     SIGNIN_DTTM,PAGED_DTTM,CHECKIN_DTTM,ARVL_LIST_REMOVE_DTTM,
                     ROOMED_DTTM,FIRST_ROOM_ASSIGN_DTTM,
                     PHYS_ENTER_DTTM,VISIT_END_DTTM,CHECKOUT_DTTM,
                     TIME_IN_ROOM_MINUTES,CYCLE_TIME_MINUTES,VISIT_GROUP_NUM,LOS_NAME,LOS_CODE,
                     DEP_RPT_GRP_THIRTYONE,APPT_ENTRY_USER_NAME_WID, 
                     ACCESS_CENTER_SCHEDULED_YN, VISIT_METHOD, VISIT_PROV_STAFF_RESOURCE_C,
                     PRIMARY_DX_CODE,ENC_CLOSED_CHARGE_STATUS,Y_ENC_COSIGN_TIME,Y_ENC_CLOSE_TIME,Y_ENC_OPEN_TIME, NPI, PAT_ENC_CSN_ID, VISITPLAN, ATTRIB_BILL_AREA
FROM CRREPORT_REP.MV_DM_PATIENT_ACCESS
WHERE CONTACT_DATE BETWEEN TO_DATE('", access_date_1,  "00:00:00', 'YYYY-MM-DD HH24:MI:SS')
				AND TO_DATE('", access_date_2, "23:59:59', 'YYYY-MM-DD HH24:MI:SS')
                     OR APPT_MADE_DTTM BETWEEN TO_DATE('", access_date_1,  "00:00:00', 'YYYY-MM-DD HH24:MI:SS')
				AND TO_DATE('", access_date_2, "23:59:59', 'YYYY-MM-DD HH24:MI:SS')")



access_raw <- dbGetQuery(con, access_sql)
#access_raw <- read.csv("access_04_12_2022.csv")

process_data_run <- process_data(access_raw)
historical.data <- process_data_run[[1]]
holid <- process_data_run[[2]]

rm(access_raw)



