

library(dplyr)
library(knitr)
library(kableExtra)
library(tinytex)
library(hms)
library(lubridate)
library(stringr)
 


 

FMOrig <- read.csv("C:/Users/christine.iyer/Downloads/MKT_output (2).csv", header = T, stringsAsFactors = F)
 
objective <- c("IG", "BR", "AG")

FM <- FMOrig %>% filter(Master_Adname_Type == "NC" )
FM <- FM %>%
  filter(Code_Product == "UG" & Code_Objective %in% objective) %>% 
  mutate(
    Code_Audience = ifelse(Master_Adname == "FA20_BR_FB_VID__TL_UG_D_1", "TL", Code_Audience), 
    Code_Product = ifelse(Master_Adname == "FA20_BR_FB_VID__TL_UG_D_1", "UG", Code_Product)) %>%
  select(1:8, 13, 15:19, 21:24, 26, 28:29, 31:39, 41) %>% 
  select(Master_Date, everything())
head(FM)
FM <- FM %>% 
  mutate_at(10:31, ~replace(., is.na(.), 0)) %>% 
  mutate(Master_Date = as.Date(Master_Date, format = "%m/%d/%Y"), 
         Master_Results = as.numeric(Master_Results), 
         Master_Engagements = as.numeric(Master_Engagements)) %>% 
  arrange(Master_Date) 

Start <- FM %>% 
  mutate(AdSet = paste0(Code_RecruitingPeriod, "_",Code_Product, "_",Code_Audience, "_",Code_Vendor, "_",Code_Medium, "_",Code_Objective)) %>% 
  group_by(AdSet) %>% summarise(Start_Date = min(Master_Date), End_Date = max(Master_Date))
 



FM <- FM %>%
  mutate(FY = ifelse(Master_Date > "2018-06-30 " & Master_Date <= "2019-06-30", "FY19",
                     ifelse(Master_Date >= "2019-07-01 " & Master_Date < "2020-07-01", "FY20","N/A")), 
         AdSet = paste0(Code_RecruitingPeriod, "_",Code_Product, "_",Code_Audience, "_",Code_Vendor, "_",Code_Medium, "_",Code_Objective), 
         Ad = paste0(Code_Audience, "_",Code_Vendor, "_",Code_Medium), 
         Vendor = paste0(Code_Vendor, "_",Code_Medium))
 

  
FMQ19 <- FM%>% 
  mutate(
    Quarter = paste0((FY),"Q",quarter(Master_Date, with_year = FALSE, fiscal_start = 7))
    #, 
    #Vendor_Medium = paste0(Code_Medium, "_", Code_Vendor),
    # Week = (cut(Master_Date + 1, "week")),
    # Week = as.Date(Week, format = "%Y-%m-%d"), 
  ) %>% 
  group_by(Quarter, AdSet, Ad, Vendor) %>% 
  summarise(
    Cost = sum(Master_Cost),
    Clicks = sum(Master_Clicks), 
    Impressions = round(sum(Master_Impressions),0), 
    ClicksToSite = sum(Master_Clicks_To_Site),
    Bounces = sum(Master_Bounces),
    Sessions = sum(Master_Sessions), 
    UPV = sum(Master_Unique_Pageviews), 
    Views = sum(Master_Views), 
    Completions = sum(Master_Completions), 
    Step1 = sum(Master_Time_On_Page)/(sum(Master_Pageviews)- sum(Master_Exits)), 
    Av_TOP = round_hms(as_hms(Step1), 5), 
    ClickRatePct = round(Clicks/Impressions * 100, 2), 
    BounceRatePct = round(Bounces/Sessions * 100, 2), 
    ViewRate = round(Views/Impressions * 100, 2), 
    VTR = round(Completions/Impressions *100, 2), 
    ClickableCompletions = sum(Master_Clickable_Completions), 
    ClickableCompRate = round(Clicks/ClickableCompletions * 100, 2), 
    Swipes = sum(Master_Swipes), 
    Opens = sum(Master_Clicks_To_Site), 
    Sends = sum(Master_Sends), 
    Engagements = sum(Master_Engagements), 
    Results = sum(Master_Results))
 


colorP <- c("#F6A704", "#0E1033","#4E7B14","#A92007","#D47E0A")
FMVM <- FMQ19  %>% 
  group_by(Quarter,AdSet, Ad, Vendor) %>%
  summarise(
    Cost = round(sum(Cost), 2),
    Clicks = sum(Clicks),
    Impressions = round(sum(Impressions),0),
    Bounces = sum(Bounces),
    Sessions = sum(Sessions),
    UPV = sum(UPV),
    Views = sum(Views),
    Completions = sum(Completions),
    Step1 = sum(Step1),
    ClickRatePct = round(Clicks/Impressions * 100, 2),
    BounceRatePct = round(Bounces/Sessions * 100, 2),
    ViewRate = round(Views/Impressions * 100, 2),
    VTR = round(Completions/Impressions *100, 2),
    ClickableCompletions = sum(ClickableCompletions),
    ClickableCompRate = round(Clicks/ClickableCompletions * 100, 2), 
    Swipes = sum(Swipes), 
    SwipeUpRatePct = round(Swipes/Impressions * 100, 2),
    Opens = sum(Opens),
    CTOR = round(Opens/Clicks * 100, 2), 
    Sends = sum(Sends), 
    LIOpens = sum(Opens),
    OpenRate = round(LIOpens/Sends * 100, 2), 
    Engagements = sum(Engagements),
    Results = sum(Results), 
    EngagementRate = round(Engagements/Impressions * 100, 2),
    EngRate = round((sum(LIOpens)+sum(Engagements))/sum(Sends)*100, 2), 
    Avg_RR = round(sum(Results)/sum(Impressions) * 100, 2)) %>%
  select(Quarter, AdSet,Ad,Vendor,Impressions, Clicks, Bounces, Sessions, Swipes, ClickRatePct, SwipeUpRatePct, UPV, BounceRatePct, VTR, CTOR,OpenRate, Sends, ViewRate, ClickableCompletions, Completions, Opens, LIOpens,  Engagements,  EngRate, EngagementRate,   Step1, Cost, Avg_RR, Results) 
NewData <- full_join(FMVM, Start) 
 

## Findings


NewData <- NewData %>%
  mutate(ClickThruRate = paste0(ClickRatePct, "%"),
         BounceRate = paste0(BounceRatePct, "%"),
         Impressions = format(as.numeric(Impressions), big.mark=","), 
         UPV = format(as.numeric(UPV),  big.mark="," ),
         Cost = paste0("$ ", Cost),
         Av_TOP = round_hms(as_hms(Step1), 5), 
         Start_Date = format(as.Date(Start_Date), "%m-%Y"), 
         End_Date = format(as.Date(End_Date), "%m-%Y"), 
         End_Date = ifelse(End_Date == format(Sys.Date(),"%m-%Y"), "Ongoing", End_Date)) %>%
  select(Quarter, AdSet, Ad,Vendor,Impressions, CTR = ClickThruRate, UPV, BR = BounceRate, Av_TOP, Cost, Start_Date, End_Date) %>% filter(Impressions > 0)

write.csv(NewData, "C:/Users/christine.iyer/Box/quarterly-ad-analytics-projects/Flowchart.csv", row.names = F)
