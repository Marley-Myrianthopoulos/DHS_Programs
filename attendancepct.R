arg_list <- commandArgs(trailingOnly = TRUE)

full_data <- read.csv("/Users/Teacher/Desktop/full_attendance.csv")

full_data$Date <- as.Date(full_data$Date, "%m/%d/%Y")

exclusions <- c("2023-09-07", "2023-09-08", "2023-11-01", "2023-11-02", "2023-11-03", "2023-11-17", "2023-12-22", "2024-01-18", "2024-01-19", "2024-01-22", "2024-03-13", "2024-03-14", "2024-03-15", "2024-03-22", "2024-05-01", "2024-05-02", "2024-05-03")

exclusions <- as.Date(exclusions)

library(dplyr)

att_pct_report <- full_data %>%
  filter(!Date %in% exclusions) %>%
  filter(between(Date,as.Date(arg_list[1]),as.Date(arg_list[2]))) %>%
  group_by(StudentID, Date) %>%
  mutate(daily = sum(case_when(Attendance %in% c("P", "T", "S") ~ 1, T ~ 0))) %>%
  ungroup() %>%
  filter(Period == 4) %>%
  group_by(StudentID) %>%
  mutate(num_records = n()) %>%
  mutate(days_present = sum(case_when(daily >= 2 ~ 1, T ~ 0))) %>%
  ungroup %>%
  mutate(att_pct = round(days_present / num_records * 100)) %>%
  select(StudentID, Name, GradeLevel, att_pct) %>%
  distinct() %>%
  filter(att_pct >= as.numeric(arg_list[3])) %>%
  arrange(desc(att_pct))

write_pathway <- paste("/Users/Teacher/Desktop/attendance_pct_above_", arg_list[3],"_btwn_",arg_list[1],"_and_",arg_list[2],".csv",sep = "")

write.csv(att_pct_report, write_pathway)

#Example: Rscript attendancepct.R 2023-10-01 2023-10-31 90 generates list of 90+% attendance in October
