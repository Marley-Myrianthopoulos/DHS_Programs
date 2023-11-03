week_number <- commandArgs(trailingOnly = TRUE)

csv_pathway <- paste("/Users/teacher/Desktop/Attendance Machine/Week ", week_number, "/attendance_week_", week_number, ".csv", sep = "")

raw_data <- read.csv(csv_pathway)

raw_data$Date <- as.Date(raw_data$Date, "%m/%d/%Y")

exclusions <- c("2023-09-07", "2023-09-08", "2023-11-01", "2023-11-02", "2023-11-03", "2023-11-17", "2023-12-22", "2024-01-18", "2024-01-19", "2024-01-22", "2024-03-13", "2024-03-14", "2024-03-15", "2024-03-22", "2024-05-01", "2024-05-02", "2024-05-03")

exclusions <- as.Date(exclusions)

library(dplyr)

modified_data <- raw_data %>%
  filter(!Date %in% exclusions) %>%
  group_by(StudentID, Date) %>%
  mutate(daily = sum(Attendance %in% c("P", "T", "S"))) %>%
  ungroup()

missed_school <- modified_data %>%
  filter(Period == 4) %>%
  group_by(StudentID) %>%
  mutate(days_missed = sum(case_when(daily < 2 ~ 1, T ~ 0))) %>%
  ungroup() %>%
  filter(days_missed != 0) %>%
  select(StudentID, Name, GradeLevel, days_missed) %>%
  distinct() %>%
  arrange(desc(days_missed), GradeLevel, StudentID)

missed_2nd_3rd <- modified_data %>%
  group_by(StudentID) %>%
  mutate(missed2nd = sum(!Attendance %in% c("P", "T", "S") & Period == 2 & daily >= 2)) %>%
  mutate(missed3rd = sum(!Attendance %in% c("P", "T", "S") & Period == 3 & daily >= 2)) %>%
  ungroup() %>%
  filter(missed2nd != 0) %>%
  select(StudentID, Name, GradeLevel, missed2nd, missed3rd) %>%
  distinct() %>%
  arrange(desc(missed2nd), desc(missed3rd), GradeLevel, StudentID)

missed_both <- modified_data %>%
  group_by(StudentID) %>%
  mutate(missed2nd = sum(!Attendance %in% c("P", "T", "S") & Period == 2 & daily >= 2)) %>%
  filter(Period == 4) %>%
  mutate(days_missed = sum(case_when(daily < 2 ~ 1, T ~ 0))) %>%
  ungroup() %>%
  filter(days_missed >= 2 & missed2nd >= 2) %>%
  select(StudentID, Name, GradeLevel, missed2nd, days_missed) %>%
  distinct() %>%
  arrange(desc(days_missed), desc(missed2nd), GradeLevel, StudentID)

perfect_all <- modified_data %>%
  group_by(StudentID) %>%
  mutate(missed_periods = sum(!Attendance %in% c("P", "T", "S"))) %>%
  ungroup() %>%
  filter(missed_periods == 0) %>%
  select(StudentID, Name, GradeLevel) %>%
  distinct() %>%
  arrange(GradeLevel, StudentID)

perfect_24 <- modified_data %>%
  filter(Period %in% c(2,4)) %>%
  group_by(StudentID) %>%
  mutate(missed24 = sum(!Attendance %in% c("P", "T", "S"))) %>%
  ungroup() %>%
  filter(missed24 == 0) %>%
  select(StudentID, Name, GradeLevel) %>%
  distinct() %>%
  arrange(GradeLevel, StudentID)

write_pathway <- paste("/Users/teacher/Desktop/Attendance Machine/Week ", week_number, sep = "")

write.csv(missed_2nd_3rd, file.path(write_pathway, "missed2nd3rd.csv"), row.names = FALSE)
write.csv(missed_school, file.path(write_pathway, "missed_school.csv"), row.names = FALSE)
write.csv(missed_both, file.path(write_pathway, "missed_both.csv"), row.names = FALSE)
write.csv(perfect_all, file.path(write_pathway, "perfect_all.csv"), row.names = FALSE)
write.csv(perfect_24, file.path(write_pathway, "perfect_24.csv"), row.names = FALSE)