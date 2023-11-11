library(dplyr)

full_attendance_data <- read.csv("/Users/marleymyrianthopoulos/Desktop/attendance.csv")

full_attendance_data$Date <- as.Date(full_attendance_data$Date, "%m/%d/%Y")

non_seniors <- full_attendance_data %>%
  filter(GradeLevel %in% c(9:11)) %>%
  filter(between(Date, as.Date("2023-10-01"), as.Date("2023-10-31"))) %>%
  filter(Period %in% c(2,4)) %>%
  group_by(StudentID) %>%
  mutate(missed = sum(!Attendance %in% c("P", "T", "DD"))) %>%
  ungroup() %>%
  filter(missed == 0) %>%
  select(StudentID, Name, GradeLevel) %>%
  distinct() %>%
  arrange(GradeLevel, StudentID)

seniors <- full_attendance_data %>%
  filter(GradeLevel == 12) %>%
  filter(between(Date, as.Date("2023-10-01"), as.Date("2023-10-31"))) %>%
  filter(Period %in% c(3,4)) %>%
  group_by(StudentID) %>%
  mutate(missed = sum(!Attendance %in% c("P", "T", "DD"))) %>%
  ungroup() %>%
  filter(missed == 0) %>%
  select(StudentID, Name, GradeLevel) %>%
  distinct() %>%
  arrange(GradeLevel, StudentID)

full_list <- bind_rows(non_seniors, seniors)

write.csv(full_list, "/Users/marleymyrianthopoulos/Desktop/perf_oct.csv")