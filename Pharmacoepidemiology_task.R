Pharmacoepidemiology task
Paraskevi Tassopoulou
16.01.2023
Setting up the session
# Load the packages
library(data.table)
library(lubridate)
library(readxl)
library(stringr)

# Set the seed for the session
addTaskCallback(function(...) {set.seed(123);TRUE})
## 1 
## 1
size_sample = 1000

# Create the example data
stamm <- data.table(PID = c(1:size_sample),
                    sex = sample(c(1,2), replace=TRUE, size=size_sample),
                    birth_qtr = sample(seq(as.Date('1950-01-01'), as.Date('2019-12-31'), by="quarter"), replace=TRUE, size = size_sample))

ins_int <-  data.table(PID = c(1:size_sample),
                       ins_start = sample(seq(as.Date('2015-01-01'), as.Date('2019-12-31'), by="year"), replace=TRUE, size = size_sample),
                       ins_end = sample(c(30,60,365,730), replace=TRUE, size = size_sample)
)
ins_int[,ins_end:=ins_start %m+% days(ins_end)]

medication <-  data.table(PID = sample(1:size_sample, replace=TRUE, size=4500),
                          prescription_dt = sample(seq(as.Date('2015-01-01'), as.Date('2020-12-31'), by="day"), replace=TRUE, size = 4500),
                          ATC_Code = sample(c("C07BB02", "A10BK01", "C07FX05","C01EB17","H02AB09",
                                              "H02AB10","H02AB11","H02AB12","H02AB13","H02AB14",
                                              "H02AB15","H02AB17","J01AA02","J01AA03","J01AA04",
                                              "J01AA05", "A10BA02"), replace=TRUE, size = 4500)
)
Start of the session
Part 1: Demographics

1. Calculate age at 2020-01-01.
# Convert birth_qtr to date format
stamm[, birth_qtr := as.Date(birth_qtr, format = "%Y-%m-%d")]

# Substract the birth_qtr from 2020-01-01, and divide by 365.25 to get the number of years
stamm[, age_at_2020 := as.numeric(as.Date("2020-01-01") - birth_qtr)/ 365.25]
2. Add a variable with age group (in steps of 10 between 0 and 90 years old: 0 - 9, 10 - 19, 20 - 29 etc..).
I use the cut() function tp create groups in steps between 0-90 years. I use seq() function to create the vector of the breaks.

Note: The ‘stamm’ data table does not contain any patients over 70. However, in my opinion, the complete answer to that question is to also include the 90. So, I created an extra ‘90+’ group. I assign a label ‘90+’ to any ages greater than 90 and add Inf to the breaks to make sure all >90 ages will be included.

stamm[, age_group := cut(age_at_2020, breaks = c(seq(0, 90, by = 10), Inf), labels = c(paste(seq(0, 80, by = 10), seq(9, 89, by = 10), sep = "-"), "90+"))]

head(stamm)
##    PID sex  birth_qtr age_at_2020 age_group
## 1:   1   1 1975-07-01   44.503765     40-49
## 2:   2   2 1979-04-01   40.752909     40-49
## 3:   3   1 2016-10-01    3.249829       0-9
## 4:   4   2 1955-04-01   64.752909     60-69
## 5:   5   2 1990-07-01   29.503080     20-29
## 6:   6   2 2011-01-01    8.999316       0-9
3. Create an overview of how many unique PIDs per age group are available. Ordered by age group.
I use the uniqueN() funtion to count the number of unique PIDs per age group and then I order by age group.

stamm[, uniqueN(PID), by = age_group][order(age_group)]
##    age_group  V1
## 1:       0-9 150
## 2:     10-19 142
## 3:     20-29 145
## 4:     30-39 130
## 5:     40-49 140
## 6:     50-59 141
## 7:     60-69 152
4. Create an overview of how many unique PIDs per age group and sex are available.
Same rationale here.

stamm[, uniqueN(PID),by =c("age_group","sex")][order(age_group, sex)]
##     age_group sex V1
##  1:       0-9   1 71
##  2:       0-9   2 79
##  3:     10-19   1 75
##  4:     10-19   2 67
##  5:     20-29   1 78
##  6:     20-29   2 67
##  7:     30-39   1 62
##  8:     30-39   2 68
##  9:     40-49   1 69
## 10:     40-49   2 71
## 11:     50-59   1 74
## 12:     50-59   2 67
## 13:     60-69   1 76
## 14:     60-69   2 76
Part 2: Insurance

5. Check which patients were fully insured in 2020 (01.01.2020-31.12.2020). Add a corresponding variable in the stamm data. Hint: create a flag (binary) variable per PID indicating whether they fulfill the requirement.
First, I create a new data.table called ‘ins_int_2020’ that contains the patients who were fully insured in 2020.

ins_int_2020 <- ins_int[ins_start <= as.Date("2020-01-01") & ins_end >= as.Date("2020-12-31")]
head(ins_int_2020)
##    PID  ins_start    ins_end
## 1:  18 2019-01-01 2020-12-31
## 2:  62 2019-01-01 2020-12-31
## 3: 117 2019-01-01 2020-12-31
## 4: 129 2019-01-01 2020-12-31
## 5: 166 2019-01-01 2020-12-31
## 6: 167 2019-01-01 2020-12-31
Then, I create a new variable called ‘fully_insured_2020’ in the ‘stamm’ which assigns a value of 1 to the rows that match the ‘PID’ between ‘stamm’ and ‘ins_int_2020’ tables, and 0 to the rows that don’t match.

stamm[ins_int_2020, fully_insured_2020 := 1, on = .(PID)]
stamm[is.na(fully_insured_2020), fully_insured_2020 := 0]
head(stamm)
##    PID sex  birth_qtr age_at_2020 age_group fully_insured_2020
## 1:   1   1 1975-07-01   44.503765     40-49                  0
## 2:   2   2 1979-04-01   40.752909     40-49                  0
## 3:   3   1 2016-10-01    3.249829       0-9                  0
## 4:   4   2 1955-04-01   64.752909     60-69                  0
## 5:   5   2 1990-07-01   29.503080     20-29                  0
## 6:   6   2 2011-01-01    8.999316       0-9                  0
Part 3: Medication

6. We only have exact dates of medication prescription, create a new variable that corresponds to the first day of the quarter per prescription date.
I create a new variable called ‘quarter_start’ in the ‘medication’ table and assign a date that corresponds to the first day of the quarter of the ‘prescription_dt’ variable. I use the floor_date() function from lubridate, which rounds down the date to the nearest specified unit (in our case is quarter).

medication[, quarter_start := as.Date(floor_date(prescription_dt, unit = "quarter"))]
head(medication)
##    PID prescription_dt ATC_Code quarter_start
## 1: 719      2019-11-12  A10BA02    2019-10-01
## 2: 678      2019-05-20  H02AB13    2019-04-01
## 3: 895      2015-04-11  H02AB13    2015-04-01
## 4: 401      2019-07-30  H02AB17    2019-07-01
## 5: 683      2019-03-26  H02AB17    2019-01-01
## 6: 842      2018-06-15  H02AB17    2018-04-01
7. Join the datasets stamm and medication together for those patients who were fully insured in 2020.
I join the two data tables on the PID variable and then I keep only the rows where the fully_insured variable equals 1.

Note: *nomatch argument is set to 0 which means that when there is no match in the join, the joined table will contain only the rows with a match.

# join the tables on PID
stamm_med_2020 <- stamm[medication, nomatch = 0, on = 'PID']

# subset the fully insured patients
stamm_med_2020 <- stamm_med_2020[fully_insured_2020 == 1]
8. Find the number of insured PIDs with ATC codes that start with C07
I filter the rows with grepl() function to check if the values in the ATC_Code column start with the string ‘C07’, I define the ‘Count’ column that I’d like to include in the output (we want to include only the unique PIDs) and I group the results by the ‘fully_insured_2020’ variable.

#
stamm_med_2020[grepl("^C07", ATC_Code) , .(Count = uniqueN(PID)), by = fully_insured_2020]
##    fully_insured_2020 Count
## 1:                  1    13
9. Join the joined stamm and medication with the external excel reference –> notice you may need to load another package! Make sure you do not lose any PIDs in this process!
  # Load a package to read the external .xls
  library(readxl)

# Read the .xls file - assign the empty cells to NA 
reference_data <- read_excel("~/Downloads/reference.xlsx", na = "")

# Convert to data table 
setDT(reference_data)
I process the reference_data before the join, to standardise the format of both columns.

# Remove leading and trailing whitespaces from both columns before the join.
library(stringr)
stamm_med_2020[, ATC_Code := str_trim(ATC_Code)]
reference_data[, code := str_trim(code)]

# Make sure every character is upper case
stamm_med_2020[, ATC_Code := str_to_upper(ATC_Code)]
reference_data[, code := str_to_upper(code)]

# Check for missing values and remove them accordinlgy
sum(is.na(stamm_med_2020$ATC_Code))
## [1] 0
sum(is.na(reference_data$code))
## [1] 4
reference_data <- reference_data[!is.na(reference_data$code),]

# Check for consistent data types
class(stamm_med_2020$ATC_Code)
## [1] "character"
class(reference_data$code)
## [1] "character"
# Check for formatting inconsistencies
sum(startsWith(stamm_med_2020$ATC_Code, "0"))
## [1] 0
sum(startsWith(reference_data$code, "0"))
## [1] 0
# Check for different levels of granularity
nchar(stamm_med_2020$ATC_Code)
##   [1] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
##  [38] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
##  [75] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
## [112] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
## [149] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
nchar(reference_data$code)
##  [1] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
Finally, I join the two tables. This time, for a change, I use the merge() function from the data.table package. all.x=TRUE is used to keep all the rows from left table even if there is no match in the right table (this is practically a left join).

merged_data <- merge(stamm_med_2020, reference_data, by.x = "ATC_Code", by.y = "code", all.x = TRUE)

# Double checking if I lost any PIDs
uniqueN(stamm_med_2020, "PID") == uniqueN(merged_data, "PID")
## [1] TRUE
10. Create a final table with an overview of the number of insured PIDs per medication description. (Note that some PIDs do not have any medication, they should be listed as having “None”)
For the final table, I subset the merged_data table based on the ‘fully_insured_2020 variable’ (has to be 1), I use the uniqueN() function to count the unique PIDs per medication description and, finally, I use the is.na() function to identify any rows with missing values in the description variable and replace them with “None”.

final_table <- merged_data[fully_insured_2020 == 1][, .(count = uniqueN(PID)), by = c("description")]
final_table[is.na(description), description := "None"]
print(final_table)
##     description count
##  1:        None    31
##  2:      SGLT2i     6
##  3:  Ivabradine    15
##  4:   Diuretics     8
##  5:    Cortison     9
##  6: Prednyliden     8
##  7:   Rimexolon    10
##  8: Deflazacort     8
##  9:  Cloprednol     8
## 10: Meprednison    11
## 11:  Cortivazol     9
## 12: Doxycyclin     16
## 13: Metacyclin     14
11. Add percentage to the table based on the total number inclusion PIDs.
I calculate the total number of insured PIDs by summing up the count column of the final_table, then add a new column to the final_table, named ‘percentage’, which calculates the percentage of insured PIDs per medication description by dividing each count by the total number of insured PIDs and multiplying the result by 100.

total_insured_pid <- sum(final_table[,count])
final_table[, percentage := (count / total_insured_pid) * 100]

print(final_table)
##     description count percentage
##  1:        None    31  20.261438
##  2:      SGLT2i     6   3.921569
##  3:  Ivabradine    15   9.803922
##  4:   Diuretics     8   5.228758
##  5:    Cortison     9   5.882353
##  6: Prednyliden     8   5.228758
##  7:   Rimexolon    10   6.535948
##  8: Deflazacort     8   5.228758
##  9:  Cloprednol     8   5.228758
## 10: Meprednison    11   7.189542
## 11:  Cortivazol     9   5.882353
## 12: Doxycyclin     16  10.457516
## 13: Metacyclin     14   9.150327