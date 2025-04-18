train_FD001 <- read_csv("train_FD001.csv")
train_FD002 <- read_csv("train_FD002.csv")
train_FD003 <- read_csv("train_FD003.csv")
train_FD004 <- read_csv("train_FD004.csv")

FD001_all_results <- read_csv("FD001_all_results.csv")
FD002_all_results <- read_csv("FD002_all_results.csv")
FD003_all_results <- read_csv("FD003_all_results.csv")
FD004_all_results <- read_csv("FD004_all_results.csv")

all_results <- bind_rows(FD001_all_results,
                         FD002_all_results,
                         FD003_all_results,
                         FD004_all_results)

write_csv(all_results, "all_results.csv")
