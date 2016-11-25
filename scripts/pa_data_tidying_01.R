# ------------------------------UTF-8-------------------------------------------
# The script is partially tidies the data on protected areas in Ukraine available
# from http://data.gov.ua/passport/9e011264-c16d-42ab-95f1-b06f7311103e.
# 
# Цей скрипт частково впорядковує дані про території природно-заповідного фонду України
# доступні з http://data.gov.ua/passport/9e011264-c16d-42ab-95f1-b06f7311103e.
# 
# How it works:
#   It takes partially preprocessed messy data from ./menr_pa_data/messy_data_01
#   directory and applies to it the changes as follows:
#                * creates and fills importance field
#                * removes redundant summarizing rows
#                * removes redundant quotes and symbols from names
#                * standardizes categories field
#                * standardizes types field
#                * removes redundant symbols from operator names
#                * removes redundant symbols from location
#    Tidied csv-files are written to the ./menr_pa_data/tidy_data_01 directory.
#    Separate file for each administrative unit is created; additionally, one
#    general file ukraine.csv merges all the data.
# 
# Як це працює:
#    Скрипт приймає частково передоброблені невпорядковані дані з каталогу
#    ./menr_pa_data/messy_data_01 та застосовує до них наступні зміни:
#                * створює та заповнює поле importance
#                * видаляє зайві підсумовуючі рядки
#                * видаляє зайві лапки та символи з назв
#                * стандартизує поле category
#                * стандартизує поле type
#                * видаляє зайві лапки та символи з desig.decision
#                * видаляє надлишкові символи з назв operator
#                * видаляє надлишкові символи з location
#    Структуровані csv-файли записуються до каталогу ./menr_pa_data/tidy_data_01.
#    Для кожної адміністративно-територіальнох одиниці створюється окремий файл;
#    додатково створюється один загальний файл ukraine.csv, який об'єднує всі дані.
#    
#    
# To avoid problems with encodng (on Windows) use:
# Для уникнення проблем з кодуванням (під Windows) використовуйте:
# Sys.setlocale(category = "LC_ALL", locale = "Ukrainian")
# eval(parse("pa_data_tidying_01.R", encoding="UTF-8"))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 1. Setup working environment
# Крок 1. Підготувати робоче середовище
# ------------------------------------------------------------------------------
## install.packages("dplyr")
library(dplyr)

## create list for file naming
## створити список для найменування файлів
adm.unit.ua <-   c("АР Крим", "Вінницька", "Волинська", "Дніпропетровська",
                   "Донецька", "Житомирська", "Закарпатська", "Запорізька",
                   "Івано-Франківська", "Київська", "Кіровоградська", "Луганська",
                   "Львівська", "Миколаївська",  "Одеська", "Полтавська",
                   "Рівненська", "Сумська", "Тернопільська", "Харківська",
                   "Херсонська", "Хмельницька", "Черкаська", "Чернівецька",
                   "Чернігівська", "місто Київ", "місто Севастополь")
adm.unit.en <-   c("ar_krym", "vinnytska", "volynska", "dnipropetrovska",
                   "donetska", "zhytomyrska", "zakarpatska", "zaporizka",
                   "ivano_frankivska", "kyivska", "kirovohradska", "luhanska",
                   "lvivska", "mykolaivska", "odeska", "poltavska", "rivnenska",
                   "sumska", "ternopilska", "kharkivska", "khersonska",
                   "khmelnytska", "cherkaska", "chernivetska", "chernihivska",
                   "kyiv_city", "sevastopol_city")
name.list <- setNames(as.list(adm.unit.ua), adm.unit.en)
files <- list.files("./menr_pa_data/messy_data_01/")

## create directory for tidy data
## створити каталог для структурованих даних
if (!dir.exists("./menr_pa_data/tidy_data_01")) {
        dir.create("./menr_pa_data/tidy_data_01", recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Step 2. Tidy the data
# Крок 2. Структурувати дані
# ------------------------------------------------------------------------------ 
for (i in 1:length(files)) {
        
        ## read the file
        ## прочитати файл
        file.path        <- paste0("./menr_pa_data/messy_data_01/", files[i])
        column.names     <- c("id", "name", "category", "type", "area.ha",
                              "location", "operator", "desig.decision",
                              rep(NA, 13))
        adm.unit.file    <- read.csv(file = file.path, header = TRUE, sep = ";",
                                     col.names  = column.names,
                                     na.strings = c("", " ", "NA", "-"),
                                     colClasses = c(rep("character", 8),
                                                    rep(NA, 13)),
                                     nrows = -1, strip.white = TRUE,
                                     stringsAsFactors = FALSE)
        adm.unit.process <- tbl_df(adm.unit.file[, 1:8])
        
        ## define and add administrative unit name field
        ## визначити та додати поле назви адміністративної одиниці
        adm.unit.name    <- sapply(strsplit(files[i], split = "[.]"),
                                   function(x) (x[1]))
        adm.unit.process <- mutate(adm.unit.process,
                                   oblast = name.list[[adm.unit.name]])
        
        ## define and add importance field
        ## визначити та додати поле importance
        n.row             <- which(grepl("місцевого", adm.unit.process$id))
        importance.values <- c("загальнодержавне", "місцеве")
        importance.counts <- c(n.row - 1, (nrow(adm.unit.process) - n.row) + 1)
        importance        <- rep(importance.values, importance.counts)
        adm.unit.process  <- mutate(adm.unit.process,
                                    importance = importance,
                                    area.ha = gsub("[[:space:]]", "", area.ha),    ## convert area to numeric values 
                                    area.ha = as.numeric(gsub(",", ".", area.ha))) ## перевести площу в числові значення 
        
        ## remove redundant summarizing rows
        ## видалити надлишкові підсумовуючі рядки
        summary.text     <- "[Вв]сього|Вссього|[Рр]азом|Разоп|Всьоо|Всьго" 
        adm.unit.process <- filter(adm.unit.process,
                                   rowSums(is.na(adm.unit.process)) < 7 &
                                   !grepl(summary.text, name))
        
        
        ## remove redundant quotes and symbols from names
        ## видалити надлишкові лапки та символи з назв
        redundant.symbol      <- "\"|_|«|»|“|”|„|\\*|^[[:punct:]]|‘‘|’’"
        adm.unit.process$name <- gsub(redundant.symbol, "", adm.unit.process$name)
        
        
        ## standardize categories
        ## стандартизувати категорії
        index <- grepl("мистецт", adm.unit.process$category)
        adm.unit.process$category[index] <- "парк-пам’ятка садово-паркового мистецтва"
        adm.unit.process$category <- gsub("дендоропарк місцевого значення|денропарк|дендропарк",
                                          "дендрологічний парк", adm.unit.process$category)
        adm.unit.process$category <- gsub("природний заповідни\\b",
                                          "природний заповідник", adm.unit.process$category)
        adm.unit.process$category <- gsub("гідрологічна пам’ятка природи|комплексна",
                                          "пам’ятка природи", adm.unit.process$category)
        adm.unit.process$category <- gsub("зоопарк|зологічний парк",
                                          "зоологічний парк", adm.unit.process$category)
        adm.unit.process$category <- gsub("Регіональні ландшафтні парки",
                                          "регіональний ландшафтний парк", adm.unit.process$category)
        
        ## standardize types
        ## стандартизувати типи
        adm.unit.process$type <- gsub("загально-зоологічних|загально- зоологічні|[Зз]агально-зоологічний|загально - зоологічний|Загальнозоологічні|Загально зоологічні|загально зоологічний|Загальнозоо-логічний",
                                      "загальнозоологічний", adm.unit.process$type)
        adm.unit.process$type <- gsub("загально-зоологічна",
                                      "загальнозоологічна", adm.unit.process$type)
        adm.unit.process$type <- gsub("гідррологічний|Гідрологічні",
                                      "гідрологічний", adm.unit.process$type)
        adm.unit.process$type <- gsub("гідрололгічна",
                                      "гідрологічна", adm.unit.process$type)
        adm.unit.process$type <- gsub("геологічні",
                                      "геологічна", adm.unit.process$type)
        adm.unit.process$type <- gsub("іхтіологічних",
                                      "іхтіологічний", adm.unit.process$type)
        adm.unit.process$type <- gsub("ландшафт-ний|ландшаф-тний",
                                      "ландшафтний", adm.unit.process$type)
        adm.unit.process$type <- gsub("Орнітологічні|орнітологіч-ний",
                                      "орнітологічний", adm.unit.process$type)
        
        
        ## finalize the file
        ## остаточно оформити файл
        adm.unit.process <- mutate(adm.unit.process, id = 1:nrow(adm.unit.process),           ## numerate field id sequentially
                                   name = gsub("\\s+"," ", name),                             ## remove redundant spaces
                                   category = tolower(category),                              ## convert uppercase to lowercase
                                   category = gsub("`|\"|'", "’", category),                  ## replace wrong apostrophes
                                   category = gsub("\\s+"," ", category),                     ## remove redundant spaces
                                   type = tolower(type),                                      ## convert uppercase to lowercase
                                   type = gsub("\\s+"," ", type),                             ## remove redundant spaces
                                   desig.decision = gsub("\\s+"," ", desig.decision),         ## remove redundant spaces
                                   desig.decision = gsub("\"|«|»|“|”|„", "", desig.decision), ## remove quotes
                                   operator = gsub("\"|«|»|“|”|„", "", operator),             ## remove quotes
                                   operator = gsub("\\s+"," ", operator),                     ## remove redundant spaces
                                   location = gsub("\"|«|»|“|”|„", "", location),             ## remove quotes
                                   location = gsub("\\s+"," ", location)) %>%                 ## remove redundant spaces
                            select(id, oblast, importance, name, category, type,              ## reorder fields
                                   area.ha, desig.decision, operator, location)
        
        ## write output tidy data file
        ## записати результуючий файл з впордякованими даними
        file.name <- paste0("./menr_pa_data/tidy_data_01/", files[i])
        write.table(adm.unit.process, file = file.name, sep = ",", na = "",
                    row.names = FALSE, fileEncoding = "UTF-8")
        
}

# ------------------------------------------------------------------------------
# Step 3. Merge all the data into single file
# Крок 3. Об'єднати всі дані в єдиний файл
# ------------------------------------------------------------------------------
data.directory <- "./menr_pa_data/tidy_data_01/"
tidy.files <- list.files(data.directory)
data.merge <- data.frame()

for (i in 1:length(tidy.files)) {
         file.name <- paste0(data.directory, tidy.files[i])
         tidy.file <- read.csv(file.name, encoding = "UTF-8",
                               stringsAsFactors = FALSE)
         data.merge <- rbind(data.merge, tidy.file)
}

data.merge <- arrange(data.merge, oblast, id) %>%
              mutate(id = 1:nrow(data.merge))
merge.name <- "./menr_pa_data/tidy_data_01/ukraine.csv"
write.table(data.merge, file = merge.name, sep = ",", na = "",
            row.names = FALSE, fileEncoding = "UTF-8")

print("Обробку файлів завершено")