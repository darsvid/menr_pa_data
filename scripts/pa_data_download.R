# ------------------------------UTF-8-------------------------------------------
# This script downloads data on protected areas in Ukraine available from
# http://data.gov.ua/passport/9e011264-c16d-42ab-95f1-b06f7311103e.
# 
# Цей скрипт завантажує дані про території природно-заповідного фонду України
# доступні з http://data.gov.ua/passport/9e011264-c16d-42ab-95f1-b06f7311103e.
# 
# 
# Arguments:
# Аргументи:
#   adm.unit: character vector of administrative units names; if not specified,
#             files for all units are downloaded by default.
#             
#             вектор символів з іменами адміністративних одиниць; стандартно
#             завантажуються файли для всіх одиниць, якщо не вказано.
# 
# 
# Returns:
# Результат:
#             original csv-files for specified administrative units downloaded
#             to the ./menr_pa_data/messy_data_00 directory; official naming
#             conventions are applied to simplify further use and analysis.
#             
#             оригінальні csv-файли для визначених адміністративних одиниць
#             завантажуються до каталогу ./menr_pa_data/messy_data_00;
#             для спрощення подальшого викристання та аналізу застосовується
#             загальноприйняте найменування.
#           
# 
# To avoid problems with encodng (on Windows) use:
# Для уникнення проблем з кодуванням (під Windows) використовуйте:
# Sys.setlocale(category = "LC_ALL", locale = "Ukrainian")
# eval(parse("pa_data_download.R", encoding="UTF-8"))
# get.pa.data() # or/ або
# get.pa.data(adm.unit = c("Вінницька", "місто Севастополь"))
# ------------------------------------------------------------------------------

get.pa.data <- function(adm.unit = c("АР Крим", "Вінницька", "Волинська",
                                  "Дніпропетровська", "Донецька", "Житомирська",
                                  "Закарпатська", "Запорізька", "Івано-Франківська",
                                  "Київська", "Кіровоградська", "Луганська",
                                  "Львівська", "Миколаївська",  "Одеська",
                                  "Полтавська", "Рівненська", "Сумська",
                                  "Тернопільська", "Харківська", "Херсонська",
                                  "Хмельницька", "Черкаська", "Чернівецька",
                                  "Чернігівська", "місто Київ", "місто Севастополь")) {
 
        
        # Step 1. Check input for correctness
        # Крок 1. Перевірити введені параметри 
        adm.unit.ua <- c("АР Крим", "Вінницька", "Волинська",
                         "Дніпропетровська", "Донецька", "Житомирська",
                         "Закарпатська", "Запорізька", "Івано-Франківська",
                         "Київська", "Кіровоградська", "Луганська",
                         "Львівська", "Миколаївська",  "Одеська",
                         "Полтавська", "Рівненська", "Сумська",
                         "Тернопільська", "Харківська", "Херсонська",
                         "Хмельницька", "Черкаська", "Чернівецька",
                         "Чернігівська", "місто Київ", "місто Севастополь")
        if (!(all(adm.unit %in% adm.unit.ua))) {
                stop("Невірна назва адміністративної одиниці.")
                }
                
        # Step 2. Create directory for data download
        # Крок 2. Створити каталог для завантаження даних
        if (!dir.exists("./menr_pa_data/messy_data_00")) {
                dir.create("./menr_pa_data/messy_data_00", recursive = TRUE)
                }
        
        # Step 3. Download the data
        # Крок 3. Завантажити дані
        # 
        ## create lists for downloading and naming files
        ## створити списки для завантаження та найменування файлів
        adm.unit.mask <- c("avtonomna_respublika_krym", "vinnycka", "volynska",
                           "dnipropetrovsk", "donecka", "zhytomyrska",
                           "zakarpatska", "zaporizka", "ivano-frankivsk",
                           "kyyivska", "kirovogradska", "luganska", "lvivska",
                           "mykolayivska", "odeska", "poltavska", "rivenska",
                           "sumska", "ternopilska", "harkivska", "hersonska",
                           "hmelnycka", "cherkaska", "chernivecka", 
                           "chernigivska", "kyyiv", "sevastopol")
        adm.unit.en <-   c("ar_krym", "vinnytska", "volynska", "dnipropetrovska",
                           "donetska", "zhytomyrska", "zakarpatska", "zaporizka",
                           "ivano_frankivska", "kyivska", "kirovohradska",
                           "luhanska", "lvivska", "mykolaivska", "odeska",
                           "poltavska", "rivnenska", "sumska", "ternopilska",
                           "kharkivska", "khersonska", "khmelnytska", "cherkaska",
                           "chernivetska", "chernihivska", "kyiv_city", "sevastopol_city")
        mask.list <- setNames(as.list(adm.unit.mask), adm.unit.ua)
        unit.download <- mask.list[adm.unit]
        name.list <- setNames(as.list(adm.unit.en), adm.unit.ua)
        unit.name <- name.list[adm.unit]
        
        ## define subsets with different file naming patterns
        ## визначити групи з різними способами найменування
        download.pattern01 <- c("Дніпропетровська", "Донецька", "Житомирська",
                                "Закарпатська", "Запорізька", "Івано-Франківська",
                                "Київська", "Кіровоградська", "Луганська",
                                "Львівська", "Миколаївська", "місто Київ")
        download.pattern02 <- c("Полтавська", "Рівненська", "Сумська",
                                "Тернопільська", "Харківська", "Херсонська",
                                "Хмельницька", "Чернівецька", "місто Севастополь")
       
        
        for (i in 1:length(unit.download)) {
                base.url <- "http://data.gov.ua/sites/default/files/media/document/570/22.02.2016/"
                
                ## apply different file naming patterns to subsets
                ## застосувати різні способи найменування до груп
                if (any(names(unit.download)[i] %in% download.pattern01)) {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapovidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya.csv"
                } else if (any(names(unit.download)[i] %in% download.pattern02)) {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapvidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya_.csv"
                
                ## apply different file naming patterns to separate files
                ## застосувати різні способи найменування до окремих файлів
                } else if (names(unit.download)[i] == "АР Крим") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapovidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya__0.csv"
                } else if (names(unit.download)[i] == "Вінницька") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapovidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya_0.csv"
                } else if (names(unit.download)[i] == "Волинська") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapovidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya__1.csv"
                } else if (names(unit.download)[i] == "Одеська") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapvidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya.csv"
                } else if (names(unit.download)[i] == "Черкаська") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapovidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya_.csv"
                } else if (names(unit.download)[i] == "Чернігівська") {
                        tail.url <- "_perelik_terytoriy_ta_obyektiv_pryrodno-zapvidnogo_fondu_zagalnoderzhavnogo_ta_miscevogo_znachennya__1.csv"
                }
                
                file.url <- paste0(base.url, unit.download[[i]], tail.url)
                
                ## simple download
                ## просте завантаження
                # dest.file <- paste0("./menr_pa_data/messy_data_00/", unit.name[[i]], ".csv")
                # download.file(file.url, destfile = dest.file, mode = "wb")
                
                ## advanced download with Last-Modified timestamp
                ## requires wget https://www.gnu.org/software/wget/
                ## покращене завантаження з часовою міткою Last-Modified
                ## потребує wget https://www.gnu.org/software/wget/
                system(paste0("wget -S -U firefox ", file.url))
                from <- paste0(unit.download[[i]], tail.url)
                to   <- paste0("./menr_pa_data/messy_data_00/", unit.name[[i]], ".csv")
                file.rename(from, to)
        }
        print("Завантаження файлів завершено.")
}
