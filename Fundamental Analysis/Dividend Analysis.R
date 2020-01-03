library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(RODBC)
library(lubridate)

LOCAL <- odbcConnect("LOCAL")

GetFinReport <- function(Url, Year, Season) {
    ResultAll <- tibble(r1 = list(), r2 = list(), r3 = list(), r4 = list(), r5 = list(), r6 = list(), year = integer(), season = integer())
    year <- Year
    season <- Season
    url <- Url
    for (y in Year) {
        for (s in Season) {
            postData <- paste0("encodeURIComponent=1&step=1&firstin=1&off=1&TYPEK=sii&year=", y, "&season=", s, "")
            result <- POST(url, body = postData)
            html <- result %>% content(as = "text") %>% read_html()
            result <- html %>% html_table()

            ResultOne <- tibble(r1 = result[2], r2 = result[3], r3 = result[4], r4 = result[5], r5 = result[6], r6 = result[7])
            ResultOne <- ResultOne %>% mutate(year = y, season = s)
            ResultAll <- rbind(ResultAll, ResultOne)

        }
        print(y)
        Sys.sleep(sample(2:5, size = 1));
    }
    return(ResultAll)
}
SelectColumn <- function(Df, Type, Category) {

    if (Type == "BS") {
        if (Category == 1) { Df <- Df %>% select("���q�N��", "�{���ά����{��", "�����ڶ��вb�B", "�겣�`�B", "�t���`�B", "�ѥ�", "�v�q�`�B", "�C�ѰѦҲb��") }
        if (Category == 2) { Df <- Df %>% select("���q�N��", "�y�ʸ겣", "�D�y�ʸ겣", "�겣�X�p", "�y�ʭt��", "�t�ŦX�p", "�ѥ�", "�v�q�X�p", "�C�ѰѦҲb��") }
        if (Category == 3) { Df <- Df %>% select("���q�N��", "�y�ʸ겣", "�D�y�ʸ겣", "�겣�`�B", "�y�ʭt��", "�t���`�B", "�ѥ�", "�v�q�`�B", "�C�ѰѦҲb��") }
        if (Category == 4) { Df <- Df %>% select("���q�N��", "�{���ά����{��", "�����ڶ��вb�B", "�겣�`�B", "�t���`�B", "�ѥ�", "�v�q�`�B", "�C�ѰѦҲb��") }
        if (Category == 5) { Df <- Df %>% select("���q�N��", "�{���ά����{��", "�����ڶ�", "�겣�`�B", "�t���`�B", "�ѥ�", "�v�q�`�B", "�C�ѰѦҲb��") }
        if (Category == 6) { Df <- Df %>% select("���q�N��", "�y�ʸ겣", "�D�y�ʸ겣", "�겣�`�B", "�y�ʭt��", "�t���`�B", "�ѥ�", "�v�q�`�B", "�C�ѰѦҲb��") }
        } else {
            if (Category == 1) { Df <- Df %>% select("���q�N��", "�Q���b���q", "�Q���H�~�b�l�q", "��~�O��", "�~����~��쥻���|��b�Q�]�b�l�^", "��L��X�l�q�]�|��^", "������X�l�q�`�B�]�|��^", "�򥻨C�Ѭվl�]���^") }
            if (Category == 2) { Df <- Df %>% select("���q�N��", "���q", "��X�ζO��", "��~�~�l�q", "������L��X�l�q�]�|��b�B�^", "������X�l�q�`�B", "�򥻨C�Ѭվl�]���^") }
            if (Category == 3) { Df <- Df %>% select("���q�N��", "��~���J", "��~����", "��~�O��", "��~�~���J�Τ�X", "�~����~��쥻���b�Q�]�b�l�^", "��L��X�l�q�]�b�B�^", "������X�l�q�`�B", "�򥻨C�Ѭվl�]���^") }
            if (Category == 4) { Df <- Df %>% select("���q�N��", "�Q���b���q", "�Q���H�~�b���q", "��~�O��", "�����|��b�Q�]�b�l�^", "������L��X�l�q�]�|��b�B�^", "������X�l�q�`�B", "�򥻨C�Ѭվl�]���^") }
            if (Category == 5) { Df <- Df %>% select("���q�N��", "��~���J", "��~����", "��~�O��", "��~�~���J�Τ�X", "�����b�Q�]�b�l�^", "��L��X�l�q�]�|��b�B�^", "������X�l�q�`�B", "�򥻨C�Ѭվl�]���^") }
            if (Category == 6) { Df <- Df %>% select("���q�N��", "���J", "��X", "�����b�Q�]�b�l�^", "��L��X�l�q", "������X�l�q�`�B", "�򥻨C�Ѭվl�]���^") }

        }


    return(Df)
}
SelectCommonColumn <- function(Df, Name) {
    Df %>% select(Name)
}
AddTime <- function(Df, Year, Season) {
    Df %>% mutate(year = Year, season = Season)
}
GetCommonColumn <- function(Df) {
    common <- Df[[1]] %>% names
    if (length(Df) > 1) {
        for (i in 2:(length(Df))) {
            if (is.null(Df[[i]])) next;
            common <- common %>% intersect(names(Df[[i]]))
        }
    }
    common
}

Rbind <- function(Df) {
    Result <- Df[[1]]
    if (length(Df) > 1) {
        for (i in 2:length(Df)) {
            Result <- rbind(Result, Df[[i]])
        }
    }
    Result
}
AggregateReport <- function(Df) {
    Df <- Df %>% GetCommonName
    Df <- Df %>% mutate(r1 = pmap(list(r1, r1name), SelectCommonColumn))
    Df <- Df %>% mutate(r2 = pmap(list(r2, r2name), SelectCommonColumn))
    Df <- Df %>% mutate(r3 = pmap(list(r3, r3name), SelectCommonColumn))
    Df <- Df %>% mutate(r4 = pmap(list(r4, r4name), SelectCommonColumn))
    Df <- Df %>% mutate(r5 = pmap(list(r5, r5name), SelectCommonColumn))
    Df <- Df %>% mutate(r6 = pmap(list(r6, r6name), SelectCommonColumn))

    Df <- Df %>% mutate(r1 = pmap(list(r1, year, season), AddTime))
    Df <- Df %>% mutate(r2 = pmap(list(r2, year, season), AddTime))
    Df <- Df %>% mutate(r3 = pmap(list(r3, year, season), AddTime))
    Df <- Df %>% mutate(r4 = pmap(list(r4, year, season), AddTime))
    Df <- Df %>% mutate(r5 = pmap(list(r5, year, season), AddTime))
    Df <- Df %>% mutate(r6 = pmap(list(r6, year, season), AddTime))
    Df <- Df %>% .[, 1:6]
    r1 <- Df$r1 %>% Rbind
    names(r1) <- names(r1) %>% gsub(pattern = "�]|�^", replacement = "")
    r2 <- Df$r2 %>% Rbind
    names(r2) <- names(r2) %>% gsub(pattern = "�]|�^", replacement = "")
    r3 <- Df$r3 %>% Rbind
    names(r3) <- names(r3) %>% gsub(pattern = "�]|�^", replacement = "")
    r4 <- Df$r4 %>% Rbind
    names(r4) <- names(r4) %>% gsub(pattern = "�]|�^", replacement = "")
    r5 <- Df$r5 %>% Rbind
    names(r5) <- names(r5) %>% gsub(pattern = "�]|�^", replacement = "")
    r6 <- Df$r6 %>% Rbind
    names(r6) <- names(r6) %>% gsub(pattern = "�]|�^", replacement = "")
    list(r1, r2, r3, r4, r5, r6)
}
SaveReport <- function(Df, Type) {
    if (Type == 'IS') {
        ISName <- c("IS1", "IS2", "IS3", "IS4", "IS5", "IS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = ISName[i], rownames = FALSE, append = TRUE)
        }
    } else {
        BSName <- c("BS1", "BS2", "BS3", "BS4", "BS5", "BS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = BSName[i], rownames = FALSE, append = TRUE)
        }
    }
}

GetCommonName <- function(Df) {
    ColumnName <- Df %>% map(~GetCommonColumn(.))
    namecolumn <- c("r1name", "r2name", "r3name", "r4name", "r5name", "r6name")
    for (i in 1:6) {
        Df[, namecolumn[i]] <- tibble(list(ColumnName[[i]]))
    }
    Df
}


GetNumericReport <- function(Report) {

    for (i in 1:nrow(Report)) {
        Report[i,] <- as.numeric(gsub(pattern = ",", replacement = "", Report[i,]))
    }
    return(Report)
}


#Income Statement
Sys.setlocale("LC_ALL", "C")
ISUrl <- "http://mops.twse.com.tw/mops/web/ajax_t163sb04"
ISALL <- GetFinReport(ISUrl, c(107:107), c(3))
IS107_3 <- ISALL %>% AggregateReport 
Sys.setlocale("LC_ALL", "cht")


saveRDS(IS106_3, file = "IS106_3.rds")

CleanData <- function(df) {
    df %>% map(~as.numeric(gsub(pattern = ",", replacement = "", .))) %>% as.tibble
}

IS106_3_1 <- IS106_3[[1]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3_2 <- IS106_3[[2]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3_3 <- IS106_3[[3]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3_4 <- IS106_3[[4]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3_5 <- IS106_3[[5]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3_6 <- IS106_3[[6]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_3 = �򥻨C�Ѭվl��)
IS106_3 <- rbind(IS106_3_1, IS106_3_2, IS106_3_3, IS106_3_4, IS106_3_5, IS106_3_6) %>% select(���q�N��, Profit106_3)

IS106_4_1 <- IS106_4[[1]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4_2 <- IS106_4[[2]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4_3 <- IS106_4[[3]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4_4 <- IS106_4[[4]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4_5 <- IS106_4[[5]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4_6 <- IS106_4[[6]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit106_4 = �򥻨C�Ѭվl��)
IS106_4 <- rbind(IS106_4_1, IS106_4_2, IS106_4_3, IS106_4_4, IS106_4_5, IS106_4_6) %>% select(���q�N��, Profit106_4)



IS107_3_1 <- IS107_3[[1]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3_2 <- IS107_3[[2]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3_3 <- IS107_3[[3]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3_4 <- IS107_3[[4]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3_5 <- IS107_3[[5]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3_6 <- IS107_3[[6]] %>% as.tibble %>% select("���q�N��", "�򥻨C�Ѭվl��", "year", "season") %>% CleanData %>% rename(Profit107_3 = �򥻨C�Ѭվl��)
IS107_3 <- rbind(IS107_3_1, IS107_3_2, IS107_3_3, IS107_3_4, IS107_3_5, IS107_3_6) %>% select(���q�N��, Profit107_3)

ISReport <- IS107_3 %>% left_join(IS106_4) %>% left_join(IS106_3)

ISReport %>% View

#Dividend

Dividend106 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend106.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
names(Dividend106) <- c("���q�N���W��", "��ƨӷ�", "���O", "���Ʒ|�Mĳ�q�L�ѧQ������", "�ѪF�|���", "���쥼���t�վlor���������l��", "�����b�Q�b�l��", "�i���t�վl��", "���t����������t�վl��", "�ѪF�t�os�վl���t���{���ѧQ����", "�ѪF�t�os�k�w�վl���nand�ꥻ���n�o�񤧲{������", "�ѪF�t�os�ѪF�t�o���{���ѧQ�`���B��", "�ѪF�t�os�վl��W��t�Ѥ���", "�ѪF�t�os�k�w�վl���nand�ꥻ���n��W��t�Ѥ���", "�ѪF�t�os�ѪF�t���`�Ѽƪ�", "���q�ѨC�ѭ��B")
Dividend106 <- Dividend106 %>% select(���q�N���W��, �ѪF�t�os�վl���t���{���ѧQ����)
GetCompany <- function(string) {
    strsplit(as.character(string), split = "-") %>% .[[1]] %>% .[1] %>% trimws

}

Dividend106 <- Dividend106 %>% mutate(���q�N�� = as.character(map(���q�N���W��, GetCompany))) %>% select(-���q�N���W��)
Dividend106 <- Dividend106 %>% mutate(�ѪF�t�os�վl���t���{���ѧQ���� = �ѪF�t�os�վl���t���{���ѧQ���� %>% as.character %>% as.numeric)
Dividend106 <- Dividend106 %>% filter(!is.na(�ѪF�t�os�վl���t���{���ѧQ����))

#Dividend Predict

DividendPredict<-ISReport %>% mutate(���q�N�� = as.character(���q�N��)) %>% left_join(Dividend106)
DividendPredict <- DividendPredict %>% mutate(Profit107_4_P = Profit107_3 * Profit106_4 / Profit106_3, DivideRatio = �ѪF�t�os�վl���t���{���ѧQ���� / Profit106_4)
DividendPredict <- DividendPredict %>% mutate(DividendPredict = Profit107_4_P * DivideRatio)



#Price
Price <- readRDS(file = "Price201902.rds")
Price<-Price %>% unnest %>% group_by(���q�N��) %>% filter(��� == max(���)) %>% select(���q�N��, ���L��)


#All Report

InterestRateReport <- DividendPredict %>% left_join(Price) %>% mutate(InterestRatePercent = DividendPredict / ���L��)

InterestRateReport %<>% mutate(InterestRateOld = �ѪF�t�os�վl���t���{���ѧQ���� / ���L��) %>% arrange(desc(InterestRatePercent))

saveRDS(InterestRateReport, file = "InterestRateReport.rds")

readRDS(file = "InterestRateReport.rds")