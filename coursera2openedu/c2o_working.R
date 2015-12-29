source("coursera2openedu.R")

folder <- "~/Documents/coursera_metrics/tests/"
fnames <- list.files(folder, full.names = TRUE)
for (filename in fnames) {
  CorrectIBtags(filename)
}

doc <- xmlTreeParse("~/Documents/coursera_metrics/tests/week_01_test_01_ibcorr.xml")

og <- OptionGroupSummary(doc)
version_card <- group_by(og, question_no, version_no, type, n_option_groups) %>%
       summarise(cardinality = prod(cardinality)) %>% ungroup()
version_card

# 
q <- GetVersion(doc, 12, 2)
qedu <- TransformNumeric(q)
qedu

# текущий план
# 1. понять как работать с картинками на openedu (размер по вертикали?)
# 2. сделать радио
# 3. сделать генератор множественных выборов

# не должно быть ; внутри img tag
# русские буквы при опции isHTML = TRUE
# в html img tag не закрывается, в xml закрывается
# http://www.w3schools.com/tags/tag_img.asp

q <- GetVersion(doc, 6, 1)

GetNumberOfOptionGroups(q)

