# read coursera test xml and convert it to markdown openedu
library("XML")
library("stringr")
library("dplyr")

doc <- xmlTreeParse("~/Downloads/econometrics-001/01__1/test_01.xml")
# r <- xmlRoot(doc)

# obtain preamble of a test from coursera xml
GetTitle = function(doc) {
  root <- XML::xmlRoot(doc)
  test_name <- XML::xmlValue(
    root[["metadata"]][["title"]][[1]])
  return(test_name)
}

# obtain preamble of a test from coursera xml
GetPreamble = function(doc) {
  root <- XML::xmlRoot(doc)
  preamble <- XML::xmlValue(root[["preamble"]])
  return(preamble)
}
GetTitle(doc)
GetPreamble(doc)


# obtain number of a questions from coursera xml
GetNumberOfQuestions <- function(doc) {
  root <- XML::xmlRoot(doc)
  number_of_questions <- 
    XML::xmlSize(root[["data"]][["question_groups"]])
  return(number_of_questions)  
}

GetNumberOfQuestions(doc)

# obtain all versions of a question from coursera xml
GetQuestion <- function(doc, question_no) {
  root <- XML::xmlRoot(doc)
  number_of_questions <- GetNumberOfQuestions(doc)
  if (question_no > number_of_questions) {
    stop("Question number ", question_no, " requested, but total number of question is ",
         number_of_questions)
  }
  return(root[[3]][[1]][[question_no]])
}

q17 <- GetQuestion(doc, 17)


# obtain specific version of a question from coursera xml
GetVersion <- function(doc, question_no, version_no) {
  question <- GetQuestion(doc, question_no)
  version <- question[[1 + version_no]]
  return(version)
}


# get child names of an xml node
ChildNames <- function(node) {
  return(XML::xmlSApply(node, XML::xmlName))
}


v_num <- GetVersion(doc, 10, 1)
v_checkbox <- GetVersion(doc, 4, 1)
v_radio <- GetVersion(doc, 3, 1)

# get type of a version (numeric/checkbox/radio)
GetType <- function(version) {
  meta <- version[["metadata"]][["parameters"]]

  type <- xmlValue(meta[["type"]])
  choice_type <- xmlValue(meta[["choice_type"]])
  
  if (is.na(type)) type <- choice_type
  return(type)
}
GetType(v_num)

# get text from version 
GetText <- function(version) {
  text <- XML::xmlValue(version[["data"]][["text"]])
  return(text)
}

# get question-level explanation from a version 
GetQExplanation <- function(version) {
  question_explanation <- XML::xmlValue(version[["data"]][["explanation"]])
  return(question_explanation)
}

# get answer from numeric question
GetNumAnswer <- function(version) {
  answer <- XML::xmlValue(version[["data"]][["option_groups"]][["option_group"]][["option"]][["text"]])
  return(answer)
}

# get point explanation from numeric question
GetNumPointExplanation <- function(version) {
  point_explanation <- xmlValue(version[["data"]][["option_groups"]][["option_group"]][["option"]][["explanation"]])
  return(point_explanation)
}

# remove <br> tags (edx does not support them)
# replace each <br> with <p> ... </p>
nobr <- function(output_string) {
  output_string <- paste0("<p>", output_string, "</p>")
  output_string <- str_replace_all(output_string, "<br>", "</p>\n<p>")
  return(output_string)
}

ChildNames(v_num)
v_num[["data"]] %>% ChildNames()

question_explanation <- GetQExplanation(v_num)
answer <- GetNumAnswer(v_num)
point_explanation <- GetNumPointExplanation(v_num)



v_checkbox[["data"]]
v_radio[["data"]]


GetText(v_num)

Version2edx <- function(version) {
  text <- GetText(version) %>% dd2brackets()
  output <- paste0(">>", text, "<<\n")
  
  return(output)
}
cat(Version2edx(v_num))
a <- Version2edx(v_num)
a

# this function converts inline equations 
# $$x^2$$ to \(x^2\)
dd2brackets <- function(output_string) {
  bracket_pairs <- str_count(output_string, pattern = "[$][$]") / 2
  for (index in 1:bracket_pairs) {
    output_string <- str_replace(output_string, 
      pattern = "[$][$]", replacement = "\\\\(")
    output_string <- str_replace(output_string, 
      pattern = "[$][$]", replacement = "\\\\)")
  }
  return(output_string)
}
cat(dd2brackets(a))


edx_xml <- xmlNode("problem")
edx_xml
?xmlNode

# create edx xml


create_numeric <- function(
  text = "Кому на Руси жить хорошо?",
  point_explanation = "here is the hint",
  answer = 4,
  question_explanation = "Тому, кто досмерти работает, до полусмерти пьет",
  hints = c("hint раз", "hint 2")) {
  
  response_node <- xmlNode("numericalresponse", 
                           attrs = c(answer = answer),
                           xmlNode("formulaequationinput"),
                           xmlNode("correcthint", point_explanation))
  
  solution_node <- xmlNode("solution", 
                           xmlNode("div", attrs = c(class="detailed-solution"), 
                                   question_explanation))
  
  
  problem_node <- xmlNode("problem", text, 
                          response_node, solution_node)
  
  if (!is.null(hints)) {
    demandhint_node <- xmlNode("demandhint")
    for (i in 1:length(hints)) {
      demandhint_node <- addChildren(demandhint_node, 
                                     xmlNode("hint", hints[i]))
    }
    problem_node <- addChildren(problem_node, demandhint_node)
  }
  return(problem_node)
}
