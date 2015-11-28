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
GetTitle(doc)

# obtain preamble of a test from coursera xml
GetPreamble = function(doc) {
  root <- XML::xmlRoot(doc)
  preamble <- XML::xmlValue(root[["preamble"]])
  return(preamble)
}

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
  return(root[["data"]][["question_groups"]][[question_no]])
}

q17 <- GetQuestion(doc, 17)




# get number of version of specific question
GetNumberOfVersions <- function(doc, question_no) {
  question <- GetQuestion(doc, question_no)
  n_versions <- xmlSize(question) - 1
  return(n_versions)
}

GetNumberOfVersions(doc, 17)

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

ChildNames(v_num)

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
GetText(v_num)

# get question-level explanation from a version 
GetQExplanation <- function(version) {
  question_explanation <- XML::xmlValue(version[["data"]][["explanation"]])
  return(question_explanation)
}
GetQExplanation(v_num)

# get answer from numeric question (we assume it's unique!!!)
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


# abandoned function. should make markdown questions
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




# create edx xml


CreateNumeric <- function(
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

# summary on questions
QuestionSummary <- function(doc) {
  n_questions <- GetNumberOfQuestions(doc)
  q_summary <- data_frame(question_no = 1:n_questions, n_versions = NA)
  for (i in 1:n_questions) {
    q_summary$n_versions[i] <- GetNumberOfVersions(doc, i)
  }
  return(q_summary)
}
QuestionSummary(doc)


GetNumberOfOptionGroups <- function(version) {
  n_option_groups <- XML::xmlSize(
    version[["data"]][["option_groups"]])
  return(n_option_groups)
}

# summary on versions of questions
VersionSummary <- function(doc) {
  q_summary <- QuestionSummary(doc)
  v_summary <- data_frame(
          question_no = rep(q_summary$question_no, 
                  times = q_summary$n_versions))
  v_summary <- v_summary %>% group_by(question_no) %>% 
    mutate(version_no = row_number())
  v_summary <- ungroup(v_summary) %>% 
    mutate(type = NA, n_option_groups = NA)
  tot_n_versions <- nrow(v_summary)
  
  for (i in 1:tot_n_versions) {
    version <- GetVersion(doc, 
                              v_summary$question_no[i],
                              v_summary$version_no[i])
    v_summary$type[i] <- GetType(version)
    v_summary$n_option_groups[i] <- GetNumberOfOptionGroups(version)
  }
  
  return(v_summary)
}

GetOptionGroup <- function(version, option_group_no) {
  option_group <- version[["data"]][["option_groups"]][[option_group_no]]
  return(option_group)
}

# summary on option groups
OptionGroupSummary <- function(doc) {
  v_summary <- VersionSummary(doc)
  tot_n_versions <- nrow(v_summary)
  indexes <- rep(1:tot_n_versions, v_summary$n_option_groups)
  og_summary <- v_summary[indexes, ]
  og_summary <- group_by(og_summary, question_no, version_no) %>%
    mutate(option_group_no = row_number()) %>% ungroup()
  og_summary <- mutate(og_summary, n_options = NA, select = NA)
  
  for (i in 1:nrow(og_summary)) {
    version <- GetVersion(doc, 
                          og_summary$question_no[i],
                          og_summary$version_no[i])
    option_group <- GetOptionGroup(version, 
                                   og_summary$option_group_no[i])
    og_summary$n_options[i] <- XML::xmlSize(option_group)
    og_summary$select[i] <- XML::xmlAttrs(option_group, "select")
  }
  
  return(og_summary)
}

GetTotalNumberOfOptions <- function(version) {
  n_option_groups <- GetNumberOfOptionGroups(version)
  tot_n_options <- 0
  for (i in 1:n_option_groups) {
    option_group <- GetOptionGroup(version, i)
    tot_n_options <- tot_n_options + XML::xmlSize(option_group)
  }
  return(tot_n_options)
}

GetTotalNumberOfOptions(v_radio)
GetTotalNumberOfOptions(v_num)
GetTotalNumberOfOptions(v_checkbox)

og <- OptionGroupSummary(doc)
v <- VersionSummary(doc)

# remove <br>, transform $$...$$ to \( ... \)
ddbr <- function(input_string) {
  output_string <- nobr(dd2brackets(input_string))
  return(output_string)
}


GetNumberOfOptionGroups(v_radio)


# 
TransformNumeric <- function(version) {
  text <- GetText(version) %>% ddbr()
  answer <- GetNumAnswer(version) %>% ddbr()
  question_explanation <- GetQExplanation(version) %>% ddbr()
  point_explanation <- NULL
  hints <- NULL
  
  # Check
  type <- GetType(version)
  n_og <- GetNumberOfOptionGroups(version)
  tot_n_options <- GetTotalNumberOfOptions(version)
  if (!type == "numeric") {
    warning("The question should be numeric, but type = ", type)
  }
  if (n_og > 1) {
    warning("The question should have one option group, but has = ", n_og)
  }
  if (tot_n_options > 1) {
    warning("The question should have one option in total, but has = ", tot_n_options)
  }
  
  
  
  edx_question <- CreateNumeric(text = text, answer = answer, 
      hints = NULL, question_explanation = question_explanation,
      point_explanation = point_explanation)
  return(edx_question)
}

TransformNumeric(v_num)

