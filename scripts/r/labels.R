################################################################################
##
## [ PROJ ] Postratify Helios survey responses
## [ FILE ] labels.R
## [ AUTH ] Justin Ortagus, Benjamin Skinner, Melvin Tanner
## [ INIT ] 13 February 2020
##
################################################################################

## -----------------------------------------------
## labels
## -----------------------------------------------

add_qlabs <- function(q) {

    ## Convert short question names to their full names for plotting.

    case_when(
        q == "q1" ~ "Consent",
        q == "q2" ~ "Primary goal at initial enrollment",
        q == "q3_1" ~ "Cost: textbooks",
        q == "q3_2" ~ "Cost: tuition and fees",
        q == "q3_3" ~ "Cost: computer and internet access",
        q == "q3_4" ~ "Cost: transportation to campus",
        q == "q3_5" ~ "Cost: living expenses (rent, utilities, healthcare, childcare, food)",
        q == "q4" ~ "Employed while taking classes?",
        q == "q5_1" ~ "Employment: lost job",
        q == "q5_2" ~ "Employment: changed careers",
        q == "q5_3" ~ "Employment: switched from part-time to full-time work",
        q == "q5_4" ~ "Employment: got second job",
        q == "q5_5" ~ "Employment: got a promotion",
        q == "q5_1q4" ~ "Employment: lost job",
        q == "q5_2q4" ~ "Employment: changed careers",
        q == "q5_3q4" ~ "Employment: switched from part-time to full-time work",
        q == "q5_4q4" ~ "Employment: got second job",
        q == "q5_5q4" ~ "Employment: got a promotion",
        q == "q6_1" ~ "Cost: difficulty completing financial aid application",
        q == "q6_2" ~ "Cost: no longer eligible for financial aid",
        q == "q6_3" ~ "Cost: scholarship ran out",
        q == "q6_4" ~ "Cost: missed payment deadline and was dropped",
        q == "q6_6" ~ "Cost: hard to access financial aid information online",
        q == "q6_5" ~ "Cost: employer stopped paying for classes",
        q == "q6_5q4" ~ "Cost: employer stopped paying for classes",
        q == "q7_1" ~ "Instructional: registration hold",
        q == "q7_2" ~ "Instructional: didn't know which classes to take next",
        q == "q7_3" ~ "Instructional: needed course unavailable online",
        q == "q7_4" ~ "Instructional: needed course was full",
        q == "q7_5" ~ "Instructional: needed course was only online",
        q == "q8_1" ~ "Other: moved out of the area",
        q == "q8_2" ~ "Other: inconsistent weekly schedule",
        q == "q8_3" ~ "Other: desired classes not at closest campus",
        q == "q8_4" ~ "Other: transportation/parking was difficult",
        q == "q8_5" ~ "Other: job was too far from campus",
        q == "q9_1" ~ "Instructional: no time to study/prepare for class",
        q == "q9_2" ~ "Instructional: struggled completing assignments",
        q == "q9_3" ~ "Instructional: required math and science courses too difficult",
        q == "q9_4" ~ "Instructional: too many required developmental/remedial courses",
        q == "q9_5" ~ "Instructional: instructor couldn't meet outside of class",
        q == "q10" ~ "Did you take any online courses?",
        q == "q11_1" ~ "Instructional: not enough interaction with online instructor",
        q == "q11_2" ~ "Instructional: unreliable internet access",
        q == "q11_3" ~ "Instructional: not enough interaction with students in online setting",
        q == "q11_4" ~ "Instructional: difficulty learning on own in online setting",
        q == "q11_5" ~ "Instructional: struggled with online course software",
        q == "q11_1q10" ~ "Instructional: not enough interaction with online instructor",
        q == "q11_2q10" ~ "Instructional: unreliable internet access",
        q == "q11_3q10" ~ "Instructional: not enough interaction with students in online setting",
        q == "q11_4q10" ~ "Instructional: difficulty learning on own in online setting",
        q == "q11_5q10" ~ "Instructional: struggled with online course software",
        q == "q12_1" ~ "Other: change in relationship status",
        q == "q12_2" ~ "Other: health emergency",
        q == "q12_3" ~ "Other: did not feel welcome on campus",
        q == "q12_4" ~ "Other: did not have reliable childcare",
        q == "q12_5" ~ "Other: did not have many friends at the college",
        q == "q13" ~ "Are there additional reasons for leaving",
        q == "q14" ~ "Description of additional reasons (open response text)",
        q == "q15" ~ "Which describes your current academic status?",
        q == "q16_1" ~ "What would help you return?: remind me of deadlines",
        q == "q16_2" ~ "What would help you return?: simplify registration and enrollment process",
        q == "q16_3" ~ "What would help you return?: give me guidance on which courses to take",
        q == "q16_4" ~ "What would help you return?: provide a greater level of financial assistance",
        q == "q16_5" ~ "What would help you return?: more options for course times and locations",
        q == "q17" ~ "What is your primary goal now that you've returned to college?",
        q == "q18_1" ~ "Reason for return: college advisor helped me decide which courses to take",
        q == "q18_2" ~ "Reason for return: text message encouraging me to return",
        q == "q18_3" ~ "Reason for return: website made the registration process easier",
        q == "q18_4" ~ "Reason for return: received additional financial aid",
        q == "q18_5" ~ "Reason for return: already planned to return",
        q == "q19_1" ~ "Willing to be interviewed for follow up? Yes, send email",
        q == "q19_2" ~ "Willing to be interviewed for follow up? Yes, call me",
        q == "q19_3" ~ "Willing to be interviewed for follow up? No")
}

add_qcats <- function(q) {

    ## Add question categories for grouping

    case_when(
        str_detect(q, "q3_") ~ "Cost",
        str_detect(q, "q5_") ~ "Employment",
        str_detect(q, "q6_") ~ "Cost",
        str_detect(q, "q7_") ~ "Instructional",
        str_detect(q, "q8_") ~ "Other",
        str_detect(q, "q9_") ~ "Instructional",
        str_detect(q, "q11_") ~ "Instructional",
        str_detect(q, "q12_") ~ "Other",
        str_detect(q, "q16_") ~ "What would help you return?",
        str_detect(q, "q18_") ~ "Reason for return",
        str_detect(q, "q19_") ~ "Willing to be interviewed for follow up?")
}

add_age_cats <- function(x) {
    factor(x, 1:4, c("18-25 year olds",
                     "26-35 year olds",
                     "36-49 year olds",
                     "50+ year olds"))
}

add_age_cats_short <- function(x) {
    factor(x, 1:4, c("18-25",
                     "26-35",
                     "36-49",
                     "50+"))
}

add_gen_cats <- function(x) {
    factor(x, 1:3, c("Men",
                     "Women",
                     "(Missing)"))
}

add_rac_cats <- function(x) {
    factor(x, 1:6, c("Black students",
                     "Hispanic students",
                     "Students of more than one racial/ethnic group",
                     "Students in other racial/ethnic group",
                     "White students",
                     "(Missing)"))
}

add_rac_cats_short <- function(x) {
    factor(x, 1:6, c("Black",
                     "Hispanic",
                     "More than one racial/ethnic group",
                     "Other racial/ethnic group",
                     "White",
                     "(Missing)"))
}

add_gpa_cats <- function(x) {
    factor(x, 1:6, c("[2.0-2.3) GPA",
                     "[2.3-2.7) GPA",
                     "[2.7-3.0) GPA",
                     "[3.0-3.3) GPA",
                     "[3.3-3.7) GPA",
                     "[3.7-4.0) GPA"))
}

add_gpa_cats_short <- function(x) {
    factor(x, 1:6, c("[2.0-2.3)",
                     "[2.3-2.7)",
                     "[2.7-3.0)",
                     "[3.0-3.3)",
                     "[3.3-3.7)",
                     "[3.7-4.0)"))
}

add_hrs_cats <- function(x) {
    factor(x, 1:6, c("Students with 30-35 earned hours",
                     "Students with 36-41 earned hours",
                     "Students with 42-47 earned hours",
                     "Students with 48-53 earned hours",
                     "Students with 54-59 earned hours",
                     "Students with 60+ earned hours"))
}

add_hrs_cats_short <- function(x) {
    factor(x, 1:6, c("30-35",
                     "36-41",
                     "42-47",
                     "48-53",
                     "54-59",
                     "60+"))

}


add_cats_list <- list("add_age_cats",
                      "add_gen_cats",
                      "add_rac_cats",
                      "add_gpa_cats",
                      "add_hrs_cats")

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
