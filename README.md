# R
Sample R Code

## Research Question
The New York City Department of Education (NYCDOE) implemented the Renewal School Program in 2014-2019 to improve academic achievements of low-performing schools.
The Renewal Schools Program started on the Fall semester of 2015, with 94 participating schools and ended with fewer schools because some of them graduated through the Rise program, others were closed, and merged. Funded with $773 million, the program was forced to shut down in 2019 after receiving sharp criticism for its high costs and low effectiveness.
The analysis explores whether the Renewal School Program has accounted for any difference in academic achievements between the Renewal Schools and other comparable schools in
NYC, and hence whether the shut-down of the program was justified. The effectiveness of the program will be measured by the program’s impact on the Math and English scores between treated and a comparable groups of non-treated schools before and after the implementation of the program.

## The Renewal Schools Program
### Selection Criteria
The NYCDOE has selected as Renewal Schools those schools that met all three of the following criteria:
1. Were Identified as Priority or Focus Schools by the State Department of Education
– Priority: The bottom 5% lowest-performing schools statewide
– Focus: The bottom 10% of progress in a subgroup
2. Demonstrated low academic achievement for each of the three prior years (2012-2014):
– Elementary and middle schools in the bottom 25% in Math and ELA scores
– High schools in the bottom 25% in four-year graduation rate
3. Scored “Proficient” or below on their most recent quality review
Additionally, NYCDOE website at the time when the program took place also mentioned “others that were added per the Chancellor’s discretion”.

### The Intervention
The treatment the schools in the Renewal Program received included:
• Tailored whole-student support, including mental health services and after-school programs;
• Extended learning time - an extra hour added to the school day to give all students additional instructional time;
• Supply of resources and support to ensure effective school leadership and rigorous instruction with collaborative teachers;
• Enhanced oversight and accountability including strict goals and clear consequences for schools that did not meet them;

## About the Data
### Quality Review Data
The Quality Review Data were collected by the New York City Public Schools in 2014-2020 through formal school visits, feedback from students, teachers, and parents from the NYC School Survey, and various student achievement measures.

Variables of interest:
• Information on the Schools: School, type, district, borough, Quality Rating (Underdeveloped,
Developing, Proficient, Well-developed)
• Learning Outcomes: average Math and English scores, graduation rates
• Students Demographics: % black, % hispanic, % English Language Learners, % with Disabilities
• Socio-Economic Status: Economic Need Index, % Temp. Housing, % Eligible HRA

The data sources were the webpages of the NYC Public Schools Infohub and NYC Open Data.

### Renewal Schools list:

Since the website for the Department of Education no longer has information on the program, a chached version of the Department of Education’s website (though a website called Way Back Machine) was used in order to extract a list, year by year, of the participating schools in the Renewal Program.
The data was entered into CSV spreadsheets for cleaning and manipulation in R. 

Variables of Interest: DBN (school code composed on the District, Borough and unique school Number), School Name and year in which they were participating the Renewal School Program.

## Analysis

The analysis encompass:
- Descriptive Statistics
- Data visualisation
- Regressions: difference-in-differences and fixed effects model

