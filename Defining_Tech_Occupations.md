Defining Canadian Tech Occupations <img src="biie-logo-web-500width.png" align="center"/>
================
<i><b>Viet Vu and Asher Zafar</b></i>

-   [Purpose](#purpose)
-   [Inputs](#inputs)
-   [Definition Source Code](#definition-source-code)
    -   [Load required R libraries.](#load-required-r-libraries.)
    -   [Process and Load O\*NET and NOC data](#process-and-load-onet-and-noc-data)
    -   [Generate Rankings for NOCs based on O\*NET Skills](#generate-rankings-for-nocs-based-on-onet-skills)
-   [Definition File Field List](#definition-file-field-list)
-   [Definition Occupation List](#definition-occupation-list)
-   [PCA-based Validation](#pca-based-validation)

Purpose
-------

This document accompanies the Brookfield Institute for Innovation + Entrepeneurship's (BII+E) 2018 [State of Canada's Tech Workers report](https://brookfieldinstitute.ca). In line with our open, iterative approach to research, this document provides a walkthrough of our approach to defining tech occupations in Canada, including source code in the R programming language (see the .Rmd file in this repository), so that others can reproduce our work, build upon it, or suggest changes. This document should be used in conjunction with the technical appendix from the report that explains the methodology more conceptually.

This code and all other items in this repository are available under the [*MIT License*](https://en.wikipedia.org/wiki/MIT_License). In a nutshell, use it freely, but please cite our work. We welcome pull requests and forks. If you'd like to discuss this work, feel free to contact [Viet](https://brookfieldinstitute.ca/team/viet-vu/) or [Asher](https://brookfieldinstitute.ca/team/asher-zafar/) directly.

Inputs
------

As described in the methodology sections of our report, we use two key data sources to define Canada's tech occupations. The first set of data on occupations is from Employment and Social Development Canada's (ESDC) National Occupational Classification (NOC) and associated linked data from Statistics Canada (StatCan). The second set of data on skills is from the US Occupational Information Network (O\*NET). BII+E developed a crosswalk table to link these data sources, also available in this repository.

Definition Source Code
----------------------

### Load required R libraries.

``` r
library(data.table)
library(stringr)
library(psych)
library(knitr)
library(tidyverse)
library(readxl)
```

### Process and Load O\*NET and NOC data

This code chunk reads in the O\*NET data from R objects, combines it into a workable format for our analysis, and joins it with the NOC codes using the crosswalk.

``` r
# Download O*NET data from https://www.onetcenter.org/database.html?p=2. Other formats available
download.file("https://www.onetcenter.org/dl_files/database/db_23_0_excel/Knowledge.xlsx", "knowledge.xlsx", mode = "wb")
download.file("https://www.onetcenter.org/dl_files/database/db_23_0_excel/Skills.xlsx", "skills.xlsx", mode = "wb")
download.file("https://www.onetcenter.org/dl_files/database/db_23_0_excel/Work%20Activities.xlsx", "workactivities.xlsx", mode = "wb")

# Read in O*NET data and BII+E crosswalk
knowledge <- as.data.table(read_excel("knowledge.xlsx"))
skill <- as.data.table(read_excel("skills.xlsx"))
work.activity <- as.data.table(read_excel("workactivities.xlsx"))
crosswalk <- as.data.table(read_csv("ONET-NOC_Crosswalk.csv"))

# Align O*NET skills, knowledge and work activity data
full.skill <- rbindlist(list(knowledge,skill,work.activity))
names(full.skill) <- c("onet","title","element.id","element.name","scale.id","scale.name","value","N","stder","lci","uci","sup","nr","date","source")
full.skill[,c("N","date","source","lci","uci"):=NULL]

# Link O*NET data to NOC using crosswalk
setkey(crosswalk,onet)
setkey(full.skill,onet)
full.crosswalk.skill <- crosswalk[full.skill,nomatch=0]
full.avg.crosswalk.skill <- full.crosswalk.skill[,mean(value),by=.(noc_title,element.id,element.name,scale.id,scale.name)]

#Clean environment
rm(full.crosswalk.skill) #Remove redundancies
rm(full.skill)
rm(work.activity,skill,knowledge)
```

### Generate Rankings for NOCs based on O\*NET Skills

This chunk of code selects the technology skills of interest, computes our ranking of technology and digital occupations, and produces our final list of digital and high-tech occupations, as outlined in methodology for the report.

``` r
# Select the technology skills of interest for each NOC
tech.skills <- c("2.B.3.b", "2.B.3.e", "2.C.3.a", "4.A.3.b.1", "2.C.3.b", "2.C.9.a")
individual.ranking <- full.avg.crosswalk.skill[element.id %in% tech.skills,prod(V1),by=.(noc_title,element.id)]
individual.ranking <- reshape(individual.ranking,direction="wide",v.names = c("V1"),timevar="element.id",idvar="noc_title")
individual.ranking <- individual.ranking[!is.na(noc_title)] #One O*NET occupation was not matched to NOC in the crosswalk and must be removed
setkey(individual.ranking,noc_title)

# Rank each NOC across each of the selected tech skills
for(n in tech.skills){
  individual.ranking[,str_c("rank.",n):=frankv(get(str_c("V1.",n)),order=-1)]
}

# Calculate the harmonic means for each NOC under three ranking systems

## Rankings across all tech skills
for(n in seq(1,483)){
  individual.ranking[n,harm.rank:=harmonic.mean(c(get("rank.2.B.3.b")+1,rank.2.B.3.e+1,
                                                  rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.3.b+1,rank.2.C.9.a+1))]
}

## Rankings across digital skills - used to distinguish between digital and high-tech occupations
for(n in seq(1,483)){
  individual.ranking[n,harm.rank.digital:=harmonic.mean(c(rank.2.B.3.e+1,
                                                          rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.9.a+1))]
}

## Rankings excluding knowledge of telecommunications
### Used for sensititivity analysis in report, but not for findings
for(n in seq(1,483)){
  individual.ranking[n,harm.rank.no.tel:=harmonic.mean(c(rank.2.B.3.b+1,rank.2.B.3.e+1,
                                                         rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.3.b+1))]
}

# Select tech, high-tech, and digital occupations based on cut-offs from analyzing rankings

tech.cut.off <- 25 #Define the tech cut off rank
digital.cut.off <- 17 #Define digital cut-off rank

individual.ranking[,tech:=0]
individual.ranking[harm.rank < tech.cut.off, tech:=1]
individual.ranking[harm.rank < tech.cut.off, digital:= "High-Tech"]
individual.ranking[harm.rank < tech.cut.off & harm.rank.digital < digital.cut.off, digital:= "Digital"]
individual.ranking[harm.rank.no.tel < tech.cut.off,tech.no.tel:=1]

# Rename fields to be easier to understand prior to output
individual.ranking <- individual.ranking %>%
  rename("2.C.3.a - Comp and Elec" = "V1.2.C.3.a",
         "2.C.3.b - Eng and Tech" = "V1.2.C.3.b",
         "2.C.9.a - Telco" = "V1.2.C.9.a" ,
         "2.B.3.b - Tech Design" = "V1.2.B.3.b",
         "2.B.3.e - Programming" = "V1.2.B.3.e" ,
         "4.A.3.b.1 - Inter w Comp" =  "V1.4.A.3.b.1",
         "Rank - Comp and Elec" = "rank.2.C.3.a",
         "Rank - Eng and Tech" = "rank.2.C.3.b",
         "Rank - Telco" = "rank.2.C.9.a",
         "Rank - Tech Design" = "rank.2.B.3.b",
         "Rank - Programming" = "rank.2.B.3.e",
         "Rank - Inter w Comp" = "rank.4.A.3.b.1"
         )

#Create a NOC code and name field for convenience
individual.ranking$noc_code <- substr(individual.ranking$noc_title, 1, 4)
individual.ranking$noc_name <- substr(individual.ranking$noc_title, 5, nchar(individual.ranking$noc_title))

#Write the CSV file containing the rankings
write.csv(individual.ranking,"tech.sector.def.csv",row.names=FALSE)

#Clean up the environment
rm(crosswalk, digital.cut.off, n, tech.cut.off, tech.skills)
```

Definition File Field List
--------------------------

The full list of scores and rankings can be seen in the *tech.sector.def.csv* file that is included in this repo and output by the corresponding R Markdown file. The data fields in this file are described as follows:

<table style="width:89%;">
<colgroup>
<col width="25%" />
<col width="63%" />
</colgroup>
<thead>
<tr class="header">
<th>Field</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><strong>noc_title</strong></td>
<td>The four-digit NOC code and name for the occupation.</td>
</tr>
<tr class="even">
<td><strong>2.C.3.a - Comp and Elec</strong></td>
<td>BII+E score based on O*NET data for &quot;Knowledge: Computers and Electronics&quot;.</td>
</tr>
<tr class="odd">
<td><strong>2.C.3.b - Eng and Tech</strong></td>
<td>BII+E score based on O*NET data for &quot;Knowledge: Engineering and Technology&quot;.</td>
</tr>
<tr class="even">
<td><strong>2.C.9.a - Telco</strong></td>
<td>BII+E score based on O*NET data for &quot;Knowledge: Telecommunications&quot;.</td>
</tr>
<tr class="odd">
<td><strong>2.B.3.b - Tech Design</strong></td>
<td>BII+E score based on O*NET data for &quot;Skills: Technology Design&quot;.</td>
</tr>
<tr class="even">
<td><strong>2.B.3.e - Programming</strong></td>
<td>BII+E score based on O*NET data for &quot;Skills: Programming&quot;.</td>
</tr>
<tr class="odd">
<td><strong>4.A.3.b.1 - Inter w Comp</strong></td>
<td>BII+E score based on O*NET data for &quot;Work Activities: Interacting with Computers&quot;.</td>
</tr>
<tr class="even">
<td><strong>Rank - X</strong></td>
<td>Six fields which correspond to the rank of the occupation in each skill among all occupations.</td>
</tr>
<tr class="odd">
<td><strong>harm.rank</strong></td>
<td>The harmonic rank of the occupation across all six skills.</td>
</tr>
<tr class="even">
<td><strong>harm.rank.digital</strong></td>
<td>The harmonic rank of the occupation across only four digital skills.</td>
</tr>
<tr class="odd">
<td><strong>tech</strong></td>
<td>Indicates whether the occupation was included as a tech occupation.</td>
</tr>
<tr class="even">
<td><strong>digital</strong></td>
<td>Indicates whether the occupation was considered a digital occupation, a high-tech occupation, or not a tech occupation.</td>
</tr>
<tr class="odd">
<td><strong>tech.no.tel</strong></td>
<td>Indicates whether the occupation would be considered tech under an alternate ranking system which did not include knowledge of telecommunications.</td>
</tr>
<tr class="even">
<td><strong>noc_code</strong></td>
<td>The four-digit NOC code split into a field for convenience.</td>
</tr>
<tr class="odd">
<td><strong>noc_name</strong></td>
<td>The name of the occupation for the code split into a field for convenience.</td>
</tr>
</tbody>
</table>

Definition Occupation List
--------------------------

A truncated list of included occupational categories is reproduced below, including an alternative ranking system which excludes telecommunications knowledge which was used for a sensitivity analysis, but not in the report's findings.

| Occupation                                                                  | Tech Definition | Tech Def w/o Telco |
|:----------------------------------------------------------------------------|:----------------|:-------------------|
| 2174 Computer programmers and interactive media developers                  | Digital         | Remain             |
| 2173 Software engineers and designers                                       | Digital         | Remain             |
| 2147 Computer engineers (except software engineers and designers)           | Digital         | Remain             |
| 2172 Database analysts and data administrators                              | Digital         | Remain             |
| 0131 Telecommunication carriers managers                                    | Digital         | Remain             |
| 2281 Computer network technicians                                           | Digital         | Remain             |
| 2283 Information systems testing technicians                                | Digital         | Remain             |
| 2175 Web designers and developers                                           | Digital         | Remain             |
| 2133 Electrical and electronics engineers                                   | Digital         | Remain             |
| 2282 User support technicians                                               | Digital         | Remain             |
| 0213 Computer and information systems managers                              | Digital         | Remain             |
| 2171 Information systems analysts and consultants                           | Digital         | Remain             |
| 2255 Technical occupations in geomatics and meteorology                     | Digital         | Remain             |
| 2161 Mathematicians, statisticians and actuaries                            | Digital         | Remain             |
| 7245 Telecommunications line and cable workers                              | Digital         | Exclude            |
| 5224 Broadcast technicians                                                  | Digital         | Exclude            |
| 7246 Telecommunications installation and repair workers                     | Digital         | Exclude            |
| 2132 Mechanical engineers                                                   | High-Tech       | Remain             |
| 2146 Aerospace engineers                                                    | High-Tech       | Remain             |
| 0211 Engineering managers                                                   | High-Tech       | Remain             |
| 2134 Chemical engineers                                                     | High-Tech       | Remain             |
| 2131 Civil engineers                                                        | High-Tech       | Remain             |
| 2252 Industrial designers                                                   | High-Tech       | Remain             |
| 2143 Mining engineers                                                       | High-Tech       | Remain             |
| 2144 Geological engineers                                                   | High-Tech       | Remain             |
| 2241 Electrical and electronics engineering technologists and technicians   | High-Tech       | Remain             |
| 2142 Metallurgical and materials engineers                                  | High-Tech       | Remain             |
| 2148 Other professional engineers, n.e.c.                                   | High-Tech       | Remain             |
| 2145 Petroleum engineers                                                    | High-Tech       | Remain             |
| 2111 Physicists and astronomers                                             | High-Tech       | Remain             |
| 2233 Industrial engineering and manufacturing technologists and technicians | High-Tech       | Remain             |
| 7247 Cable television service and maintenance technicians                   | High-Tech       | Exclude            |
| 1254 Statistical officers and related research support occupations          | NA              | Add                |
| 7232 Tool and die makers                                                    | NA              | Add                |

PCA-based Validation
--------------------

To empirically assess our method, we sought to understand how O\*NET skills covary across occupations without subjectively selecting the skills ourselves. We conducted the following analysis based on principal component analysis (PCA) to see which skills tended to covary. Our report contains more conceptual information on PCA and our approach. This script also generates two open data files which are located in this repository and available for use:

-   **"PCA\_skill\_loads.csv"** contains the PCA loadings for each skill and the first five PCs (this can be increased if desired). This file was the basis for our validation analysis.
-   **"PCA\_occ\_scores.csv"** contains the composite scores for each PC and occupation. We did not use this file in our analysis, but thought it could be of use to others.

``` r
#Load and process skill and occupation data
onet.s <- full.avg.crosswalk.skill
onet.s$element<- paste(substr(onet.s$element.id, 1, 5), onet.s$element.name)

onet.s <- onet.s %>%
  select(noc_title, element, scale.name, V1) %>%
  filter(!is.na(noc_title)) %>%
  spread(scale.name, V1) %>%
  mutate(score = Importance * Level) %>% # Multiplied importance by level  of skill per O*NET recommendations
  select(noc_title, element, score) %>%
  spread(element, score) %>%
  remove_rownames %>% 
  column_to_rownames(var="noc_title")


#Create principal components
onet.s.pca <- prcomp(onet.s, center=TRUE, scale.=TRUE, rank. = 5) # The last argument retains the first 5 PCs. This can be changed if you desire.

#Analyze principal component skill loadings
onet.loading <- as.data.frame(onet.s.pca$rotation) %>% rownames_to_column(var = "Skill") %>% arrange(desc(PC3))
write.csv(as.data.frame(onet.s.pca$rotation) %>% arrange(PC3), "PCA_skill_loads.csv")

#Analyze occupational scores
onet.pca <- as.data.frame(predict(onet.s.pca, newdata = onet.s))
write.csv(onet.s.pca$rotation, "PCA_occ_scores.csv")
```

From our PC loadings, we found that PC3's heavily weighted skills aligned very closely to our selected skills, and it accounted for a significant amount of variance in the O\*NET data. The following skills were most heavily weighted on one end of PC3 (the other end heavily weighted skills such as caring for others and coaching):

    ## Importance of first k=5 (out of 109) components:
    ##                           PC1    PC2    PC3     PC4     PC5
    ## Standard deviation     6.4489 3.9767 2.7742 2.45908 1.83926
    ## Proportion of Variance 0.3816 0.1451 0.0706 0.05548 0.03104
    ## Cumulative Proportion  0.3816 0.5266 0.5972 0.65272 0.68375

| Skill                               |    PC1|    PC2|   PC3|    PC4|    PC5|
|:------------------------------------|------:|------:|-----:|------:|------:|
| 2.B.3 Programming                   |  -0.05|   0.03|  0.23|   0.00|   0.03|
| 4.A.3 Interacting With Computers    |  -0.11|  -0.01|  0.20|   0.00|  -0.01|
| 2.C.3 Computers and Electronics     |  -0.09|   0.03|  0.20|   0.00|   0.10|
| 4.A.2 Processing Information        |  -0.11|   0.03|  0.17|  -0.08|  -0.16|
| 2.C.4 Mathematics                   |  -0.07|   0.11|  0.17|   0.05|  -0.08|
| 2.A.1 Mathematics                   |  -0.09|   0.09|  0.17|   0.04|  -0.13|
| 2.B.3 Technology Design             |  -0.05|   0.12|  0.17|   0.01|   0.14|
| 4.A.2 Analyzing Data or Information |  -0.12|   0.04|  0.16|  -0.06|  -0.11|
| 2.C.3 Engineering and Technology    |  -0.03|   0.19|  0.14|   0.06|   0.09|
| 2.A.1 Science                       |  -0.07|   0.09|  0.13|  -0.19|  -0.05|

Our selected skills are all present here except for telecommunications, which we discuss thoroughly in our report's appendix. Our selected skills are also interspersed with scientific and mathmatical skills, which is no surprise given the commonalities among "STEM" fields (science, technology, engineering and math). Given our focus on tech, we chose to focus on the "T" and "E"" in STEM in our definition rather than the "S" and the "M".
