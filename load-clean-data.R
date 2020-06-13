library(tidyverse)
library(here)
library(lubridate)
library(googledrive)
library(magrittr)

if(!file.exists("IVF.xlsx")){drive_download("https://docs.google.com/spreadsheets/d/1BBA9MmP-fj_E23Lhhhf5AMf-ihx_qxz6Zy3QINROD4w/edit?usp=sharing",
               path="./IVF")}


iv  = readxl::read_xlsx("IVF.xlsx",col_names = T,.name_repair = "unique")
savenames <- colnames(iv)
colnames(iv) %<>% gsub(pattern = "/",rep="vs",.)
colnames(iv) %<>% gsub(pattern = " ",rep=".",.)

iv$Horodateur<- ymd_hms(iv$Horodateur)

table(iv$Specialty)
table(iv$Ward)


ggplot(iv,aes(x=Ward))+geom_bar(fill="Maroon")+
        labs(title="Wards Assesed",
             y="Number Patients")

ggplot(iv,aes(Weekend.vs.Weekday))+geom_bar()
iv %<>% rename(IVDOC.notes=Documentation.in.Patient.notes.to.prescribe.IVF,
              WD=Weekend.vs.Weekday,
              FluidStatusDOC=Fluid.Status.Documented.on.Notes,
              Weight7D=Weight.Recorded.in.Last.7.days,
              FluidBalance24H=Fluids.Balance.Recorded.for.last.24.H,
              UE24H=`U&E.for.last.24.Hours`,
              FluidIndicationInPlan=`Indication.for.IVF.noted.in.the.Plan?`,
              IVF24.FPDOC=IVF.prescribed.for.24H..or.Fluid.Plan.noted,
              IVFvol=IVF.Volume..Noted.in.Prescription,
              IVFrate=IVF.Duration.Noted,
              IV.K=`IV.Potassium.Prescribed?`,
              AppMain=`Appropriate.Fluids.Prescribed?.[Maintenance]`,
              AppReplace=`Appropriate.Fluids.Prescribed?.[Replacement]`,
              AppResus=`Appropriate.Fluids.Prescribed?.[Resuscitation]`
               )

# cleaning up  fluid composition variable
iv$Composition %>% table

iv$Composition[grep("\\.45",iv$Composition)] <-  "0.45%NaCl 5%Dextrose"
iv$Composition[grep("^Insulin",iv$Composition)] <-  "0.45%NaCl 5%Dextrose"
iv$Composition[grep("Non",iv$Composition)] <-  NA
iv$Composition[grep("\\.18",iv$Composition)] <- "0.18%NaCl 4%Dextrose"
iv$Composition %<>% gsub("NACL","NaCl",.)

iv[,-c(1,2)] %<>% lapply(as_factor)

##loading assement algorithm image

if(!file.exists("ivf.pdf")){url <- "https://www.nice.org.uk/guidance/cg174/resources/intravenous-fluid-therapy-in-adults-in-hospital-algorithm-poster-set-191627821"
                download.file(url,"ivf.pdf")
                pdftools::pdf_convert("ivf.pdf",format="png",1)
                pdftools::pdf_convert("ivf.pdf",format="png",2)
                pdftools::pdf_convert("ivf.pdf",format="png",3)
                pdftools::pdf_convert("ivf.pdf",format="png",4)
                }

