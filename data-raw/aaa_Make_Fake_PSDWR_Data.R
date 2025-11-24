## Creates weight-length data for a variety of species (some with sub-groups) to
##   be used as examples in PSD and Wr calculations. Running the script will produce
##   a data.frame called PSDWRtest that is distributed with FSA.
set.seed(633437)

bgdf <- data.frame(species="Bluegill Sunfish",
                   location="Bass Lake",
                   len=round(c(rnorm(30,mean=100,sd=15),
                               rnorm(70,mean=150,sd=25),
                               rnorm(20,mean=200,sd=25)),0)) |>
  dplyr::mutate(wt=10^(-5.37)*len^3.316,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=NA_character_)

bktdf <- data.frame(species="Brook Trout",
                    location="Trout Lake",
                    len=round(c(rnorm(20,mean=175,sd=25),
                                rnorm(50,mean=255,sd=25),
                                rnorm(10,mean=310,sd=25)),0)) |>
  dplyr::mutate(wt=10^(-5.2)*len^3.1,
                wt=round(wt+rnorm(dplyr::n(),wt*0,10),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE),
                sex=NA_character_)

brt1df <- data.frame(species="Brown Trout",
                     location="Trout Lake",
                     len=round(c(rnorm(20,mean=150,sd=20),
                                 rnorm(50,mean=230,sd=25),
                                 rnorm(10,mean=325,sd=25)),0)) |>
  dplyr::mutate(wt=1.4e-05*len^2.96,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE),
                sex=NA_character_)

brt2df <- data.frame(species="Brown Trout",
                     location="Brushy Creek",
                     len=round(c(rnorm(24,mean=200,sd=20),
                                 rnorm(60,mean=325,sd=25),
                                 rnorm(15,mean=450,sd=25)),0)) |>
  dplyr::mutate(wt=3.65e-06*len^3.1,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE))

iaddf <- FSAdata::InchLake2 |>
  dplyr::filter(species=="Iowa Darter") |>
  dplyr::mutate(species=="Iowa Darter",
                location="Bass Lake",
                sex=NA_character_,
                len=round(length*25.4,0),
                wt=NA) |>
  dplyr::select(species,location,len,wt=weight,sex)

lmbdf <- data.frame(species="Largemouth Bass",
                    location="Bass Lake",
                    len=round(c(rnorm(30,mean=200,sd=15),
                                rnorm(40,mean=300,sd=25),
                                rnorm(20,mean=350,sd=25)),0)) |>
  dplyr::mutate(wt=10^(-5.5)*len^3.2,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=NA_character_)

lktdf <- data.frame(species="Lean Lake Trout",
                    location="Trout Lake",
                    len=round(c(rnorm(10,mean=300,sd=40),
                                rnorm(60,mean=500,sd=50),
                                rnorm(30,mean=750,sd=50)),0)) |>
  dplyr::mutate(wt=10^(-5.7)*len^3.246,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE))

muedf <- data.frame(species="Muskellunge",
                    location="Long Lake",
                    len=round(c(rnorm(10,mean=600,sd=60),
                                rnorm(25,mean=800,sd=70),
                                rnorm(10,mean=1000,sd=60)),0)) |>
  dplyr::mutate(wt=10^(-6.0)*len^3.32,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M","U"),dplyr::n(),c(0.4,0.4,0.2),replace=TRUE))

rufdf <- data.frame(species="Ruffe",
                    location="Round Lake",
                    len=round(c(rnorm(10,mean=70,sd=40),
                                rnorm(20,mean=120,sd=20),
                                rnorm(10,mean=160,sd=20)),0)) |>
  dplyr::mutate(wt=3.03e-06*len^3.26,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=NA_character_)

waedf1 <- data.frame(species="Walleye",
                     location="Bass Lake",
                     len=round(rnorm(50,mean=100,sd=25),0)) |>
  dplyr::filter(len<150) |>
  dplyr::mutate(wt=10^(-4.8)*len^2.87,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.05),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=NA_character_)

waedf2 <- data.frame(species="Walleye",
                     location="Bass Lake",
                     len=round(c(rnorm(20,mean=250,sd=25),
                                 rnorm(60,mean=425,sd=40),
                                 rnorm(30,mean=600,sd=40)),0)) |>
  dplyr::filter(len>=150) |>
  dplyr::mutate(wt=10^(-5.45)*len^3.18,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.1),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE),
                sex=ifelse(len<300,NA,sex))

yepdf <- data.frame(species="Yellow Perch",
                    location="Bass Lake",
                    len=round(c(rnorm(100,mean=150,sd=25),
                                rnorm(50,mean=250,sd=25),
                                rnorm(20,mean=300,sd=25)),0)) |>
  dplyr::mutate(wt=10^(-5.38)*len^3.23,
                wt=round(wt+rnorm(dplyr::n(),0,wt*0.10),1),
                wt=ifelse(wt<=0,min(wt[wt>0]),wt),
                sex=sample(c("F","M"),dplyr::n(),replace=TRUE),
                sex=ifelse(len<100,NA,sex))


PSDWRtest <- rbind(bgdf,bktdf,brt1df,brt2df,iaddf,lmbdf,lktdf,muedf,rufdf,waedf1,waedf2,yepdf)

usethis::use_data(PSDWRtest,internal=FALSE,overwrite=TRUE)
