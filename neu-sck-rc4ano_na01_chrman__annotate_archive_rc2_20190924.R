rm(list = ls())
###_______________________________________________====
### ENVIRONMENT __________________________________====


# initiate environment ----

#' environmetn functions and settings

source ("~/ws/inienv/envrst.R") # to initiate F.env.create.clone.git and  F.env.copy.s3.aws.to.local functions
F.env.copy.s3.aws.to.local ('iniset','s3://intpol-s3e-vsb-tes-manifest/googlesheets','HylmarJ')

#' internal functions

ENV.path = Sys.getenv("HOME")

source (paste0 (ENV.path, "/ws/inicom/comrst/F.common.trigger.R"))
F.common.trigger(
  c(
    'connection.athena',
    'F.create.folder',
    'F.athena.list.tables',
    'F.athena.list.table.columns',
    'F.translate.items',
    'F.translate.list',
    'F.translate.list.danter.temporary',
    'F.ymd.min.max.mat.MMYYYY',
    'F.time.stamps',
    'F.gsheet.vector.2long',
    'F.copy.s3.aws.to.local.recursive',
    'F.list.s3.aws.recursive',
    'F.parse.manifest.autorisation',
    'F.write.csv.tsv',
    'F.create.folder.multiple'
  )
)


'%!in%' <- function(x, y) ! ('%in%'(x, y))

#' external libraries

library(dplyr)
library(dbplyr)
library(tidyr)
library(jsonlite)
library(stringi)
library(rmarkdown)
library(readr)
library(imager)

# source manifest ----

# browseURL('chrome https://docs.google.com/spreadsheets/d/1de7Dt_JzD9M8Ot_cD7L-E5g8PeI9qoxnf3eZKXB02J4/edit#gid=572563962')

df.files = paste0 (ENV.path, "/ws/iniset/"); df.files = list.files(df.files, recursive = T, full.names = T)

df.manifest = list()

for (i in seq_along(df.files)){
  df.manifest[[i]] = read.csv(
    df.files[[i]],
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding =  "UTF-8")
  
}
df.files.names = gsub (' - ','_',basename(df.files)); df.files.names = gsub ('\\..*','',basename(df.files.names)); names(df.manifest) = df.files.names

df.manifest
names (df.manifest)

# input config ----

par_out_config = df.manifest$sck_rc4ann_02__environments_main_out_config
par_out_config = par_out_config[,c('nam','neu.sck.rc4ano_na01_chrman__annotate_archive_rc2_20190924')]
par_out_config.tem = t(as.data.frame(unlist (par_out_config[,2])))
colnames(par_out_config.tem) = as.vector (unlist (par_out_config[,1]))
par_out_config = as.data.frame (par_out_config.tem)
rownames(par_out_config) = NULL
par_out_config

# define parameters ----

(par_sys_pronambas = as.vector (par_out_config$par_sys_pronambas))
(par_sys_inp_mo1 = paste0 ('//',as.vector (par_out_config$par_sys_inp_mo1)))
(par_sys_loc_mo1 = paste0 (ENV.path,'/',as.vector (par_out_config$par_sys_loc_mo1)))
(d4m_hid_sck = as.vector (par_out_config$d4m_hid_sck))

(par_sys_aws_1_profile = as.vector (par_out_config$par_sys_aws_1_profile))
(par_sys_aws_2_profile = as.vector (par_out_config$par_sys_aws_2_profile))

# check mount ----

mo1.mount = paste0 ('sudo mount -t cifs ',par_sys_inp_mo1,' ',par_sys_loc_mo1,'/',' -o username=hylmarJ,password=FeR43?fd,uid=hylmarj')

try (
  system(mo1.mount)
  )

# unmount ----
#' mo1.unmount = paste0 ('sudo umount -l ',par_sys_loc_mo1)
#' system(mo1.unmount)

# prepare save path ----

(sav.A.pat.hom = paste0 (ENV.path, "/ws/"))

(sav.B.pat.aws.tem = paste0 ('scratch_', par_sys_pronambas,"__", d4m_hid_sck))

#  remove scratch for refresh RECURSIVE ----
#' never put mounts to scratch :-)

try (
  system(paste0('rm -r ', paste0 (ENV.path, "/ws/",sav.B.pat.aws.tem)))
  )

#sav.c.fol.pronam = pronambas

#(sav.d.fol.prgnam = paste0 (par_sys_pronambas, "__", d4m_hid_sck))

dir.pat.scr.lis = paste0 (
  sav.A.pat.hom
  ,sav.B.pat.aws.tem
  #'/',
  #sav.c.fol.pronam,
  #'/',
  #sav.d.fol.prgnam
)

dir.pat.scr = paste0 (sav.A.pat.hom, sav.B.pat.aws.tem)

sav.e.fol.outF.athena.list.table.columnsa = c("ima", "dat", "doc")

sav.fol.vec.1 = list ()
for (i in seq_along(sav.e.fol.outF.athena.list.table.columnsa)) {
  sav.fol.vec.1[[i]] = c(sav.B.pat.aws.tem
                         #,sav.c.fol.pronam
                         #,sav.d.fol.prgnam
                         ,sav.e.fol.outF.athena.list.table.columnsa[[i]])
}

for (i in seq_along(sav.e.fol.outF.athena.list.table.columnsa)) {
  F.create.folder(sav.A.pat.hom, sav.fol.vec.1[[i]])
}

sav.DOC = list.dirs(dir.pat.scr.lis)[grep('/doc', list.dirs(dir.pat.scr.lis))]
sav.IMA = list.dirs(dir.pat.scr.lis)[grep('/ima', list.dirs(dir.pat.scr.lis))]
sav.DAT = list.dirs(dir.pat.scr.lis)[grep('/dat', list.dirs(dir.pat.scr.lis))]

###____________________________ environment ready ====

###_______________________________________________====
### DATA READ ____________________________________====

# read all data available ----

par_sys_loc_mo1

# df.path = "~/datastore/AlfaDWbh6/ext-imareg/rc2_csv_3"
# df.path = paste0 (par_sys_loc_mo1,"/rc2_csv_3")
# 
# df.files = list.files(df.path, recursive = T, full.names = T)
# df.x = lapply(df.files,
#               function(i) {
#                 read.csv(
#                   file = i,
#                   header = TRUE,
#                   stringsAsFactors = FALSE,
#                   fileEncoding =  "UTF-8"
#                 )
#               })
# df.x = do.call(rbind,df.x)
# df.x.col = colnames (df.x)
# colnames (df.x) = tolower(df.x.col)
# 
# names (df.x)

df.x = read_csv("datastore/AlfaDWbh6/ext-imareg/rc2_20190924__synchro_renamcol_imasiz.csv")


# read all images available ----

# image lists by image types

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files = list.files(df.path, recursive = T, full.names = T)
df.files = df.files [grepl ('norori', df.files)]
df.files.id = substr(df.files,nchar(df.files)-91,nchar(df.files))

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files.norsta = list.files(df.path, recursive = T, full.names = T)
df.files.id.norsta = substr(df.files.norsta,nchar(df.files.norsta)-91,nchar(df.files.norsta))

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files.norcro = list.files(df.path, recursive = T, full.names = T)
df.files.norcro = df.files.norcro [grepl ('norcro', df.files.norcro)]
df.files.id.norcro = substr(df.files.norcro,nchar(df.files.norcro)-91,nchar(df.files.norcro))

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files.invsta = list.files(df.path, recursive = T, full.names = T)
df.files.invsta = df.files.invsta [grepl ('invsta', df.files.invsta)]
df.files.id.invsta = substr(df.files.invsta,nchar(df.files.invsta)-91,nchar(df.files.invsta))

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files.invcro = list.files(df.path, recursive = T, full.names = T)
df.files.invcro = df.files.invcro [grepl ('invcro', df.files.invcro)]
df.files.id.invcro = substr(df.files.invcro,nchar(df.files.invcro)-91,nchar(df.files.invcro))

df.path = paste0 (par_sys_loc_mo1,"/rc2_20190924/staana.alfa1@gmail.com")
df.files.oveviesta = list.files(df.path, recursive = T, full.names = T)
df.files.oveviesta = df.files.oveviesta [grepl ('oveviesta', df.files.oveviesta)]
df.files.id.oveviesta = substr(df.files.oveviesta,nchar(df.files.oveviesta)-94,nchar(df.files.oveviesta))


# check availability data and images -----

nrow(df.x)
length(df.files.id)
df.kp = df.x[df.x$keypri %in% df.files.id, ]
nrow(df.kp)
setdiff(df.files.id,df.x$keypri)
setdiff(df.x$keypri,df.files.id)

# check data and remove duplicates -----

df.kp$keypri[duplicated(df.kp$keypri)]

###______________________________ data read ready ====

###_______________________________________________====
### DATA PREPRO __________________________________====

# method gather coo ----
#' aws dynamo in the future

# reduce unused data attributes ----

# quality info

df.kp = df.kp[,-grep("_kpp|_kpl|_ver|_kpc|_ppl", colnames(df.kp))]

# values to be separated ----

colnames(df.kp)
length (colnames(df.kp))

colnames(df.kp)[c(-grep("_cox|_coy|_lsi|_lto|_wid|_hei",colnames(df.kp)))]
length (colnames(df.kp)[c(-grep("_cox|_coy|_lsi|_lto|_wid|_hei",colnames(df.kp)))])

colnames(df.kp)[c(grep("_cox|_coy",colnames(df.kp)))]
length (colnames(df.kp)[c(grep("_cox|_coy",colnames(df.kp)))])

###_______________________________________________====

# method gather detectron structure ----

# gather x coo

df.kp.cox = gather(df.kp, #' df
                   key = "key",
                   value = "cox",
                   colnames(df.kp)[c(grep("_cox",colnames(df.kp)))] #' values to gather
)
df.kp.cox = df.kp.cox[c(-grep("_coy|_lsi|_lto|_wid|_hei",colnames(df.kp.cox)))]
df.kp.cox = df.kp.cox[order(df.kp.cox$parbas_tim),] 
df.kp.cox$key = gsub ("_cox","",df.kp.cox$key)
nrow(df.kp.cox)

# garther y coo

df.kp.coy = gather(df.kp, #' df
                   key = "key",
                   value = "coy",
                   colnames(df.kp)[c(grep("_coy",colnames(df.kp)))] #' values to gather
)
df.kp.coy = df.kp.coy[c(-grep("_cox|_lsi|_lto|_wid|_hei",colnames(df.kp.coy)))]
df.kp.coy = df.kp.coy[order(df.kp.coy$parbas_tim),] 
df.kp.coy$key = gsub ("_coy","",df.kp.coy$key)
nrow(df.kp.coy)

common_col_names = intersect(names(df.kp.cox), names(df.kp.coy))

# merge df to print ----

df.kp.coo = merge(df.kp.cox, df.kp.coy, by=common_col_names, all=TRUE)

# filter out different format service output ----

tem.fil = df.kp.coo$key[!grepl("facana|objsce",df.kp.coo$key)]
df.kp.coo = df.kp.coo %>% filter (key %in% tem.fil)
df.kp.coo.fil.col = colnames (df.kp.coo) [c(grepl("facana",colnames (df.kp.coo)))]
df.kp.coo = df.kp.coo[ , !(names(df.kp.coo) %in% df.kp.coo.fil.col)]
#colnames(df.kp.coo)

# df coo imareg ----

tem.fil = df.kp.coo$key[!grepl("tpo",df.kp.coo$key)]
tem.fil = tem.fil[grepl("imareg",tem.fil)]
df.kp.coo.imareg = df.kp.coo %>% filter (key %in% tem.fil)
#colnames(df.kp.coo.imareg)

# df coo kp1 ----

tem.fil = df.kp.coo$key[!grepl("tpo",df.kp.coo$key)]
tem.fil = tem.fil[grepl("keypo1",tem.fil)]
df.kp.coo.keypo1 = df.kp.coo %>% filter (key %in% tem.fil)
#colnames(df.kp.coo.keypo1)

# check df

df.kp.coo.keypo1.NA = df.kp.coo[is.na(df.kp.coo.keypo1$cox) & is.na(df.kp.coo.keypo1$coy),]
ifelse(nrow(df.kp.coo.keypo1.NA)==0,"keypo1.NA PASS NAs not in cox, coy","keypo1.NA FAIL NAs in cox, coy")

# df pol imareg  ----

tem.fil = df.kp.coo$key[grepl("tpo",df.kp.coo$key)]
tem.fil = tem.fil[grepl("imareg",tem.fil)]
df.kp.pol.imareg = df.kp.coo %>% filter (key %in% tem.fil)
#colnames(df.kp.pol.imareg)

# df pol keypo1 ----

tem.fil = df.kp.coo$key[grepl("tpo",df.kp.coo$key)]
tem.fil = tem.fil[grepl("keypo1",tem.fil)]
df.kp.pol.keypo1 = df.kp.coo %>% filter (key %in% tem.fil)
#colnames(df.kp.pol.keypo1)

#check df
# df.kp.pol.NA = df.kp.pol[is.na(df.kp.pol$cox) & is.na(df.kp.pol$coy),]
# ifelse(nrow(df.kp.pol.NA)==0,"keypo1.NA PASS NAs not in cox, coy","keypo1.NA FAIL NAs in cox, coy")

###_______________________________________________====

# method gather AWS structure ----

# gather par aws

df.kp.aws = gather(df.kp, #df
                   key = "key",
                   value = "value",
                   colnames(df.kp)[c(grep("_lsi|_lto|_wid|_hei",colnames(df.kp)))] #values to gather
)
df.kp.aws = df.kp.aws[c(-grep("_cox|_coy",colnames(df.kp.aws)))]

head(as.data.frame(df.kp.aws),10)
colnames(df.kp.aws)

df.kp.aws = df.kp.aws[order(df.kp.aws$parbas_tim),] 

# df objsce ----

fil.objsce = df.kp.aws$key [c(grep ('objsce',df.kp.aws$key))]
df.kp.aws.objsce = df.kp.aws[df.kp.aws$key %in% fil.objsce, ]
df.kp.aws.objsce$key = substr (df.kp.aws.objsce$key,nchar(df.kp.aws.objsce$key)-2, nchar (df.kp.aws.objsce$key))
df.kp.aws.objsce = as.data.frame (spread(df.kp.aws.objsce,key,value))
# check df
head (as.data.frame(df.kp.aws.objsce))
df.kp.aws.objsce[!complete.cases(df.kp.aws.objsce), ]

# df facana ----

fil.facana = df.kp.aws$key [c(grep ('facana',df.kp.aws$key))]
df.kp.aws.facana = df.kp.aws[df.kp.aws$key %in% fil.facana, ]
df.kp.aws.facana$key = substr (df.kp.aws.facana$key,nchar(df.kp.aws.facana$key)-2, nchar (df.kp.aws.facana$key))
df.kp.aws.facana = as.data.frame (spread(df.kp.aws.facana,key,value))
#check df
head (as.data.frame(df.kp.aws.facana))
as.data.frame (df.kp.aws.facana[!complete.cases(df.kp.aws.facana), ])


#  consolidate data to list ----

head(df.kp.aws.objsce,3)
head(df.kp.aws.facana,3)
head(df.kp.coo.keypo1,3)
head(df.kp.coo.imareg,3)
head(df.kp.pol.keypo1,3)
head(df.kp.pol.imareg,3)

df.kp.lis.raw = list(
  df.kp.aws.objsce,
  df.kp.aws.facana,
  df.kp.coo.keypo1,
  df.kp.coo.imareg,
  df.kp.pol.keypo1,
  df.kp.pol.imareg
)

names(df.kp.lis.raw) = c("aws.objsce","aws.facana","coo.keypo1","coo.imareg","pol.keypo1","pol.imareg")

df.kp.lis = df.kp.lis.raw

###____________________________ data prepro ready ====

head (df.kp.lis,5)

json.sample = toJSON(lapply(df.kp.lis,'[',c(1:3),), pretty = TRUE)

###_______________________________________________====
### DATA ADD CALCULATED HERE __________________________====

# need to know width

###____________________ data add calculated ready ====

###_______________________________________________====
### DRAW SETTINGS READ ___________________________====
# read draw project -----

# https://docs.google.com/spreadsheets/d/1jSjkc6NXB3B-iWVHj_i4kDGjUA708L9OaPdhnPlEzr0/edit#gid=1165339074
drapol <- read_csv(paste0 (par_sys_loc_mo1,"/neu-valter-ngc - pol.csv"))
dracoo <- read_csv(paste0 (par_sys_loc_mo1,"/neu-valter-ngc - coo.csv"))

###__________________________ draw settings ready ====

### MLOOP ________________________________________====
###xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx====

# select  images all types, complete ----

df.files.id = 
  c(
    df.files.id [grepl("masB01",df.files.id)][1:21],
    df.files.id [grepl("masC02",df.files.id)],
    df.files.id [grepl("masC03",df.files.id)],
    df.files.id [grepl("masF04",df.files.id)][1:9],
    df.files.id [grepl("masP05",df.files.id)][1:3]
  )


df.files.id

e = 2#debug----

for (e in 1:length(df.files.id)){
  
  
  # bdecb4f2-9c9e-11e9-a885-005056840793.png
  # bdecb4ca-9c9e-11e9-a885-005056840793.png
  # bdecb466-9c9e-11e9-a885-005056840793.png
  
  #type 2
  #accf461c-b6ca-11e9-ad58-005056840793.png
  #accf3c08-b6ca-11e9-ad58-005056840793.png
  #accf3078-b6ca-11e9-ad58-005056840793.png
  
  ###_______________________________________________====
  ### IMAGE SELECT & FETCH _________________________====

  # select images ----
  
  ima.uid = df.files.id[[e]]
  ima.pat = df.files[str_detect(df.files, ima.uid)]
  ima.pat.norsta = df.files.norsta[str_detect(df.files.id.norsta, gsub ("norori","norsta",ima.uid))]
  ima.pat.norcro = df.files.norcro[str_detect(df.files.id.norcro, gsub ("norori","norcro",ima.uid))]
  ima.pat.invsta = df.files.invsta[str_detect(df.files.id.invsta, gsub ("norori","invsta",ima.uid))]
  ima.pat.invcro = df.files.invcro[str_detect(df.files.id.invcro, gsub ("norori","invcro",ima.uid))]
  #ima.pat.oveviesta = df.files.id.oveviesta[str_detect(df.files.id.oveviesta, gsub ("norori","oveviesta",ima.uid))]
  
  # fetch images and get basic info ----
  
  image = load.image(ima.pat)
  image.norsta = load.image(ima.pat.norsta)
  image.invcro = load.image(ima.pat.invcro)
  image.invsta = load.image(ima.pat.invsta)
  image.norcro = load.image(ima.pat.norcro)
  #image.oveviesta = load.image(ima.pat.oveviesta)
  
  image = implot(image,text(600,50,gsub ('staana.alfa1@gmail.com',"",ima.uid),cex=2))
  image.norsta = implot(image.norsta,text(250,50,gsub ('staana.alfa1@gmail.com',"",ima.uid),cex=1))
  image.invcro = implot(image.invcro,text(250,50,gsub ('staana.alfa1@gmail.com',"",ima.uid),cex=1,col='white'))
  image.invsta = implot(image.invsta,text(250,50,gsub ('staana.alfa1@gmail.com',"",ima.uid),cex=1))
  #image.oveviesta = implot(image.oveviesta,text(250,50,gsub ('staana.alfa1@gmail.com',"",ima.uid),cex=1))

  ###_________________________ select & fetch ready ====

  ###_______________________________________________====
  ### CASE CHECK ORIGINAL __________________________====

  # image names for saving ----
  
  str.nam = unlist (strsplit (ima.pat, '/'))
  str.nam = str.nam[8:length(str.nam)]
  ima.nam = str.nam[length(str.nam)]
  str.nam.cre = str.nam[-length(str.nam)]
  str.nam.cre = str.nam.cre[c(2,4)]
  
  path.outDir.ful = paste0( sav.IMA,
                            "/",
                            str.nam.cre[1],
                            "/",
                            str.nam.cre[2],
                            "/",
                            ima.nam)
  TES = 0 #  test output folder----
  if (TES == 0) {
    path.outDir = sav.IMA
  } else {
    "NO ALTERNATIVE TO SAVE"
  }
  
  fol.vec = str.nam.cre
  F.create.folder.multiple(path.outDir, fol.vec)
  
  cex.tex = 2
  
  #png(path.outDir.ful,width = 1123, height = 1685) # !!! comment not to save image ----
  
  par(mfrow=c(1,1))
  par(mar=c(0,0,0,0))  
  
  # plot -----
  
  plot(image,axes = FALSE)
  
  # filter data for a single image ----
  
  df.kp.lis.fil = list()
  for (i in seq_along(df.kp.lis)){
    df.kp.lis.fil[[i]] = df.kp.lis[[i]] %>% filter (keypri %in% ima.uid)
  }
  names(df.kp.lis.fil) = c("aws.objsce","aws.facana","coo.keypo1","coo.imareg","pol.keypo1","pol.imareg")
  
  ###_______________________________________________====
  ### DATA ADD CALCULATED __________________________====
  
  # add xy to aws method ----
  
  df.kp.lis.fil$aws.facana$coxupplef =  width(image)*df.kp.lis.fil$aws.facana$lsi
  df.kp.lis.fil$aws.facana$coyupplef =  height(image)*df.kp.lis.fil$aws.facana$lto
  df.kp.lis.fil$aws.facana$coxdowrig =  width(image)*df.kp.lis.fil$aws.facana$wid + width(image)*df.kp.lis.fil$aws.facana$lsi
  df.kp.lis.fil$aws.facana$coydowrig =  height(image)*df.kp.lis.fil$aws.facana$hei + height(image)*df.kp.lis.fil$aws.facana$lto
  
  df.kp.lis.fil$aws.objsce$coxupplef =  width(image)*df.kp.lis.fil$aws.objsce$lsi
  df.kp.lis.fil$aws.objsce$coyupplef =  height(image)*df.kp.lis.fil$aws.objsce$lto
  df.kp.lis.fil$aws.objsce$coxdowrig =  width(image)*df.kp.lis.fil$aws.objsce$wid + width(image)*df.kp.lis.fil$aws.objsce$lsi
  df.kp.lis.fil$aws.objsce$coydowrig =  height(image)*df.kp.lis.fil$aws.objsce$hei + height(image)*df.kp.lis.fil$aws.objsce$lto
  
  # info for other ploting than norori ----
  
  drops <- c("key","cox","coy")
  df.pl.tra.imareg = df.kp.lis.fil$coo.keypo1[ , !(names(df.kp.lis.fil$coo.keypo1) %in% drops)]
  df.pl.tra.imareg = df.pl.tra.imareg[!duplicated(df.pl.tra.imareg$keypri), ]
  df.pl.tra.imareg = df.pl.tra.imareg %>% filter (keypri %in% ima.uid)
  
  names (df.kp.lis.fil)
  
  # add xy tra to coo.keypo1 method ----
  
  df.kp.lis.fil$coo.keypo1$tracox = df.kp.lis.fil$coo.keypo1$cox*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracox
  df.kp.lis.fil$coo.keypo1$tracoy =  df.kp.lis.fil$coo.keypo1$coy*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracoy
  
  # add xy tra to pol.keypo1 method ----
  
  df.kp.lis.fil$pol.keypo1$tracox = df.kp.lis.fil$pol.keypo1$cox*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracox
  df.kp.lis.fil$pol.keypo1$tracoy =   df.kp.lis.fil$pol.keypo1$coy*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracoy
  
  # add xy tra topol.imareg method ----
  
  df.kp.lis.fil$pol.imareg$tracox = df.kp.lis.fil$pol.imareg$cox*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracox
  df.kp.lis.fil$pol.imareg$tracoy =   df.kp.lis.fil$pol.imareg$coy*
    df.pl.tra.imareg$imareg_norsta_trasca+
    df.pl.tra.imareg$imareg_norsta_tracoy
  
  ###____________________ data add calculated ready ====
  ###_______________________________________________====
  
  
  # pair with draw project -----
  
  for (i in 3:6){
    df.kp.lis.fil[[i]]$keypro = substr(df.kp.lis.fil[[i]]$key, nchar(df.kp.lis.fil[[i]]$key)-18, nchar(df.kp.lis.fil[[i]]$key))
  }
  
  # clean dataframes for direct plotting -----
  
  df.pl.coo.keypo1 = merge(dracoo,df.kp.lis.fil$coo.keypo1,by.x = 'tpo', by.y = 'keypro')
  
  df.pl.pol.imareg = merge(drapol,df.kp.lis.fil$pol.imareg,by.x = 'tpo', by.y = 'keypro')
  df.pl.fil = df.pl.pol.imareg$tpg[!grepl("bfe|box|bos|bfd",df.pl.pol.imareg$tpg)]
  df.pl.pol.imareg = df.pl.pol.imareg %>% filter (tpg %in% df.pl.fil)
  
  df.pl.pol.keypo1 = merge(drapol,df.kp.lis.fil$pol.keypo1,by.x = 'tpo', by.y = 'keypro')
  df.pl.fil = df.pl.pol.keypo1$tpg[!grepl("bfe|bfo",df.pl.pol.keypo1$tpg)]
  df.pl.pol.keypo1 = df.pl.pol.keypo1 %>% filter (tpg %in% df.pl.fil)
  
  # rectangles object scene -----
  
  rect(
    df.kp.lis.fil$aws.objsce$coxupplef,
    df.kp.lis.fil$aws.objsce$coydowrig,
    df.kp.lis.fil$aws.objsce$coxdowrig,
    df.kp.lis.fil$aws.objsce$coyupplef,
    border = "red"
  )
  
  text(df.kp.lis.fil$aws.objsce$coxupplef-25,
       df.kp.lis.fil$aws.objsce$coyupplef-25,
       "A",
       col = "red",
       cex = cex.tex
  )
  
  # rectangles face analysis -----
  
  rect(
    df.kp.lis.fil$aws.facana$coxupplef,
    df.kp.lis.fil$aws.facana$coydowrig,
    df.kp.lis.fil$aws.facana$coxdowrig,
    df.kp.lis.fil$aws.facana$coyupplef,
    border = "red"
  )
  
  text(df.kp.lis.fil$aws.facana$coxupplef+25,
       df.kp.lis.fil$aws.facana$coyupplef+25,
       "B",
       col = "red",
       cex = cex.tex
  )
  
  # keupoints coo method -----
  
  points(
    df.pl.coo.keypo1$cox,
    df.pl.coo.keypo1$coy,
    pch = 21,
    bg ="green",
    col="green",
    cex = 0.7
    
  )
  
  text(df.pl.coo.keypo1$cox+15,
       df.pl.coo.keypo1$coy+15,
       df.pl.coo.keypo1$tecano,
       col = "green",
       cex = cex.tex
  )
  
  # rectangles coo method -----
  
  dra.pol.F = function(DF.PL.POL, COL.DRA, CEX.TEX.POL){
    # DF.PL.POL = df.pl.pol.imareg # debug
    # COL.DRA = "orange"
    # CEX.TEX.POL = 1
    
    df.pl.spl = split(DF.PL.POL,DF.PL.POL$tpg)
    
    for (i in 1:length(df.pl.spl)){
      df.pl.spl[[i]] = df.pl.spl[[i]][order(df.pl.spl[[i]]$tpg_ord),]   
    }
    
    for (i in 1:length(df.pl.spl)){
      rect(
        df.pl.spl[[i]]$cox[1],
        df.pl.spl[[i]]$coy[2],
        df.pl.spl[[i]]$cox[2],
        df.pl.spl[[i]]$coy[1],
        border = COL.DRA
      )
    }
    
    for (i in 1:length(df.pl.spl)){
      text(df.pl.spl[[i]]$cox[1]+25,
           df.pl.spl[[i]]$coy[1]+25,
           df.pl.spl[[i]]$tecano,
           col = COL.DRA,
           cex = CEX.TEX.POL
      )
    }
  }
  
  df.pl.tra.imareg
  
  # detectron bounging box coo method -----
  
  dra.pol.F (df.pl.pol.keypo1,"orange",cex.tex)
  
  # registration area coo method -----
  
  dra.pol.F (df.pl.pol.imareg,"blue",cex.tex)
  
  # upper left
  
  points(
    min (df.kp.lis.fil$pol.imareg$cox[!is.na (df.kp.lis.fil$pol.imareg$cox)]),
    min (df.kp.lis.fil$pol.imareg$coy[!is.na (df.kp.lis.fil$pol.imareg$coy)]),
    col = 'blue',
    pch = 24,
    cex=2
  )
  
  # lower right
  
  points(
    max (df.kp.lis.fil$pol.imareg$cox[!is.na (df.kp.lis.fil$pol.imareg$cox)]),
    max (df.kp.lis.fil$pol.imareg$coy[!is.na (df.kp.lis.fil$pol.imareg$coy)]),
    col = 'blue',
    pch = 24,
    cex=2
  )
  
  # view area coo method -----
  
  # down right
  
  points(
    (width(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracox) / df.pl.tra.imareg$imareg_norsta_trasca,
    (height(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracoy) / df.pl.tra.imareg$imareg_norsta_trasca,
    pch = 24,
    bg ="brown",
    col="brown",
    cex = 2
  )
  
  # upper left
  
  points(
    -df.pl.tra.imareg$imareg_norsta_tracox / df.pl.tra.imareg$imareg_norsta_trasca,
    -df.pl.tra.imareg$imareg_norsta_tracoy / df.pl.tra.imareg$imareg_norsta_trasca,
    pch = 24,
    bg ="brown",
    col="brown",
    cex = 2
  )
  
  # rectangle
  
  rect(
    (width(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracox) / df.pl.tra.imareg$imareg_norsta_trasca,
    (height(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracoy) / df.pl.tra.imareg$imareg_norsta_trasca,
    -df.pl.tra.imareg$imareg_norsta_tracox / df.pl.tra.imareg$imareg_norsta_trasca,
    -df.pl.tra.imareg$imareg_norsta_tracoy / df.pl.tra.imareg$imareg_norsta_trasca,
    border = "red",
    lty = 2,
    lwd = 1
  )
  
  text((width(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracox) / df.pl.tra.imareg$imareg_norsta_trasca - 25,
       (height(image.norsta) - df.pl.tra.imareg$imareg_norsta_tracoy) / df.pl.tra.imareg$imareg_norsta_trasca - 25,
       "G",
       col = "brown",
       cex = cex.tex
  )
  
  #dev.off()# !!! comment not to save image ----
  
  print (path.outDir.ful)
  
  ###____________________ case check original ready ====

  ###_______________________________________________====
  ### CASE CHECK STANDARD & CROPPED ________________====
  
  # par(mfrow=c(1,5))
  
  # names for saving ----
  
  str.nam = unlist (strsplit (ima.pat, '/'))
  str.nam = str.nam[8:length(str.nam)]
  ima.nam = str.nam[length(str.nam)]
  str.nam.cre = str.nam[-length(str.nam)]
  str.nam.cre = str.nam.cre[c(2,3)]
  
  path.outDir.ful = paste0( sav.IMA,
                            "/",
                            str.nam.cre[1],
                            "/",
                            str.nam.cre[2],
                            "/",
                            ima.nam)
  
  TES = 0 # !!! test output folder----
  if (TES == 0) {
    path.outDir = sav.IMA
  } else {
    "NO ALTERNATIVE TO SAVE"
  }

  fol.vec = str.nam.cre
  
  F.create.folder.multiple(path.outDir, fol.vec)
  
  # STANDARD ----
  
  # rectangles coo standard ----
  
  #png(path.outDir.ful,width = 1080, height = 1080) # !!! comment not to save image ----
  
  # layout ----
  
  layout(matrix(c(1,1, 0,2),2,2), widths=c(1,1), heights=c(1,1))
  
  # layout.show(1)
  
  par(mar=c(0,0,0,0))  
  
  # plot standard ----
  
  plot(image.norsta,axes=FALSE)
  
  # plot checks ----
  
  dra.pol.sta.F = function(DF.PL.POL, COL.DRA, CEX.TEX.POL){
    # DF.PL.POL = df.pl.pol.imareg # debug
    # COL.DRA = "orange"
    # CEX.TEX.POL = 1
    
    df.pl.spl = split(DF.PL.POL,DF.PL.POL$tpg)
    
    for (i in 1:length(df.pl.spl)){
      df.pl.spl[[i]] = df.pl.spl[[i]][order(df.pl.spl[[i]]$tpg_ord),]   
    }
    
    for (i in 1:length(df.pl.spl)){
      rect(
        df.pl.spl[[i]]$tracox[1],
        df.pl.spl[[i]]$tracoy[2],
        df.pl.spl[[i]]$tracox[2],
        df.pl.spl[[i]]$tracoy[1],
        border = COL.DRA
      )
    }
    
    for (i in 1:length(df.pl.spl)){
      text(df.pl.spl[[i]]$tracox[1]+25,
           df.pl.spl[[i]]$tracoy[1]+25,
           df.pl.spl[[i]]$tecano,
           col = COL.DRA,
           cex = CEX.TEX.POL
      )
    }
    
  }
  
  cex.tex = 2
  
  # polygon bounding box
  
  dra.pol.sta.F (df.pl.pol.keypo1,"blue",cex.tex)
  
  # polygon registration area
  
  dra.pol.sta.F (df.pl.pol.imareg,"blue",cex.tex)
  
  # keypoints
  
  points(
    df.kp.lis.fil$coo.keypo1$tracox,
    df.kp.lis.fil$coo.keypo1$tracoy,
    pch = 21,
    bg ="green",
    col="green",
    cex = 0.5
    
  )
  
  # analysis lines
  
  abline(v =df.kp.lis.fil$coo.keypo1$tracox[2],
         col = "red",lwd = 0.5, lty = 2)
  abline(v =df.kp.lis.fil$coo.keypo1$tracox[3],
         col = "blue",lwd = 0.5, lty = 2)
  
  abline(h = (df.kp.lis.fil$coo.keypo1$tracoy[10]-
                df.kp.lis.fil$coo.keypo1$tracoy[11])/2+
           df.kp.lis.fil$coo.keypo1$tracoy[10],
         col = "magenta",lwd = 0.5, lty = 2)
  
  # CROPPED ----
  
  plot(image.invcro,axes = FALSE)
  
  grid(10, 10, col = "lightgray", lty = "dotted",
       lwd = 0.5, equilogs = TRUE)
  
  #dev.off()# !!! comment not to save image ----
  
  print (path.outDir.ful)
  print (e)
  
  ###__________ case check standard & cropped ready ====

}

###xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx====
###__________________________________ MLOOP ready ====

