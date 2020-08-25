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
    'F.create.folder.multiple',
    'F.copy.s3.aws.to.local.recursive',
    'F.list.s3.aws.recursive'
  )
)


'%!in%' <- function(x, y) ! ('%in%'(x, y))

#' external libraries

library(dplyr)
library(dbplyr)
library(tidyr)
library(jsonlite)
library(rjson)
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
par_out_config = par_out_config[,c('nam','neu.sck.rc4ano_nb01_chrman__annotate_online_rc4')]
par_out_config.tem = t(as.data.frame(unlist (par_out_config[,2])))
colnames(par_out_config.tem) = as.vector (unlist (par_out_config[,1]))
par_out_config = as.data.frame (par_out_config.tem)
rownames(par_out_config) = NULL
t(par_out_config)

# define parameters ----

(par_sys_pronambas = as.vector (par_out_config$par_sys_pronambas))
(d4m_hid_sck = as.vector (par_out_config$d4m_hid_sck))

(par_sys_aws_1_profile = as.vector (par_out_config$par_sys_aws_1_profile))
(par_sys_aws_2_profile = as.vector (par_out_config$par_sys_aws_2_profile))
(par_sys_aws_3_profile = as.vector (par_out_config$par_sys_aws_3_profile))
(par_sys_inp_bu1 = as.vector (par_out_config$par_sys_inp_bu1))

# prepare save path ----

(sav.A.pat.hom = paste0 (ENV.path, "/ws/"))

(sav.B.pat.aws.tem = paste0 ('scratch_', par_sys_pronambas,"__", d4m_hid_sck))

# remove scratch for refresh RECURSIVE ----
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

# copy from aws ----

F.copy.s3.aws.to.local.recursive (sav.B.pat.aws.tem,par_sys_inp_bu1,par_sys_aws_1_profile)

df.path = gsub ('/dat','/json', sav.DAT)
df.files = list.files(df.path, recursive = T, full.names = T)

df.lis.1.att = rjson::fromJSON(paste(readLines(df.files[5]), collapse=""))
(df.lis.1.att.nam = names(df.lis.1.att))

df.lis.1.2.imgsup = df.lis.1.att$imgsup
(df.lis.1.2.imgsup.len = length (df.lis.1.2.imgsup))

(df.lis.1.2.imgsup.nam = names (df.lis.1.2.imgsup[[1]]))

df.lis.1.2.imgsup = df.lis$df.lis.1.att[[1]]
df.lis.1.2.imgsup.nam = names (df.lis.1.2.imgsup)


df.x = lapply(df.files,
              function(i) {
                fromJSON(
                  file = i
                )
              })
