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
    'F.list.s3.aws.recursive',
    'F.random.names'
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
par_out_config = par_out_config[,c('nam','neu.sck.rc4cat_nc01_chrman__catalogue_tespro_rc4')]
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
(par_sys_out_bu1 = as.vector (par_out_config$par_sys_out_bu1))
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
### CASE EXPERIMENTAL ____________________________====

df.files = list.files(sav.DAT, recursive = T, full.names = T)
df.files = df.files [grep('B__',df.files)]
df.files.ren = gsub('/home/hylmarj/ws/scratch_neu-sck-rc4cat_nc01_chrman__catalogue_tespro_rc4__95e83a27-7544-4e0c-8180-19fe1c4ccbbf/dat/tespro/B__experiment/','',df.files)

# group by path or other available info ----

df.files.groups = strsplit (df.files.ren,'/')

groups = lapply(df.files.groups,'[',1)
groups = unlist (unique(groups))

names.groups = list()
for (i in seq_along(groups)){
  names.groups[[i]] = df.files.ren [grep(groups[[i]],df.files.ren)]  
}
names(names.groups) = groups

for (i in seq_along(names.groups)){
  names.groups[[i]] = df.files.ren [grep(groups[[i]],df.files.ren)]  
}

names.groups.split = lapply(names.groups, function(x) strsplit (x,'/'))
names.groups.split = lapply(names.groups.split, function(x) do.call (rbind, x))

names.groups.split.bind = do.call (rbind, names.groups.split)

# add new names with use of random names function ----

neu_names_random_temp <- read_csv("ws/temp/neu_names_random - temp.csv")

names.groups.split.bind = merge (neu_names_random_temp,neu_names_random_temp [,c(2,3)])
colnames (names.groups.split.bind) = c('id', 'acronym', 'group')

df.files.rename = data.frame (df.files,basename (df.files))
as.tibble (df.files.rename)
colnames (df.files.rename) = c('pathA','id')

df.files.rename.merge = merge (df.files.rename,names.groups.split.bind)
(df.files.rename.merge = as.tibble (df.files.rename.merge))

df.files.rename.merge$pathB = paste0(
  sav.IMA
  ,'/experiment/'
  , df.files.rename.merge$acronym
  , paste0 ('.',gsub('.*\\.','',df.files.rename.merge$id))
  )

as.data.frame (df.files.rename.merge)

# rename files in local ----

file.rename(
  as.vector (df.files.rename.merge$pathA),
  as.vector (df.files.rename.merge$pathB)
  )

# metainfo to json ----

df.files.rename.merge.json = df.files.rename.merge
colnames (df.files.rename.merge.json)

df.files.rename.merge.json = df.files.rename.merge.json[,c('id','pathB','acronym','group')]
df.files.rename.merge.json$ima_metinf = 
  gsub('\\.*...$','.json',df.files.rename.merge.json$pathB)
df.files.rename.merge.json$ima_basnam = basename (df.files.rename.merge.json$pathB)
df.files.rename.merge.json$ima_typ = gsub('.*\\.','',df.files.rename.merge.json$ima_basnam)
df.files.rename.merge.json$ima_nam = gsub('\\.*...$','',df.files.rename.merge.json$ima_basnam)

df.files.rename.merge.json = df.files.rename.merge.json [,c('ima_nam','ima_typ','group','id','ima_basnam')]
colnames(df.files.rename.merge.json) = c('ima_namful','ima_typ','gro_act','ima_namori','ima_nambas')

df.files.rename.merge.json$gro_act = gsub('plumline','plumbline',df.files.rename.merge.json$gro_act)

df.files.rename.merge.json = split(
  df.files.rename.merge.json,
  df.files.rename.merge.json$ima_namful
  )

i = 1
for ( i in seq_along(df.files.rename.merge.json)){
  sav.pat.tem = paste0(sav.IMA,'/','experiment','/',df.files.rename.merge.json[[i]]$ima_nambas)
  sav.pat.tem = gsub('\\.*...$','.json',sav.pat.tem)
  writeLines(
    jsonlite::toJSON(df.files.rename.merge.json[[i]],pretty = TRUE),
    sav.pat.tem)
}

# copy files to aws ----

df.files = list.files(sav.IMA, recursive = T, full.names = T)
df.files.aws = gsub(sav.IMA,par_sys_out_bu1,df.files)

aws.tra = paste0(
  'aws s3 cp --profile ',par_sys_aws_3_profile ,' ',
  df.files,
  ' ',
  df.files.aws,
  ' --acl public-read'
)


for (i in seq_along(aws.tra)) {
  #i=1
  system(aws.tra[[i]])
  print (aws.tra[[i]])
}

###______________________ case experimental ready ====

###_______________________________________________====
### CASE VERIFIED ________________________________====

# verified ----

gre.pat = paste(
c('20190702095433','20190702095434','20190702095435','20190804171150','20190804171152','20190804171154'),
collapse = '|'
)

t(par_out_config)

df.files = list.files('~/datastore/AlfaDWbh6/ext-imareg/rc2_20190924', recursive = T, full.names = T)
df.files = df.files[grep(gre.pat, df.files)]

# copy files to aws ----

df.files.aws = gsub(
  '/home/hylmarj/datastore/AlfaDWbh6/ext-imareg/rc2_20190924/staana.alfa1@gmail.com/',
  paste0 (par_sys_out_bu1,'/','verified','/'),
  df.files
  )

aws.tra = paste0(
  'aws s3 cp --profile ',par_sys_aws_3_profile ,' ',
  df.files,
  ' ',
  df.files.aws,
  ' --acl public-read'
)


for (i in seq_along(aws.tra)) {
  #i=1
  system(aws.tra[[i]])
  print (aws.tra[[i]])
}

###______________________ case verified ready ====























