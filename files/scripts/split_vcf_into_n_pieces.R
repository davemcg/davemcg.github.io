#!/usr/bin/env Rscript
# run examples:
# 1. Rscript split_vcf_into_n_pieces.R yourVCF.header
# 2. Rscript split_vcf_into_n_pieces.R yourVCF.header 200 vcf_region_split_200_coords.txt 3e9
# if you want tab separate values
# 3. Rscript split_vcf_into_n_pieces.R yourVCF.header 200 vcf_region_split_200_coords.txt 3e9 bed

# header file is generate as follows (replace vcf.gz with whatever you file is):
# `zcat EGAD00001002656.GATK.vcf.gz | head -n 1000 | grep ^## > yourVCF.header`

args = commandArgs(trailingOnly=TRUE)

header_file = args[1]
pieces_desired = as.numeric(args[2])
output_file = args[3]
genome_size = as.numeric(args[4])
output_type = args[5]

# default values, if not given by user
if (is.na(pieces_desired)) { pieces_desired = 100 }
if (is.na(output_file )) { output_file = 'vcf_region_split_100_coords.txt' }
if (is.na(genome_size)) { genome_size = 3.23483e9 } # human genome size in base pairs
if (is.na(output_type)) { output_type = 'snakemake' } # defaults to snakemake also takes 'bed' which prints tab separate chr star stop values for -R in bcftools view 

library(tidyverse)
library(stringr)
vcf_header = scan(header_file, what='character', quiet = T)
vcf_header[grepl('contig',vcf_header)]


## Parse out chr / contig sizes
# turn into data frame (well, a tibble)
contig_size <- vcf_header[grepl('contig', vcf_header)] %>% 
  data.frame() %>% 
  select(1, 'header' = 1) %>% 
  # separate by ,
  separate(header, c('contig','length','assembly'),',') %>% 
  # extract values by splitting against = and taking the last element (first after reversing)
  rowwise() %>% 
  mutate(contig = str_split(contig,'=')[[1]] %>% gsub('>','',.) %>% rev() %>% .[[1]],
         length = str_split(length,'=')[[1]] %>% gsub('>','',.) %>% rev() %>% .[[1]] %>% as.numeric(),
         assembly = str_split(assembly,'=')[[1]] %>% gsub('>','',.) %>% rev() %>% .[[1]])


## function to split chr above (genome_size / pieces_desired) base pairs into equal(ish) size pieces
# `ceiling` will allow intervals a bit less than (genome_size / pieces_desired) by rounding up the number of pieces per chromsome. Would rather have more splits with less than the target size. 
n_split <- function(size){
  pieces <- ceiling(size / (genome_size / pieces_desired))
  seq(1, size, size/pieces)
}

## function to print coordinates given a chromosome / contig
n_printer <- function(chr) {
  # grab the legnth of chr or contig
  size <- contig_size %>% dplyr::filter(contig == chr) %>% pull(length)
  # split into ~30e7 sized pieces
  sequence <- n_split(size)
  # add the max size to end (plus another base pair since the loop below reduces size by 1 to eliminate overlaps)
  sequence <- c(sequence, size+1)
  df <- data.frame()
  for(i in 1:length(sequence)){
    row <- cbind(chr, as.integer(sequence[max(i-1,1)]), # for first row, makes sure you don't pick the 0 position, which doesn't exit
                 as.integer(sequence[i]-1)) # decrements by one so you don't overlap
    df <- rbind(df, row)
  }
  colnames(df) <- c('chr','start','end')
  # skip first row which has dummy values
  df[-1,]
}

## calc your new coordinates
regions <- data.frame()
for (i in contig_size %>% dplyr::filter(length > (genome_size / pieces_desired), contig != 'hs37d5') %>% pull(contig)){
  regions <- rbind(regions,(n_printer(i)))
}

# write out results
if (output_type == 'snakemake') {
  write(regions %>% mutate(f = paste(paste(chr, start, sep =':'), end, sep='-')) %>% pull(f), file=output_file)
  write(contig_size %>% dplyr::filter(length < (genome_size / pieces_desired), contig != 'hs37d5') %>% pull(contig) %>% paste(., collapse=','), file=output_file, append = T)
} else {
  write(regions %>% mutate(f = paste(paste(chr, start, sep ='\t'), end, sep='\t')) %>% pull(f), file=output_file)
  write(contig_size %>% dplyr::filter(length < (genome_size / pieces_desired), contig != 'hs37d5') %>% pull(contig) %>% paste(., collapse=','), file=output_file, append = T)
}