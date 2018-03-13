options(stringsAsFactors = FALSE)
library('grid')
library('gridExtra')
source('config.R')
library('lattice')
library('tm')
library('quanteda')
library('koRpus')
library('RMySQL')
library('ggplot2')
library('parallel')
library('plyr')
library('dplyr')
library('grid')
library('gridExtra')

n.cores = detectCores()

get_tok_set = function(child_name){
    chi_rows = all[all$child == child_name,]
    tot_tok = nrow(chi_rows)    
    chi_rows = chi_rows[chi_rows$chi_index %in% 1:(tot_tok - tot_tok%%512),]
    tot_tok = nrow(chi_rows)    
    chi_rows$tok_grp = sort(rep(1:(tot_tok/512), times = 512))
    tok_512 = split(chi_rows, factor(chi_rows$tok_grp))
    tok_1024 = lapply(1:(length(tok_512)-1), function(x) rbind(tok_512[[x]], tok_512[[x+1]]))
    names(tok_1024) = unlist(lapply(1:length(tok_1024), function(x) median(tok_1024[[x]]$age, na.rm = TRUE)))
    return(tok_1024)
}

tagging_text = function(child_rows){
    tokens = as.character(child_rows$gloss)
    tokens = paste(tokens, collapse = " ")
    tagged.text <- tokenize(tokens, format="obj", lang="en")
    return(tagged.text)
}

compute_inv_K = function(tagged_text){
    return(1/K.ld(tagged_text, detailed=FALSE, char=c(), quiet=TRUE)@K.ld)
}

compute_U = function(child_rows){
    tokens = as.character(child_rows$gloss)
    tokens = paste(tokens, collapse = " ")
    chi_dfm = dfm(tokens)
    chi_lexdiv = unname(lexdiv(x = chi_dfm, measure = "U"))
    return(chi_lexdiv)
 }                      
 
compute_MTLD = function(tagged_text){
      return(MTLD(tagged_text)@MTLD$MTLD)
}

compute_VOCD = function(tagged_text){
	hdd1 = HDD(tagged_text)
	print(hdd1)
	print(names(hdd1))
    return(HDD(tagged_text)@HDD$HDD)
}

ld_metric_1024 = function(tok_1024){#tok_1024 is a dataframe with 1024 rows
    U_LD = compute_U(tok_1024)
    tt = tagging_text(tok_1024)
    inv_K_LD = compute_inv_K(tt)
    MTLD_LD = compute_MTLD(tt)
	vocd_LD = compute_VOCD(tt)
    med_age = median(tok_1024$age, na.rm = TRUE)
    return(data.frame(med_age, U_LD, inv_K_LD, MTLD_LD, vocd_LD))
}
                    
process_one_child = function(child_name){
    pw_results = lapply(get_tok_set(child_name = child_name), ld_metric_1024)
    pw_results_df = do.call("rbind", pw_results)
    return(pw_results_df)
}
