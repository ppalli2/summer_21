setwd("/Users/pranavpalli/Desktop/summer_21")
set.seed(133)
library("DESeq2")

table_count <- read.table(file = "./data/all_raw_table_WRI1_062920202.txt", header = TRUE, row.names=1)
table_cond <- read.csv(file = "./data/sample_info.csv", header = TRUE, row.names=1)

table_cond$time <- factor(table_cond$time)
table_cond$wr1 <- factor(table_cond$wr1)
table_cond$dgat <- factor(table_cond$dgat)
table_cond$kas <- factor(table_cond$kas)

countdata1 <- table_count[ , rownames(table_cond)]
rn_f <- rownames(countdata1)

##############################################
#Full model 
##############################################

# This full model doesn't work
#dds <- DESeqDataSetFromMatrix(countData = countdata1, colData = table_cond,
#                              design = ~ wr1+dgat+kas+time+wr1:dgat
#                              +wr1:kas+wr1:time+dgat:time+kas:time
#                              +wr1:dgat:time+wr1:kas:time)

dds <- DESeqDataSetFromMatrix(countData = countdata1, colData = table_cond,
                              design = ~ wr1+time+dgat+kas+wr1:time+dgat:time+kas:time)

ddsTC <- DESeq(dds, fitType='local')
resTC <- results(ddsTC)
resTC <- resTC[order(resTC$padj),]
write.csv(as.data.frame(resTC), file = "./results/starter_code_results/full_model_result.csv")

#highlight Wr1's impact to the system 
###############################
ddsLRT <- DESeq(dds, fitType='local', test="LRT", reduced= ~ time+dgat+kas+dgat:time+kas:time)
resLRT <- results(ddsLRT)
resLRT <- resLRT[order(resLRT$padj),]
write.csv(as.data.frame(resLRT), file = "./results/starter_code_results/wr1_sig.csv")

#highlight Dgat's impact to the system 
###############################
ddsLRT <- DESeq(dds, fitType='local', test="LRT", reduced= ~ time+wr1+dgat+wr1:time+dgat:time)
resLRT <- results(ddsLRT)
resLRT <- resLRT[order(resLRT$padj),]
write.csv(as.data.frame(resLRT), file = "./results/starter_code_results/dgat_sig.csv")

#highlight Kas's impact to the system 
###############################
ddsLRT <- DESeq(dds, fitType='local', test="LRT", reduced= ~ time+wr1+kas+wr1:time+kas:time)
resLRT <- results(ddsLRT)
resLRT <- resLRT[order(resLRT$padj),]
write.csv(as.data.frame(resLRT), file = "./results/starter_code_results/kas_sig.csv")


