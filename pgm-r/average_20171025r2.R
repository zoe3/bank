pred_rpart <- read.csv("../submit/kawasoe_submit_20171011_1_rpart.csv", header=FALSE)
pred_logi <- read.csv("../submit/kawasoe_submit_20171018r2_1_logi.csv", header=FALSE)

pred_rpart2 <- read.csv("../submit/harada_submit_1025_1_rpart.csv", header=FALSE)
pred_logi2 <- read.csv("../submit/harada_submit_1025_1_logi.csv", header=FALSE)

out <- data.frame(pred_rpart[1],(pred_rpart[2]+pred_logi[2]+pred_rpart2[2]*2+pred_logi2[2]*2)/6)

str(out)

write.table(out, #出力データ
            "../submit/submit_20171025_3_ensemble_average.csv", #出力先
            quote=FALSE, #文字列を「"」で囲む有無
            col.names=FALSE, #変数名(列名)の有無
            row.names=FALSE, #行番号の有無
            sep="," #区切り文字の指定
)

