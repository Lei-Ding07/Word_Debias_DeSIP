# Word_Debias_DeSIR

## Word Embeddings via Causal Inference: Gender Bias Reducing and Semantic Information Preserving (AAAI 2022)

### Our paper Word Embeddings via Causal Inference: Gender Bias Reducing and Semantic Information Preserving is accepted by AAAI 2022. The arXiv version of the paper: https://arxiv.org/abs/2112.05194

This repo contains the code for the paper.

---
Our debiased embedding:

U-Desip: https://drive.google.com/file/d/1xIbODofL-rGhH70uH0Kh7WSGAsFyD18t/view?usp=sharing
P-Desip: https://drive.google.com/file/d/1Q14-FLXmBn6K3pUUiHpv-D6kKhoVuHJ-/view?usp=sharing
You can use the load_embedding.py file to load the embeddings.

---

The original GloVe embedding (pre-trained on 2017 January dump of English Wikipedia) can be downloaded here: https://drive.google.com/file/d/1w7tl3xSg69L1zURTpQ6qDg0uXab_XKFw/view?usp=sharing 


R_codes contains R code for calculate screening and Y_hat for P-DeSIP and U-DeSIP and Semantic information preservation experiment.

residual notebook for calucate residual Y_delta_ortho and final output of debiased embedding.

Wordlist document contains word list and corresponding word vectors(from original GloVe).

Evaluation folder and notebook calculate result for experiments section.

Document DownStreamTasks contains code for training the downstream task, including POS, POS chunking and NER.



---
This work was supported by the Economic and Social Research Council (ESRC ES/T012382/1) and the Social Sciences and Humanities Research Council (SSHRC 2003-
2019-0003) under the scheme of the Canada-UK Artificial
Intelligence Initiative. The project title is BIAS: Responsible AI for Gender and Ethnic Labour Market Equality
