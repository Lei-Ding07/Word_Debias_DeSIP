import numpy as np

def get_emb(vec_file):
    f = open(vec_file, 'r', errors='ignore')
    contents = f.readlines()[1:]
    word_emb = {}
    vocabulary = {}
    vocabulary_inv = {}
    for i, content in enumerate(contents):
        content = content.strip()
        tokens = content.split(' ')
        word = tokens[0]
        vec = tokens[1:]
        vec = np.array([float(ele) for ele in vec])
        word_emb[word] = np.array(vec)
        vocabulary[word] = i
        vocabulary_inv[i] = word

    return word_emb, vocabulary, vocabulary_inv