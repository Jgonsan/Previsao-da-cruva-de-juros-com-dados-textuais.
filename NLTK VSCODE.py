# -*- coding: utf-8 -*-
"""
Implementação da biblioteca NLTK
"""

import nltk
nltk.download('punkt')

from nltk.tokenize import sent_tokenize
from textblob import TextBlob
nltk.download('punkt')
nltk.download('wordnet')
nltk.download('averaged_perceptron_tagger')
nltk.download('brown')
import string
import glob
import pandas as pd

import csv
import glob
import re
import string
import sys
import datetime as dt
import matplotlib.pyplot as plt
import datetime as dt
import sys
import pandas as pd
import os
import PyPDF2

# Caminho para os arquivos .txt
ES = r'C:\Users\jhona\OneDrive\Documentos\Web screping\Tutorial\Atas\atas_en\teste\*.txt'

# Obter todos os arquivos .txt
files = glob.glob(ES)

# Listas para armazenar os dados
file_names = []
contents = []

# Loop sobre os arquivos para ler o conteúdo e armazenar
for file in files:
    with open(file, 'r', encoding='utf-8') as f:
        content = f.read()
        file_names.append(file)  # Nome do arquivo
        contents.append(content)  # Conteúdo do arquivo

# Criar um DataFrame
df = pd.DataFrame({
    'file_name': file_names,
    'content': contents
})

# Exibir o DataFrame
print(df)

copom = df
df.describe()


#%% Colocando a data

# Extraindo a data do nome do arquivo
copom['DATE'] = copom['file_name'].apply(lambda x: os.path.basename(x).replace('.txt', ''))

# Convertendo para o formato de data do Pandas
copom['DATE'] = pd.to_datetime(copom['DATE'], format='%Y-%m-%d')

# Somando 07 dias à coluna DATE
copom['DATE'] = copom['DATE'] + pd.Timedelta(days=7)

# Selecionando a última observação de cada mês
copom = copom.sort_values(by='DATE').groupby(copom['DATE'].dt.to_period('M')).tail(1)

# Ajustar a coluna DATE para o último dia do mês
copom['DATE'] = pd.to_datetime(copom['DATE']) + pd.offsets.MonthEnd(0)


copom['content'].describe()


# Função para limpar e processar o texto
def clean_text(text):
    text = text.translate(str.maketrans('', '', string.punctuation + string.digits))  # Remove pontuação e números
    text = text.lower()  # Converte para minúsculas
    return text

# Aplica a função de limpeza em cada célula da coluna 'content'
copom['content'] = copom['content'].apply(clean_text)

_stopwords = ['ME', 'MY', 'MYSELF', 'WE', 'OUR', 'OURS', 'OURSELVES', 'YOU', 'YOUR', 'YOURS',
              'YOURSELF', 'YOURSELVES', 'HE', 'HIM', 'HIS', 'HIMSELF', 'SHE', 'HER', 'HERS', 'HERSELF',
              'IT', 'ITS', 'ITSELF', 'THEY', 'THEM', 'THEIR', 'THEIRS', 'THEMSELVES', 'WHAT', 'WHICH',
              'WHO', 'WHOM', 'THIS', 'THAT', 'THESE', 'THOSE', 'AM', 'IS', 'ARE', 'WAS', 'WERE', 'BE',
              'BEEN', 'BEING', 'HAVE', 'HAS', 'HAD', 'HAVING', 'DO', 'DOES', 'DID', 'DOING', 'AN',
              'THE', 'AND', 'BUT', 'IF', 'OR', 'BECAUSE', 'AS', 'UNTIL', 'WHILE', 'OF', 'AT', 'BY',
              'FOR', 'WITH', 'ABOUT', 'BETWEEN', 'INTO', 'THROUGH', 'DURING', 'BEFORE',
              'AFTER', 'ABOVE', 'BELOW', 'TO', 'FROM', 'UP', 'DOWN', 'IN', 'OUT', 'ON', 'OFF', 'OVER',
              'UNDER', 'AGAIN', 'FURTHER', 'THEN', 'ONCE', 'HERE', 'THERE', 'WHEN', 'WHERE', 'WHY',
              'HOW', 'ALL', 'ANY', 'BOTH', 'EACH', 'FEW', 'MORE', 'MOST', 'OTHER', 'SOME', 'SUCH',
              'NO', 'NOR', 'NOT', 'ONLY', 'OWN', 'SAME', 'SO', 'THAN', 'TOO', 'VERY', 'CAN',
              'JUST', 'SHOULD', 'NOW', 'AMONG']

# Transformando todas as palavras em minúsculas
_stopwords = [word.lower() for word in _stopwords]


# Função para remover stopwords de um texto
def remove_stopwords(text):
    if isinstance(text, str):  # Verifica se o valor é uma string
        text = text.lower()  # Converte para minúsculas
        text = text.split()  # Tokeniza o texto
        text = [word for word in text if word not in _stopwords]  # Remove as stopwords
        text = ' '.join(text)  # Reconstroi o texto sem stopwords
    return text

# Aplicar a função na coluna desejada do DataFrame
copom['content'] = copom['content'].apply(remove_stopwords)

copom['blob'] = copom['content'].apply(lambda x: TextBlob(str(x)))


# Criar uma nova coluna com a polaridade de cada texto
copom['polaridade'] = copom['content'].astype(str).apply(lambda x: TextBlob(x).sentiment.polarity)

# Exibir as primeiras linhas para conferir
copomtextblod = copom[['DATE', 'polaridade']]

copomtextblod.head()

#%% Fazer o filtro

import matplotlib  
import matplotlib.pyplot as plt



plt.figure(figsize=(10, 6))
plt.plot(copomtextblod["DATE"], copomtextblod["polaridade"], linestyle='-', color='b')
plt.title('Evolução da Polaridade das atas do Copom de 2003 ate 2024')
plt.xlabel('Data')
plt.ylabel('Polaridade')
plt.grid(True)
plt.xticks(rotation=45)
plt.show()

copomtextblod.to_excel('polaridade_copom.xlsx', index=False)
    
    
'''
from textblob import TextBlob

# Exemplo de texto
texto = df.iloc[0, 1]

# Criando um objeto TextBlob
blob = TextBlob(texto)

# Analisando o sentimento (polaridade e subjetividade)
polaridade = blob.sentiment.polarity  # -1 a 1, sendo -1 negativo e 1 positivo
subjetividade = blob.sentiment.subjectivity  # 0 a 1, sendo 0 objetivo e 1 subjetivo

print(f"Polaridade: {polaridade}, Subjetividade: {subjetividade}")
'''


#%% FOMC


# Caminho para os arquivos .txt
ES = r'C:\Users\jhona\OneDrive\Documentos\Web screping\Tutorial\Atas\fomc\teste\*.txt'

# Obter todos os arquivos .txt
files = glob.glob(ES)

# Listas para armazenar os dados
file_names = []
contents = []

# Loop sobre os arquivos para ler o conteúdo e armazenar
for file in files:
    with open(file, 'r', encoding='utf-8') as f:
        content = f.read()
        file_names.append(file)  # Nome do arquivo
        contents.append(content)  # Conteúdo do arquivo

# Criar um DataFrame
df = pd.DataFrame({
    'file_name': file_names,
    'content': contents
})

# Exibir o DataFrame
print(df)

fomc = df
df.describe()





#%% Colocando a data

# Transformar a coluna file name na data
fomc['DATE'] = pd.to_datetime(fomc['file_name'].str.extract(r'(\d{8})')[0], format='%Y%m%d')
# Somando 21 dias à coluna DATE
fomc['DATE'] = fomc['DATE'] + pd.Timedelta(days=21)

# Ajustar a coluna DATE para o último dia do mês
fomc['DATE'] = pd.to_datetime(fomc['DATE']) + pd.offsets.MonthEnd(0)


#%% # Função para limpar e processar o texto
def clean_text(text):
    text = text.translate(str.maketrans('', '', string.punctuation + string.digits))  # Remove pontuação e números
    text = text.lower()  # Converte para minúsculas
    return text

# Aplica a função de limpeza em cada célula da coluna 'content'
copom['content'] = copom['content'].apply(clean_text)

_stopwords = ['ME', 'MY', 'MYSELF', 'WE', 'OUR', 'OURS', 'OURSELVES', 'YOU', 'YOUR', 'YOURS',
              'YOURSELF', 'YOURSELVES', 'HE', 'HIM', 'HIS', 'HIMSELF', 'SHE', 'HER', 'HERS', 'HERSELF',
              'IT', 'ITS', 'ITSELF', 'THEY', 'THEM', 'THEIR', 'THEIRS', 'THEMSELVES', 'WHAT', 'WHICH',
              'WHO', 'WHOM', 'THIS', 'THAT', 'THESE', 'THOSE', 'AM', 'IS', 'ARE', 'WAS', 'WERE', 'BE',
              'BEEN', 'BEING', 'HAVE', 'HAS', 'HAD', 'HAVING', 'DO', 'DOES', 'DID', 'DOING', 'AN',
              'THE', 'AND', 'BUT', 'IF', 'OR', 'BECAUSE', 'AS', 'UNTIL', 'WHILE', 'OF', 'AT', 'BY',
              'FOR', 'WITH', 'ABOUT', 'BETWEEN', 'INTO', 'THROUGH', 'DURING', 'BEFORE',
              'AFTER', 'ABOVE', 'BELOW', 'TO', 'FROM', 'UP', 'DOWN', 'IN', 'OUT', 'ON', 'OFF', 'OVER',
              'UNDER', 'AGAIN', 'FURTHER', 'THEN', 'ONCE', 'HERE', 'THERE', 'WHEN', 'WHERE', 'WHY',
              'HOW', 'ALL', 'ANY', 'BOTH', 'EACH', 'FEW', 'MORE', 'MOST', 'OTHER', 'SOME', 'SUCH',
              'NO', 'NOR', 'NOT', 'ONLY', 'OWN', 'SAME', 'SO', 'THAN', 'TOO', 'VERY', 'CAN',
              'JUST', 'SHOULD', 'NOW', 'AMONG']

# Transformando todas as palavras em minúsculas
_stopwords = [word.lower() for word in _stopwords]


# Função para remover stopwords de um texto
def remove_stopwords(text):
    if isinstance(text, str):  # Verifica se o valor é uma string
        text = text.lower()  # Converte para minúsculas
        text = text.split()  # Tokeniza o texto
        text = [word for word in text if word not in _stopwords]  # Remove as stopwords
        text = ' '.join(text)  # Reconstroi o texto sem stopwords
    return text

# Aplicar a função na coluna desejada do DataFrame
fomc['content'] = fomc['content'].apply(remove_stopwords)

fomc['blob'] = fomc['content'].apply(lambda x: TextBlob(str(x)))


# Criar uma nova coluna com a polaridade de cada texto
fomc['polaridade'] = fomc['content'].astype(str).apply(lambda x: TextBlob(x).sentiment.polarity)

# Exibir as primeiras linhas para conferir
copomtextblod_fomc = fomc[['DATE', 'polaridade']]

copomtextblod_fomc.head()

#%% Fazer o filtro

import matplotlib  
import matplotlib.pyplot as plt



plt.figure(figsize=(10, 6))
plt.plot(copomtextblod_fomc["DATE"], copomtextblod_fomc["polaridade"], linestyle='-', color='b')
plt.title('Evolução da Polaridade das atas do Fomc de 2003 ate 2024')
plt.xlabel('Data')
plt.ylabel('Polaridade')
plt.grid(True)
plt.xticks(rotation=45)
plt.show()

copomtextblod_fomc.to_excel('polaridade_fomc.xlsx', index=False)


# -*- coding: utf-8 -*-
"""
Created on Sun Feb  2 00:02:20 2025

@author: jhona
"""

import seaborn as sns
import requests
import pdfplumber
import datetime as dt
import sys
import os
import pandas as pd
import matplotlib.pyplot as plt
import nltk 
from nltk import tokenize
from sklearn.feature_extraction.text import CountVectorizer
from wordcloud import WordCloud 
from string import punctuation
import unidecode
from sklearn.feature_extraction.text import TfidfVectorizer
from nltk import ngrams
import re
from scipy.io import savemat

    


#%% Carregar a base de dados textuais

polaridade_copom = pd.read_excel(r'C:/Users/jhona/OneDrive/Desktop/polaridade_copom.xlsx')
polaridade_fomc = pd.read_excel(r'C:/Users/jhona/OneDrive/Desktop/polaridade_fomc.xlsx')



# Gerar uma lista de datas do último dia de cada mês entre 2003 e 2024
date_range = pd.date_range(start='2003-01-31', end='2024-12-31', freq='M')
# Criar o DataFrame com a coluna de datas
df_last_days = pd.DataFrame(date_range, columns=['DATE'])


# Realizando o merge com o df_last_days como o primeiro item
polaridade_copom = df_last_days.merge(polaridade_copom, on='DATE', how='outer')
polaridade_fomc = df_last_days.merge(polaridade_fomc, on='DATE', how='outer')


# Ordena o dataframe pela coluna 'DATE'
polaridade_copom.sort_values(by='DATE', inplace=True)
polaridade_fomc.sort_values(by='DATE', inplace=True)




# Reinicia os índices para manter a sequência (opcional)
polaridade_copom.reset_index(drop=True, inplace=True)
polaridade_fomc.reset_index(drop=True, inplace=True)




polaridade_copom = polaridade_copom.ffill()
polaridade_fomc = polaridade_fomc.ffill()


# Filtrando os dados entre agosto de 2003 e outubro de 2024
start_date = '2004-01-01'
end_date = '2024-12-31'
polaridade_copom = polaridade_copom[(polaridade_copom['DATE'] >= start_date) & (polaridade_copom['DATE'] <= end_date)]
polaridade_fomc = polaridade_fomc[(polaridade_fomc['DATE'] >= start_date) & (polaridade_fomc['DATE'] <= end_date)]

# Reinicia os índices para manter a sequência (opcional)
polaridade_copom.reset_index(drop=True, inplace=True)
polaridade_fomc.reset_index(drop=True, inplace=True)




polaridade_copom = polaridade_copom.drop(columns=['DATE'])
polaridade_fomc = polaridade_fomc.drop(columns=['DATE'])

from scipy.io import savemat
savemat('polaridade_copom.mat', {'DataTable': polaridade_copom})
savemat('polaridade_fomc.mat', {'DataTable': polaridade_fomc})

savemat('polaridade_copom.mat', polaridade_copom)