import pandas as pd
from googletrans import Translator, constants

df = pd.read_csv('translate.csv')
print(df)

translator = Translator()


a = []
for i in df['English']:
    translation = translator.translate(i, dest="es")
    a.append(translation.text)

for i in a :
  df['translated_phrase']= a

df.to_csv('t2.csv')
