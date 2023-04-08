import re
import pandas as pd
import numpy as np


def remplacer_libelles(dataframe_base, dataframe_nomenclature, code_colonne, libelle_colonne):
    dictionnaire_nomenclature = dict(zip(dataframe_nomenclature[code_colonne], dataframe_nomenclature[libelle_colonne]))
    dataframe_base[libelle_colonne] = dataframe_base[code_colonne].map(dictionnaire_nomenclature).fillna(dataframe_base[libelle_colonne])
    return dataframe_base


def clean_libelle(libelle):
    libelle = libelle.upper()
    libelle = re.sub(r'[^\w\s]', ' ', libelle)
    libelle = re.sub(r'\s+', ' ', libelle)
    accents = {'É': 'E', 'È': 'E', 'À': 'A'}
    for accent, lettre in accents.items():
        libelle = libelle.replace(accent, lettre)
    libelle = re.sub(r'(^|\b)ADJ\b', r'\1ADJOINT', libelle)
    libelle = re.sub(r'\bAGT\b', 'AGENT', libelle)
    libelle = re.sub(r'\bCL\b', 'CLASSE', libelle)
    libelle = re.sub(r'\bSUP\b', 'SUPERIEUR', libelle)
    libelle = re.sub(r'\bLABO\b', 'LABORATOIRE', libelle)
    libelle = re.sub(r'\bPUER\b', 'PUERICULT', libelle)
    libelle = re.sub(r'\bINF\b', 'INFIRMIER', libelle)
    libelle = re.sub(r'\bTEC\b', 'TECHNIQUE', libelle)
    libelle = re.sub(r'\bTECH\b', 'TECHNIQUE', libelle)
    libelle = re.sub(r'\bTECHN\b', 'TECHNIQUE', libelle)
    libelle = re.sub(r'\bADM\b', 'ADMINISTRATIF', libelle)
    libelle = re.sub(r'\bADMI\b', 'ADMINISTRATIF', libelle)
    libelle = re.sub(r'\bADMINIS\b', 'ADMINISTRATIF', libelle)
    libelle = re.sub(r'\bADMIN\b', 'ADMINISTRATIF', libelle)
    libelle = re.sub(r'\b1ERE\b', 'PREMIERE', libelle)
    libelle = re.sub(r'\b2EME\b', 'DEUXIEME', libelle)
    libelle = libelle.strip()
    return libelle


def function_similarity(lib_ind, lib_nom):
    if lib_ind == "miss":
        return np.nan
    else:
        str1 = set([*lib_ind])
        str2 = set([*lib_nom])
        intersection = str1.intersection(str2)
        union = str1.union(str2)
        return len(intersection)/len(union)
    

def approxim_libelle(lib_ind, set_nomenclature):
    if lib_ind == "miss":
        print(f"{lib_ind} --> {np.nan}")
        #return np.nan
    else:
        similarities = dict()
        for libe_nom in set_nomenclature:
            similarity_index = function_similarity(lib_ind, libe_nom)*100
            if similarity_index == 100:
                return libe_nom
            else:
                similarities[libe_nom] = similarity_index
        best = sorted(similarities.items(), key=lambda x: x[1], reverse=True)
        print(f"{lib_ind} --> {best[0][0]}")
        #return best[0][0
        

    
def remplacer_code(df_base, df_nomenclature, code, libelle):
    dict_nomenclature = dict(zip(df_nomenclature[libelle],df_nomenclature[code]))
    df_base[code] = df_base[libelle].map(dict_nomenclature).fillna(df_base[code])
    return df_base


