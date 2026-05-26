import pandas as pd
import numpy as np
import os

variaveis = [
    "TR","TM","Tresp","ER","TRPV",
    "PV","NC","P_final_graus","P_final_cm","ER_1sub"
]

pasta = "dados_brutos"
n_participantes = 40

resultados = {v: [] for v in variaveis}


def ler_arquivo(caminho):
    if not os.path.exists(caminho):
        return None

    df = pd.read_csv(
        caminho,
        sep="\t",
        header=None,
        skiprows=0
    )

    df = df.astype(str)
    df = df.applymap(lambda x: float(x.replace(",", ".")))
    df = df.iloc[:, :10]
    df.columns = variaveis
    
    # Substituir TR < 150 por NaN em TODAS as variáveis
    df.loc[(df["TR"] < 150) | (df["TR"] > 600) | (df["TM"] < 300), variaveis] = np.nan
    
    return df


for p in range(1, n_participantes+1):

    grupo = "GAG" if p <= 20 else "GCG"

    # =================
    # AQUISIÇÃO
    # =================
    caminho_aq = os.path.join(pasta, f"Participante_{p}_aq")

    df_aq = ler_arquivo(caminho_aq)

    if df_aq is None:
        continue

    # criar blocos de 6 tentativas (antes de qualquer remoção)
    df_aq["bloco"] = (np.arange(len(df_aq)) // 6) + 1

    # calcular médias (já ignora NaN)
    medias_aq = df_aq.groupby("bloco")[variaveis].mean()

    # =================
    # RETENÇÃO
    # =================
    caminho_ret = os.path.join(pasta, f"Participante_{p}_ret")

    df_ret = ler_arquivo(caminho_ret)

    if df_ret is not None:
        df_ret["bloco"] = 11
        medias_ret = df_ret.groupby("bloco")[variaveis].mean()
        medias = pd.concat([medias_aq, medias_ret])
    else:
        medias = medias_aq

    for var in variaveis:
        linha = {
            "Participante": p,
            "Grupo": grupo
        }

        for b in medias.index:
            linha[f"Bloco_{b}"] = medias.loc[b, var]

        resultados[var].append(linha)


# =================
# salvar planilhas
# =================

for var in variaveis:
    df_saida = pd.DataFrame(resultados[var])
    df_saida.to_excel(f"{var}.xlsx", index=False)

