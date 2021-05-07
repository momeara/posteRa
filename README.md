# posteRa
R interface to the PostEra Manifold API


## install

    install.packages("remotes")
    remotes::install_github("momeara/posteRa")
    
## basic usage

    library(posteRa)
    smiles <- "'O=C(O)CN1CN(c2ccccc2)C2(CCN(C(=O)OCC3c4ccccc4-c4ccccc43)CC2)C1=O"
    API_KEY <- "################"
    posteRa::med_chem_alerts(smiles = smiles, api_key = API_KEY)
    
