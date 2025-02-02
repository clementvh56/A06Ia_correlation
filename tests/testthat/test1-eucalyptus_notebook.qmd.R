# Vérifications de eucalyptus_notebook.qmd
eucalyptus <- parse_rmd("../../eucalyptus_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes eucalyptus_notebook est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("eucalyptus_notebook.qmd"))
  # La version compilée HTML du carnet de notes eucalyptus_notebook est
  # introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.
  
  expect_true(is_rendered_current("eucalyptus_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document eucalyptus_notebook est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Analyse descriptive",
    "Corrélation entre hauteur et circonférence",
    "Discussion et conclusions") %in% (rmd_node_sections(eucalyptus) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes eucalyptus_notebook ne sont
  # pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).
  
  expect_true(all(c("setup", "import", "desccomment", "labelise", "plot",
    "plotcomment", "corrp", "corrs", "corrcomment", "corrtest",
    "corrtestcomment") %in% rmd_node_label(eucalyptus)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent dans
  # eucalyptus_notebook.qmd
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).
  
  expect_true(any(duplicated(rmd_node_label(eucalyptus))))
  # Un ou plusieurs labels de chunks sont dupliqués dans eucalyptus_notebook.qmd
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans eucalyptus_ca ?", {
  expect_true(eucalyptus[[1]]$author != "___")
  expect_true(!grepl("__", eucalyptus[[1]]$author))
  expect_true(grepl("^[^_]....+", eucalyptus[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.
  
  expect_true(grepl("[a-z]", eucalyptus[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
  
  expect_true(grepl("[A-Z]", eucalyptus[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import', 'desccomment', 'labelise' : importation des données `eucalyptus`", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `eucalyptus` importé ne sont pas celles
  # attendues
  # Votre jeu de données de départ n'est pas correct.
  
  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `eucalyptus` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.
  
  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `eucalyptus` est incorrect
  # Vérifiez le chunk d'importation des données `import`.
  
  expect_true(is_identical_to_ref("desccomment"))
  # Les commentaires sur les données sont(partiellement) faux dans le chunk
  # 'corrcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
  
  expect_true(is_identical_to_ref("labelise", "names"))
  # Les colonnes dans le tableau `eucalyptus` importé ne sont pas celles
  # attendues
  # Votre jeu de données labélisé n'est pas correct.
  
  expect_true(is_identical_to_ref("labelise", "classes"))
  # La nature des variables (classe) dans le tableau `eucalyptus` est incorrecte
  # Vérifiez le chunk `labelise`.
  
  expect_true(is_identical_to_ref("labelise", "nrow"))
  # Le nombre de lignes dans le tableau `eucalyptus` est incorrect
  # Vérifiez le chunk `labelise`.
})

test_that("Chunks 'plot', 'plotcomment' : graphique des données", {
  expect_true(is_identical_to_ref("plot"))
  # Le graphique produit par le chunk 'plot' n'est pas celui attendu.
  # Vérifiez le graphique réalisé qui doit être un nuage de points de la hauteur
  # en fonction de la circonférence.
  
  expect_true(is_identical_to_ref("plotcomment"))
  # Les commentaires sur le graphique sont (partiellement) faux dans le chunk
  # 'plotcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'corrp', 'corrs', 'corrcomment' : corrélation entre hauteur et circonférencte", {
  expect_true(is_equal_to_ref("corrp"))
  # Le coefficient de corrélation de Pearson dans le chunk 'corrp' n'est pas
  # celui attendu
  # Vérifiez votre code, et en particulier, que vous utilisez bien la fonction
  # cor() pour le calculer et non correlation() plutôt réservée pour les
  # tableaux de corrélation impliquant trois variables ou plus.

  expect_true(is_equal_to_ref("corrs"))
  # Le coefficient de corrélation de Spearman dans le chunk 'corrs' n'est pas
  # celui attendu
  # Vérifiez votre code, et en particulier, que vous utilisez bien la fonction
  # cor() pour le calculer et non correlation() plutôt réservée pour les
  # tableaux de corrélation impliquant trois variables ou plus.
  # Vérifiez aussi que vous avez indiqué que c'est le coefficient de Spearman
  # que vous souhaitez obtenir

  expect_true(is_identical_to_ref("corrcomment"))
  # L'interprétation des coefficients de corrélation est (partiellement) fausse
  # dans le chunk 'corrcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'corrtest', 'corrtestcomment' : test de corrélation de Pearson", {
  expect_true(is_equal_to_ref("corrtest"))
  # Le calcul du test d'hypothèse dans le chunk 'corrtest' n'est pas celui
  # attendu
  # Vérifiez que vous réalisez un test de corrélation selon Pearson.
  # Vérifiez la formule que vous utilisez pour spécifier vos variables.
  # Vérifiez enfin si vous devez utiliser un test bilatéral, unilatéral à gauche
  # ou unilatéral à droite.
  
  expect_true(is_identical_to_ref("corrtestcomment"))
  # L'interprétation du test d'hypothèse de la corrélation de Pearson est
  # (partiellement) fausse dans le chunk 'corrtestcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(eucalyptus, by_section("Discussion et conclusions")) |>
      as_document() |> grepl("...Votre discussion ici...", x = _,
        fixed = TRUE) |> any()))
  # La discussion et les conclusions ne sont pas faites
  # Remplacez "...Votre discussion ici..." par vos phrases de commentaires
  # libres (à noter que le contenu de cette section n'est pas évalué
  # automatiquement, mais il le sera par vos enseignants).
})
