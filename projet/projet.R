library(readxl)
library(tidyverse)

# --- 1. CHARGEMENT ET NETTOYAGE ROBUSTE ---
bac_data <- read_excel("fr_en_reussite_au_baccalaureat_origine_sociale.xlsx", 
                       range = "A2:K337")

# Listes de référence (Vérifie bien l'orthographe par rapport à ton Excel)
csp_fav <- c(
  "Cadres, professions intellectuelles supérieures : professeurs et assimilés",
  "Cadres, professions intellectuelles supérieures",
  "Cadres et professions intellectuelles supérieures",
  "Artisans, commerçants et chefs d'entreprise",
  "Artisans, commerçants, chefs d'entreprise",
  "Agriculteurs exploitants",
  "Professions intermédiaires",
  "Professions intermédiaires : instituteurs et assimilés"
)

csp_defav <- c(
  "Ouvriers", 
  "Employés", 
  "Autres personnes sans activité professionnelle"
)

bac_clean <- bac_data %>%
  # NETTOYAGE CRUCIAL : On enlève les espaces invisibles avant et après
  mutate(`Origine sociale` = str_squish(`Origine sociale`)) %>%
  filter(!`Origine sociale` %in% c("Retraités", "Ensemble", "Indéterminé")) %>%
  mutate(
    post_reforme = if_else(Année >= 2021, 1, 0),
    origine_favorisee = case_when(
      `Origine sociale` %in% csp_fav ~ 1,
      `Origine sociale` %in% csp_defav ~ 0,
      TRUE ~ NA_real_ 
    )
  )

# --- 2. DIAGNOSTIC DU PROBLÈME (À exécuter pour comprendre) ---
# On vérifie combien de lignes on a réussi à classer en 0 et en 1
cat("Nombre de lignes classées 'Favorisée (1)' : ", sum(bac_clean$origine_favorisee == 1, na.rm = TRUE), "\n")
cat("Nombre de lignes classées 'Défavorisée (0)' : ", sum(bac_clean$origine_favorisee == 0, na.rm = TRUE), "\n")

# Si le chiffre pour 0 est 0, c'est que la liste csp_defav ne matche pas les données Excel.
# On affiche les CSP qui n'ont pas été classées (sont restées NA) pour voir s'il y a des erreurs
non_classees <- bac_clean %>% 
  filter(is.na(origine_favorisee)) %>% 
  distinct(`Origine sociale`)
print("Catégories non classées (vérifie s'il y a des oublis) :")
print(non_classees)

# --- 3. SUITE DU TRAITEMENT ---
bac_clean <- bac_clean %>% filter(!is.na(origine_favorisee))

bac_long <- bac_clean %>%
  pivot_longer(
    cols = starts_with("Pourcentage"),
    names_to = "type_bac_label",
    values_to = "taux_reussite"
  ) %>%
  mutate(
    # --- CORRECTION DU TYPE DE DONNÉES ---
    # 1. On remplace la virgule par un point (cas fréquent Excel FR)
    taux_reussite = str_replace(taux_reussite, ",", "."),
    # 2. On force la conversion en nombre
    taux_reussite = as.numeric(taux_reussite),
    
    # Nettoyage des labels
    type_bac = case_when(
      str_detect(type_bac_label, "général") ~ "General",
      str_detect(type_bac_label, "technologique") ~ "Technologique",
      str_detect(type_bac_label, "professionnel") ~ "Professionnel"
    )
  )

# Vérification rapide avant le calcul
# Si tu vois "numeric" ou "double", c'est gagné.
print(class(bac_long$taux_reussite)) 

bac_diff <- bac_long %>%
  group_by(type_bac, Année, post_reforme, origine_favorisee) %>%
  summarise(mean_taux = mean(taux_reussite, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = origine_favorisee,
    values_from = mean_taux,
    names_prefix = "origine_"
  ) %>%
  mutate(
    inegalite_reussite = origine_1 - origine_0
  )

# Affichage final
print(head(bac_diff))


# Statistiques descriptives préliminaires = pour repérer s'il y a des tendances parallèles

ggplot(bac_diff, aes(x = Année, y = inegalite_reussite, color = type_bac)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Évolution des inégalités de réussite au bac",
    x = "Année",
    y = "Différence de taux de réussite (favorisés - non favorisés)",
    color = "Type de bac"
  ) +
  theme_minimal()


#Le graphique montre bien des tendances similaires entre 2004 et 2019 entre général et techno donc tendances parallèles ok



# 1. Préparation des données pour le DiD
did_data <- bac_diff %>%
  # CORRECTION ICI : Attention aux Majuscules définies précédemment
  filter(type_bac %in% c("General", "Technologique")) %>% 
  mutate(
    # On définit le groupe traité (ex: le Général)
    treat = if_else(type_bac == "General", 1, 0),
    # On définit la période post-réforme
    post = if_else(Année >= 2021, 1, 0)
  )

# Vérification de sécurité : est-ce qu'on a bien des lignes ?
if(nrow(did_data) == 0) {
  stop("Erreur : Le tableau did_data est vide. Vérifie l'orthographe exacte dans bac_diff$type_bac")
}

# 2. Modèle de Différence de Différences (DiD)
# On utilise une spécification avec Effets Fixes (Année et Type de Bac)
# Le coefficient d'intérêt est l'interaction treat:post
did_model <- lm(
  inegalite_reussite ~ treat:post + factor(type_bac) + factor(Année),
  data = did_data
)

# 3. Affichage des résultats
summary(did_model)

#Le type de bac joue un rôle : les bac techno ont moins d’inégalité de réussite que les bac général.
#L’effet de la réforme réduit l’inégalité de manière importante
#Effet du traitement (DiD) : après le traitement, l’inégalité diminue de 4,21 points dans le groupe traité. Très significatif (p < 0,001).


# REGRESSION SUR DISCONTINUITE

# On filtre pour garder Général (Traité) et Techno (Contrôle)
cits_data <- bac_diff %>%
  filter(type_bac %in% c("General", "Technologique")) %>%
  mutate(
    # Variable de temps centrée (0 = année de la réforme)
    annee_centree = Année - 2021,
    
    # Dummy pour la période après réforme (le saut temporel)
    post = if_else(Année >= 2021, 1, 0),
    
    # Dummy pour le groupe traité (1 = Général, 0 = Techno)
    is_general = if_else(type_bac == "General", 1, 0)
  )

# MODELE
model_cits <- lm(
  inegalite_reussite ~ annee_centree * post * is_general, 
  data = cits_data
)

summary(model_cits)

# La variable post n'est pas significative donc l'effet du covid n'est pas significatif pour les bac techno
# L'intéraction entre les variables post et is_general n'est pas significative. On ne peut donc pas savoir si la réforme du bac a diminué les inégalités. 

# GRAPHIQUE
ggplot(cits_data, aes(x = Année, y = inegalite_reussite, color = type_bac)) +
  # Points réels
  geom_point(alpha = 0.6) +
  
  # Ligne de réforme
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "black") +
  
  # Courbes de tendances (séparées avant/après et par groupe)
  # L'astuce est l'interaction dans 'group'
  geom_smooth(aes(group = interaction(type_bac, post)), 
              method = "lm", 
              se = TRUE) + # se = TRUE affiche l'intervalle de confiance
  
  scale_color_manual(values = c("General" = "#E41A1C", "Technologique" = "#377EB8")) +
  
  labs(
    title = "Impact comparé de la réforme (Général vs Technologique)",
    subtitle = "Discontinuité des inégalités sociales en 2021",
    y = "Écart de réussite (Favorisé - Défavorisé)",
    x = "Année",
    color = "Type de Bac"
  ) +
  theme_minimal()

# Le graphique montre bien qu'on ne peut rien conclure. 