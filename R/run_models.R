library(lme4)
library(car)
library(betapart)
library(future)
library(furrr)
library(geosphere)

# Calcula a similaridade de Bray-Curtis balanceada (1 - bray.bal) entre um par
# de sites (s1, s2), a partir dos dados de abundancia por especie em `data`.
get_bray <- function(s1, s2, data) {

  sp_data <- data |>
    # filter out the SSBS that matches the pair of sites we're interested in
    dplyr::filter(SSBS %in% c(s1, s2)) |>
    # pull out the site name, species name and abundance information
    dplyr::select(SSBS, Taxon_name_entered, Measurement) |>
    # pivot so that each column is a species and each row is a site
    tidyr::pivot_wider(names_from = Taxon_name_entered, values_from = Measurement) |>
    # set the rownames to the SSBS and then remove that column
    tibble::column_to_rownames("SSBS")

  # if one of the sites doesn't have any individuals in it (row sum is 0)
  if (sum(rowSums(sp_data) == 0, na.rm = TRUE) == 1) {
    # then the similarity between sites should be 0
    bray <- 0
    # if both sites have no individuals
  } else if (sum(rowSums(sp_data) == 0, na.rm = TRUE) == 2) {
    # then class the similarity as NA
    bray <- NA
    # otherwise if both sites have individuals, calculate the balanced bray-curtis
    # as similarity (1-bray)
  } else {
    bray <- 1 -
      betapart::bray.part(sp_data) |>
      purrr::pluck("bray.bal") |>
      purrr::pluck(1)
  }

  bray
}

# Inverso do logit ajustado usado para a transformacao da similaridade
# composicional (f = valor a ser retro-transformado, a = ajuste usado na
# transformacao original).
inv_logit <- function(f, a) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}

# Estima os modelos de Abundancia Total (ab_m) e Similaridade Composicional
# (cd_m) para um data frame `diversity` ja filtrado/reclassificado por
# process_diversity(), e salva as predicoes em Output/predicted_values_<label>.rds.
# `label` identifica a combinacao de Realm/Biome (ex.: "NE_neotropic", "NE_global").
run_diversity_models <- function(diversity, label, output_dir = "Output",
                                  workers = max(1, parallel::detectCores() - 1)) {

  # Calculate diversity indices ----
  # Total Abundance ----
  abundance_data <- diversity |>
    # pull out just the abundance measures
    dplyr::filter(Diversity_metric_type == "Abundance") |>
    # group by SSBS (each unique value corresponds to a unique site)
    dplyr::group_by(SSBS) |>
    # now add up all the abundance measurements within each site
    dplyr::mutate(TotalAbundance = sum(Effort_corrected_measurement)) |>
    # ungroup
    dplyr::ungroup() |>
    # pull out unique sites
    dplyr::distinct(SSBS, .keep_all = TRUE) |>
    # now group by Study ID
    dplyr::group_by(SS) |>
    # pull out the maximum abundance for each study
    dplyr::mutate(MaxAbundance = max(TotalAbundance)) |>
    # ungroup
    dplyr::ungroup() |>
    # now rescale total abundance, so that within each study, abundance varies from 0 to 1.
    dplyr::mutate(RescaledAbundance = TotalAbundance / MaxAbundance)

  # Compositional Similarity ----
  cd_data_input <- diversity |>
    # drop any rows with unknown LandUse
    dplyr::filter(!is.na(LandUse)) |>
    # pull out only the abundance data
    dplyr::filter(Diversity_metric_type == "Abundance") |>
    # group by Study
    dplyr::group_by(SS) |>
    # calculate the number of unique sampling efforts within that study
    dplyr::mutate(n_sample_effort = dplyr::n_distinct(Sampling_effort)) |>
    # calculate the number of unique species sampled in that study
    dplyr::mutate(n_species = dplyr::n_distinct(Taxon_name_entered)) |>
    # check if there are any Primary minimal sites in the dataset
    dplyr::mutate(n_primin_records = sum(LandUse == "Primary minimal")) |>
    # ungroup
    dplyr::ungroup() |>
    # now keep only the studies with one unique sampling effort
    dplyr::filter(n_sample_effort == 1) |>
    # and keep only studies with more than one species
    # as these studies clearly aren't looking at assemblage-level diversity
    dplyr::filter(n_species > 1) |>
    # and keep only studies with at least some Primary minimal data
    dplyr::filter(n_primin_records > 0) |>
    # drop empty factor levels
    droplevels()

  # get a vector of each study to loop over
  studies <- cd_data_input |>
    dplyr::distinct(SS) |>
    dplyr::pull()

  site_comparisons <- purrr::map_dfr(
    .x = studies,
    .f = function(x) {

      # filter out the given study
      site_data <- dplyr::filter(cd_data_input, SS == x) |>
        # pull out the SSBS and LandUse information
        dplyr::select(SSBS, LandUse) |>
        # simplify the data so we only have one row for each site
        dplyr::distinct(SSBS, .keep_all = TRUE)

      # pull out the sites that are Primary minimal (we only want to use comparisons with the baseline)
      baseline_sites <- site_data |>
        dplyr::filter(LandUse == "Primary minimal") |>
        dplyr::pull(SSBS)

      # pull out all the sites
      site_list <- site_data |>
        dplyr::pull(SSBS)

      # get all site x site comparisons for this study
      site_comparisons <- expand.grid(baseline_sites, site_list) |>
        # rename the columns so they will be what the compositional similarity function expects for ease
        dplyr::rename(s1 = Var1, s2 = Var2) |>
        # remove the comparisons where the same site is being compared to itself
        dplyr::filter(s1 != s2) |>
        # make the values characters rather than factors
        dplyr::mutate(
          s1 = as.character(s1),
          s2 = as.character(s2),
          # add the full name
          contrast = paste(s1, "vs", s2, sep = "_"),
          # add the study id
          SS = as.character(x)
        )

      return(site_comparisons)
    }
  )

  future::plan("multisession", workers = workers)

  # We're using map2 (because there are two arguments we're passing through - s1 and s2)
  # and the map2_dbl because the output we want is a vector of numbers (double rather than integer format)
  # so this function is going to go through each s1 and s2 in turn
  # and pass them into the get_bray function
  bray <- furrr::future_map2_dbl(
    .x = site_comparisons$s1,
    .y = site_comparisons$s2,
    ~get_bray(s1 = .x, s2 = .y, data = cd_data_input),
    .options = furrr::furrr_options(seed = TRUE)
  )

  # stop running things in parallel for now
  future::plan("sequential")

  # for the other required information, we don't need to run loops
  latlongs <- cd_data_input |>
    # for each site in the dataset
    dplyr::group_by(SSBS) |>
    # pull out the lat and long
    dplyr::summarise(
      Lat = unique(Latitude),
      Long = unique(Longitude)
    )

  lus <- cd_data_input |>
    # for each site in the dataset
    dplyr::group_by(SSBS) |>
    # pull out the land use
    dplyr::summarise(lu = unique(LandUse))

  # now let's put all the data together
  cd_data <- site_comparisons |>
    # add in the bray-curtis data (already in the same order as site_comparisons)
    dplyr::mutate(bray = bray) |>
    # get the lat and long for s1
    dplyr::left_join(latlongs, by = c("s1" = "SSBS")) |>
    dplyr::rename(s1_lat = Lat, s1_long = Long) |>
    # get the lat and long for s2
    dplyr::left_join(latlongs, by = c("s2" = "SSBS")) |>
    dplyr::rename(s2_lat = Lat, s2_long = Long) |>
    # calculate the geographic distances between s1 and s2 sites
    dplyr::mutate(
      geog_dist = geosphere::distHaversine(
        cbind(s1_long, s1_lat), cbind(s2_long, s2_lat)
      )
    ) |>
    # get the land use for s1
    dplyr::left_join(lus, by = c("s1" = "SSBS")) |>
    dplyr::rename(s1_lu = lu) |>
    # get the land use for s2
    dplyr::left_join(lus, by = c("s2" = "SSBS")) |>
    dplyr::rename(s2_lu = lu) |>
    # create an lu_contrast column (what we'll use for modelling)
    dplyr::mutate(lu_contrast = paste(s1_lu, s2_lu, sep = "_vs_"))

  # Run the statistical analysis ----
  # Total Abundance
  # run a simple model
  ab_m <- lme4::lmer(
    sqrt(RescaledAbundance) ~ LandUse + (1 | SS) + (1 | SSB),
    data = abundance_data
  )

  # Compositional Similarity
  # there is some data manipulation we want to do before modelling
  cd_data <- dplyr::mutate(
    cd_data,
    # logit transform the compositional similarity
    logitCS = car::logit(bray, adjust = 0.001, percents = FALSE),
    # log10 transform the geographic distance between sites
    log10geo = log10(geog_dist + 1),
    # make primary minimal-primary minimal the baseline again
    lu_contrast = factor(lu_contrast),
    lu_contrast = relevel(lu_contrast, ref = "Primary minimal_vs_Primary minimal")
  )

  # Model compositional similarity as a function of the land-use contrast and the geographic distance between sites
  cd_m <- lme4::lmer(
    logitCS ~ lu_contrast + log10geo + (1 | SS) + (1 | s2),
    data = cd_data
  )

  # Projecting the model ----
  # Model predictions

  # let's start with the abundance model

  # set up a dataframe with all the levels you want to predict diversity for
  # so all the land-use classes in your model must be in here
  newdata_ab <- data.frame(LandUse = levels(abundance_data$LandUse)) |>
    # now calculate the predicted diversity for each of these land-use levels
    # setting re.form = NA means random effect variance is ignored
    # then square the predictions (because we modelled the square root of abundance, so we have to back-transform it to get the real predicted values)
    dplyr::mutate(ab_m_preds = predict(ab_m, dplyr::across(dplyr::everything()), re.form = NA) ^ 2)

  # now the compositional similarity model

  # once again, set up the dataframe with all the levels you want to predict diversity for
  # because we had an extra fixed effect in this model (log10geo), we also have to set a baseline level for this
  # We're interested in the compositional similarity when we discount natural turnover, so we want to set this to a static value. We'll use 0 here, but we can also set it to the median geographic distance in the original data or any other meaningful level.
  newdata_cd <- data.frame(
    lu_contrast = levels(cd_data$lu_contrast),
    log10geo = 0
  ) |>
    dplyr::mutate(
      cd_m_preds = predict(cd_m, dplyr::across(dplyr::everything()), re.form = NA) |>
        inv_logit(a = 0.001)
    )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  results <- list(ab_m = ab_m, cd_m = cd_m, newdata_ab = newdata_ab, newdata_cd = newdata_cd)

  saveRDS(results, file.path(output_dir, paste0("predicted_values_", label, ".rds")))

  results
}
