#' Create an .onload function to set package options during load
#'
#' - getRad.key_prefix is the default prefix used when setting or getting
#' secrets using keyring.
#' - getRad.user_agent is the string used as a user agent for the http calls
#' generated in this package. It incorporates the package version using
#' `getNamespaceVersion`.
#' - getRad.max_cache_age_seconds is the default max cache age for the httr2
#' cache in seconds.
#' - getRad.max_cache_size_bytes is the default max cache size for the httr2
#' cache in bytes.
#' @noRd
.onLoad <- function(libname, pkgname) {
  # nolint
  op <- options()
  op.getRad <- list(
    getRad.key_prefix = "getRad_",
    getRad.user_agent = paste(
      "R package getRad",
      getNamespaceVersion("getRad")
    ),
    getRad.aloft_data_url = "https://aloftdata.s3-eu-west-1.amazonaws.com",
    getRad.nexrad_data_url = "https://unidata-nexrad-level2.s3.amazonaws.com",
    getRad.birdcast_vpts_data_url = "https://birdcastdata.s3.amazonaws.com",
    getRad.cache = cachem::cache_mem(
      max_size = 128 * 1024^2,
      max_age = 60^2 * 24
    ),
    getRad.vpts_col_types = list(
      radar = vroom::col_factor(),
      datetime = vroom::col_datetime(),
      height = vroom::col_integer(),
      u = vroom::col_double(),
      v = vroom::col_double(),
      w = vroom::col_double(),
      ff = vroom::col_double(),
      dd = vroom::col_double(),
      sd_vvp = vroom::col_double(),
      gap = vroom::col_logical(),
      eta = vroom::col_double(),
      dens = vroom::col_double(),
      dbz = vroom::col_double(),
      dbz_all = vroom::col_double(),
      n = vroom::col_integer(),
      n_dbz = vroom::col_integer(),
      n_all = vroom::col_integer(),
      n_dbz_all = vroom::col_integer(),
      rcs = vroom::col_double(),
      sd_vvp_threshold = vroom::col_double(),
      vcp = vroom::col_integer(),
      radar_longitude = vroom::col_double(),
      radar_latitude = vroom::col_double(),
      radar_height = vroom::col_integer(),
      radar_wavelength = vroom::col_double(),
      source_file = vroom::col_character()
    )
  )
  toset <- !(names(op.getRad) %in% names(op))
  if (any(toset)) {
    options(op.getRad[toset])
  }
  rlang::run_on_load()
  invisible()
}
rlang::on_load(rlang::local_use_cli(inline = TRUE))

# References

vptsReferences <- c(
  "ecog-04003" = bibentry(
    bibtype = "Misc",
    key = "ecog-04003",
    author = c(
      person(given = "Cecilia", family = "Nilsson"),
      person(given = "Adriaan", family = "Dokter"),
      person(given = "Liesbeth", family = "Verlinden"),
      person(given = "Judy", family = "Shamoun-Baranes"),
      person(given = "Baptiste", family = "Schmid"),
      person(given = "Peter", family = "Desmet"),
      person(given = "Silke", family = "Bauer"),
      person(given = "Jason", family = "Chapman"),
      person(given = c("Jose", "A."), family = "Alves"),
      person(given = c("Phillip", "M."), family = "Stepanian"),
      person(given = "Nir", family = "Sapir"),
      person(given = "Charlotte", family = "Wainwright"),
      person(given = "Mathieu", family = "Boos"),
      person(given = "Anna", family = "G\u00F3rska"),
      person(given = c("Myles", "H.", "M."), family = "Menz"),
      person(given = "Pedro", family = "Rodrigues"),
      person(given = "Hidde", family = "Leijnse"),
      person(given = "Pavel", family = "Zehtindjiev"),
      person(given = "Robin", family = "Brabant"),
      person(given = "G\u00FCnther", family = "Haase"),
      person(given = "Nadja", family = "Weisshaupt"),
      person(given = "Micha\u0142", family = "Ciach"),
      person(given = "Felix", family = "Liechti")
    ),
    title = "Supplementary material for 'Revealing patterns of nocturnal migration using the European weather radar network'",
    month = "apr",
    year = "2018",
    publisher = "Zenodo",
    doi = "10.5281/zenodo.1172801",
    url = "https://doi.org/10.5281/zenodo.1172801"
  ),
  uva = bibentry(
    bibtype = "Misc",
    key = "uva",
    author = c(
      person(given = "Judy", family = "Shamoun-Baranes"),
      person(given = "Berend-Christiaan", family = "Wijers"),
      person(given = "Bart", family = "Kranstauber"),
      person(given = "Bart", family = "Hoekstra"),
      person(given = "Pieter", family = "Huybrechts"),
      person(given = c("Adriaan", "M."), family = "Dokter"),
      person(given = "Hidde", family = "Leijnse"),
      person(given = "Maarten", family = "Reyniers"),
      person(given = "Klaus", family = "Stephan"),
      person(given = "Peter", family = "Desmet")
    ),
    title = "UVA_VPTS - Vertical profiles of biological targets derived from weather radars in Belgium, Germany and the Netherlands",
    month = "jan",
    year = "2025",
    publisher = "Research Institute for Nature and Forest (INBO)",
    doi = "10.5281/zenodo.14711244",
    url = "https://doi.org/10.5281/zenodo.14711244"
  ),
  baltrad = bibentry(
    bibtype = "Misc",
    key = "baltrad",
    author = c(
      person(given = "Peter", family = "Desmet"),
      person(given = "Judy", family = "Shamoun-Baranes"),
      person(given = "Bart", family = "Kranstauber"),
      person(given = c("Adriaan", "M."), family = "Dokter"),
      person(given = "Nadja", family = "Weisshaupt"),
      person(given = "Baptiste", family = "Schmid"),
      person(given = "Silke", family = "Bauer"),
      person(given = "G\u00FCnther", family = "Haase"),
      person(given = "Bart", family = "Hoekstra"),
      person(given = "Pieter", family = "Huybrechts"),
      person(given = "Hidde", family = "Leijnse"),
      person(given = "Nicolas", family = "No\u00E9"),
      person(given = "Stijn", family = "Van Hoey"),
      person(given = "Berend-Christiaan", family = "Wijers"),
      person(given = "Cecilia", family = "Nilsson")
    ),
    title = "BALTRAD_VPTS - Vertical profiles of biological targets derived from European weather radars",
    month = "jan",
    year = "2025",
    publisher = "Research Institute for Nature and Forest (INBO)",
    doi = "10.5281/zenodo.14711024",
    url = "https://doi.org/10.5281/zenodo.14711024"
  ),
  dark_ecology = bibentry(
    bibtype = "Article",
    key = "dark_ecology",
    author = c(
      person(given = "Daniel", family = "Sheldon"),
      person(given = "Kevin", family = "Winner"),
      person(given = "Iman", family = "Deznabi"),
      person(given = "Garrett", family = "Bernstein"),
      person(given = "Pankaj", family = "Bhambani"),
      person(given = "Tsung-Yu", family = "Lin"),
      person(given = "Peter", family = "Desmet"),
      person(given = c("Adriaan", "M."), family = "Dokter"),
      person(given = c("Kyle", "G."), family = "Horton"),
      person(given = "Cecilia", family = "Nilsson"),
      person(given = c("Benjamin", "M."), family = "Van Doren"),
      person(given = "Andrew", family = "Farnsworth"),
      person(given = c("Frank", "A."), family = "La Sorte"),
      person(given = "Subhransu", family = "Maji")
    ),
    title = "The Dark Ecology Dataset: Measurements of Aerial Biomass in US Weather Radar from 1995 to 2025",
    year = "2026",
    doi = "10.64898/2026.06.20.733536",
    publisher = "Cold Spring Harbor Laboratory",
    abstract = "The US NEXRAD radar network has monitored the aerosphere over the US and its territories continuously since the 1990s and archived nearly 300 million radar volume scans. These data contain a wealth of information about the movements of birds, bats, and insects. Historically, this biological information was difficult to access due to the amount of data and challenges in analyzing it. In the last 15 years, fueled by computational and methodological advances, large-scale aeroecology research has blossomed. However, comprehensive analyses of the NEXRAD archive remain very costly. We collected measurements of biological activity from every volume scan in the NEXRAD archive{\\textemdash}nearly 300 million data files total{\\textemdash}to assemble a dataset of aerial biomass over the US from 1995 to 2025. The core data are vertical profiles, which summarize biological activity at different heights above the radar station for each volume scan. We also provide time series data products that aggregate vertical profiles to point measurements at radar stations across time. These data products can support a range of aeroecology analyses at significantly reduced effort.Competing Interest StatementThe authors have declared no competing interest.U.S. National Science Foundation, https://ror.org/021nxhr62, 1661259, 1661329, 2210979Swiss National Science Foundation, https://ror.org/00yjd3n13, 31BD30_216840Belgian Federal Science Policy Office, RT/24/HiRADNetherlands Organisation for Applied Scientific Research, https://ror.org/01bnjb948, EP.1512.22.003Academy of Finland, 359864Leon Levy Foundation, https://ror.org/033hnyq61Lyda Hill Philanthropies, https://ror.org/032sf2845",
    url = "https://www.biorxiv.org/content/early/2026/06/23/2026.06.20.733536",
    eprint = "https://www.biorxiv.org/content/early/2026/06/23/2026.06.20.733536.full.pdf",
    journal = "bioRxiv"
  )
)


# see ?usethis::use_release_issue() #nolint
release_bullets <- function() {
  c(
    "Update codemeta.json with: `codemetar::write_codemeta()`",
    "Update CITATION.cff with `cffr::cff_write(dependencies = FALSE)`
    (after incrementing version)"
  )
}
