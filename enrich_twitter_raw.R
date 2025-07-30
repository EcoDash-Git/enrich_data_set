#!/usr/bin/env Rscript
# ──────────────────────────────────────────────────────────────────────────────
#  enrich_twitter_raw.R
#  ─────────────────────────────────────────────────────────────────────────────
#  • downloads   twitter_raw               (existing table)
#  • appends     main_id  + tweet_type  + logical flags
#  • uploads to  twitter_raw_plus_flags   (overwrite)
# -----------------------------------------------------------------------------
#  Runs ad‑hoc or inside a GitHub Action.  All credentials are expected in
#  environment variables – NO hard‑coding in the script.
# ──────────────────────────────────────────────────────────────────────────────

## 0 – packages ----------------------------------------------------------------
need <- c("DBI", "RPostgres", "dplyr", "stringr", "tibble", "purrr")  # ← added purrr
new  <- need[!need %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(need, library, character.only = TRUE))

## 1 – connect to Supabase (as Postgres) ---------------------------------------
trim_env <- function(var) trimws(Sys.getenv(var, unset = ""))

creds <- c(
  SUPABASE_HOST = trim_env("SUPABASE_HOST"),
  SUPABASE_PORT = trim_env("SUPABASE_PORT"),
  SUPABASE_DB   = trim_env("SUPABASE_DB"),
  SUPABASE_USER = trim_env("SUPABASE_USER"),
  SUPABASE_PWD  = trim_env("SUPABASE_PWD")
)


if (any(!nzchar(creds)))
  stop("❌ One or more Supabase env vars are missing – aborting.")

# sanity check  ───────────────────────────────────────────────────────────────
cat("\n--- Supabase env vars --------------------------------\n")
safe_print <- function(key) {
  val <- Sys.getenv(key)
  if (key == "SUPABASE_PWD") val <- sub(".+", "********", val)  # mask pwd
  cat(key, "=", val, "\n")
}
purrr::walk(names(creds), safe_print)
cat("--------------------------------------------------------\n")

## 2 – DB connection -----------------------------------------------------------
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = creds[["SUPABASE_HOST"]],
  port     = as.integer(creds[["SUPABASE_PORT"]]),
  dbname   = creds[["SUPABASE_DB"]],
  user     = creds[["SUPABASE_USER"]],
  password = creds[["SUPABASE_PWD"]],
  sslmode  = "require"
)



## 2 – download twitter_raw ----------------------------------------------------
twitter_raw <- DBI::dbReadTable(con, "twitter_raw")
cat("✓ downloaded", nrow(twitter_raw), "rows from twitter_raw\n")

## 3 – canonical‑ID lookup -----------------------------------------------------
main_ids <- tibble::tribble(
  ~username,            ~main_id,
  "weave_db",           "1206153294680403968",
  "OdyseeTeam",         "1280241715987660801",
  "ardriveapp",         "1293193263579635712",
  "redstone_defi",      "1294053547630362630",
  "everpay_io",         "1334504432973848577",
  "decentlandlabs",     "1352388512788656136",
  "KYVENetwork",        "136377177683878784",
  "onlyarweave",        "1393171138436534272",
  "ar_io_network",      "1468980765211955205",
  "Permaswap",          "1496714415231717380",
  "communitylabs",      "1548502833401516032",
  "usewander",          "1559946771115163651",
  "apus_network",       "1569621659468054528",
  "fwdresearch",        "1573616135651545088",
  "perma_dao",          "1595075970309857280",
  "Copus_io",           "1610731228130312194",
  "basejumpxyz",        "1612781645588742145",
  "AnyoneFDN",          "1626376419268784130",
  "arweaveindia",       "1670147900033343489",
  "useload",            "1734941279379759105",
  "protocolland",       "1737805485326401536",
  "aoTheComputer",      "1750584639385939968",
  "ArweaveOasis",       "1750723327315030016",
  "aox_xyz",            "1751903735318720512",
  "astrousd",           "1761104764899606528",
  "PerplexFi",          "1775862139980226560",
  "autonomous_af",      "1777500373378322432",
  "Liquid_Ops",         "1795772412396507136",
  "ar_aostore",         "1797632049202794496",
  "FusionFiPro",        "1865790600462921728",
  "vela_ventures",      "1869466343000444928",
  "beaconwallet",       "1879152602681585664",
  "VentoSwap",          "1889714966321893376",
  "permawebjournal",    "1901592191065300993",
  "Botega_AF",          "1902521779161292800",
  "samecwilliams",      "409642632",
  "TateBerenbaum",      "801518825690824707",
  "ArweaveEco",         "892752981736779776"
)

## 4 – enrich dataframe --------------------------------------------------------
twitter_enriched <- twitter_raw %>%
  left_join(main_ids, by = "username") %>%                 # add main_id
  mutate(
    is_rt_text = stringr::str_detect(text, "^RT @"),
    
    tweet_type = dplyr::case_when(
      is_retweet | is_rt_text                                 ~ "retweet",
      user_id == main_id & !is_rt_text &
        stringr::str_detect(text, "https://t.co")             ~ "quote",
      user_id == main_id                                      ~ "original",
      TRUE                                                    ~ "other"
    ),
    
    # logical flags
    is_quote         = tweet_type == "quote",
    is_retweet       = tweet_type == "retweet",
    is_originaltweet = tweet_type == "original",
    is_other         = tweet_type == "other"
  )

cat("✓ added main_id, tweet_type and logical flags\n")

## 5 – upload to new table -----------------------------------------------------
dest_tbl <- "twitter_raw_plus_flags"  # change if desired
DBI::dbWriteTable(
  con,
  name      = dest_tbl,
  value     = twitter_enriched,
  overwrite = TRUE,
  row.names = FALSE
)

cat("✓ uploaded", nrow(twitter_enriched),
    "rows to table", dest_tbl, "\n")

DBI::dbDisconnect(con)
cat("✓ finished at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
