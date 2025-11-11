##############################################################
# R02_MMNL_independent_BASE.R
# Versi awal (yang sempat jalan): ASC + time + cost
# - Build long dari wide
# - avail diset 1 (empat alternatif lengkap)
# - Draws kecil, nCores=1
##############################################################

## 0) Reset & init
rm(list = ls(all.names = TRUE))
invisible(gc())

library(apollo)
apollo_initialise()
set.seed(1234)

## 1) Baca data WIDE
# GANTI path berikut sesuai file kamu
wide_path <- "DATA/apollo_modeChoiceData.csv"
wide <- read.csv(wide_path, stringsAsFactors = FALSE)

## 2) Normalisasi 'choice' di wide → 1..4 (car=1, bus=2, air=3, rail=4)
alt_map <- c(car=1L, bus=2L, air=3L, rail=4L)
if (is.numeric(wide$choice) && all(wide$choice %in% 1:4, na.rm=TRUE)) {
  wide$choice_num <- as.integer(wide$choice)
} else if (is.character(wide$choice)) {
  tmp <- tolower(wide$choice)
  wide$choice_num <- unname(alt_map[tmp])
} else {
  stop("Kolom 'choice' harus 1..4 atau car/bus/air/rail.")
}

## 3) task_id dari SP_task (prioritas) atau RP_journey, else indeks per ID
if ("SP_task" %in% names(wide)) {
  wide$task_id <- paste0("SP_", wide$SP_task)
} else if ("RP_journey" %in% names(wide)) {
  wide$task_id <- paste0("RP_", wide$RP_journey)
} else {
  o <- order(wide$ID)
  wide <- wide[o,]
  wide$task_id <- ave(wide$ID, wide$ID, FUN = seq_along)
}

## 4) Build LONG (1 baris per alternatif), minimal: time + cost
alts <- c("car","bus","air","rail")

one_alt <- function(w, alt){
  tcol <- paste0("time_", alt)
  ccol <- paste0("cost_", alt)
  data.frame(
    ID      = w$ID,
    task_id = w$task_id,
    alt     = alt,
    time    = as.numeric(w[[tcol]]),
    cost    = as.numeric(w[[ccol]]),
    # dummy 0/1: alt ini yang dipilih?
    choice_dummy = as.integer(w$choice_num == alt_map[[alt]]),
    stringsAsFactors = FALSE
  )
}

long <- do.call(rbind, lapply(alts, function(a) one_alt(wide, a)))
long <- long[ long$alt %in% alts & !is.na(long$time) & !is.na(long$cost), ]

## 5) Keep hanya set lengkap (4 alt) & ubah dummy → choice 1..4
key <- paste(long$ID, long$task_id)
tab <- table(key)
valid_keys <- names(tab)[tab==4L]
db <- long[key %in% valid_keys, ]

chosen <- db[ db$choice_dummy==1, c("ID","task_id","alt") ]
if (nrow(chosen)>0) chosen <- unique(chosen)
chosen$choice <- unname(alt_map[ chosen$alt ])
chosen$alt <- NULL

db <- merge(db, chosen, by=c("ID","task_id"), all.x=TRUE)
db <- db[ !is.na(db$choice), ]

key2 <- paste(db$ID, db$task_id)
tab2 <- table(key2)
valid2 <- names(tab2)[tab2==4L]
db <- db[key2 %in% valid2, ]

cat("Rows ready:", nrow(db), "\n")
print(table(db$alt))
print(table(db$choice))

## 6) Expose ke Apollo
database <- db

apollo_control <- list(
  modelName  = "MMNL_independent_BASE",
  modelDescr = "MMNL independent (ASC + time + cost)",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 1
)

## 7) Draws kecil + random coeff independen
apollo_draws <- list(
  interDrawsType = "halton",
  interNDraws    = 200,                 # kecil & cepat
  interUnifDraws = c(),
  interNormDraws = c("draws_time","draws_cost")
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs){
  rc <- list()
  rc$b_time <- mu_time + sigma_time * draws_time
  rc$b_cost <- mu_cost + sigma_cost * draws_cost
  rc
}

## 8) Starting values (moderate)
apollo_beta <- c(
  asc_bus     = -1.0,
  asc_air     = -0.2,
  asc_rail    =  0.3,
  mu_time     = -0.010,
  mu_cost     = -0.080,
  sigma_time  =  0.020,
  sigma_cost  =  0.100
)
apollo_fixed <- c()

## 9) Probabilities: ASC + time + cost, avail=1
if (exists("apollo_probabilities")) rm(apollo_probabilities)

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  V <- list()
  V[["car"]]  = 0 +        b_time*(time*(alt=="car"))  + b_cost*(cost*(alt=="car"))
  V[["bus"]]  = asc_bus +  b_time*(time*(alt=="bus"))  + b_cost*(cost*(alt=="bus"))
  V[["air"]]  = asc_air +  b_time*(time*(alt=="air"))  + b_cost*(cost*(alt=="air"))
  V[["rail"]] = asc_rail + b_time*(time*(alt=="rail")) + b_cost*(cost*(alt=="rail"))
  
  mnl_settings <- list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=1, bus=1, air=1, rail=1),
    choiceVar    = choice,
    V            = V
  )
  
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

## 10) Validate → Estimate
apollo_inputs <- apollo_validateInputs()
invisible(apollo_probabilities(apollo_beta, apollo_inputs, functionality="validate"))
cat("Validation passed.\n")

mmnl_model <- apollo_estimate(apollo_beta, apollo_fixed,
                              apollo_probabilities, apollo_inputs)

apollo_modelOutput(mmnl_model)
# apollo_saveOutput(mmnl_model)  # opsional
