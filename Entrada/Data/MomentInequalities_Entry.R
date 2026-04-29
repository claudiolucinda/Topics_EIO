# ================================================================
# Moment Inequalities Estimator for Discrete Entry Game
# Wal-Mart vs. Kmart — Jia (2008) Data
#
# Baseado em:
#   Pakes, Porter, Ho e Ishii (2005)
#   Ciliberto e Tamer (2009)
#   Ellickson e Misra (2011)
#
# Este arquivo implementa:
#   1. Estimação por desigualdades de momento (minimização de Q)
#   2. Inferência por bootstrap (limiar c_n via Romano & Shaikh)
#   3. Gráfico da função critério projetada em delta
#
# Modelo de lucros (eq. 1 do paper):
#   pi_im = alpha_i'*Xm + gamma_i'*Zim - delta * y_{-im} + eps_im
#
# Desigualdades de momento:
#   Firma i entrou  =>  E[ pi_i(theta) * z ] >= 0
#   Firma i não entrou  =>  E[ -pi_i(theta) * z ] >= 0
#
# Robusto à multiplicidade de equilíbrios: as desigualdades valem
# para QUALQUER equilíbrio selecionado nos dados.
# ================================================================

library(parallel)   # bootstrap paralelo

# ================================================================
# 0. CONFIGURAÇÕES GLOBAIS
# ================================================================

set.seed(42)

BOOT_REPS  <- 200    # réplicas bootstrap (aumente para 500+ em produção)
ALPHA      <- 0.05   # nível de significância para conjunto identificado
N_GRID_D   <- 60     # pontos no grid de log(delta) para projeção

# ================================================================
# 1. CARREGAR DADOS
# ================================================================
setwd("G:/Meu Drive/Aulas/GV/Curso de OI - Pós/Mini Curso USP/Topics_EIO/Entrada/Data")

jiadat <- read.csv("jiadata2R.csv", header = TRUE)

nmkts <- nrow(jiadat)
ints  <- rep(1, nmkts)

build_Xmats <- function(dat) {
  n   <- nrow(dat)
  one <- rep(1, n)
  Wx  <- cbind(one, dat$population, dat$SPC, dat$urban,
               dat$dBenton, dat$southern)
  Kx  <- cbind(one, dat$population, dat$SPC, dat$urban,
               dat$MidWest)
  list(Wx = Wx, Kx = Kx,
       WalMart = dat$WalMart,
       Kmart   = dat$Kmart)
}

base_data <- build_Xmats(jiadat)

# Referência global (usada pelas funções abaixo)
Wxmat   <- base_data$Wx
Kxmat   <- base_data$Kx
WalMart <- base_data$WalMart
Kmart   <- base_data$Kmart


# ================================================================
# 2. FUNÇÕES CORE  (aceitam dados como argumento para o bootstrap)
# ================================================================

# --- Lucro predito ---
# pi_i(theta) avaliado no que a rival observadamente fez.
# Nota: delta entra como -delta * y_{-i}, pois competição reduz lucros.

compute_profits <- function(theta, Wx, Kx, W_obs, K_obs) {
  theta.W <- c(theta[1], theta[3:5], theta[6:7])
  theta.K <- c(theta[2], theta[3:5], theta[8])
  delta   <- exp(theta[9])

  pi.W <- as.vector(Wx %*% theta.W) - delta * K_obs
  pi.K <- as.vector(Kx %*% theta.K) - delta * W_obs

  list(pi.W = pi.W, pi.K = pi.K)
}

# --- Momentos ---
# 8 desigualdades (4 por firma), todas escritas como >= 0:
#   Entrou:     E[ pi * z  | entrou ]  >= 0
#   Não entrou: E[ -pi * z | não entrou ] >= 0
# Instrumentos: z1 = population/mean, z2 = SPC/mean

compute_moments <- function(theta, Wx, Kx, W_obs, K_obs,
                            z1, z2) {
  pr   <- compute_profits(theta, Wx, Kx, W_obs, K_obs)
  pi.W <- pr$pi.W
  pi.K <- pr$pi.K

  W1 <- W_obs;  W0 <- 1 - W_obs
  K1 <- K_obs;  K0 <- 1 - K_obs

  c(
    mean( pi.W * z1 * W1),   # m1: W entrou,    z1
    mean( pi.W * z2 * W1),   # m2: W entrou,    z2
    mean(-pi.W * z1 * W0),   # m3: W não entrou, z1
    mean(-pi.W * z2 * W0),   # m4: W não entrou, z2
    mean( pi.K * z1 * K1),   # m5: K entrou,    z1
    mean( pi.K * z2 * K1),   # m6: K entrou,    z2
    mean(-pi.K * z1 * K0),   # m7: K não entrou, z1
    mean(-pi.K * z2 * K0)    # m8: K não entrou, z2
  )
}

# --- Função critério Q ---
# Q(theta) = sum_j [ min(m_j, 0) ]^2
# Zero quando todas as desigualdades satisfeitas.

make_criterion <- function(Wx, Kx, W_obs, K_obs) {
  # Instrumentos normalizados (calculados uma vez por dataset)
  z1 <- W_obs  # será substituído abaixo — apenas estrutura
  z1 <- jiadat$population / mean(jiadat$population)
  z2 <- jiadat$SPC        / mean(jiadat$SPC)

  # Se estivermos num bootstrap, recalcular com os índices corretos
  # (a normalização usa a média do subsample bootstrap)
  if (!identical(W_obs, WalMart)) {
    # Índices não disponíveis diretamente — usamos a média do subsample
    # como proxy; alternativa: passar os dados brutos e recalcular
    z1 <- Wx[, 2] / mean(Wx[, 2])   # coluna 2 = population
    z2 <- Wx[, 3] / mean(Wx[, 3])   # coluna 3 = SPC
  }

  function(theta) {
    m <- compute_moments(theta, Wx, Kx, W_obs, K_obs, z1, z2)
    sum(pmin(m, 0)^2)
  }
}

# Função critério para os dados originais
criterion_Q <- make_criterion(Wxmat, Kxmat, WalMart, Kmart)


# ================================================================
# 3. ESTIMAÇÃO — GRID SEARCH + REFINAMENTO LOCAL
# ================================================================

cat("================================================================\n")
cat("PASSO 1: Grid Search\n")
cat("================================================================\n")

# Ponto de referência (próximo ao NFXP para ancoragem dos params fixos)
ref <- c(-4.90, -15.38, 1.67, 0.93, 2.47, -1.46, 2.03, 2.13, 0.69)

# Grid sobre intercepts e log(delta)
grid_iW    <- seq(-8,  -2,  length.out = 15)
grid_iK    <- seq(-22, -10, length.out = 15)
grid_ldelta <- seq(-0.5, 2,  length.out = 15)

total_pts <- length(grid_iW) * length(grid_iK) * length(grid_ldelta)
cat(sprintf("Avaliando %d pontos no grid...\n", total_pts))

grid_results <- data.frame(iW = NA_real_, iK = NA_real_,
                            ldelta = NA_real_, Q = NA_real_)[0, ]
best_Q     <- Inf
best_theta <- ref
counter    <- 0L

for (iW in grid_iW) {
  for (iK in grid_iK) {
    for (ld in grid_ldelta) {
      counter       <- counter + 1L
      theta_try     <- ref
      theta_try[1]  <- iW
      theta_try[2]  <- iK
      theta_try[9]  <- ld
      Q_val         <- criterion_Q(theta_try)
      grid_results  <- rbind(grid_results,
                             data.frame(iW = iW, iK = iK,
                                        ldelta = ld, Q = Q_val))
      if (Q_val < best_Q) {
        best_Q     <- Q_val
        best_theta <- theta_try
      }
    }
  }
  cat(sprintf("\r  Progresso: %.0f%%",
              100 * counter / total_pts))
}
cat(sprintf("\n  Melhor Q no grid: %.6f\n\n", best_Q))

# --- Refinamento local ---
cat("PASSO 2: Refinamento local (BFGS)\n")
mi_res    <- optim(best_theta, criterion_Q, method = "BFGS",
                   control = list(maxit = 5000, reltol = 1e-12,
                                  trace = 0))
theta_hat <- mi_res$par
Q_hat     <- mi_res$value
delta_hat <- exp(theta_hat[9])
cat(sprintf("  Q(theta_hat) = %.8f\n", Q_hat))
cat(sprintf("  delta_hat    = %.4f\n\n", delta_hat))


# ================================================================
# 4. INFERÊNCIA POR BOOTSTRAP  (Romano & Shaikh, 2010)
# ================================================================
# Algoritmo:
#   Para cada réplica b:
#     1. Amostrar nmkts mercados com reposição -> dados_b
#     2. Construir Q_b(theta) com dados_b
#     3. Minimizar Q_b(theta) -> Q_b*
#     4. Registrar: stat_b = sqrt(nmkts) * Q_b*
#
#   c_n = quantil (1-alpha) de { stat_b }
#
#   Conjunto identificado: { theta : Q(theta) <= Q_hat + c_n/sqrt(nmkts) }
#
# Interpretação: c_n captura a variabilidade amostral de Q.
# Valores de theta que violam as desigualdades por mais do que
# essa variabilidade são excluídos do conjunto.

cat("================================================================\n")
cat(sprintf("PASSO 3: Bootstrap (B = %d réplicas)\n", BOOT_REPS))
cat("================================================================\n")

# parLapply funciona em Windows e Unix (ao contrário de mclapply,
# que depende de fork e não roda no Windows).
n_cores <- max(1L, detectCores() - 1L)
cat(sprintf("  Usando %d núcleo(s)...\n", n_cores))

run_one_boot <- function(b) {
  set.seed(b)
  idx    <- sample(nmkts, nmkts, replace = TRUE)
  dat_b  <- jiadat[idx, ]
  mats_b <- build_Xmats(dat_b)
  Q_b    <- make_criterion(mats_b$Wx, mats_b$Kx,
                            mats_b$WalMart, mats_b$Kmart)
  res_b  <- optim(theta_hat, Q_b, method = "BFGS",
                  control = list(maxit = 2000, reltol = 1e-10,
                                 trace = 0))
  sqrt(nmkts) * res_b$value
}

boot_stats <- if (n_cores > 1) {
  # Criar cluster PSOCK — funciona em Windows e Unix
  cl <- makeCluster(n_cores, type = "PSOCK")
  # Exportar tudo que run_one_boot precisa para cada worker
  clusterExport(cl, varlist = c("nmkts", "jiadat", "build_Xmats",
                                "make_criterion", "compute_moments",
                                "compute_profits", "theta_hat",
                                "Wxmat", "Kxmat", "WalMart", "Kmart"))
  on.exit(stopCluster(cl), add = TRUE)   # garante limpeza mesmo com erro
  cat("  Cluster iniciado. Rodando bootstrap...\n")
  parLapply(cl, seq_len(BOOT_REPS), run_one_boot)
} else {
  # Fallback sequencial com barra de progresso
  lapply(seq_len(BOOT_REPS), function(b) {
    if (b %% 20 == 0)
      cat(sprintf("\r  Bootstrap: %d/%d", b, BOOT_REPS))
    run_one_boot(b)
  })
}
cat("\n")

boot_stats <- unlist(boot_stats)

# Limiar bootstrap
c_n      <- quantile(boot_stats, 1 - ALPHA)
thresh   <- Q_hat + c_n / sqrt(nmkts)

cat(sprintf("  c_n (quantil %.0f%%): %.6f\n", 100*(1-ALPHA), c_n))
cat(sprintf("  Limiar Q <= %.6f\n\n", thresh))


# ================================================================
# 5. CONJUNTO IDENTIFICADO NO GRID
# ================================================================

identified_set <- grid_results[grid_results$Q <= thresh, ]
cat(sprintf("Pontos do grid em Theta_I: %d / %d (%.1f%%)\n\n",
            nrow(identified_set), nrow(grid_results),
            100 * nrow(identified_set) / nrow(grid_results)))


# ================================================================
# 6. PROJEÇÃO DE Q EM DELTA  (função critério projetada)
# ================================================================
# Para cada valor fixo de log(delta), minimizamos Q(theta) sobre
# todos os outros parâmetros. Isso dá:
#
#   Q_proj(delta) = min_{alpha_W, alpha_K, ...} Q(theta)
#
# O conjunto identificado projetado é o intervalo de delta para
# o qual Q_proj(delta) <= thresh.
#
# Nota: a projeção é CONSERVADORA — o intervalo resultante contém
# o intervalo verdadeiro (e pode ser mais largo).

cat("================================================================\n")
cat("PASSO 4: Projeção da função critério em delta\n")
cat("================================================================\n")

ldelta_grid <- seq(-0.3, 2.2, length.out = N_GRID_D)
Q_proj      <- numeric(N_GRID_D)

cat(sprintf("  Minimizando Q sobre %d valores de log(delta)...\n",
            N_GRID_D))

for (j in seq_along(ldelta_grid)) {
  ld_fixed <- ldelta_grid[j]

  # Função com log(delta) fixo — otimiza sobre os outros 8 params
  Q_fixed_delta <- function(par8) {
    theta_full    <- c(par8, ld_fixed)  # par8 = theta[-9]
    criterion_Q(theta_full)
  }

  res_j    <- optim(theta_hat[-9], Q_fixed_delta,
                    method  = "BFGS",
                    control = list(maxit = 1000, reltol = 1e-10,
                                   trace = 0))
  Q_proj[j] <- res_j$value
  cat(sprintf("\r  Progresso: %d/%d", j, N_GRID_D))
}
cat("\n\n")

# Conjunto identificado projetado em delta
delta_grid      <- exp(ldelta_grid)
in_proj_set     <- Q_proj <= thresh
delta_proj_set  <- delta_grid[in_proj_set]

if (length(delta_proj_set) > 0) {
  cat(sprintf("  Conjunto identificado projetado em delta:\n"))
  cat(sprintf("    [%.4f,  %.4f]\n\n",
              min(delta_proj_set), max(delta_proj_set)))
} else {
  cat("  Nenhum ponto no conjunto identificado projetado.\n")
  cat("  Tente ampliar o grid ou aumentar BOOT_REPS.\n\n")
}


# ================================================================
# 7. RESULTADOS
# ================================================================

param_names <- c(
  "Intercept (Wal-Mart)",
  "Intercept (Kmart)",
  "Population",
  "Retail Sales p.c. (SPC)",
  "Urban",
  "Distance to Bentonville (W)",
  "South (W)",
  "MidWest (K)",
  "log(delta)"
)

cat("================================================================\n")
cat("RESULTADOS\n")
cat("================================================================\n\n")

cat(sprintf("  %-32s  %10s\n", "Parâmetro", "theta_hat"))
cat(paste(rep("-", 48), collapse = ""), "\n")
for (j in seq_along(theta_hat)) {
  cat(sprintf("  %-32s  %10.4f\n", param_names[j], theta_hat[j]))
}
cat(sprintf("  %-32s  %10.4f\n", "delta = exp(log_delta)", delta_hat))
cat("\n")

# Momentos em theta_hat
z1_base <- jiadat$population / mean(jiadat$population)
z2_base <- jiadat$SPC        / mean(jiadat$SPC)
m_hat   <- compute_moments(theta_hat, Wxmat, Kxmat,
                            WalMart, Kmart, z1_base, z2_base)

mnames <- c(
  "m1: E[pi_W*pop  | W entrou]  >=0",
  "m2: E[pi_W*SPC  | W entrou]  >=0",
  "m3: E[-pi_W*pop | W ficou]   >=0",
  "m4: E[-pi_W*SPC | W ficou]   >=0",
  "m5: E[pi_K*pop  | K entrou]  >=0",
  "m6: E[pi_K*SPC  | K entrou]  >=0",
  "m7: E[-pi_K*pop | K ficou]   >=0",
  "m8: E[-pi_K*SPC | K ficou]   >=0"
)
cat("Momentos em theta_hat:\n")
cat(sprintf("  %-38s  %9s  %5s\n", "Momento", "Valor", "OK?"))
cat(paste(rep("-", 58), collapse = ""), "\n")
for (j in seq_along(m_hat)) {
  ok <- ifelse(m_hat[j] >= 0, " OK ", "VIOL")
  cat(sprintf("  %-38s  %9.5f  [%s]\n", mnames[j], m_hat[j], ok))
}
cat(sprintf("\n  Q(theta_hat) = %.8f\n", Q_hat))
cat(sprintf("  c_n          = %.6f\n", c_n))
cat(sprintf("  Limiar       = %.8f\n\n", thresh))


# ================================================================
# 8. GRÁFICOS
# ================================================================

pdf("MI_results.pdf", width = 11, height = 5)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1.5))

# ── Gráfico 1: Conjunto identificado nos intercepts ──────────────
grid_agg <- aggregate(Q ~ iW + iK, data = grid_results, FUN = min)
col_pts  <- ifelse(grid_agg$Q <= thresh, "#2db88a", "#d0d0d0")

plot(grid_agg$iW, grid_agg$iK,
     col  = col_pts, pch = 15, cex = 1.3,
     xlab = expression(paste("Intercept Wal-Mart (", alpha[W], ")")),
     ylab = expression(paste("Intercept Kmart (", alpha[K], ")")),
     main = expression(paste("Conj. Identificado — projeção em (",
                             alpha[W], ", ", alpha[K], ")")))
points(theta_hat[1], theta_hat[2],
       pch = 8, cex = 2, col = "black", lwd = 2)
legend("topright",
       legend = c(expression(hat(Theta)[I]), expression(hat(theta))),
       col = c("#2db88a", "black"), pch = c(15, 8),
       pt.cex = c(1.3, 1.5), bty = "n", cex = 0.9)

# ── Gráfico 2: Função critério projetada em delta ─────────────────
# Estratégia de desenho (ordem importa):
#   1. Frame vazio (type="n") para definir escala e eixos
#   2. Faixa vertical do conjunto identificado (fundo)
#   3. Curva tilde(Q) por cima da faixa
#   4. Limiar bootstrap por cima da curva (garante visibilidade)
#   5. Linha vertical de delta_hat por cima de tudo

x_fill <- delta_grid[in_proj_set]
y_hi   <- max(Q_proj) * 1.12
y_lo   <- 0

# Frame vazio — define escala linear e eixos
plot(delta_grid, Q_proj,
     type = "n",
     xlab = expression(delta ~ "(efeito competição)"),
     ylab = expression(tilde(Q)(delta)),
     main = expression(paste("Função critério projetada em ", delta)),
     ylim = c(y_lo, y_hi))

# 1. Faixa vertical: conjunto identificado (desenhada no fundo)
if (length(x_fill) > 1) {
  rect(min(x_fill), y_lo, max(x_fill), y_hi,
       col    = adjustcolor("#2db88a", alpha.f = 0.30),
       border = NA)
}

# 2. Curva tilde(Q) — por cima da faixa
lines(delta_grid, Q_proj, lwd = 2.5, col = "#333333")

# 3. Limiar bootstrap — desenhado POR CIMA da curva preta
abline(h = thresh, lty = 2, col = "#c0392b", lwd = 2)

# 4. delta_hat
abline(v = delta_hat, lty = 4, col = "#2980b9", lwd = 1.8)

legend("topright",
       legend = c(
         expression(tilde(Q)(delta)),
         bquote(hat(delta) == .(round(delta_hat, 3))),
         bquote("limiar  " * alpha == .(ALPHA)),
         expression(hat(Theta)[I] ~ "(faixa)")
       ),
       lty    = c(1,         4,          2,           NA),
       fill   = c(NA,        NA,         NA,           adjustcolor("#2db88a", 0.4)),
       border = c(NA,        NA,         NA,           NA),
       col    = c("#333333", "#2980b9",  "#c0392b",    NA),
       lwd    = c(2.5,       1.8,        2,            NA),
       merge  = FALSE,
       bty    = "n", cex = 0.85)

dev.off()
cat("Gráficos salvos em: MI_results.pdf\n\n")


# ================================================================
# 9. COMPARAÇÃO COM NFXP
# ================================================================

nfxp_ref <- c(-13.17, -20.56, 1.90, 1.61, 1.34,
              -1.03,   0.58,  0.34, log(1.10))

cat("================================================================\n")
cat("COMPARAÇÃO: MI vs. NFXP\n")
cat("(Estimadores não diretamente comparáveis — ver nota no código)\n")
cat("================================================================\n\n")
cat(sprintf("  %-32s  %10s  %10s\n", "Parâmetro", "MI", "NFXP"))
cat(paste(rep("-", 58), collapse = ""), "\n")
for (j in 1:9) {
  cat(sprintf("  %-32s  %10.4f  %10.4f\n",
              param_names[j], theta_hat[j], nfxp_ref[j]))
}
cat(sprintf("  %-32s  %10.4f  %10.4f\n",
            "delta", delta_hat, 1.10))

cat("
================================================================
NOTAS
================================================================

Bootstrap (Romano & Shaikh):
  A estatística de bootstrap é sqrt(M) * Q_b*(theta_hat), onde
  Q_b* é o mínimo de Q sobre os dados bootstrap. O limiar c_n é
  o quantil (1-alpha) dessa distribuição. O conjunto identificado
  é { theta : Q(theta) <= Q_hat + c_n/sqrt(M) }.

Projeção em delta:
  Q_proj(delta) = min_{outros params} Q(theta | log(delta) fixo).
  A projeção é CONSERVADORA: o intervalo projetado em delta
  contém o intervalo do conjunto identificado original.
  Isso é análogo ao que Ciliberto-Tamer (2009) reportam nas
  figuras de projeção do conjunto identificado.

Identificação parcial vs. pontual:
  MI entrega um INTERVALO para delta, não um ponto.
  O intervalo captura tanto incerteza amostral (via c_n)
  quanto incerteza de identificação (via múltiplos equilíbrios).
  NFXP entrega um ponto, mas ao custo de impor uma regra de
  seleção de equilíbrio implícita.
")
