# =============================================================================
# NSCA 0.4.5 手動檢查腳本（只檢查 0.4.5）
#
#   第 1 部分  安裝：把 0.4.5 裝進「暫存」library，不動你原本的套件。
#   第 2 部分  範例：沒有 \dontrun、只有 nsca() 用 \donttest、每個範例計時。
#   第 3 部分  Bug 修正：每一項都印出實際數值，並與「應該得到的值」比對。
#   第 4 部分  文件：版本、日期、NEWS、說明頁內容。
#   第 5 部分  （選用）R CMD check --as-cran。
#
# 使用方式：
#   1. 把 NSCA_0.4.5.tar.gz 放在桌面、下載資料夾，或這個腳本旁邊。
#   2. 在 RStudio 開啟這個檔案按 Source，或在 R 執行：
#        source("完整路徑/NSCA_0.4.5_manual_check.R", encoding = "UTF-8")
#   3. 最後印出總表，每一項是 PASS 或 FAIL。
#
# 建議在新的 R session 執行（RStudio：Session > Restart R），
# 避免已經載入的舊版 NSCA 干擾。
# =============================================================================

## ---- 設定 -------------------------------------------------------------------

# 留 NA 表示自動尋找；也可以直接填完整路徑，例如
#   TARBALL <- "C:/Users/furfa/OneDrive/Desktop/NSCA_0.4.5.tar.gz"
# Windows 路徑請用 / 或 \\，不要用單一個 \。
TARBALL <- NA

# 第 5 部分要不要跑 R CMD check --as-cran（約 2–3 分鐘）
RUN_CRAN_CHECK <- TRUE

# 沒有安裝 LaTeX（MiKTeX / TinyTeX）就保留 "--no-manual"；有的話可以刪掉
CHECK_ARGS <- c("--as-cran", "--no-manual")


## ---- 找到 tarball -----------------------------------------------------------

work <- file.path(tempdir(), "nsca_manual_check")
dir.create(work, showWarnings = FALSE, recursive = TRUE)

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) NA_character_
)
if (is.na(script_dir) && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_dir <- tryCatch({
    path <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(path)) dirname(normalizePath(path)) else NA_character_
  }, error = function(e) NA_character_)
}
home <- Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME")
# Windows 開了 OneDrive 備份時，桌面實際上在 OneDrive 底下
onedrive <- Sys.getenv(c("OneDrive", "OneDriveConsumer", "OneDriveCommercial"))
onedrive <- unique(c(onedrive[nzchar(onedrive)], file.path(home, "OneDrive")))
search_dirs <- unique(stats::na.omit(c(
  getwd(), script_dir,
  file.path(home, c("Desktop", "桌面", "Downloads", "下載")),
  file.path(rep(onedrive, each = 2L), c("Desktop", "桌面"))
)))
search_dirs <- search_dirs[dir.exists(search_dirs)]

if (is.na(TARBALL)) {
  # 允許瀏覽器改過的檔名，例如 "NSCA_0.4.5 (1).tar.gz"
  found <- unlist(lapply(search_dirs, list.files,
                         pattern = "NSCA_0\\.4\\.5.*\\.tar\\.gz$",
                         full.names = TRUE))
  if (length(found) > 0L) {
    TARBALL <- found[which.max(file.mtime(found))]
  } else if (interactive()) {
    message("找不到 NSCA_0.4.5.tar.gz，請在視窗中選擇它。")
    TARBALL <- file.choose()
  } else {
    stop("找不到 NSCA_0.4.5.tar.gz。已搜尋：\n  ",
         paste(search_dirs, collapse = "\n  "),
         "\n請把檔案放進其中一個資料夾，或在腳本開頭把 TARBALL 設成完整路徑。",
         call. = FALSE)
  }
}
if (!file.exists(TARBALL)) {
  stop("指定的檔案不存在：", TARBALL, call. = FALSE)
}
TARBALL <- normalizePath(TARBALL)
cat("使用：", TARBALL, "\n")

# 複製成標準檔名：R CMD check 以檔名判斷套件名稱
tarball <- file.path(work, "NSCA_0.4.5.tar.gz")
file.copy(TARBALL, tarball, overwrite = TRUE)


## ---- 記錄結果 ---------------------------------------------------------------

results <- data.frame(item = character(), result = character(),
                      detail = character(), stringsAsFactors = FALSE)
record <- function(item, ok, detail = "") {
  mark <- if (isTRUE(ok)) "PASS" else "FAIL"
  results[nrow(results) + 1L, ] <<- list(item, mark, detail)
  cat("  [", mark, "] ", item, "  ", detail, "\n", sep = "")
}
# format() 依顯示寬度補空白，中文標籤才能對齊
show <- function(label, value) {
  cat("    ", format(label, width = 34), format(value, digits = 4), "\n",
      sep = "")
}


## ---- 第 1 部分：安裝 --------------------------------------------------------

cat("\n== 第 1 部分：安裝 ==\n")
needed <- c("NCA", "SCAtools", "ggplot2", "testthat")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1L),
                               quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  install.packages(missing_pkgs)
}

if ("NSCA" %in% loadedNamespaces()) {
  # 已載入的 NSCA 可能是別的版本；卸載不了就請使用者重開 R
  ok <- tryCatch({ unloadNamespace("NSCA"); TRUE }, error = function(e) FALSE)
  if (!ok) {
    stop("這個 R session 已經載入了 NSCA。請先 Session > Restart R 再執行。",
         call. = FALSE)
  }
}
lib <- file.path(work, "lib")
dir.create(lib, showWarnings = FALSE)
install.packages(tarball, lib = lib, repos = NULL, type = "source", quiet = TRUE)
library(NSCA, lib.loc = lib)

version <- as.character(utils::packageVersion("NSCA", lib.loc = lib))
record("載入的版本是 0.4.5", identical(version, "0.4.5"), version)
record("載入的 NSCA 來自暫存 library",
       identical(normalizePath(dirname(find.package("NSCA"))), normalizePath(lib)))


## ---- 第 2 部分：範例 --------------------------------------------------------

cat("\n== 第 2 部分：範例 ==\n")
db <- tools::Rd_db("NSCA", lib.loc = lib)
rd_text <- vapply(db, function(rd) paste(as.character(rd), collapse = ""),
                  character(1L))
topics <- sub("\\.Rd$", "", names(db))

dontrun <- topics[grepl("\\\\dontrun", rd_text)]
donttest <- topics[grepl("\\\\donttest", rd_text)]
no_examples <- topics[!grepl("\\\\examples", rd_text)]

record("沒有任何 \\dontrun", length(dontrun) == 0L, paste(dontrun, collapse = ", "))
record("每個說明頁都有範例", length(no_examples) == 0L,
       paste(no_examples, collapse = ", "))
record("只有 nsca 使用 \\donttest", identical(donttest, "nsca"),
       paste(donttest, collapse = ", "))
record("nsca() 預設仍為 1000 次置換", identical(formals(nsca)$test.rep, 1000),
       format(formals(nsca)$test.rep))

# 以 CRAN 計時的方式執行：不跑 \donttest。範例印出的表格收進
# capture.output()，畫面上只留秒數；圖畫到 pdf(NULL)，不開視窗。
# 第一個範例會多出約 2 秒，是第一次載入 NCA／SCAtools 的固定成本。
grDevices::pdf(NULL)
timing <- vapply(topics, function(topic) {
  unname(system.time(utils::capture.output(
    utils::example(topic, package = "NSCA", lib.loc = lib,
                   character.only = TRUE, ask = FALSE, echo = FALSE,
                   run.donttest = FALSE)
  ))[["elapsed"]])
}, numeric(1L))
donttest_time <- unname(system.time(utils::capture.output(
  utils::example("nsca", package = "NSCA", lib.loc = lib, ask = FALSE,
                 echo = FALSE, run.donttest = TRUE)
))[["elapsed"]])
grDevices::dev.off()

record("計時範例全部 < 5 秒", all(timing < 5),
       sprintf("最慢 %s = %.2f 秒", names(which.max(timing)), max(timing)))
cat("  各範例秒數：\n")
print(round(sort(timing, decreasing = TRUE), 3))
cat(sprintf("  （參考）nsca() 的 \\donttest（1000 次）執行 %.1f 秒，不計入 5 秒上限\n",
            donttest_time))


## ---- 第 3 部分：Bug 修正 ----------------------------------------------------

cat("\n== 第 3 部分：Bug 修正 ==\n")

# (a) 單一條件：兩列只有 Y、沒有 X，且 Y 在原本範圍外。
#     正確行為：這兩列不影響結果，所以與完整資料完全相同。
#     （0.4.4 會算出 0.0557、reconstruction_error 0.110、geometry_acceptable FALSE）
set.seed(2)
n <- 50
x <- runif(n)
y <- pmin(pmax(x + rnorm(n, 0, 0.1), 0), 1)
clean <- data.frame(X = x, Y = y)
padded <- rbind(clean, data.frame(X = c(NA, NA), Y = c(-1, 2)))
tab_clean <- nsca_table(nsca_analysis(clean, "X", "Y", ceilings = "ce_fdh"))
tab_padded <- nsca_table(nsca_analysis(padded, "X", "Y", ceilings = "ce_fdh"))
cat("  (a) 單一條件，加入 X 缺失的列\n")
show("完整資料 admissible_region_share", tab_clean$admissible_region_share)
show("加缺失列 admissible_region_share", tab_padded$admissible_region_share)
show("加缺失列 reconstruction_error", tab_padded$reconstruction_error)
show("加缺失列 geometry_acceptable", tab_padded$geometry_acceptable)
record("(a) X 缺失的列不改變容許區",
       isTRUE(all.equal(tab_padded$admissible_region_share,
                        tab_clean$admissible_region_share)) &&
         isTRUE(tab_padded$geometry_acceptable))

# (a2) 兩個條件，X2 有缺失。
#      正確行為：X2 的結果與只用 X2 完整配對單獨分析時相同。
#      （0.4.4 的 X2 reconstruction_error 為 0.648）
d2 <- data.frame(X1 = c(x, 0.5, 0.5), X2 = c(runif(n), NA, NA),
                 Y = c(y, -1, 2))
tab_two <- nsca_table(nsca_analysis(d2, c("X1", "X2"), "Y", ceilings = "ce_fdh"))
tab_alone <- nsca_table(nsca_analysis(na.omit(d2[c("X2", "Y")]), "X2", "Y",
                                      ceilings = "ce_fdh"))
x2_row <- tab_two[tab_two$condition == "X2", ]
cat("  (a2) 兩個條件，X2 有缺失\n")
show("X2 admissible_region_share", x2_row$admissible_region_share)
show("X2 單獨分析時的值", tab_alone$admissible_region_share)
show("X2 reconstruction_error", x2_row$reconstruction_error)
record("(a2) 多條件時 X2 與單獨分析一致",
       isTRUE(all.equal(x2_row$admissible_region_share,
                        tab_alone$admissible_region_share)) &&
         x2_row$reconstruction_error < 0.02)

# (b) relevance 用具名字串。正確行為：轉成數字，並依名稱排序。
#     （0.4.4 會保留為字串）
set.seed(3)
xb <- sort(runif(40))
dat_b <- data.frame(X = xb, Y = pmin(pmax(xb + rnorm(40, 0, 0.1), 0), 1))
fit_b <- nsca_analysis(dat_b, "X", "Y", ceilings = "ce_fdh",
                       relevance = c(sufficiency = "0.2", necessity = "0.1"))
rel <- attr(fit_b, "nsca")$relevance
cat("  (b) relevance = c(sufficiency = \"0.2\", necessity = \"0.1\")\n")
show("儲存的型別", typeof(rel))
show("儲存的值", paste(names(rel), rel, sep = " = ", collapse = ", "))
record("(b) relevance 轉為數字且順序正確",
       identical(rel, c(necessity = 0.1, sufficiency = 0.2)))

# (c) 從未抽過亂數時使用 shared.seed，結束後不應留下亂數狀態。
#     （0.4.4 會留下）這裡先暫存你的亂數狀態，檢查完再放回去。
set.seed(6)
xc <- sort(runif(40))
dat_c <- data.frame(X = xc, Y = pmin(pmax(xc + rnorm(40, 0, 0.08), 0), 1))
saved_seed <- get(".Random.seed", envir = globalenv())
rm(".Random.seed", envir = globalenv())
invisible(nsca_analysis(dat_c, "X", "Y", ceilings = "ce_fdh",
                        shared.test.rep = 3, shared.seed = 1))
left_behind <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
assign(".Random.seed", saved_seed, envir = globalenv())
cat("  (c) shared.seed 之後是否留下 .Random.seed\n")
show("留下亂數狀態", left_behind)
record("(c) 不留下亂數狀態", identical(left_behind, FALSE))

# (d)(e) nsca_extract()
set.seed(1)
xd <- sort(runif(60))
dat_d <- data.frame(X = xd, Y = pmin(pmax(xd + rnorm(60, 0, 0.1), 0), 1))
fit_d <- nsca_analysis(dat_d, "X", "Y", ceilings = "ce_fdh")

# (d) 0.4.0 改名的欄位，警告應寫 0.4.0（0.4.4 寫成 0.3.0）
msg_d <- tryCatch(nsca_extract(fit_d, param = "data_zone_share"),
                  warning = conditionMessage)
cat("  (d) nsca_extract(fit, param = \"data_zone_share\") 的警告\n")
cat("     ", msg_d, "\n")
record("(d) 警告寫 0.4.0", grepl("in NSCA 0.4.0", msg_d, fixed = TRUE))

# (e) 沒估計過的前沿技術應報錯（0.4.4 回傳 NA）
msg_e <- tryCatch(
  nsca_extract(fit_d, ceiling = "cr_fdh", param = "nec:Effect size"),
  error = function(e) paste("錯誤:", conditionMessage(e))
)
cat("  (e) nsca_extract(fit, ceiling = \"cr_fdh\", param = \"nec:Effect size\")\n")
cat("     ", format(msg_e), "\n")
record("(e) 未估計的前沿技術會報錯",
       grepl("was not estimated", msg_e, fixed = TRUE))


## ---- 第 4 部分：文件 --------------------------------------------------------

cat("\n== 第 4 部分：文件 ==\n")
desc <- utils::packageDescription("NSCA", lib.loc = lib)
news_versions <- unique(utils::news(package = "NSCA", lib.loc = lib)$Version)[1:5]
table_rd <- rd_text[["nsca_table.Rd"]]

record("DESCRIPTION Version = 0.4.5", identical(desc$Version, "0.4.5"))
record("DESCRIPTION Date = 2026-09-30", identical(desc$Date, "2026-09-30"))
record("NEWS 版本連續（0.4.5, 0.4.4, 0.4.3, …）",
       identical(news_versions, c("0.4.5", "0.4.4", "0.4.3", "0.4.2", "0.4.1")),
       paste(news_versions, collapse = ", "))
record("?nsca_table 公式已改用 admissible_region_share",
       grepl("admissible\\\\_region\\\\_share", table_rd) &&
         !grepl("data\\\\_zone\\\\_share", table_rd))


## ---- 第 5 部分：R CMD check --as-cran（選用） -------------------------------

if (isTRUE(RUN_CRAN_CHECK)) {
  cat("\n== 第 5 部分：R CMD check", paste(CHECK_ARGS, collapse = " "),
      "（約 2–3 分鐘）==\n")
  check_dir <- file.path(work, "check")
  dir.create(check_dir, showWarnings = FALSE)
  file.copy(tarball, check_dir, overwrite = TRUE)
  old_wd <- setwd(check_dir)
  # 不連 CRAN 查詢「是否為新套件」等資訊，避免網路問題讓檢查失敗
  old_env <- Sys.getenv("_R_CHECK_CRAN_INCOMING_REMOTE_", unset = NA)
  Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false")
  log <- system2(file.path(R.home("bin"), "R"),
                 c("CMD", "check", CHECK_ARGS, "NSCA_0.4.5.tar.gz"),
                 stdout = TRUE, stderr = TRUE)
  if (is.na(old_env)) {
    Sys.unsetenv("_R_CHECK_CRAN_INCOMING_REMOTE_")
  } else {
    Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = old_env)
  }
  setwd(old_wd)
  status <- grep("^Status:", log, value = TRUE)
  cat(grep("NOTE|WARNING|ERROR|Status:|examples|tests", log, value = TRUE),
      sep = "\n")
  record("R CMD check 沒有 ERROR / WARNING",
         length(status) == 1L && !grepl("ERROR|WARNING", status), status)
  cat("  完整紀錄：", file.path(check_dir, "NSCA.Rcheck", "00check.log"), "\n")
}


## ---- 總表 -------------------------------------------------------------------

cat("\n================ 總表 ================\n")
for (i in seq_len(nrow(results))) {
  cat(results$result[[i]], "  ", format(results$item[[i]], width = 46), "  ",
      results$detail[[i]], "\n", sep = "")
}
cat(sprintf("\nPASS %d / %d\n", sum(results$result == "PASS"), nrow(results)))
