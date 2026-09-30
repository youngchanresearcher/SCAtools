# =============================================================================
# NSCA 0.4.5 手動檢查腳本
#
# 用途：在你自己的電腦上逐項確認 0.4.5 的每一項修改。
#   第 1 部分  安裝：把 0.4.4 與 0.4.5 裝進兩個「暫存」library，不動你原本的套件。
#   第 2 部分  範例：確認沒有 \dontrun、列出 \donttest、每個範例計時（CRAN 上限 5 秒）。
#   第 3 部分  Bug：每個 bug 在 0.4.4 與 0.4.5 上各跑一次，並列出前後數值。
#   第 4 部分  完整資料：48 種設定下，0.4.4 與 0.4.5 的輸出必須完全相同。
#   第 5 部分  文件：版本、NEWS、說明頁內容。
#   第 6 部分  （選用）R CMD check --as-cran。
#
# 使用方式：
#   1. 把 NSCA_0.4.5.tar.gz（和 NSCA_0.4.4.tar.gz）放在這個腳本旁邊、
#      桌面，或留在「下載」資料夾。
#   2. 在 RStudio 開啟這個檔案按 Source，或在 R 執行：
#        source("完整路徑/NSCA_0.4.5_manual_check.R", encoding = "UTF-8")
#   3. 最後會印出一張總表，每一項是 PASS 或 FAIL。
#
# 腳本會在「工作目錄」「腳本所在資料夾」「桌面（含 OneDrive 桌面）」
# 「下載資料夾」裡找 tarball，檔名前後
# 多了字（例如瀏覽器存成 "NSCA_0.4.5 (1).tar.gz"）也找得到。都找不到時，
# 互動模式會跳出視窗讓你選檔案。
#
# 兩個版本不能在同一個 R session 裡同時載入，所以每項檢查都在獨立的子行程
# (Rscript) 裡執行：一次用 0.4.4、一次用 0.4.5。
# =============================================================================

## ---- 設定 -------------------------------------------------------------------

# 留 NA 表示自動尋找；也可以直接填完整路徑，例如
#   NEW_TARBALL <- "C:/Users/furfa/OneDrive/Desktop/NSCA_0.4.5.tar.gz"
# Windows 路徑請用 / 或 \\，不要用單一個 \。

# 0.4.5：這次要上傳的版本（必要）
NEW_TARBALL <- NA

# 0.4.4：你之前上傳、被 CRAN 退件的版本（選用；找不到就只檢查 0.4.5）
OLD_TARBALL <- NA

# 第 6 部分要不要跑 R CMD check --as-cran（約 2–3 分鐘）
RUN_CRAN_CHECK <- TRUE

# 沒有安裝 LaTeX（MiKTeX / TinyTeX）就保留 "--no-manual"；有的話可以刪掉
CHECK_ARGS <- c("--as-cran", "--no-manual")


## ---- 準備 -------------------------------------------------------------------

work <- file.path(tempdir(), "nsca_manual_check")
dir.create(work, showWarnings = FALSE, recursive = TRUE)

# 這個腳本所在的資料夾：source() 時可取得；RStudio 另外再試 rstudioapi。
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

locate <- function(given, version, required) {
  if (!is.na(given)) {
    if (!file.exists(given)) {
      stop("指定的檔案不存在：", given, call. = FALSE)
    }
    return(normalizePath(given))
  }
  # 允許前後多出的字：c6456c62-NSCA_0.4.4.tar.gz、NSCA_0.4.5 (1).tar.gz
  pattern <- paste0("NSCA_", gsub(".", "\\.", version, fixed = TRUE),
                    ".*\\.tar\\.gz$")
  found <- unlist(lapply(search_dirs, list.files, pattern = pattern,
                         full.names = TRUE))
  if (length(found) > 0L) {
    # 同名多份時用最新的那份
    newest <- found[which.max(file.mtime(found))]
    return(normalizePath(newest))
  }
  if (interactive()) {
    message("找不到 NSCA_", version, ".tar.gz，請在視窗中選擇它",
            if (required) "。" else "（不需要的話按取消）。")
    chosen <- tryCatch(file.choose(), error = function(e) NA_character_)
    if (!is.na(chosen)) {
      return(normalizePath(chosen))
    }
  }
  if (required) {
    stop(
      "找不到 NSCA_", version, ".tar.gz。已搜尋：\n  ",
      paste(search_dirs, collapse = "\n  "),
      "\n請把檔案放進其中一個資料夾，或在腳本開頭把 NEW_TARBALL 設成完整路徑。",
      call. = FALSE
    )
  }
  NA_character_
}

new_found <- locate(NEW_TARBALL, "0.4.5", required = TRUE)
old_found <- locate(OLD_TARBALL, "0.4.4", required = FALSE)
has_old <- !is.na(old_found)
cat("0.4.5 使用：", new_found, "\n")
cat("0.4.4 使用：", if (has_old) old_found else "（找不到，前後比較會略過）", "\n")

# 複製成標準檔名再使用：R CMD check 以檔名判斷套件名稱，
# "NSCA_0.4.5 (1).tar.gz" 這類檔名會讓它失敗。
NEW_TARBALL <- file.path(work, "NSCA_0.4.5.tar.gz")
file.copy(new_found, NEW_TARBALL, overwrite = TRUE)
if (has_old) {
  OLD_TARBALL <- file.path(work, "NSCA_0.4.4.tar.gz")
  file.copy(old_found, OLD_TARBALL, overwrite = TRUE)
}

needed <- c("NCA", "SCAtools", "ggplot2", "testthat")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1L),
                               quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  install.packages(missing_pkgs)
}

rscript <- file.path(R.home("bin"), "Rscript")

results <- data.frame(item = character(), result = character(),
                      detail = character(), stringsAsFactors = FALSE)
record <- function(item, ok, detail = "") {
  results[nrow(results) + 1L, ] <<- list(item, if (isTRUE(ok)) "PASS" else "FAIL",
                                         detail)
  cat(sprintf("  [%s] %s  %s\n", if (isTRUE(ok)) "PASS" else "FAIL", item,
              detail))
}

# 在指定的 library 裡開一個新的 R 行程執行 fun()，把回傳值帶回來。
# fun 必須自己 library(NSCA)。這樣兩個版本互不干擾，也都是「全新 session」。
in_version <- function(lib, fun) {
  f_in <- tempfile(fileext = ".rds", tmpdir = work)
  f_out <- tempfile(fileext = ".rds", tmpdir = work)
  f_r <- tempfile(fileext = ".R", tmpdir = work)
  saveRDS(fun, f_in)
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", deparse(normalizePath(lib, "/"))),
    sprintf("fun <- readRDS(%s)", deparse(normalizePath(f_in, "/"))),
    "value <- fun()",
    sprintf("saveRDS(value, %s)", deparse(normalizePath(f_out, "/", FALSE)))
  ), f_r)
  status <- system2(rscript, shQuote(f_r), stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(status), 0L) || !file.exists(f_out)) {
    stop("子行程失敗（lib = ", lib, "）。可手動執行 ", f_r, " 查看錯誤。")
  }
  readRDS(f_out)
}

# 兩個版本都跑；沒有 0.4.4 時 old 為 NULL
both <- function(fun) {
  list(old = if (has_old) in_version(lib_old, fun) else NULL,
       new = in_version(lib_new, fun))
}


## ---- 第 1 部分：安裝到暫存 library ----------------------------------------

cat("\n== 第 1 部分：安裝 ==\n")
lib_new <- file.path(work, "lib_045")
lib_old <- file.path(work, "lib_044")
dir.create(lib_new, showWarnings = FALSE)
install.packages(NEW_TARBALL, lib = lib_new, repos = NULL, type = "source",
                 quiet = TRUE)
if (has_old) {
  dir.create(lib_old, showWarnings = FALSE)
  install.packages(OLD_TARBALL, lib = lib_old, repos = NULL, type = "source",
                   quiet = TRUE)
}

versions <- both(function() as.character(utils::packageVersion("NSCA")))
record("安裝的新版本是 0.4.5", identical(versions$new, "0.4.5"), versions$new)
if (has_old) {
  record("安裝的舊版本是 0.4.4", identical(versions$old, "0.4.4"), versions$old)
}


## ---- 第 2 部分：範例 --------------------------------------------------------

cat("\n== 第 2 部分：範例 ==\n")

examples <- in_version(lib_new, function() {
  db <- tools::Rd_db("NSCA")
  text <- vapply(db, function(rd) paste(as.character(rd), collapse = ""),
                 character(1L))
  topics <- sub("\\.Rd$", "", names(db))
  has_examples <- grepl("\\\\examples", text)

  library(NSCA)
  grDevices::pdf(NULL)
  # 以 CRAN 計時的方式執行：不跑 \donttest
  timing <- vapply(topics[has_examples], function(topic) {
    unname(system.time(
      utils::example(topic, package = "NSCA", character.only = TRUE,
                     ask = FALSE, echo = FALSE, run.donttest = FALSE)
    )[["elapsed"]])
  }, numeric(1L))
  # nsca() 的 \donttest 部分（預設 1000 次置換檢定）另外計時
  donttest <- unname(system.time(
    utils::example("nsca", package = "NSCA", ask = FALSE, echo = FALSE,
                   run.donttest = TRUE)
  )[["elapsed"]])

  list(
    no_examples = topics[!has_examples],
    dontrun = topics[grepl("\\\\dontrun", text)],
    donttest = topics[grepl("\\\\donttest", text)],
    timing = timing,
    donttest_time = donttest,
    nsca_default_rep = formals(nsca)$test.rep
  )
})

record("沒有任何 \\dontrun", length(examples$dontrun) == 0L,
       paste(examples$dontrun, collapse = ", "))
record("每個說明頁都有範例", length(examples$no_examples) == 0L,
       paste(examples$no_examples, collapse = ", "))
record("只有 nsca 使用 \\donttest", identical(examples$donttest, "nsca"),
       paste(examples$donttest, collapse = ", "))
record("nsca() 預設仍為 1000 次置換", identical(examples$nsca_default_rep, 1000),
       format(examples$nsca_default_rep))
record("計時範例全部 < 5 秒", all(examples$timing < 5),
       sprintf("最慢 %s = %.2f 秒", names(which.max(examples$timing)),
               max(examples$timing)))
cat(sprintf("  （參考）nsca() 的 \\donttest（1000 次）執行 %.1f 秒；不計入 5 秒上限\n",
            examples$donttest_time))
print(round(sort(examples$timing, decreasing = TRUE), 3))


## ---- 第 3 部分：Bug 前後比較 ------------------------------------------------

cat("\n== 第 3 部分：Bug 修正（左 0.4.4 / 右 0.4.5）==\n")

# format() 依顯示寬度補空白，中文標籤才能對齊（sprintf 的 %-30s 以字元數計）
show <- function(label, x) {
  cat("  ", format(label, width = 34),
      " 0.4.4: ", format(if (is.null(x$old)) "-" else format(x$old, digits = 4),
                         width = 12),
      " 0.4.5: ", format(x$new, digits = 4), "\n", sep = "")
}

# (a) 單一條件：兩列只有 Y、沒有 X，而且 Y 在原本範圍外
bug_a <- both(function() {
  library(NSCA)
  set.seed(2)
  n <- 50
  x <- runif(n)
  y <- pmin(pmax(x + rnorm(n, 0, 0.1), 0), 1)
  clean <- data.frame(X = x, Y = y)
  padded <- rbind(clean, data.frame(X = c(NA, NA), Y = c(-1, 2)))
  cols <- c("admissible_region_share", "reconstruction_error",
            "geometry_acceptable")
  list(
    clean = nsca_table(nsca_analysis(clean, "X", "Y", ceilings = "ce_fdh"))[cols],
    padded = nsca_table(nsca_analysis(padded, "X", "Y", ceilings = "ce_fdh"))[cols]
  )
})
cat("  (a) 單一條件，X 缺失的列\n")
show("完整資料 admissible_region_share",
     lapply(bug_a, function(v) v$clean$admissible_region_share))
show("加缺失列 admissible_region_share",
     lapply(bug_a, function(v) v$padded$admissible_region_share))
show("加缺失列 reconstruction_error",
     lapply(bug_a, function(v) v$padded$reconstruction_error))
show("加缺失列 geometry_acceptable",
     lapply(bug_a, function(v) v$padded$geometry_acceptable))
record("(a) X 缺失不改變容許區",
       isTRUE(all.equal(bug_a$new$padded$admissible_region_share,
                        bug_a$new$clean$admissible_region_share)) &&
         bug_a$new$padded$geometry_acceptable)

# (a2) 兩個條件：X2 有缺失
bug_a2 <- both(function() {
  library(NSCA)
  set.seed(2)
  n <- 50
  x <- runif(n)
  y <- pmin(pmax(x + rnorm(n, 0, 0.1), 0), 1)
  d <- data.frame(X1 = c(x, 0.5, 0.5), X2 = c(runif(n), NA, NA),
                  Y = c(y, -1, 2))
  tab <- nsca_table(nsca_analysis(d, c("X1", "X2"), "Y", ceilings = "ce_fdh"))
  alone <- nsca_table(nsca_analysis(na.omit(d[c("X2", "Y")]), "X2", "Y",
                                    ceilings = "ce_fdh"))
  list(error = tab$reconstruction_error[tab$condition == "X2"],
       share = tab$admissible_region_share[tab$condition == "X2"],
       alone = alone$admissible_region_share)
})
cat("  (a2) 兩個條件，X2 有缺失\n")
show("X2 reconstruction_error", lapply(bug_a2, `[[`, "error"))
show("X2 admissible_region_share", lapply(bug_a2, `[[`, "share"))
show("X2 單獨分析時的值", lapply(bug_a2, `[[`, "alone"))
record("(a2) 多條件時 X2 與單獨分析一致",
       isTRUE(all.equal(bug_a2$new$share, bug_a2$new$alone)) &&
         bug_a2$new$error < 0.02)

# (b) relevance 用具名字串
bug_b <- both(function() {
  library(NSCA)
  set.seed(3)
  x <- sort(runif(40))
  dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(40, 0, 0.1), 0), 1))
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                       relevance = c(necessity = "0.1", sufficiency = "0.2"))
  typeof(attr(fit, "nsca")$relevance)
})
cat("  (b) relevance = c(necessity = \"0.1\", sufficiency = \"0.2\")\n")
show("儲存的型別", bug_b)
record("(b) relevance 轉為數字", identical(bug_b$new, "double"))

# (c) 全新 session 裡使用 shared.seed，結束後不應留下亂數狀態
bug_c <- both(function() {
  library(NSCA)
  set.seed(6)
  x <- sort(runif(40))
  dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(40, 0, 0.08), 0), 1))
  rm(".Random.seed", envir = globalenv())      # 模擬「從未抽過亂數」
  invisible(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                          shared.test.rep = 3, shared.seed = 1))
  exists(".Random.seed", envir = globalenv(), inherits = FALSE)
})
cat("  (c) shared.seed 之後是否留下 .Random.seed\n")
show("留下亂數狀態", bug_c)
record("(c) 不留下亂數狀態", identical(bug_c$new, FALSE))

# (d)(e) nsca_extract()
bug_de <- both(function() {
  library(NSCA)
  set.seed(1)
  x <- sort(runif(60))
  dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  warn <- function(p) tryCatch(nsca_extract(fit, param = p),
                               warning = conditionMessage)
  list(
    d = warn("data_zone_share"),
    e = tryCatch(nsca_extract(fit, ceiling = "cr_fdh", param = "nec:Effect size"),
                 error = function(e) paste("錯誤:", conditionMessage(e)))
  )
})
cat("  (d) nsca_extract(fit, param = \"data_zone_share\") 的警告\n")
cat("      0.4.4:", if (has_old) bug_de$old$d else "-", "\n")
cat("      0.4.5:", bug_de$new$d, "\n")
record("(d) 警告寫 0.4.0", grepl("in NSCA 0.4.0", bug_de$new$d, fixed = TRUE))
cat("  (e) nsca_extract(fit, ceiling = \"cr_fdh\", param = \"nec:Effect size\")\n")
cat("      0.4.4:", if (has_old) format(bug_de$old$e) else "-", "\n")
cat("      0.4.5:", format(bug_de$new$e), "\n")
record("(e) 未估計的前沿技術會報錯",
       grepl("was not estimated", bug_de$new$e, fixed = TRUE))


## ---- 第 4 部分：完整資料下結果不變 ------------------------------------------

cat("\n== 第 4 部分：完整資料，0.4.4 與 0.4.5 輸出必須完全相同 ==\n")
battery <- function() {
  library(NSCA)
  out <- list()
  for (seed in 1:6) {
    set.seed(seed)
    n <- 30 + 10 * seed
    x1 <- runif(n)
    x2 <- runif(n)
    y <- pmin(pmax(x1 + rnorm(n, 0, 0.15), 0), 1)
    dat <- data.frame(X1 = x1, X2 = x2, Y = y)
    for (dir in c("HH", "LH", "HL", "LL")) {
      for (sc in list(NULL, c(-0.2, 1.2, -0.3, 1.3))) {
        fit <- suppressWarnings(nsca_analysis(
          dat, c("X1", "X2"), "Y", direction = dir,
          ceilings = c("ce_fdh", "cr_fdh", "ce_vrs", "c_lp"), scope = sc
        ))
        out[[paste(seed, dir, is.null(sc))]] <- list(
          table = suppressWarnings(nsca_table(fit)),
          thresholds = nsca_thresholds(fit)
        )
      }
    }
  }
  out
}
if (has_old) {
  full <- both(battery)
  record("48 種設定輸出完全相同", identical(full$old, full$new),
         sprintf("%d 種設定", length(full$new)))
} else {
  cat("  （沒有 0.4.4，略過）\n")
}


## ---- 第 5 部分：文件 --------------------------------------------------------

cat("\n== 第 5 部分：文件 ==\n")
docs <- in_version(lib_new, function() {
  desc <- utils::packageDescription("NSCA")
  news <- utils::news(package = "NSCA")
  db <- tools::Rd_db("NSCA")
  table_rd <- paste(as.character(db[["nsca_table.Rd"]]), collapse = "")
  list(
    version = desc$Version,
    date = desc$Date,
    news_versions = unique(news$Version)[1:5],
    old_name_in_formula = grepl("data\\\\_zone\\\\_share", table_rd),
    new_name_in_formula = grepl("admissible\\\\_region\\\\_share", table_rd)
  )
})
record("DESCRIPTION Version = 0.4.5", identical(docs$version, "0.4.5"))
record("DESCRIPTION Date = 2026-09-30", identical(docs$date, "2026-09-30"))
record("NEWS 版本連續（0.4.5, 0.4.4, 0.4.3, …）",
       identical(docs$news_versions, c("0.4.5", "0.4.4", "0.4.3", "0.4.2", "0.4.1")),
       paste(docs$news_versions, collapse = ", "))
record("?nsca_table 公式已改用 admissible_region_share",
       docs$new_name_in_formula && !docs$old_name_in_formula)


## ---- 第 6 部分：R CMD check --as-cran（選用） -------------------------------

if (isTRUE(RUN_CRAN_CHECK)) {
  cat("\n== 第 6 部分：R CMD check", paste(CHECK_ARGS, collapse = " "),
      "（約 2–3 分鐘）==\n")
  check_dir <- file.path(work, "check")
  dir.create(check_dir, showWarnings = FALSE)
  file.copy(NEW_TARBALL, check_dir, overwrite = TRUE)
  old_wd <- setwd(check_dir)
  # 不連 CRAN 查詢「是否為新套件」等資訊，避免網路問題讓檢查失敗。
  # 用 Sys.setenv 而非 system2(env = )，後者在 Windows 上不支援。
  old_env <- Sys.getenv("_R_CHECK_CRAN_INCOMING_REMOTE_", unset = NA)
  Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false")
  log <- system2(file.path(R.home("bin"), "R"),
                 c("CMD", "check", CHECK_ARGS, shQuote(basename(NEW_TARBALL))),
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
         length(status) == 1L && !grepl("ERROR|WARNING", status),
         status)
  cat("  完整紀錄：", file.path(check_dir, "NSCA.Rcheck", "00check.log"), "\n")
}


## ---- 總表 -------------------------------------------------------------------

cat("\n================ 總表 ================\n")
for (i in seq_len(nrow(results))) {
  cat(results$result[[i]], "  ", format(results$item[[i]], width = 46), "  ",
      results$detail[[i]], "\n", sep = "")
}
cat(sprintf("\nPASS %d / %d\n", sum(results$result == "PASS"), nrow(results)))
