# NSCA

充分必要條件分析(Necessary and Sufficient Condition Analysis)。

## 安裝

原始碼與問題回報：<https://github.com/youngchanresearcher/NSCA>

NSCA 的必要性由 NCA 計算、充分性由 SCAtools 計算。三者都在 CRAN 上,從 CRAN 安裝
NSCA 會一併裝好另外兩個：

```r
install.packages("NSCA")
```

若要從原始碼 tarball 安裝,必須先裝好相依套件,因為 `repos = NULL` 不會自動處理相依：

```r
install.packages(c("NCA", "SCAtools", "ggplot2"))
install.packages("NSCA_0.4.5.tar.gz", repos = NULL, type = "source")
```

SCAtools 有自己的 repository：<https://github.com/youngchanresearcher/SCAtools>

需要 R (>= 3.5.0)、NCA (>= 5.0.2) 與 SCAtools (>= 0.4.1)。

## 它在估計什麼

充要條件陳述會同時固定**兩個**空角落。把充分性寫成 X → Y、必要性寫成 Y → X,各自對位換質之後,兩個空區落在同一張散佈圖的**對角**:

| NSCA 編號 | 方向 | 充分性空角 | 必要性空角 |
|---|---|---|---|
| 1 | `HH` | 4 右下 | 1 左上 |
| 2 | `LH` | 3 左下 | 2 右上 |
| 3 | `HL` | 2 右上 | 3 左下 |
| 4 | `LL` | 1 左上 | 4 右下 |

NSCA 編號是邏輯方向標籤，不是一個物理空角。例如 NSCA 1（`HH`）必須
同時檢查物理 corner 1 與 corner 4。
若 High／Low 是嚴格互補，`HH` 與 `LL` 是同一種充要條件模式，`LH` 與
`HL` 則是另一種；不能把四個方向當成四個彼此獨立的假設檢定。

其中一條 frontier 永遠從上方界定點雲,另一條從下方。**夾在中間的區域,就是兩個主張都沒有排除的部分。**

```r
fit <- nsca_analysis(dat, "X", "Y", direction = "HH",
                     ceilings = c("ce_fdh", "cr_fdh"), test.rep = 1000)
fit
nsca_results(fit)        # 必要、充分、聯合三張表
nsca_thresholds(fit)
nsca_plot(fit)
```

## 一條恆等式撐住所有面積

兩個空區與夾在中間的區域**鋪滿**整個 scope，所以由容斥原理：

```
admissible_region_share = 1 - d_nec - d_suf + overlap_share
joint_empty_zone_coverage = d_nec + d_suf - overlap_share = 1 - admissible_region_share
```

其餘所有面積量都由此推導，而不是各自定義。兩個結果立刻跟著出來。

**界是精確的，而且是緊的。** 由 `admissible_region_share >= 0` 得 `d_nec + d_suf <= 1 + O`，於是

```
weakest_effect        <= 0.5 + O/2
balanced_joint_effect <= 1   + O
```

而且完美對角線**恰好取等號**，此時 `O = 1/(n - 1)`。n = 40 時實際值是 0.5128 與
1.0256，界也是 0.5128 與 1.0256。超出的部分是離散化不是證據；因為它是等式而不是
數量級，讀者可以直接把它**減掉**，而不是「大致扣掉」。

**只有 coverage 是無條件有界的。** `admissible_region_share` 是 scope 的一個比例，所以它
與 `joint_empty_zone_coverage` 永遠落在 `[0, 1]`。另外兩個只有在加上 overlap 之後
才有界，而階梯型 frontier 的 overlap 從來不是零。

### `reconstruction_error`

這條恆等式有兩條計算路徑。`admissible_region_share` 與 `overlap_share` 是從**本套件重建**
的 frontier 積分出來的；`d_nec` 與 `d_suf` 則是兩個引擎用**自己內部**的 frontier
回報的。兩條路徑的殘差現在逐列回報。

它是唯一能檢查「重建是否仍與引擎真正配適的東西一致」的量，也抓得住圖表抓不到的
那一種錯誤：重建錯的 frontier 畫出來仍像一條 frontier，面積也仍然合理，只有恆等式
會拒絕閉合。

## 同樣兩個成分，三種摘要方式

`d_nec` 與 `d_suf` 是主要結果。其餘都是把這兩個數字合起來的方式，
`nsca_table()` 會報告**三種**，而不是挑一種當成「那個」聯合效果量 ——
因為兩個空區的幾何本身並不指定該用哪一種。

| | 問什麼 | 上限 | 補償性 | 弱點 |
|---|---|---|---|---|
| `weakest_effect` = `min(d_nec, d_suf)` | 較弱那一側有多強 | 約 0.5 | 完全不補償 | 被曲率壓低 |
| `balanced_joint_effect` = `2*sqrt(d_nec*d_suf)` | 有多強**且**有多平衡 | 1 | 部分補償 | 同樣被曲率壓低，但較輕 |
| `joint_empty_zone_coverage` = `1 - admissible_region_share` | 有多接近確定性函數 | 1 | 完全補償 | 低端被墊高 |

### min、幾何平均、總和是同一個家族

它們是冪平均 M_p 在 `p = -Inf`、`p = 0`、`p = 1` 三個點。所以選哪一個是在選
**允許多少補償**，不是在選對錯。`nsca_joint()` 直接把這個參數打開：

```r
nsca_joint(0.40, 0.40, p = 0)     # 0.800  平衡
nsca_joint(0.70, 0.10, p = 0)     # 0.529  總和相同，因不平衡被扣分
nsca_joint(0.40, 0.40, p = 1)     # 0.800  總和分不出這兩者，
nsca_joint(0.70, 0.10, p = 1)     # 0.800  這才是反對總和的理由
```

幾何平均是**部分補償（partially compensatory）**。較大的那一側**確實**會把
指標拉高，只是邊際遞減；同時只要任一側歸零，指標仍然歸零。把它描述成
完全非補償的邏輯 AND 是誇大了 —— `min` 才是這個家族完全不補償的那一端。

### 為什麼乘 2 只是正規化，不是新增假設

當 `p <= 1` 時冪平均不超過算術平均 `(d_nec + d_suf)/2`；兩個空區是同一個
scope 盒子裡互斥的子集，所以 `d_nec + d_suf <= 1`，算術平均最多 0.5。因此
`balanced_joint_effect` 落在 `[0, 1]`，而且**恰好**在 `d_nec = d_suf = 0.5` 時等於 1 ——
那就是兩條 frontier 重合、帶寬閉合、整張圖只有一條線的情形。

**階梯型 frontier 會略微超過上限。** 相鄰兩個觀測點之間上下階梯彼此交錯，
空區重疊約一個階梯寬，兩個成分各被墊高 O(1/n)。40 點的完美對角線得到
`weakest_effect = 0.513`、`balanced_joint_effect = 1.03`。這個超出量由 `overlap_share` 回報，而且
指標**不做上限截斷**，好讓離散化留在檯面上而不是被抹掉。

### 三者在哪裡分歧

曲率是最有趣的情形。`Y = X`、`Y = X^3`、`Y = X^(1/3)` 都是完美雙射，
所以三者都是完美的充分必要：

| | `d_nec` | `d_suf` | `weakest_effect` | `balanced_joint_effect` | `joint_empty_zone_coverage` |
|---|---|---|---|---|---|
| `Y = X` | 0.50 | 0.50 | 0.50 | 1.00 | 1.00 |
| `Y = X^3` | 0.75 | 0.25 | 0.25 | 0.87 | 1.00 |
| `Y = X^(1/3)` | 0.25 | 0.75 | 0.25 | 0.87 | 1.00 |

以上是單位正方形上的極限值。有限樣本的 `ce_fdh` 會替每個成分加上 `1/n`
階梯項，所以在 `[0.001, 1]` 上取 150 點，對角線得到 0.5034 / 0.5034，
三次方得到 0.7531 / 0.2536。

三個都沒有算錯。`joint_empty_zone_coverage` 問的是「Y 有沒有被 X 釘住」，答案是有；
另外兩個問的是「用成分自己的面積尺度來看，兩個主張各有多強」，而偏斜的
曲線**確實**讓其中一個空區變小。反方向的問題則是墊高：一條鬆散的斜向點帶
`joint_empty_zone_coverage` 給約 0.17，`weakest_effect` 只給 0.02。

| `weakest_effect` / `balanced_joint_effect` | `joint_empty_zone_coverage` | 讀法 |
|---|---|---|
| 高 | 高 | 強充要條件 |
| 低 | 高 | 確定性但偏斜，某一側幾乎沒有內容 |
| 低 | 低 | 關係鬆散 |
| 任意，但 `degenerate` 為真 | | 有一軸根本沒有變異 |
| 任意，但 p 不顯著 | | 沒有證據排除隨機配對 |

### 不提供量級基準

三個指標都不附基準，這是刻意的而不是漏掉。單一 NCA 效果量的經驗閾值
**不能移植**：尺度不同，而且在獨立虛無假設下兩個成分都帶有正的有限樣本
偏誤，`balanced_joint_effect` 會把它**放大**而不是抵銷，放大幅度取決於 `n`、frontier
方法與 scope。要說某個值大或小之前，先在手上這個設計上做模擬校準。

## 兩種都叫「水平線」的情況

兩者落在相反的位置，而且只有一種是問題。

**frontier 是水平的，scope 被點雲填滿。** X 與 Y 都鋪滿 scope，天花板平貼在
上緣。兩側都沒有空區：`d_nec = d_suf = 0`，帶寬等於整個 scope，三個指標都
是 0。這是正確的，也不需要任何特別處理。

**資料本身是水平的，也就是 Y 為常數。** 沒有理論 scope 時直接**拒絕**：
常數 outcome 沒有自己的 scope，所有空區都會被除以零高度。有 scope 時會執行、
發出警告、並標記 `degenerate` —— 因為此時答案是**最大值而不是零**。常數落在
單位 scope 中央時，上半與下半都是空的，`d_nec = d_suf = 0.5`，帶寬閉合，
三個指標全都替一組毫無資訊的資料回報完美聯合支持。把同一條線移到 `Y = 0.8`，
`balanced_joint_effect` 掉到 0.80 —— 資料一個位元都沒變。這個敏感性就是破綻。

這是三個面積型摘要**共同**的盲點，而且 `balanced_joint_effect` 是其中最脆弱的：常數一旦
偏離中央，`weakest_effect` 至少還會跟著較弱那一側走。有兩道防線：`degenerate` 旗標，
以及置換篩檢 —— 重排常數 outcome 不改變任何東西，每一次重抽都複製出同樣的
幾何，兩個 p 值都會是 1。

## 合取檢定

`p_nsca_iut = max(p_nec, p_suf)`。只有當**兩個**方向性成分檢定都在指定水準下拒絕
時才通過。這是交集–聯集規則（IUT）：它是 level-α 的，而且**不需要**假設兩個成分
p 值互相獨立，所以僅為了組合兩個事先指定的檢定不需再做校正。跨多個 X 或多種
frontier 的多重性是另一回事。

它同時通常是**保守的** —— 兩個 p 值的最大值在隨機上大於任一個。因此 NSCA 設計
需要的樣本量比任一成分單獨做都大，這件事該在設計階段就算進去，而不是事後才發現。

統計範圍要說清楚：置換檢定打散 X–Y 配對，檢驗的是 random-pairing／independence
虛無假設。`p_nsca_iut` 是兩個方向性空白區檢定的合取篩檢，不是對完整邏輯虛無假設
「非必要或非充分」的直接檢定，更不是因果或確定性充要條件關係的證明。

### 共用置換序列

`shared.test.rep` 用**一條**序列同時驅動兩個引擎，取代引擎各自跑的兩次置換。每一次
重抽把 outcome 洗牌一次，兩個引擎都在**同一份**洗牌資料上重新配適，`d_nec`、
`d_suf` 與兩者的最小值一起記錄下來。

```r
fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                     shared.test.rep = 999, shared.seed = 1)
```

這才讓 `p_weakest_perm` 有意義。IUT 是把兩個 p 值組合起來，不需要對兩者如何共變做
任何假設；但 `min(d_nec, d_suf)` 是**兩個成分的**統計量，它的虛無分布正好取決於那個
共變。用兩組獨立置換會把「兩個空區是同一個 scope 裡互斥子集」這個限制丟掉，產生
一個任何資料都生不出來的虛無分布。

用了共用序列之後，`p_nec` 與 `p_suf` 也來自同一條序列，四個 p 值彼此一致；
`p_source` 會說明是哪一種來源。p 值帶 Phipson–Smyth 修正，所以不可能是 0。

`p_weakest_perm` **不取代** `p_nsca_iut`。它的虛無假設只有隨機配對，而隨機配對是
聯集虛無「非必要或非充分」的**真子集** —— 那個聯集裡包含「必要但不充分」這種根本
不是隨機配對的狀態。所以拿隨機配對校準，對那個聯集沒有 level-α 保證。它是敏感度
分析；判定仍然由 IUT 負責。

## 支持判定

四個旗標而不是一個，這樣失敗時看得出是哪一項沒過。

| 旗標 | 需要 |
|---|---|
| `necessity_significant` | `p_nec <= alpha` |
| `necessity_supported` | 顯著，且若有設 `relevance` 則 `d_nec >= relevance` |
| `geometry_acceptable` | 非退化、`reconstruction_error` 夠小、overlap 不超過階梯型本身的 `1/(n - 1)` |
| `joint_support` | 兩個成分都 supported **且** 幾何可接受 |

```r
fit <- nsca_analysis(dat, "X", "Y", test.rep = 1000,
                     relevance = c(necessity = 0.10, sufficiency = 0.10))
```

`relevance` 是事先指定的實務相關性門檻。沒設的話，支持就只靠顯著性，而 `print()`
會**明講**這件事，不會讓讀者以為有套過門檻。退化的列永遠不可能被判定為 supported。

## 0.3.0 與 0.4.0 退役的名稱

舊名仍然可用：`nsca_table(legacy = TRUE)` 與 `nsca_thresholds(legacy = TRUE)`
會把它們附加回來，`nsca_extract()` 接受舊名並發警告，退役的引數會發警告但仍然
有效，`nsca_legacy_names()` 回傳對照表。

| 退役 | 現行 | 屬於哪張表 |
|---|---|---|
| `d_nsca` | `weakest_effect` | 聯合表 |
| `j_nsca` | `balanced_joint_effect` | 聯合表 |
| `determinacy` | `joint_empty_zone_coverage` | 聯合表 |
| `weaker_side` | `weaker_component` | 聯合表 |
| `p_nsca` | `p_nsca_iut` | 聯合表 |
| `undetermined_share`、`data_zone_share` | `admissible_region_share` | 聯合表 |
| `nsca_supported` | `joint_support` | 聯合表 |
| `conjunction` | `joint_support_status` | 聯合表 |
| `undetermined_width`、`data_zone_width` | `necessity_sufficiency_interval` | 門檻表 |
| `region` | `threshold_status` | 門檻表 |
| `boundary`（引數） | `inequality` | 門檻表 |

0.4.0 改用論文《condition analysis in degree》的術語：兩個成分都相容的區域是
**admissible region**（可容許區域）；在同一個結果水準上兩個門檻之間的距離是
**necessity-sufficiency interval**；兩個成分都有證據叫 **joint support**；而
**boundary** 專指「理論上的邊界線」（frontier 是它的估計值），所以控制不等號嚴格性
的引數改名為 `inequality`。`nsca_terms()` 列出完整對照。

`determinacy` 這個名字承諾了數字本身賺不到的詮釋 —— 常數 outcome 在它上面拿 1 分。
兩條 frontier 之間的區域叫 **admissible region**（可容許區域）：那是觀測值仍**被允許**出現的區域，
不等於宣稱那裡真的**有**觀測值。

## 三層結果都會報告

`print(fit)`、`summary(fit)` 與 `nsca_results(fit)` 都會報告：

1. Necessary Condition Analysis；
2. Sufficiency Condition Analysis；
3. 聯合 NSCA（含 `weakest_effect`、`balanced_joint_effect`、`joint_empty_zone_coverage`、`p_nsca_iut`、
   `degenerate` 旗標與合取判定）。

第三張表是前兩張表的合取摘要，不是第三個彼此獨立的資料分析。

## 一列一種 frontier,兩側必須相同

聯合效果量是一個**比較**。比較只有在兩邊用同一把尺量的時候才有意義。

包絡型(`ce_fdh`、`ce_vrs`)貼著資料走,空白面積最大;迴歸型(`cr_fdh`、`cr_vrs`、`cols`、`qr`、`c_lp`)會切進資料裡,面積較小。同樣 80 個點,同一側的 d 從 0.298 到 0.400,**純粹取決於畫法**。所以跨側混用等於讓「誰比較弱」由畫法決定,而不是由證據決定。

因此 `ceilings` 對兩側一體適用;給多個方法就產生多列,**每一列內部兩側相同**。比較不同列是敏感度分析;跨側混用則在結構上不可能發生。

`nsca_plot()` 不受此限。圖上不做任何數值比較,所以每一種方法都會在兩側畫出來。

## 雙門檻

對每一個 outcome 水準，兩個成分把條件軸切成三段。對 High X 方向：

| 區域 | 意義 |
|---|---|
| 低於 `necessity_threshold` | 這個水準**達不到** |
| 兩個門檻之間 | **有可能,但不保證** |
| 高於 `sufficiency_threshold` | frontier **保證**達到 |

對 Low X 方向，不等號反向：高於 necessity threshold 時達不到，低於
sufficiency threshold 時由 frontier 保證。`threshold_gap_actual` 已依方向
定向，所以四個方向中正值都代表存在未決區；若為負值，套件會報告
`frontiers overlap`，不再偷偷截成 0。

`necessity_sufficiency_interval` 是中間那段的寬度,也就是同一個結果水準上兩個門檻在 X 軸上的距離。在關係為確定性的地方會收縮成零,`threshold_status` 會標成 `exact correspondence`。**NCA 或 SCA 單獨都給不出這張表**,因為各自只提供其中一邊。

## 效能

引擎的 purity metrics 預設關閉(`purity = TRUE` 可開啟)。它只在**兩軸都未翻轉**的角落才計算,而 NSCA 用到的兩個角落最多只有一個符合,且是哪一側取決於方向:`HH` 在必要性側、`LL` 在充分性側、`LH` 與 `HL` 兩側皆無。

它在**很多觀測落在 frontier 上**時特別慢,而那正是 NSCA 要描述的近確定性情形 —— 完美對角線會讓每一個點都成為 frontier 點。NSCA 沒有任何統計量用到它。

## 詮釋上的限制

- 效果量是相對於所宣告 scope 的面積,會隨 scope 改變。
- 面積型測度對 X 或 Y 的單調變換**不具不變性**。這是從 NCA 繼承的性質,此處所有量都適用。
- 置換檢定問的是「打散 X-Y 配對之後,這麼大的空區是否罕見」。它不建立機制。
- 空區型態不等於確定性充要條件關係的證明。時序、研究設計、測量品質、scope、樣本外驗證都仍然重要。

## 引用

必要性由 NCA(Jan Dul、Govert Buijs)計算,充分性由 SCAtools 計算。

- Dul, J. (2016). Necessary Condition Analysis (NCA). *Organizational Research Methods, 19*(1), 10-52.
- Berger, R. L. (1982). Multiparameter hypothesis testing and acceptance sampling. *Technometrics, 24*(4), 295-300.

## 參照線：OLS 只畫，不計入

`ceilings` 一直都拒絕 `"ols"`：它估的是集中趨勢而不是空區，所以既不能當必要性
成分、也不能當充分性成分。0.4.1 起它仍然可以畫出來：

```r
fit <- nsca_analysis(
  dat, "X", "Y", direction = "HH",
  ceilings = "ce_fdh",
  reference = "ols"
)

nsca_reference(fit)   # 截距、斜率、R 平方
nsca_plot(fit)        # 灰色點線，不進成分圖例
```

兩個成分完全不受影響：參照線不進任何 joint index、不進 threshold、不進 p 值、
也不進 joint support 判定。畫它的理由是平均效果與條件分析回答的是同一個關係的
不同問題，並列呈現比合併成一個數字更有資訊。

