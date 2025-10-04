#Day2
#task1
library(cowplot)

# --- Location plot 本体（凡例なし） ---
p1_core <- ggplot(all_summary,
                  aes(x = "All", y = percent, fill = region_type)) +
  geom_bar(stat = "identity", width = 0.25) +
  geom_text(aes(label = sprintf("%.2f%%", percent * 100)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
  scale_fill_manual(values = loc_cols,
                    breaks = c("distal_intergenic","distal_intragenic","proximal")) +
  labs(x=NULL, y="Percentage (%)", fill="Location", title="Location") +
  theme_classic(base_size=12) +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5, face="bold"))

# Location 凡例だけ取り出し
leg1 <- get_legend(
  p1_core + theme(legend.position="right", legend.title=element_text(face="bold"))
)

# Location の図＋凡例を横に並べて1パネル
p1_with_leg <- plot_grid(p1_core, leg1, ncol=2, rel_widths=c(3,1))

# --- Transcription plot 本体（凡例なし） ---
p2_core <- ggplot(intergenic_summary,
                  aes(x="Intergenic", y=percent, fill=trxn_type)) +
  geom_bar(stat="identity", width = 0.25) +
  geom_text(aes(label=sprintf("%.2f%%", percent*100)),
            position=position_stack(vjust=0.5),
            color="white", fontface="bold", size=4) +
  scale_y_continuous(labels=percent_format(accuracy = 1), limits=c(0,1)) +
  scale_fill_manual(values=trxn_cols,
                    breaks=c("untranscribed","ambiguous","transcribed")) +
  labs(x=NULL, y="Percentage (%)", fill="Transcription", title="Transcription") +
  theme_classic(base_size=12) +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5, face="bold"))

# Transcription 凡例
leg2 <- get_legend(
  p2_core + theme(legend.position="right", legend.title=element_text(face="bold"))
)
# Transcription の図＋凡例
p2_with_leg <- plot_grid(p2_core, leg2, ncol=2, rel_widths=c(3,1))
# --- 最後に2つのパネルを横並びに ---
final_plot <- plot_grid(p1_with_leg, p2_with_leg, ncol=2)
# 保存
ggsave("demo.bar.pretty.one_plot.zoom.legend.pdf", final_plot,
       width=14, height=5, device="pdf")

#Can you combine scope All and scope Intergenic into one signal plot as two values on x axis, keeping the same color and order of the stacks.
library(dplyr)
library(ggplot2)
library(scales)

# ---- データ準備 ----
# All summary（Location）
all_summary <- dat2 %>%
  filter(!is.na(region_type)) %>%
  count(region_type, name = "n") %>%
  mutate(percent = n / sum(n),
         scope = "All",
         fill_var = region_type)

# Intergenic summary（Transcription）
intergenic_summary <- dat2 %>%
  filter(region_type == "distal_intergenic", !is.na(trxn_type)) %>%
  count(trxn_type, name = "n") %>%
  mutate(percent = n / sum(n),
         scope = "Intergenic",
         fill_var = trxn_type)

# 結合
plot_df <- bind_rows(all_summary, intergenic_summary)

# ---- 色と順序の設定 ----
fill_levels <- c("distal_intergenic","distal_intragenic","proximal",
                 "untranscribed","ambiguous","transcribed")

fill_colors <- c("distal_intergenic"="#fdbf6f",
                 "distal_intragenic"="#ff7f00",
                 "proximal"          ="#984ea3",
                 "untranscribed"     ="#377eb8",
                 "ambiguous"         ="#4daf4a",
                 "transcribed"       ="#e41a1c")

plot_df$fill_var <- factor(plot_df$fill_var, levels = fill_levels)

# ---- プロット ----
p <- ggplot(plot_df, aes(x = scope, y = percent, fill = fill_var)) +
  geom_bar(stat="identity", width=0.5) +
  geom_text(aes(label = sprintf("%.2f%%", percent*100)),
            position=position_stack(vjust=0.5),
            color="white", fontface="bold", size=4) +
  scale_fill_manual(values = fill_colors, breaks = fill_levels) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0,1)) +
  labs(x=NULL, y="Percentage (%)", fill=NULL, title="CRE summary") +
  theme_classic(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# ---- 保存 ----
ggsave("demo.bar.pretty.one_plot.pdf", p, width=8, height=6, device="pdf")

