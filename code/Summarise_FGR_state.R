
library(tidyverse)
library(ggplot2)
# library(waffle)
library(ggtext)
# library(patchwork)
# library(paletteer)
# library(ggraph)
# library(igraph)
library(RColorBrewer)
library(Polychrome)
library(plotly)

### dirs -----------------------------------------------------------------------

wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Data-analysis/Forest-Genetic-Resources/"
dirData <- paste0(wd,"data-raw/")

### read in data ---------------------------------------------------------------

df.FGR <- read.csv(paste0(dirData,"FGR_summary_figure2.csv"))
head(df.FGR)
summary(df.FGR)
str(df.FGR)

# sort out columns
# df.FGR <- dfFGR %>% 
#   mutate(Molecular.studies = recode(Molecular.studies,
#                                     is.na(Molecular.studies) = 0,
#                                     1 = 1))

df.FGR$Molecular.studies[which(is.na(df.FGR$Molecular.studies))] <- 0
df.FGR$Common.garden.expts[which(is.na(df.FGR$Common.garden.expts))] <- 0

#df.FGR$Molecular.studies <- as.factor(df.FGR$Molecular.studies)
#df.FGR$Common.garden.expts <- as.factor(df.FGR$Common.garden.expts)

df.FGR[,3:12] <- df.FGR[,3:12] %>% mutate_if(is.character, as.numeric)
str(df.FGR)

### wrangle --------------------------------------------------------------------

# # remove 'traits studied' and remove duplicates for now
# 
# df.clean <- dfFGR %>% 
#   mutate(Traits.studied = NULL) %>% 
#   distinct()

# change 'yes' to count

# df.FGR$Provenance.trials[which(df.FGR$Provenance.trials == "Yes")] <- 1
# df.FGR$In.situ.conservation.GCU[which(df.FGR$In.situ.conservation.GCU == "Yes")] <- 1
# df.FGR$Ex.situ.conservation.seed.bank[which(df.FGR$Ex.situ.conservation.seed.bank == "Yes")] <- 1
# df.FGR$Ex.situ.conservation.clone.bank[which(df.FGR$Ex.situ.conservation.clone.bank == "Yes")] <- 1
# 
# df.FGR$Provenance.trials <- as.numeric(df.FGR$Provenance.trials)
# df.FGR$In.situ.conservation.GCU <- as.numeric(df.FGR$In.situ.conservation.GCU)
# df.FGR$Ex.situ.conservation.clone.bank <- as.numeric(df.FGR$Ex.situ.conservation.clone.bank)
# df.FGR$Ex.situ.conservation.seed.bank <- as.numeric(df.FGR$Ex.situ.conservation.seed.bank)

# summarise

# df.clean <- df.FGR %>% 
#   replace(is.na(.), 0) %>%
#   mutate(coverage = rowSums(across(where(is.numeric))),
#          Scientific.name = NULL,
#          Genomic.characterisation = NULL,
#          ct = 1) %>% 
#   group_by(coverage,Species, Understanding.of.diversity) %>% 
#   summarise(
#     n = sum(ct)) %>% 
#   ungroup()


### waffle plot ----------------------------------------------------------------
# 
# ggplot(data = df.clean, aes(values = n, fill = Understanding.of.diversity))+
#   geom_waffle(n_cols = 2,
#               #n_rows = 3,
#               color = "white",
#               na.rm = TRUE)+
#   facet_grid(~coverage)+
#   coord_equal()+
#   theme_bw()
# 
# ### sunburst plot --------------------------------------------------------------
# 
# # reorder is close to order, but is made to change the order of the factor levels.
# df.clean$Species = with(df.clean, reorder(Species, coverage, decreasing = T))
# 
# # Add lines to the initial dataset
# data <- df.clean
# empty_bar <- 10
# to_add <- matrix(NA, empty_bar, ncol(data))
# colnames(to_add) <- colnames(data)
# data <- rbind(data, to_add)
# data$id <- seq(1, nrow(data))
# 
# # edit labels
# # Get the name and the y position of each label
# label_data <- data
# number_of_bar <- nrow(label_data)
# angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust <- ifelse( angle < -90, 1, 0)
# label_data$angle <- ifelse(angle < -90, angle+180, angle)
# 
# ggplot(data, aes(coverage, Species, color = Understanding.of.diversity)) +
#   # This segment represents the vertical lines
#   geom_segment(aes(x = Species, xend = Species, y = 0, yend = coverage), linewidth = 1) +
#   # Add points on the end of each line
#   geom_point(aes(x = Species, y = coverage), size = 2) +
#   geom_text(data=label_data, aes(x=id, y=coverage+2, label=Species, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
#   scale_color_paletteer_d(`"awtools::a_palette"`)+
#   coord_polar(start = 0)+
#   theme_minimal()


### new type -------------------------------------------------------------------

# example https://r-graph-gallery.com/313-basic-circle-packing-with-several-levels.html
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
flare <- ggraph::flare
edges <- flare$edges

# # Usually we associate another dataset that give information about each node of the dataset:
# vertices <- flare$vertices
# 
# # Then we have to make a 'graph' object using the igraph library:
# mygraph <- graph_from_data_frame( edges, vertices=vertices )
# 
# # Make the plot
# ggraph(mygraph, layout = 'circlepack') + 
#   geom_node_circle() +
#   theme_void()
# 
# ### convert df to edge list
# 
# #e.g.
# #genomic.char - spp
# #no.char - spp
# #seed.stand.orchard - spp
# #seed.stand - spp
# #seed.orchard - spp
# 
# str(df.FGR)
# FGR.summary <- df.FGR %>% 
#   group_by(Species) %>% 
#   summarise(Genomic.data1 = ifelse(Molecular.studies > 0 | Common.garden.expts > 0, 1, NA), # one source of genomic characterisation
#             Genomic.data2 = ifelse(Molecular.studies > 0 & Common.garden.expts > 0, 1, NA), # two sources of genomic charactertisation
#             Seed.source.1 = ifelse(Seed.stands.count > 0 | Seed.orchards.count > 0, 1, NA), # one type of seed source
#             Seed.source.2 = ifelse(Seed.stands.count > 0 & Seed.orchards.count > 0, 1, NA), # two types of seed source 
#             Total.seed.stands = ifelse(Seed.stands.count > 0 , sum(Seed.stands.count), NA),
#             Total.seed.orchards = ifelse(Seed.orchards.count > 0, sum(Seed.orchards.count), NA),
#             Conservation = ifelse(Gene.conservation.units > 0 & Ex.situ.germplasm > 0 | Gene.conservation.units > 0 & Ex.situ.collections.living > 0, "Both",
#                            ifelse(Ex.situ.germplasm >= 0 | Ex.situ.collections.living >= 0, "Ex-situ",
#                                   ifelse(Gene.conservation.units > 0, "In-situ", NA)))) %>% 
#   pivot_longer(cols = Genomic.data1:Seed.source.2,
#                names_to = "Type",
#                values_to = "Coverage")
# 
# # make longer
# 
# # FGR.edges <- FGR.summary %>% 
# #   pivot_longer(cols = Genomic.char:Seed.sources,
# #                names_to = "edges") %>% 
# #   mutate(value = NULL)
# # 
# # ### vertices - extra info
# # # spp - total count of stands/orchards
# # # spp - consv.status - in.situ/ex.situ/none
# # 
# # FGR.vertices <- FGR.edges[,c(1,2:3)]
# # 
# # FGR.edges <- FGR.edges %>% 
# #   mutate(Seed.sources.count = NULL,
# #          Consv = NULL)
# # 
# # colnames(FGR.edges) <- c("to","from")
# # colnames(FGR.vertices) <- c("name", "count", "conservation")
# # 
# # # Then we have to make a 'graph' object using the igraph library:
# # FGRgraph <- graph_from_data_frame(FGR.edges, vertices = FGR.vertices)
# # 
# # # Make the plot
# # ggraph(FGRgraph, layout = 'circlepack') + 
# #   geom_node_circle() +
# #   theme_void()
# 
# 
# # simple option
# 
# ggplot(FGR.summary)+#, 
#        #aes(Seed.sources, Species, fill = Conservation)) +
#   # This segment represents the vertical lines
#   #geom_segment(aes(x = Species, xend = Species, y = 0, yend = Seed.sources.count), linewidth = 1) +
#   # Add points on the end of each line
#   geom_col(aes(x = Species, y = Total.seed.stands, fill = Conservation)) +
#   #geom_text(data=label_data, aes(x=id, y=coverage+2, label=Species, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
#   #scale_color_paletteer_d(`"awtools::a_palette"`)+
#   coord_polar(start = 0)+
#   #facet_wrap(~Genomic.char)+
#   theme_minimal()

### v2 -------------------------------------------------------------------------

# convert to binary and select only the new columns
df.FGR.summary <- df.FGR |> 
  mutate(seed.stands = ifelse(Seed.stands.count > 0, 1, NA),
         seed.orchards = ifelse(Seed.orchards.count > 0, 1, NA),
         seed.none = ifelse(is.na(seed.stands & seed.orchards), 1, NA),
         genomic.molecular = ifelse(Molecular.studies > 0, 1, NA),
         genomic.trial = ifelse(Common.garden.expts > 0, 1, NA),
         genomic.none = ifelse(is.na(genomic.molecular) & is.na(genomic.trial), 1, NA),
         consv.in.situ = ifelse(Gene.conservation.units > 0, 1, NA),
         consv.ex.situ = ifelse(Ex.situ.germplasm >= 0 | Ex.situ.collections.living >= 0, 1, NA),
         consv.both = ifelse(consv.in.situ > 0 & consv.ex.situ > 0, 1, NA),
         consv.none = ifelse(is.na(consv.in.situ) & is.na(consv.ex.situ) & is.na(consv.both), 1, NA)) |> 
  select(Species, 
         seed.stands, seed.orchards, seed.none,
         genomic.molecular, genomic.trial, genomic.none,
         consv.in.situ, consv.ex.situ, consv.none)

# palette faff
colorCount <- length(unique(df.FGR.summary$Species))
getPalette <- colorRampPalette(brewer.pal(min(12, colorCount), "Paired"))

# plot coverage of seed source types
(p1 <- df.FGR.summary |> 
  pivot_longer(cols = starts_with("seed"),
               names_to = "Seed.sources",
               values_to = "Source.count",
               values_drop_na = TRUE) |> 
  group_by(Seed.sources, Species) |> 
  tally() |> 
  ggplot()+
    geom_col(aes(Seed.sources,n, fill = Species))+
    scale_fill_manual(values = rev(getPalette(colorCount)), name = NULL) +
    scale_x_discrete(labels=c("seed.stands"="Seed stand(s)", 
                              "seed.orchards"="Seed orchard(s)",
                              "seed.none"="None"))+
    theme_bw()+
    labs(x='Seed sources available',
         y='Tree & shrub species count')+
    theme(legend.position = "bottom")+
    coord_flip())

# try ggplotly (converts to interactive)
ggplotly(p1, tooltip = c("Species"))

# plot coverage of whether or not species have genomic characterisation
(p2 <- df.FGR.summary|> 
  pivot_longer(cols = starts_with("genomic"),
               names_to = "Genomic.characterisation",
               values_to = "Genomic.count",
               values_drop_na = TRUE)|>
  group_by(Genomic.characterisation, Species) |> 
  tally() |> 
  #count(Source.count, sort = TRUE)
  ggplot()+
    geom_col(aes(Genomic.characterisation,n, fill = Species))+
    scale_fill_manual(values = rev(getPalette(colorCount)), name = NULL) +
    theme_bw()+
    coord_flip())

# ggplotly (converts to interactive)
ggplotly(p2, tooltip = c("Species"))

# plot if species have material conserved either in-situ (GCUs) or ex-situ (seed banks)
(p3 <- df.FGR.summary|> 
  pivot_longer(cols = starts_with("consv"),
               names_to = "Conservation",
               values_to = "Consv.count",
               values_drop_na = TRUE) |>
  group_by(Conservation, Species) |> 
  tally() |> 
  ggplot()+
    geom_col(aes(Conservation,n, fill = Species))+    
    scale_fill_manual(values = rev(getPalette(colorCount)), name = NULL) +
    theme_bw()+
    coord_flip())

# ggplotly (converts to interactive)
ggplotly(p3, tooltip = c("Species"))
