-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Report.Sheet.Template where

import Data.List
import Report.Types
import Text.Printf
import Text.StringTemplate

vsRegex,log2Regex,cutOffRegex :: String
vsRegex = "[^:]+ vs [[:print:]]+"
log2Regex = "[Ll]og2 "
cutOffRegex = "[[:space:]][[:digit:]]\\.[[:digit:]]{1,2}[[:space:]]"


fcFormula, afcFormula, lfcFormula :: Int -> String
fcFormula n = "=SIGN(RC[" ++ show (6+n) ++ "]-RC[" ++ show (7+n) ++ 
              "])*POWER(2,ABS(RC[" ++ show (6+n) ++ "]-RC[" ++ show (7+n) ++ "]))"
afcFormula n = "=POWER(2,ABS(RC[" ++ show (4+n) ++ "]-RC[" ++ show (5+n) ++ "]))"
lfcFormula n = "=RC[" ++ show (5+n) ++ "]-RC[" ++ show (6+n) ++ "]"

gFCAbsTemplate :: Stringable a => StringTemplate a
gFCAbsTemplate =
  newSTMP
  "=POWER(2,ABS(AVERAGE(RC[$g1Beg$]:RC[$g1End$])-AVERAGE(RC[$g2Beg$]:RC[$g2End$])))"


toTTestTemplate :: Stringable a => TTest -> StringTemplate a
toTTestTemplate tCon = 
  newSTMP $ 
  "=TTEST(RC[$g1Beg$]:RC[$g1End$],RC[$g2Beg$]:RC[$g2End$],2,"++ typeStr ++")"
  where
    typeStr = case tCon of
      Paired -> "1"
      _      -> "2"

tfcTemplate, fcTemplate, lfcTemplate, afcTemplate, rgTemplate, tabHeaderTemplate, grawTemplate, gnorTemplate :: Stringable a => StringTemplate a

avgStr :: Int -> Int -> String
avgStr beg end =
  "=AVERAGE(" ++ (intercalate ", " $ map (\i -> "RC[" ++ show i ++ "]") [beg..end]) ++ ")"

tfcTemplate = newSTMP
              "=SUM(RC[$rc3Beg$]:RC[$rc3End$])/SUM(RC[$rc4Beg$]:RC[$rc4End$])"

fcTemplate = newSTMP
             "Fold change([$s1$] vs [$s2$])"
lfcTemplate = newSTMP
              "Log Fold change([$s1$] vs [$s2$])"
afcTemplate = newSTMP
              "Absolute Fold change([$s1$] vs [$s2$])"
              
rgTemplate = newSTMP
             "Regulation([$s1$] vs [$s2$])"
grawTemplate = newSTMP
               "[$g$](raw)"
gnorTemplate = newSTMP
               "[$g$](normalized)"

tabHeaderTemplate = newSTMP "$s1$ vs $s2$ $fc$ fold $reg$ regulated $mol$"


groupTemplate :: Stringable a => Setting -> StringTemplate a
groupTemplate = newSTMP . groupStr

groupStr :: Setting -> String  
groupStr (Setting _ rna spec _) =
  case rna of
    Coding -> commonStr
    _      -> intercalate "\n\n" $ [commonStr, sourceStr spec, relationStr spec]
  where
    commonStr =
      "# Fold Change cut-off: $fcCutOff$\n\
      \# P-value cut-off: $pCutOff$\n\
      \# Condition pairs:  $gName1$ vs $gName2$\n\
      \\n\
      \# Column A: ProbeName, it represents probe name.\n\
      \# Column B: P-value, the p-values calculated from $pairOrUnpair$ t-test.\n\
      \# Column C: Absolute Fold change, the absolute fold change between two groups. \n\
      \# Column D: Regulation, it depicts which group has greater or lower intensity values wrt other group.\n\
      \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$.\n\
      \# Column $grBeg$, $grEnd$: Raw intensity of each group.\n\
      \# Column $gnBeg$, $gnEnd$: Normalized intensity of each group (log2 transformed).\n\
      \# Column $rawBeg$ ~ $rawEnd$: Raw intensity of each sample.\n\
      \# Column $norBeg$ ~ $norEnd$: Normalized intensity of each sample (log2 transformed).\n"

sampleTemplate :: Stringable a => Setting -> StringTemplate a
sampleTemplate = newSTMP . sampleStr


sampleStr :: Setting -> String
sampleStr (Setting _ rna spec _) =
  case rna of
    Coding -> commonStr
    _      -> intercalate "\n\n" $ [commonStr, sourceStr spec, relationStr spec]
  where
    commonStr =
      "# Fold Change cut-off: $fc$\n\
      \# Condition pairs:  $s1$ vs $s2$\n\
      \\n\
      \# Column A: ProbeName, it represents probe name.\n\
      \# Column B: Fold change, positive value indicates up-regulation and negative value indicates down-regulation.\n\
      \# Column C: Log Fold change, log2 value of absolute fold change. \
      \Positive value indicates up-regulation and negative value indicates down-regulation.\n\
      \# Column D: Absolute Fold change between two samples. \n\
      \# Column E: Regulation, it depicts which sample has greater or lower intensity values wrt other sample.\n\
      \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$.\n\
      \# Column $srBeg$, $srEnd$: Raw intensity of each sample.\n\
      \# Column $snBeg$, $snEnd$: Normalized intensity of each sample (log2 transformed).\n"

relationStr s = 
  "# Columns $relaBeg$ ~ $relaEnd$: the relationship of lncRNA and its nearby coding gene and the coordinate of the coding gene, \
  \including relationship, Associated_gene_acc, Associated_gene_name, Associated_gene_strand, Associated_gene_start, Associated_gene_end.\n" ++ 
  relaSpecific
  where
    relaSpecific = case s of
      Rat -> 
        "\"sense_exon_overlap\": the LncRNA's exon is overlapping a coding transcript exon on the same genomic strand;\n\
        \\"sense_intron_overlap\": the LncRNA is overlapping the intron of a coding transcript on the same genomic strand;\n\
        \\"antisense_exon_overlap\": the LncRNA is transcribed from the antisense strand and overlapping with a coding transcript; \n\
        \\"antisense_intron_overlap\": the LncRNA is transcribed from the antisense strand without sharing overlapping exons;\n\
        \\"bidirection\": the LncRNA is oriented head to head to a coding transcript within 1000 bp;\n\
        \\"intergenic\": there are no coding transcripts within 30 kb of the LncRNA;\n\
        \\"others\":  means those LncRNAs, within 30kb of which, there are non-overlapping coding transcripts transcribed from same strand; \
        \or, there are non-overlapping coding transcripts transcribed tail to tail; \
        \or there are non-overlapping coding transcripts transcribed head to head with their TSSs separated by more than 1 kb.\n"
      Mouse ->
        "\"sense overlap\": the LncRNA's exon is overlapping a coding transcript exon on the same genomic strand;\n\
        \\"antisense overlap\": the lncRNA is transcribed from the antisense strand and overlapping with a coding transcript;\n\
        \\"bidirectional\": the LncRNA is oriented head to head to a coding transcript within 1000 bp;\n\
        \\"intergenic\": there are no overlapping or bidirectional coding transcripts nearby the LncRNA.\n"
      Human ->
        "\"exon sense-overlapping\": the LncRNA's exon is overlapping a coding transcript exon on the same genomic strand;\n\
        \\"intron sense-overlapping\": the LncRNA is overlapping the intron of a coding transcript on the same genomic strand;\n\
        \\"intronic antisense\": the LncRNA is overlapping the intron of a coding transcript on the antisense strand;\n\
        \\"natural antisense\": the LncRNA is transcribed from the antisense strand and overlapping with a coding transcript;\n\
        \\"bidirectional\": the LncRNA is oriented head to head to a coding transcript within 1000 bp;\n\
        \\"intergenic\": there are no overlapping or bidirectional coding transcripts nearby the LncRNA.\n"


sourceStr s = 
  "Note: \n\
  \# Column $source$: source, the source of LncRNA is collected from.\n" ++ source
  where
    source = case s of
      Human -> insertStr humanSource 
      Mouse -> insertStr mouseSource 
      _     -> ratSource 
    insertStr c =
          "RefSeq_NR: RefSeq validated non-coding RNA;\n\
          \UCSC_knowngene: UCSC known genes annotated as \"non-coding\", \"near-coding\" and \"antisense\" \
          \(http://genome.ucsc.edu/cgi-bin/hgTables/);\n\
          \Ensembl: Ensembl (http://www.ensembl.org/index.html);\n" ++ c ++
          "RNAdb: RNAdb2.0 (http://research.imb.uq.edu.au/rnadb/);\n\
          \NRED: NRED (http://jsm-research.imb.uq.edu.au/nred/cgi-bin/ncrnadb.pl);\n\
          \UCR: \"ultra-conserved region\" among human, mouse and rat (http://users.soe.ucsc.edu/~jill/ultra.html);\n\
          \lincRNA: lincRNA identified by John Rinn's group (Guttman et al. 2009; Khalil et al. 2009);\n\
          \misc_LncRNA: other sources."
    humanSource = "H-invDB: H-invDB (http://www.h-invitational.jp/);\n"
    mouseSource = "Fantom: Fantom project (http://fantom.gsc.riken.jp/);\n"
    ratSource = "RefSeq_NR: RefSeq validated non-coding RNA;\n\
                \RefSeq_XR: RefSeq un-validated non-coding RNA;\n\
                \mouse_ortholog: rat LncRNAs which are obtained by sequence comparison with mouse LncRNAs;\n\
                \UCR: \"ultra-conserved region\" among human, mouse and rat (http://users.soe.ucsc.edu/~jill/ultra.html);\n\
                \misc_LncRNA: other sources.\n"




allTargetTemplate :: Stringable a => Setting -> StringTemplate a
allTargetTemplate = newSTMP . allTargetStr 

allTargetStr :: Setting -> String
allTargetStr (Setting _ rna spec _) =
  case rna of
    Coding -> commonStr
    _      -> intercalate "\n\n" $ [commonStr, sourceStr spec, relationStr spec]
  where commonStr =     
          "All Targets Value ($atHeadStr$)\n\
          \# Column A: ProbeName, it represents the probe name.\n\
          \# Column $rawBeg$ ~ $rawEnd$: Raw Intensity of each sample.\n\
          \# Column $norBeg$ ~ $norEnd$: Log2 value of Normalized Intensity of each sample.\n\
          \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$.\n"

boxPlotStr :: String
boxPlotStr =
  "Box Plot\n\n\
  \    The boxplot is a traditional method for visualizing the distribution of a dataset. \
  \They are most useful for comparing the distributions of several datasets.\n\n\
  \    Here, a boxplot view is used to look at,  and compare, the distributions of expression values for \
  \the samples or conditions in an experiment after normalization.\n\n\
  \    Press Ctrl and rolling button of your mouse to zoom in.\n"

scatterPlotStr :: String
scatterPlotStr =
  "Scatter Plot\n\n\
  \    The scatterplot is a visualization that is useful for assessing \
  \the variation (or reproducibility) between chips.\n\n\   
  \    Press Ctrl and rolling button of your mouse to zoom in.\n"

clusteringTemplate :: Stringable a => Setting -> StringTemplate a
clusteringTemplate (Setting chip rna _ _) =
  newSTMP $
  "Heat Map and Unsupervised Hierarchical Clustering\n\n\
  \    Hierarchical clustering is one of the simplest and widely used clustering \
  \techniques for analysis of gene expression data. Cluster analysis arranges samples into \
  \groups based on their expression levels, which allows us to hypothesize about the relationships \
  \among samples. The dendrogram shows the relationships among the expression levels of samples.\n\n\      
  \    Here, hierarchical clustering was performed based on \"All Targets Value\". \
  \Your experiment consists of $nSample$ different samples. The result of hierarchical clustering on \
  \conditions shows distinguishable " ++ polymer ++  " expression profiling among samples.\n\n\
  \    Press Ctrl and rolling button of your mouse to zoom in.\n"
  where
    polymer = case chip of
                GE -> "gene"
                _  -> case rna of
                       Coding -> "mRNAs"
                       _      -> "LncRNAs"

volcanoPoltStr :: CutOff -> Setting -> String
volcanoPoltStr (C fc (Just (_,p))) (Setting c r _ _) =
  "Volcano Plots\n\n\
  \    Volcano Plots are useful tools for visualizing differential expression between two different conditions. \
  \They are constructed using fold-change values and P-values, and thus allow you to visulaize the relationship \
  \between fold-change (magnitude of change) and statistical significance \
  \(which takes both magnitude of change and variability into consideration). \
  \They also allow subsets of genes to be isolated, based on those values.\n\n\
  \    The vertical lines correspond to "++ fcCutOff ++"-fold up and down and the horizontal line \
  \represents a P-value of "++ pCutOff ++ ". So the red point in the plot represents the differentially \
  \expressed " ++ polymer ++ " with statistical significance.\n\n\
  \    Press Ctrl and rolling button of your mouse to zoom in.\n\
  \    Note: Points with p <= 1e-4 are set to 1e-4.\n"
  where
    fcCutOff = printf "%.1f" fc
    pCutOff = printf "%.2f" p
    polymer = case c of
                GE -> "genes"
                _  -> case r of
                       Coding -> "mRNAs" 
                       _      -> "LncRNAs"
