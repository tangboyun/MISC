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
module Template where

import Text.StringTemplate
import Types

fcFormula, afcFormula, lfcFormula :: String
fcFormula = "=SIGN(RC[6]-RC[7])*POWER(2,ABS(RC[6]-RC[7]))"
afcFormula = "=POWER(2,ABS(RC[4]-RC[5]))"
lfcFormula = "=RC[5]-RC[6]"

ttestTemplate, tfcTemplate, fcTemplate, lfcTemplate, afcTemplate, rgTemplate, tabHeaderTemplate :: Stringable a => StringTemplate a
ttestTemplate = newSTMP
                "=T.TEST(RC[$rc1Beg$]:RC[$rc1End$],RC[$rc2Beg$]:RC[$rc2End$],2,2)"

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

tabHeaderTemplate = newSTMP "$s1$ vs $s2$ $fc$ fold $reg$ regulated $mol$"


groupTemplate :: Stringable a => StringTemplate a
groupTemplate =
  newSTMP "# Fold Change cut-off: $fcCutOff$\n\
  \# P-value cut-off: $pCutOff$\n\
  \# Condition pairs:  $gName1$ vs $gName2$\n\
  \\n\
  \# Column A: ProbeName, it represents probe name.\n\
  \# Column B: P-value, the p-values calculated from $pairOrUnpair$ t-test.\n\
  \# Column C: FC (abs), Absolute Fold change between two groups. \n\
  \# Column D: Regulation, it depicts which group has greater or lower intensity values wrt other group.\n\
  \# Column E, F: Raw intensity of each group.\n\
  \# Column G, H: Normalized intensity of each group (log2 transformed).\n\
  \# Column $rawBeg$ ~ $rawEnd$: Raw intensity of each sample.\n\
  \# Column $norBeg$ ~ $norEnd$: Normalized intensity of each sample (log2 transformed).\n\
  \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$."

sampleTemplate :: Stringable a => Setting -> StringTemplate a
sampleTemplate s = newSTMP (sampleStr s)


sampleStr :: Setting -> String
sampleStr (Setting _ rna spec) =
  case rna of
    Coding -> commonStr
    _      -> commonStr ++ "\n\n" ++ sourceStr ++ source spec ++ relationStr
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
      \# Column F, G: Raw intensity of each sample.\n\
      \# Column H, I: Normalized intensity of each sample (log2 transformed).\n\
      \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$.\n"
    relationStr = 
      "# Columns $relaBeg$ ~ $relaEnd$: the relationship of LncRNA and its nearby coding gene and the coordinate of the coding gene, \
      \including relationship, Associated_gene_acc, Associated_gene_name, Associated_gene_strand, \
      \Associated_gene_start, Associated_gene_end.\n\
      \\"sense_overlapping\": the LncRNA's exon is overlapping a coding transcript exon on the same genomic strand;\n\
      \\"intronic\": the LncRNA is overlapping the intron of a coding transcript on the same genomic strand;\n\
      \\"natural antisense\": the LncRNA is transcribed from the antisense strand and overlapping with a coding transcript; \n\
      \\"non-overlapping antisense\": the LncRNA is transcribed from the antisense strand without sharing overlapping exons;\n\
      \\"bidirectional\": the LncRNA is oriented head to head to a coding transcript within 1000 bp;\n\
      \\"intergenic\": there are no overlapping or bidirectional coding transcripts nearby the LncRNA."
    sourceStr = 
      "Note: \n\
      \# Column $source$: source, the source of LncRNA is collected from.\n"
    source s = case s of
      Human -> insert humanSource ++ "\n\n"
      Mouse -> insert mouseSource ++ "\n\n"
      _     -> ""
      where
        insert c =
          "RefSeq_NR: RefSeq validated non-coding RNA;\n\
          \UCSC_knowngene: UCSC known genes annotated as \"non-coding\", \"near-coding\" and \"antisense\" \
          \(http://genome.ucsc.edu/cgi-bin/hgTables/);\n\
          \Ensembl: Ensembl (http://www.ensembl.org/index.html);\n" ++ c ++
          "RNAdb: RNAdb2.0 (http://research.imb.uq.edu.au/rnadb/);\n\
          \NRED: NRED (http://jsm-research.imb.uq.edu.au/nred/cgi-bin/ncrnadb.pl);\n\
          \UCR: \"ultra-conserved region\" among human, mouse and rat (http://users.soe.ucsc.edu/~jill/ultra.html);\n\
          \lincRNA: lincRNA identified by John Rinn's group (Guttman et al. 2009; Khalil et al. 2009);\n\
          \misc_lncRNA: other sources."
        humanSource = "H-invDB: H-invDB (http://www.h-invitational.jp/);\n"
        mouseSource = "Fantom: Fantom project (http://fantom.gsc.riken.jp/);\n"

