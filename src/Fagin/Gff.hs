{-# LANGUAGE ScopedTypeVariables #-}

{-|

Perhaps the most error prone step of preparing data for Fagin is gathering
correct GFF files for each species.

Here are the lexical requirements:

 * has 9 TAB-delimited columns
 * column 1 matches the names of an entry in a genome fasta file
 * column 3 must include `mRNA`, `exon` and `CDS` labels
 * column 4 and 5 must be counting numbers where c5 > c4
 * column 7 must contain strand info
    [@+@] plus strand
    [@-@] minus stand
    [@?@] stranded but unknown
    [@.@] unstranded
 * column 9 must be a ';'-delimited string of '<tag>=<value>' entries

I also impose these semantic requirements:

 * Every exon and CDS entry must have at least one Parent, where the value
   matchs the value of an mRNA's ID
 * Every mRNA must have at least one exon
 * Every mRNA must have at least one CDS
 * Every CDS is subsumed by an exon in the same mRNA

I am systematically more permissive than required by the specification

 * column 1 can be a string of any non-TAB characters
 * column 8 (phase) can be missing for CDS (I don't use it)
 * column 6 (score) can be anything (again, I don't use it)
 * I allow a single untagged value in column 9, which I assign to ID if ID is missing

There are several pathological cases that a GFF parsers needs to be able to
deal with. These are outlined in the GFF specification:

 * single exon genes
 * polycistronic transcripts
 * genes with inteins
 * trans-spliced trancripts
 * programmed frameshift
 * operons
 * circular genomes (well, I consider this pathological)

All of these need special consideration. Currently they are not handled.

-}


module Fagin.Gff (
    readGff
  , IntervalType(..)
  , GffEntry(..)
  , Attribute(..)
  -- exported just for benchmarking
  , readType''
  , readInt''
  , readStrand''
  , readAttributes''
  , toGff''
  , model2gff
) where

import Data.ByteString.Char8 (readInteger)

import Fagin.Prelude
import Fagin.Interval
import Fagin.Report

import Data.Map (Map)
import qualified Data.Map as DM

import qualified Control.Monad as CM

-- | Holds the types that are currently used by Fagin. I may extend this later.
-- Since these types are required to be Sequence Ontology terms, I really ought
-- to just import the whole ontology table and allow all terms. Then, since
-- this is an ontology, I might as well port the relations between the terms.
-- This would be a time-consuming task, but may be worthwhile eventually.
data IntervalType
  = MRna
  | CDS
  | Exon
  | Gene
  | Other !ConstantString
  deriving(Eq,Ord,Show,Generic,NFData)

instance BShow IntervalType where
  bshow MRna      = "mRNA"
  bshow CDS       = "CDS"
  bshow Exon      = "exon"
  bshow Gene      = "gene"
  bshow (Other t) = (fromShort t)

-- | Attributes of a GFF entry, exactly according to the specification. The
-- data constructors nearly follow the tag names, except that they have been
-- converted to camel case, as per Haskell conventions, for example
-- "Derives_from" is converted to "DerivesFrom".
data Attribute
    -- | The unique ID for this entry. Presence in more than one GFF entry
    -- implies the entries are members of a single discontinuous feature (their
    -- types should be the same).
    = AttrID !ConstantString

    -- | The display name of the feature. Does not have to be unique.
    | AttrName !ConstantString

    -- | List of aliases for the feature (for example locus and model ids)
    | AttrAlias ![ConstantString]

    -- | List of parents of this feature. Indicates a part_of relationship.
    | AttrParent ![ConstantString]

    -- | Not currently used by Fagin
    | AttrTarget !ConstantString
    
    -- | Not currently used by Fagin
    | AttrGap !ConstantString

    -- | Not currently used by Fagin
    | AttrDerivesFrom !ConstantString

    -- | Free notes about the entry. These notes do not have to be quoted
    -- (according to the specification). Thus any special characters, which
    -- include commas, need to be encoded.
    | AttrNote ![ConstantString]

    -- | A database cross reference
    | AttrDbxref ![ConstantString]

    -- | Ontology cross reference
    | AttrOntologyTerm ![ConstantString]

    -- | Is the sequence circular (e.g. a mitochondrial or bacterial genome)
    | AttrIsCircular !Bool

    -- | The tags defined above are all the tags with predefined meanings.
    -- Users are free to use any additional flags they desire. These tags must
    -- be lowercase, since the spec reserves uppercase tags be for future
    -- official use.
    | AttrUserDefined !ConstantString !ConstantString

    -- | According to the spec, all entries in the attribute column should be
    -- wrapped in tag-value pairs. However, it is common for programs to add
    -- untagged values, which sometimes function as ID.
    | AttrUntagged !ConstantString
    deriving(Eq,Ord,Show,Generic,NFData)

instance BShow Attribute where
  bshow (AttrID           s     ) = "ID="            ++ fromShort s
  bshow (AttrName         s     ) = "Name="          ++ fromShort s
  bshow (AttrAlias        ss    ) = "Alias="         ++ unsplit ',' (map fromShort ss)
  bshow (AttrParent       ss    ) = "Parent="        ++ unsplit ',' (map fromShort ss)
  bshow (AttrTarget       s     ) = "Target="        ++ fromShort s
  bshow (AttrGap          s     ) = "Gap="           ++ fromShort s
  bshow (AttrDerivesFrom  s     ) = "Derives_from="  ++ fromShort s
  bshow (AttrNote         ss    ) = "Note="          ++ unsplit ',' (map fromShort ss)
  bshow (AttrDbxref       ss    ) = "Dbxref="        ++ unsplit ',' (map fromShort ss)
  bshow (AttrOntologyTerm ss    ) = "Ontology_term=" ++ unsplit ',' (map fromShort ss)
  bshow (AttrIsCircular   True  ) = "Is_circular=true"
  bshow (AttrIsCircular   False ) = "Is_circular=false"
  bshow (AttrUserDefined  t v   ) = (fromShort t) ++ "=" ++ (fromShort v)
  bshow (AttrUntagged     s     ) = "Untagged=" ++ (fromShort s)


-- | Holds the data from a GFF entry that is relevant to Fagin. Some GFF
-- columns are skipped. These may be added later, but for now I don't need
-- them.
data GffEntry = GffEntry {

    -- | GFF column 1. The name of the genomic scaffold and chromosome to which
    -- the feature maps
    gff_chrid :: !ConstantString

    -- | GFF column 3. The type of the feature. This must be a Sequence
    -- Ontology term or identification id.
    , gff_type :: !IntervalType

    -- | GFF column 4 and 5. The 1-based start and stop positions of this
    -- feature.
    , gff_interval :: !Interval

    {-| GFF column 7. The strand on which the interval resides. This must be
       one of the following:
        * '+' - plus sense
        * '-' - negative sense
        * '.' - strand is irrelevant
        * '?' - strand is relevant but unknown
    -}
    , gff_strand :: !(Maybe Strand)

    -- | GFF column 9. Feature attributes (see 'Attribute')
    , gff_attr :: ![Attribute]
  }
  deriving(Eq,Ord,Show,Generic,NFData)

-- | Construct a GffEntry from the full data of a parsed GFF line
gffEntry
  :: ByteString   -- ^ seqid
                  --   source
  -> IntervalType -- ^ type
  -> Integer      -- ^ start
  -> Integer      -- ^ stop
                  --   score
  -> Maybe Strand -- ^ strand
                  --   phase
  -> [Attribute]  -- ^ attributes
  -> GffEntry
gffEntry chrid   ftype a b   strand   attr = 
  GffEntry {
      gff_chrid    = (toShort chrid)
    , gff_type     = ftype
    , gff_interval = Interval a b
    , gff_strand   = strand
    , gff_attr     = attr
  }

instance BShow GffEntry where
  bshow GffEntry { 
      gff_chrid    = chrid
    , gff_type     = ftype
    , gff_interval = Interval a b
    , gff_strand   = strand
    , gff_attr     = attr
  } = unsplit '\t'
    [
        (fromShort chrid)
      , "."
      , bshow ftype
      , bshow a
      , bshow b
      , "."
      , maybe "." bshow strand
      , "."
      , (unsplit ';' . map bshow $ attr)
    ]

data GeneModel = GeneModel {
      model_chrid  :: !ConstantString         -- ^ chromosome/scaffold
    , model_modid  :: !(Maybe ConstantString) -- ^ model id
    , model_locid  :: !(Maybe ConstantString) -- ^ parent (a 'gene') is given
    , model_bound  :: !(Maybe Interval)       -- ^ bounds of the gene model
    , model_cds    :: ![Interval]             -- ^ list of coding intervals
    , model_exon   :: ![Interval]             -- ^ list of exon intervals
    , model_strand :: !(Maybe Strand)         -- ^ strand [+-.?]
  } deriving(Show,Generic,NFData)

-- this is mostly meant for diagnostics, I would usually just convert back to GFF and view that
-- but this gives a clean one-liner
instance BShow GeneModel where
  bshow GeneModel {
      model_chrid  = chrid  
    , model_modid  = modid  
    , model_locid  = locid  
    , model_bound  = bound  
    , model_cds    = cds    
    , model_exon   = exon   
    , model_strand = strand 
  } = unsplit '|' [say chrid, say locid, say modid, say bound, say cds, say exon, say strand] where
    say :: Show a => a -> ByteString
    say = pack . show



model2gff :: GeneModel -> ReportS [GffEntry]
model2gff GeneModel { model_modid  = Nothing } = fail' "BadGeneModel: no model id"
model2gff GeneModel { model_locid  = Nothing } = fail' "BadGeneModel: no locus (gene) id"
model2gff GeneModel { model_bound  = Nothing } = fail' "BadGeneModel: no model bound (lacking mRNA or gene entry)"
model2gff GeneModel { model_strand = Nothing } = fail' "BadGeneModel: no strand"
model2gff GeneModel { model_cds = []         } = fail' "BadGeneModel: no CDS included"
model2gff GeneModel {
      model_chrid  = chrid
    , model_modid  = Just modid
    , model_locid  = Just locid
    , model_bound  = Just bound
    , model_cds    = cdss
    , model_exon   = exons
    , model_strand = Just strand
  } =
  pass' $
    [   GffEntry {
            gff_chrid    = chrid
          , gff_type     = Gene
          , gff_interval = bound
          , gff_strand   = Just strand
          , gff_attr     = [AttrID locid]
        }
      , GffEntry {
            gff_chrid    = chrid
          , gff_type     = MRna
          , gff_interval = bound
          , gff_strand   = Just strand
          , gff_attr     = [AttrID modid, AttrParent [locid]]
        }
    ] ++ map (makeChild CDS) cdss ++ map (makeChild Exon) exons
  where
    makeChild :: IntervalType -> Interval -> GffEntry
    makeChild t i
      = GffEntry {
          gff_chrid    = chrid
        , gff_type     = t
        , gff_interval = i
        , gff_strand   = Just strand
        , gff_attr     = [AttrParent [modid]]
      }

type GParser a = ByteString -> ReportS a

readInt'' :: GParser Integer
readInt'' s = case {-# SCC "gffEntry_integer" #-} readInteger s of
  Just (x,"") -> pass' x
  _ -> fail' $ "GffParse: expected integer, found '" ++ s ++ "'"

readType'' :: GParser IntervalType
readType'' s = {-# SCC "gffEntry_type" #-} case s of
  ""                -> fail' "GffParse: exected <type> in column 3, found nothing"

  "gene"            -> pass' Gene
  "SO:0000704"      -> pass' Gene

  "mRNA"            -> pass' MRna
  "messenger RNA"   -> pass' MRna -- synonym
  "SO:0000234"      -> pass' MRna
  -- - this may or may not be a coding transcript
  -- - technically, mRNA is_a transcript, and a CDS or exon is only transitively
  --   a part of ta transcript.
  "transcript"      -> pass' MRna
  "SO:0000673"      -> pass' MRna

  "CDS"             -> pass' CDS
  "coding_sequence" -> pass' CDS -- synonym
  "coding sequence" -> pass' CDS -- synonym
  "SO:0000316"      -> pass' CDS

  "exon"            -> pass' Exon
  "SO:0000147"      -> pass' Exon
  -- This is slightly more specific then exon, hence the different SO id,
  -- however, it is not terribly common.
  "coding_exon"     -> pass' Exon
  "coding exon"     -> pass' Exon -- synonym
  "SO:0000195"      -> pass' Exon
  x                 -> pass' $ Other (toShort x)

readStrand'' :: GParser (Maybe Strand)
readStrand'' s = {-# SCC "gffEntry_strand" #-} case s of
  "+" -> pass' $ Just Plus
  "-" -> pass' $ Just Minus
  "." -> pass' Nothing -- The distinction between '.' and '?' is a nuance I
  "?" -> pass' Nothing -- will ignore for now
  v   -> fail' $ "GffParse: expected strand from set [+-.?], found '" ++ v

readAttributes'' :: GParser [Attribute]
readAttributes'' =
    sequenceR
  . map toAttr
  . map (split '=')
  . split ';'
  where

  toAttr :: [ByteString] -> ReportS Attribute

  toAttr ["ID",       v] = {-# SCC "readAttributes_id" #-}     pass' $!! AttrID  (toShort v)
  toAttr ["Parent" , vs] = {-# SCC "readAttributes_parent" #-} pass' $!! AttrParent $!! map toShort (split ',' vs)
  
  toAttr ["Name",   v] = pass' $!! AttrName    (toShort v)
  toAttr ["Target", v] = pass' $!! AttrTarget  (toShort v)
  toAttr ["Gap",    v] = pass' $!! AttrGap     (toShort v)
  toAttr ["Derives_from" , v] = pass' $!! AttrDerivesFrom  (toShort v)
  -- multiple value entries
  toAttr ["Alias"         , vs] = pass' $!! AttrAlias        $!! map toShort (split ',' vs)
  toAttr ["Note"          , vs] = pass' $!! AttrNote         $!! map toShort (split ',' vs)
  toAttr ["Dbxref"        , vs] = pass' $!! AttrDbxref       $!! map toShort (split ',' vs)
  toAttr ["Ontology_term" , vs] = pass' $!! AttrOntologyTerm $!! map toShort (split ',' vs)
  -- boolean entries
  toAttr ["Is_circular", "true"  ] = pass' $!! AttrIsCircular True
  toAttr ["Is_circular", "false" ] = pass' $!! AttrIsCircular False
  toAttr ["Is_circular", _       ] = fail' "GFFAttribute: Is_circular tag must be either 'true' or 'false'"

  -- other entries
  -- TODO: Note if the tag is upper case, since these should be
  --       reserved for future use
  toAttr [v, t] = pass' $!! AttrUserDefined (toShort v) (toShort t)

  -- untagged entry
  -- TODO interpret untagged value as an ID if no ID is provided The spec does
  -- not require this, but I should add it in to handle the shit _certain_
  -- programs throw at us.
  toAttr [v] = pass' $!! AttrUntagged (toShort v)
  toAttr []             = fail' $ "GffAttribute: expected attribute (<tag>=<val>), found ''"
  toAttr fs             = fail' $ concat 
    ["GffAttribute: expected attribute (<tag>=<val>), found '", unsplit '=' fs, "'"]


  -- TODO: should warn about repeats
  -- warnIfTagsRepeat :: [(ByteString, ByteString)] -> ReportS [(ByteString, ByteString)]
  -- warnIfTagsRepeat ps = case mapMaybe headMay . map (drop 1) . group . sort . map fst $ ps of
  --   [] -> pass' ps
  --   es -> pass' ps >>= warn' ("GFFAttribute: each tag may appear only once, offending tag(s): [" ++ tags ++ "]") where
  --     tags = unsplit ',' es


toMap
  :: (Foldable f, Ord k, Monad m)
  => (a -> m (Maybe k))      -- try to extract key
  -> (a -> m (Maybe b))      -- create new element if possible (key wasn't in map)
  -> (a -> b -> m (Maybe b)) -- merge element (key is in map)
  -> f a                     -- the functor of stuff
  -> m (Map k b)             -- the output map

toMap ek mab mabb xs = foldr magic (return DM.empty) xs
  where
    -- magic :: a -> m (Map k b) -> m (Map k b)
    magic a' b' = ek a' >>= maybe b' alterB
      where
        -- alterB :: k -> m (Map k b)
        alterB k = toB k >>= maybe b' (\v -> DM.insert k v <$> b')
        -- toB :: k -> m b
        toB k = (DM.lookup k <$> b') >>= maybe (mab a') (mabb a')

toTable :: ByteString -> [[ByteString]]
toTable =
  filter (\s -> length(s) /= 0) . -- Filter out lines of inappropriate length
  map (split '\t')              . -- Break tests by line and TAB
  filter (not . isPrefixOf "#") . -- or start with a comment (#) character
  split '\n'                      -- this allows space in fields

toGff'' :: [ByteString] -> ReportS GffEntry
toGff'' [c1,_,c3,c4,c5,_,c7,_,c9]
  =   gffEntry
  <$> pure             c1 -- seqid   (used as is)
                          -- source  (not used)
  <*> readType''       c3 -- type
  <*> readInt''        c4 -- start
  <*> readInt''        c5 -- stop
                          -- score   (not used)
  <*> readStrand''     c7 -- strand
                          -- phase   (not used)
  <*> readAttributes'' c9 -- attributes
toGff'' fs = fail' $ concat [
  "GffFormatError: Expected 9 columns, offending column:\n  "
  ++ unsplit '\t' fs]


readGff :: ByteString -> ReportS [GeneModel]
readGff s = fmap DM.elems $ (CM.sequence . map toGff'' . toTable) s >>= toMap ek mab mabb where

  getParent :: [Attribute] -> ReportS (Maybe ConstantString)
  getParent [] = pass' Nothing
  getParent ((AttrParent [p]):_) = pass' (Just p)
  getParent ((AttrParent  _ ):_) = fail' "MultipleParentError: send me an angry email"
  getParent (_:xs)               = getParent xs

  getID :: [Attribute] -> Maybe ConstantString
  getID [] = Nothing
  getID ((AttrID d):_) = Just d
  getID (_:xs) = getID xs

  ek :: GffEntry -> ReportS (Maybe ConstantString)
  ek GffEntry { gff_type = MRna, gff_attr = attr } =
    maybe
      (fail' "Each mRNA entry must have an ID attribute")
      (\x -> pass' $ Just x)
      (getID attr)
    >>= note' "get mRNA ID key"

  ek GffEntry { gff_type = Gene, gff_attr = attr } =
    maybe
      (fail' "Each Gene entry must have an ID attribute")
      (\x -> pass' $ Just x)
      (getID attr)
    >>= note' "get gene ID key"

  ek GffEntry { gff_type = CDS, gff_attr = attr } =
    getParent attr >>=
      maybe
        (fail' "Each CDS entry must have an Parent attribute")
        (\x -> pass' $ Just x)
      >>= note' "get CDS parent key"

  ek GffEntry { gff_type = Exon, gff_attr = attr } =
    getParent attr >>=
      maybe
        (fail' "Each Exon entry must have an Parent attribute")
        (\x -> pass' $ Just x)
      >>= note' "get exon parent key"

  -- ignore any types other than gene, mRNA, exon, and CDS
  ek _ = pass' Nothing
      

  mab :: GffEntry -> ReportS (Maybe GeneModel)
  -- initialize the model if one doesn't yet exist
  mab g
    | t == Gene = (              pass' $ Just $ (commonModel g) { model_locid = d, model_modid = Nothing, model_bound = Just i } ) >>= \x -> note' ("init gene: " ++ maybe "NIL" bshow x) x
    | t == MRna = ( mp >>= \p -> pass' $ Just $ (commonModel g) { model_locid = p, model_modid = d, model_bound = Just i }       ) >>= \x -> note' ("init mRNA: " ++ maybe "NIL" bshow x) x
    | t == Exon = ( mp >>= \p -> pass' $ Just $ (commonModel g) {                  model_modid = p, model_exon  = [i] }          ) >>= \x -> note' ("init exon: " ++ maybe "NIL" bshow x) x
    | t == CDS  = ( mp >>= \p -> pass' $ Just $ (commonModel g) {                  model_modid = p, model_cds   = [i] }          ) >>= \x -> note' ("init CDS: "  ++ maybe "NIL" bshow x) x
    | otherwise = pass' Nothing
    where
      t  = gff_type     g
      i  = gff_interval g
      mp = getParent (gff_attr g)
      d  = getID     (gff_attr g)

      commonModel g' = GeneModel {
            model_chrid  = gff_chrid g'
          , model_modid  = Nothing
          , model_locid  = Nothing
          , model_bound  = Nothing
          , model_cds    = []
          , model_exon   = []
          , model_strand = gff_strand g'
        }

  -- merge gff line into the model if it does already exist
  mabb :: GffEntry -> GeneModel -> ReportS (Maybe GeneModel)
  mabb e g
    -- Sometimes the mRNA may be missing, so all required information needs to
    -- be extracted from the gene entry
    | t == Gene = ( pm >>= \p -> pass' $ Just $
      g {   model_locid  = p
          , model_strand = gff_strand e
        } ) >>= \x -> note' ("mabb gene: " ++ maybe "NIL" bshow x) x
    -- mRNA needs to set the model bound, it must override any bound set
    -- by the Gene entry.
    | t == MRna = ( pm >>= \p -> pass' $ Just $
      g {   model_locid  = p
          , model_modid  = d
          , model_strand = gff_strand e
          , model_bound  = Just i
        } ) >>= \x -> note' ("mabb mRNA: " ++ maybe "NIL" bshow x) x
    -- The CDS and Exon entries both append the interval list
    -- There are a few checks I should add eventually 
    | t == CDS  = ( pass' $ Just $ g { model_cds  = (model_cds  g) ++ [i] } ) >>= \x -> note' ("mabb CDS: "  ++ maybe "NIL" bshow x) x
    | t == Exon = ( pass' $ Just $ g { model_exon = (model_exon g) ++ [i] } ) >>= \x -> note' ("mabb exon: " ++ maybe "NIL" bshow x) x
    -- Any other entry I currently skip
    | otherwise = pass' (Just g)  >>= \x -> note' ("mabb other: " ++ maybe "NIL" bshow x) x

    where
      t  = gff_type     e
      i  = gff_interval e
      pm = getParent (gff_attr e)
      d  = getID     (gff_attr e)
