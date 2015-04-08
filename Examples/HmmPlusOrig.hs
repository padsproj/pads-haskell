{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.HmmPlusOrig where
import Language.Pads.Padsc
import Language.Pads.GenPretty
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[\t ]+|$" "foo"

amino = "ACDEFGHIKLMNPQRSTVWY"
nucleotide = "ACTG"

[pads|
  data SmurfFile = SmurfFile { header::SmurfHeader, hmm::HMM <| getAlphabet header |> }
  
  type SmurfHeader = [Line HeaderLine] terminator Try (LitRE 'HMM ')
--  type SmurfHeader = [Line HeaderLine] terminator EOF
  
  data HeaderLine = HeaderLine { tag::Tag, ws, payload::Payload tag }
  
  data Tag = FileVersion "HMMER3/a" -- this string literal will change with major file version changes
            | NAME | ACC | DESC | LENG | ALPH | RF | CS | MAP | DATE | MEAN | RMSD
            | COM | NSEQ | EFFN | CKSUM | GA | TC | NC | STATS | BETA | Other (StringSE ws)
  
  data Payload (t::Tag) = case t of
      FileVersion -> Version VersionString
    | NAME -> Name StringLn
    | ACC -> Accession StringLn
    | DESC -> Description StringLn
    | LENG -> ModelLength Int
    | ALPH -> Alphabet StringLn
    | RF -> ReferenceAnnotation StringLn
    | CS -> ConsensusStructure StringLn
    | MAP -> MapAnnotation StringLn
    | DATE -> Date StringLn
    | COM -> CommandLog StringLn
    | NSEQ -> SequenceNumber Int
    | EFFN -> EffectiveSeq Double
    | CKSUM -> Checksum Int
    | GA -> PfamGathering (Double, ws, Double)
    | TC -> PfamTrusted (Double, ws, Double)
    | NC -> PfamNoise (Double, ws, Double)
    | STATS -> Stats {"LOCAL", ws, scoredist::ScoreDistribution, ws, values::[Double | ws] terminator (Try EOR) }
    | BETA -> Beta StrandPair
    | Other tag -> BadTag StringLn
    | otherwise -> OtherTag StringLn
    
  type VersionString = StringLn
  
  data ScoreDistribution = VLAMBDA | VMU | FTAU | MSV | VITERBI | FORWARD
  
   -- firstRes secondRes Length maxGap parallelism exposure
  data StrandPair = StrandPair {
        firstStart :: Int, ' ',
        secondStart :: Int, ' ',
        pairLength :: Int, ' ',
        maxGap :: Int, ' ',
        parallel :: Int, ' ',
        exposure :: StringLn
  }
    
  
  data HMM (alphabet::String) = HMM {
    "HMM", ws, hmmAlphabet::[Letter alphabet | ws] length <| length alphabet |>, ws, EOR,
    ws, transitionHeader::TransitionDescription, EOR,
    composition::Maybe (ws, "COMPO", ws, EmissionProbabilities alphabet, ws, EOR),
    insertZeroEmissions::InsertEmissions alphabet,
    stateZeroTransitions::StateTransitions <| length transitionHeader |>,
    nodes::[HmmNode <| (alphabet, length transitionHeader ) |>] terminator "//" }
--    rest::[StringLn | EOR] terminator EOF }
              
  type Letter (alphabet::String) = constrain c::Char where <| c `elem` alphabet |>
  
  type EmissionProbabilities (alphabet::String) = [ Double | ws ] length <| length alphabet |> 
  
  type TransitionProbabilities (numStates::Int) = [ LogProbability | ws ] length numStates
  
  type TransitionDescription = [ StringSE ws | ws] terminator (Try EOR)
  
  type InsertEmissions (alphabet::String) = (ws, EmissionProbabilities alphabet, ws, EOR)
  
  type StateTransitions (numStates::Int) = (ws, TransitionProbabilities numStates, ws, EOR)
  
  data HmmNode (alphabet::String, numStates::Int) = HmmNode {
                ws, nodeNum::Int, ws, matchEmissions::EmissionProbabilities alphabet, ws, annotations::EmissionAnnotationSet, EOR,
                insertionEmissions::InsertEmissions alphabet,
                transitions::StateTransitions numStates
  }
  
  type EmissionAnnotationSet = (EmissionAnnotation, ws, EmissionAnnotation, ws, EmissionAnnotation)
  
  data EmissionAnnotation = MAPA Int
                          | Unused '-'
                          | RForCS Char
                      
                      
              
  data LogProbability = NonZero Double
                      | LogZero '*'

|]

getAlphabet :: SmurfHeader -> String
getAlphabet header = amino -- replace this with actual code to search the HeaderLine list


result = do
        { (SmurfFile header hmm, md) <- parseFile "Examples/data/test.hmm+"
        ; return (header, hmm, md)
        }


result2= do
        { (header, md) <- parseFileWith  smurfHeader_parseM "Examples/data/hmmSmall"
        ; return (header, md)
        }
