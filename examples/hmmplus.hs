{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}

module HmmPlus where
import Language.Pads.Padsc
import Language.Pads.GenPretty
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[\t ]+|$" " "

amino = "ACDEFGHIKLMNPQRSTVWY"
nucleotide = "ACTG"

[pads|
  data SmurfFile = SmurfFile { header::SmurfHeader, hmm::HMM <| (getAlphabet header, getNumNodes header) |> }
  
  type SmurfHeader = [Line HeaderLine] terminator Try (LitRE 'HMM ')
  
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
    
  
  data HMM (alphabet::String, numNodes::Int) = HMM {
    "HMM", ws, hmmAlphabet::[Letter alphabet | ws] length <| length alphabet |>, ws, EOR,
    ws, transitionHeader::TransitionDescription, EOR,
    composition::Maybe (ws, "COMPO", ws, EmissionProbabilities alphabet, ws, EOR),
    insertZeroEmissions::InsertEmissions alphabet,
    stateZeroTransitions::StateTransitions,
    nodes::[HmmNode <| alphabet |>] terminator "//" where <| numNodes == length nodes |> }
--    rest::[StringLn | EOR] terminator EOF }
              
  type Letter (alphabet::String) = constrain c::Char where <| c `elem` alphabet |>
  
  type EmissionProbabilities (alphabet::String) = [ Double | ws ] length <| length alphabet |> 
  
--  type TransitionProbabilities (numStates::Int) = [ LogProbability | ws ] length numStates
  
  data TransitionProbabilities = TransitionProbabilities {
      mm :: MatchToMatch, ws,
      mi :: MatchToInsertion, ws,
      md :: MatchToDeletion, ws,
      im :: InsertionToMatch, ws,
      ii :: InsertionToInsertion, ws,
      dm :: DeletionToMatch, ws,
      dd :: DeletionToDeletion
  }
  
  type TransitionDescription = [ StringSE ws | ws] terminator (Try EOR)
  
  type InsertEmissions (alphabet::String) = (ws, EmissionProbabilities alphabet, ws, EOR)
  
  type StateTransitions = (ws, TransitionProbabilities, ws, EOR)
  
  data HmmNode (alphabet::String) = HmmNode {
                ws, nodeNum::Int, ws, matchEmissions::EmissionProbabilities alphabet, ws, annotations::EmissionAnnotationSet, EOR,
                insertionEmissions::InsertEmissions alphabet,
                transitions::StateTransitions
  }
  
  
  type EmissionAnnotationSet = (EmissionAnnotation, ws, EmissionAnnotation, ws, EmissionAnnotation)
  
  data EmissionAnnotation = MAPA Int
                          | Unused '-'
                          | RForCS Char

  data LogProbability = NonZero Double
                      | LogZero '*'
                      
-- do we want these to be types or newtypes? newtype enforces type checking
-- but might prove cumbersome in the algorithm.
-- consider just making these type aliases.                      
  newtype MatchToMatch = MatchToMatch LogProbability
  newtype MatchToInsertion = MatchToInsertion LogProbability
  newtype MatchToDeletion = MatchToDeletion LogProbability
  newtype InsertionToMatch = InsertionToMatch LogProbability
  newtype InsertionToInsertion = InsertionToInsertion LogProbability
  newtype DeletionToMatch = DeletionToMatch LogProbability
  newtype DeletionToDeletion = DeletionToDeletion LogProbability

|]

getAlphabet :: SmurfHeader -> String
getAlphabet ((HeaderLine {tag, payload}):xs) = case tag of
                    ALPH -> case payload of
                              Alphabet "amino" -> amino
                              Alphabet "nucleotide" -> nucleotide
                              otherwise -> error "Invalid alphabet"
                    otherwise -> getAlphabet xs

getNumNodes :: SmurfHeader -> Int
-- getNumNodes header = 343 -- replace this  
getNumNodes ((HeaderLine {tag, payload}):xs) = case tag of
                    LENG -> case payload of
                              ModelLength i -> i
                              otherwise -> error "Invalid model length"
                    otherwise -> getNumNodes xs
                  

result = do
        { (SmurfFile header hmm, md) <- parseFile "Examples/data/test.hmm+"
        ; return (header, hmm, md)
        }

test = do (header, hmm, (topmd,restmd)) <- result
          return topmd