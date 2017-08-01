{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, TemplateHaskell, QuasiQuotes,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.FirstPads where
import Language.Pads.Padsc

padsExp = [pads| type Halloween = [StringFW 4 | EOR] terminator EOF |]

