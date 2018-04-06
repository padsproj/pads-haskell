{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances #-}

module VCard
    ( -- $doc
      VCard(..)
    , CommonName
    , IndividualNames(..)
    , VCardProperty(..)
    , AddrType(..)
    , TelType(..)
    , EmailType(..)
    , AgentData(..)
    , VCardData(..)
    , Class(..)
    ) where

import Data.List (intercalate)
import Data.Time (UTCTime, TimeZone, FormatTime, formatTime)

import Language.Pads.Padsc

-- Lines are delimited with carriage return/line-feed (control-M, newline)  \r\n
-- Nested Vcards are separated by \n rather than \r\n

[pads|
-- KSF: Added to describe sequence of VCards
newtype VCards = VCards (partition [Line VCard] terminator EOF using windows)

-- KSF: moved IndividualNames and Common Name into VCardPropety because these attributes are not
--      guaranteed to be in a particular order.
data VCard = VCard  ("BEGIN:", vcardRE, EOR, [Line Entry] terminator "END:", vcardRE)

data Entry = Entry { prefix   :: Maybe ("item", Int, '.'),
                     tag      :: Tag,
                     sep      :: StringME colonsemiRE,
                     property :: VCardProperty tag }

data Tag = VERSION | FN | NICKNAME | BDAY | ADR | LABEL
         | TEL | EMAIL | MAILER | TZ |GEO | TITLE | ROLE | LOGO | AGENT
         | ORG | CATEGORIES | NOTE | PRODID | REV | SORTSTRING "SORT-STRING"
         | SOUND | UID | URL | CLASS | KEY
         | EXTENSION ("X-", VCardString)
         | ITEM "item"
         | PHOTO
         | N


data VCardProperty (tag :: Tag) = case tag of
    -- | Version number of VCard file format
      VERSION ->  Version (Int, '.', Int)

    -- | A breakdown of the vCard entity's name, as described by IndividualNames
    | N -> Names IndividualNames

    -- | Formated name of the represented person
    --
    -- > CommonName "Mr. Michael A. F. Schade"
    | FN -> FName VCardString

    -- | A list of nicknames belonging to the VCard entity. E.g.,
    --
    -- > Nickname ["Mike", "Mikey"]
    | NICKNAME -> Nickname NameSs

    -- | A photo of the VCard entity. E.g.,
    --
    -- > Photo Nothing (URI "http://accentuate.us/smedia/images/michael.jpg")
    | PHOTO -> Photo VCardData

    -- | Specifies the birth date of the VCard entity. E.g.,
    --
    | BDAY -> Birthday { bdayType :: Maybe ("value=date:", ())
                       , bdate    :: DateFSE <| ("%Y-%m-%d", RE "$") |>
                       }
    -- | A physical address associated with the vCard entity. E.g.,
    --
    -- > Address [AddrParcel, AddrPostal] "PO Box 935" "" "" "Fenton" "MO"
    -- >                                  "63026" "USA"
    | ADR -> Address   { addrType      :: TypeL AddrType
                       , poBox         :: VCardString, ';'
                       , extAddress    :: VCardString, ';'
                       , streetAddress :: VCardString, ';'
                       , locality      :: VCardString, ';' -- ^ City
                       , region        :: VCardString, ';' -- ^ State or Province
                       , postalCode    :: VCardString, ';'
                       , countryName   :: VCardString
                       }
    -- | Formatted text about the delivery address. This is typically similar
    -- to the information in Address. E.g.,
    --
    -- > Label  [AddrParcel, AddrPostal]
    -- >        ["Michael Schade", "PO Box 935", "Fenton, MO 63026"]
    | LABEL -> Label { labelType :: TypeL AddrType
                     , label     :: VCardString
                     }
    -- | A telephone number for the VCard entity, as well as a list of
    -- properties describing the telephone number. E.g.,
    --
    -- > Telephone [TelCell, TelPreferred] "+1-555-555-5555"
    | TEL -> Telephone { telType   :: TypeL TelType
                       , number    :: VCardString
                       }
    -- | An email address for the VCard entity, including a list of properties
    -- describing it. E.g.,
    --
    -- > Email [EmailInternet, EmailPreferred] "hackage@mschade.me"
    | EMAIL -> Email { emailType :: TypeL EmailType
                     , emailAddr :: VCardString
                     }
    -- | Specifies the mailing agent the vCard entity uses. E.g.,
    --
    -- > Mailer "MichaelMail 4.2" -- Not a real mailing agent, unfortunately :(
    | MAILER -> Mailer VCardString
    -- | Represents the time zone of the vCard entity. E.g.,
    --
    -- > TZ (hoursToTimeZone (-6))
    | TZ -> Tz TZone

    -- | Relates to the global positioning of the vCard entity. The value is
    -- (latitude, longitude) and must be specified as decimal degrees,
    -- preferably to six decimal places.
    --
    -- > Geo (37.386013, -122.082932)
    | GEO -> Geo (Double, ';', Double)
    -- | The VCard entity's job title or other position. E.g.,
    --
    -- > Title "Co-Founder"
    | TITLE -> Title VCardString
    -- | Specifies the role associated with the title. E.g.,
    --
    -- > Role "Anything"   -- For the co-founder, or
    -- > Role "Programmer" -- For someone the title "Research and Development"
    | ROLE -> Role VCardString
    -- | An image of the vCard entity's logo. This would typically relate to
    -- their organization. E.g.,
    --
    -- > Logo Nothing (URI "http://spearheaddev.com/smedia/images/logo-trans.png")
    | LOGO -> Logo  VCardData
    -- | Indicates the vCard of an assistant or area administrator who is
    -- typically separately addressable. E.g.,
    --
    -- > Agent (AgentURI "CID:JQPUBLIC.part3.960129T083020.xyzMail@host3.com")
    --
    -- or
    --
    -- > Agent (AgentVCard (VCard   [ CommonName "James Q. Helpful"
    -- >                            , Email [EmailInternet] "j@spearheaddev.com"
    -- >                            ]))
    | AGENT -> Agent AgentData
    -- | The organization to which an entity belongs followed by organizational
    -- unit names. E.g.,
    --
    -- > Organization ["Spearhead Development, L.L.C.", "Executive"]
    | ORG -> Organization ([VCardString | ';'] terminator LitRE ';$|$')
    -- | General categories to describe the vCard entity. E.g.,
    --
    -- > Categories ["Internet", "Web Services", "Programmers"]
    | CATEGORIES -> Categories ([VCardString | ','] terminator Try EOR)
    -- | A general note about the vCard entity. E.g.,
    --
    -- > Note "Email is the absolute best contact method."
    | NOTE -> Note VCardString
    -- | Specifies the identifier of the product that created this vCard. E.g.,
    --
    -- > ProductId "-//ONLINE DIRECTORY//NONSGML Version 1//EN"
    --
    -- Please note well that, by RFC 2426 guidelines, \"implementations SHOULD
    -- use a method such as that specified for Formal Public Identifiers in ISO
    -- 9070 to assure that the text value is unique,\" but this module does not
    -- support that.
    | PRODID -> ProductId VCardString
    -- | Distinguishes the current revision from other renditions. E.g.,
    --
    -- > Revision $ UTCTime (fromGregorian 2011 04 16) (secondsToDiffTime 0)
    | REV -> Revision { revDate :: DateFSE <|("%Y-%m-%d", RE "T")|>
                      , revTime :: Maybe ('T', DateFSE <|("%H-%M-%SZ", RE "$")|>)
                      }
    -- | Provides a locale- or national-language-specific formatting of the
    -- formatted name based on the vCard entity's family or given name. E.g.,
    --
    -- > SortString "Schade"
    | SORTSTRING -> SortString VCardString
    -- | Specifies information in a digital sound format to annotate some
    -- aspect of the vCard. This is typically for the proper pronunciation of the
    -- vCard entity's name. E.g.,
    --
    -- > Sound  "BASIC"
    -- >        (URI "CID:JOHNQPUBLIC.part8.19960229T080000.xyzMail@host1.com")
    | SOUND -> Sound { sndType   :: Maybe (TypeS, ';') -- ^ Registered IANA format
                     , sndData   :: VCardData
                     }
    -- | A value to uniquely identify the vCard. Please note well that this
    -- should be one of the registered IANA formats, but as of this time, this
    -- module does not support listing the UID type. E.g.,
    --
    -- > UID "19950401-080045-40000F192713-0052"
    | UID -> Uid { uidType :: Maybe (TypeS, ';')
                 , uidData :: VCardString }
    -- | A website associated with the vCard entity. E.g.,
    --
    -- > URL "http://spearheaddev.com/"
    | URL -> Url { urlType :: TypeL URLType
                 , urlAddr :: VCardString
                 }
    -- | Describes the general intention of the vCard owner as to how
    -- accessible the included information should be. E.g.,
    --
    -- > Class ClassConfidential
    | CLASS -> Class Class
    -- | Specifies a public key or authentication certificate associated with
    -- the vCard entity. E.g.,
    --
    -- > Key "x509" (Binary "dGhpcyBjb3VsZCBiZSAKbXkgY2VydGlmaWNhdGUK")
    | KEY -> Key { keyType   :: Maybe (TypeS, ';') -- ^ Registered IANA format
                 , keyData   :: VCardData
                 }
    | EXTENSION s -> Extension VCardString
    | otherwise -> Other StringLn

-- | A breakdown of the vCard entity's name, corresponding, in sequence, to
-- Family Name, Given Name, Additional Names, Honorific Prefixes, and Honorific
-- Suffixes. E.g.,
--
-- > IndividualNames ["Schade"] ["Michael"] ["Anthony", "Fanetti"] [] ["Esq."]
data IndividualNames =  IndividualNames { familyName        :: NameSs
                                        , givenName         :: NameSs
                                        , additionalNames   :: NameSs
                                        , honorificPrefixes :: NameSs
                                        , honorificSuffixes :: NameSs
                                        }

data TZone = TzText ("VALUE=text:", StringLn)
           | TzInt  (TimeZoneSE <| RE "$" |>)


-- | Represents the various types or properties of an address.
data AddrType   = AddrDomestic (StringME 'DOM|dom')
                | AddrInternational (StringME 'INTL|intl')
                | AddrPostal (StringME 'POSTAL|postal')
                | AddrParcel (StringME 'PARCEL|parcel')
                | AddrHome (StringME 'HOME|home')
                | AddrWork (StringME 'WORK|work')
                | AddrPreferred (StringME 'PREF|pref')

-- | Represents the various types or properties of a telephone number.
data TelType    = TelHome "HOME"
                | TelMessage "MSG"
                | TelWork "WORK"
                | TelVoice "VOICE"
                | TelFax "FAX"
                | TelCell "CELL"
                | TelVideo "VIDEO"
                | TelPager "PAGER"
                | TelBBS "BBS"
                | TelModem "MODEM"
                | TelCar "CAR"
                | TelISDN "ISDN"
                | TelPCS "PCS"
                | TelMain "MAIN"
                | TelPreferred (StringME 'PREF|pref')

-- | Represents the various types or properties of a url.
data URLType =   URLPreferred (StringME 'PREF|pref')
               | URLWork "WORK"
               | URLHome "HOME"

-- | Represents the various types or properties of an email address.
data EmailType = EmailInternet "INTERNET"
               | EmailX400  "X400"
               | EmailPreferred (StringME 'PREF|pref')
               | EmailWork "WORK"

-- | Represents the data associated with a vCard's Agent. This could be a URI
-- to such a vCard or the embedded contents of the vCard itself.
data AgentData = AgentURI ("VALUE=uri:", VCardString)
               | AgentVCard VCard

-- | Represents the various types of data that can be included in a vCard.
data VCardData = VURI    ("VALUE=uri:", VCardString)
               | VBinary ("ENCODING=b", Maybe(';', TypeS), ':', WrappedEncoding )
               | VBase64 ("BASE64:", EOR, WrappedEncoding)

--type WrappedEncoding = [Line (StringLnP startsWithSpace)]
type WrappedEncoding = VCardString


-- | Classifies the vCard's intended access level.
data Class = ClassPublic "PUBLIC"
           | ClassPrivate "PRIVATE"
           | ClassConfidential "CONFIDENTIAL"

-- Need to code base type StringESC.  It takes a list of pairs.
-- Each pair represents a stopping condition.
-- If parser sees first component of tuple, it stops.
-- Second component is prefix to escape first component, so //, does not stop.
-- Pretty printer prefixes stopping components with escape sequence.
type VCardString = StringESCLn <| ('\\', ",:;") |>

type NameSs = [VCardString | ','] terminator ';|$'


type TypeS = (typeRE, '=', CommaL VCardString)
type TypeL  a = [(typeRE, '=', CommaL a) | ';'] terminator ':'
type CommaL a = [a|','] terminator Try (LitRE '[:;]')

|]
type CommonName = String


colonsemiRE = RE "[:;]"
commasemiRE = RE "[,;]"
vcardRE = REd "VCARD|vCard" "VCARD"
typeRE = REd "TYPE|type" "TYPE"


startsWithSpace s = case s of
   [] -> False
   ' ':s' -> True
   '\t':s' -> True
   '\v':s' -> True
   otherwise -> False

vcard_file_small  = "Examples/data/VcardSmall.vcf"
vcard_file_large  = "Examples/data/VcardSmall.vcf"
vcard_file = vcard_file_large

result n  = do
     { (VCards rep, md) <- parseFile vcard_file
     ; return (Prelude.take n rep, fst md)
     }

test = do
     { (VCards rep, md) <- parseFile vcard_file
     ; return (fst md)
     }

entry_input = "N:Brush;A.J.;;;\r\n"
entry_result = entry_parseS entry_input

entry_input2 = "X-ABUID:3CC68169-6DC9-4457-94B3-B1B3C69E832A\\:ABPerson"
entry_result2 = entry_parseS entry_input2
