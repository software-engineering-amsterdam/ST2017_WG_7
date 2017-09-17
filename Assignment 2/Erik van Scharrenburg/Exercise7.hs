module Exercise7 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe

-- Implementing and testing IBAN validation
-- Exercise 7

iban :: String -> Bool
iban s = checkCountry s
      && (read
        . charsToDigits
        . moveFirstFour) s `mod` 97 == 1

checkCountry :: String -> Bool
checkCountry s = checkLength (lookup countryCode countryList)
  where checkLength (Just x) = length s == x
        checkLength _ = False
        countryCode = take 2 s
        -- Country codes and no. of chars from (1)
        countryList = [("AD",24),("AE",23),("AL",28),("AO",25),("AT",20),("AZ",28),("BA",20),("BE",16),("BF",27),("BG",22),("BH",22),("BI",16),("BJ",28),("BR",29),("BY",28),("CG",27),("CH",21),("CI",28),("CM",27),("CR",22),("CV",25),("CY",28),("CZ",24),("DE",22),("DK",18),("DO",28),("DZ",24),("EE",20),("EG",27),("ES",24),("FI",18),("FO",18),("FR",27),("GA",27),("GB",22),("GE",22),("GI",23),("GL",18),("GR",27),("GT",28),("HN",28),("HR",21),("HU",28),("IE",22),("IL",23),("IR",26),("IS",26),("IT",27),("JO",30),("KM",27),("KW",30),("KZ",20),("LB",28),("LI",21),("LT",20),("LU",20),("LV",21),("MA",28),("MC",27),("MD",24),("ME",22),("MG",27),("MK",19),("ML",28),("MR",27),("MT",31),("MU",30),("MZ",25),("NE",28),("NI",32),("NL",18),("NO",15),("PK",24),("PL",28),("PS",29),("PT",25),("QA",29),("RO",24),("RS",22),("SA",24),("SE",24),("SI",19),("SK",24),("SM",27),("SN",28),("TD",27),("TG",28),("TL",23),("TN",24),("TR",26),("UA",29),("VG",24),("XK",20)]

moveFirstFour :: String -> String
moveFirstFour xs = snd halves ++ fst halves
  where halves = splitAt 4 xs

charsToDigits :: String -> String
charsToDigits [] = []
charsToDigits (x:xs)
  | isJust index = show (fromJust index + 10) ++ charsToDigits xs
  | otherwise   = x : charsToDigits xs
  where index = elemIndex x ['A'..'Z']

ibanTest :: Property
ibanTest = forAll ibanTestListGen ibt
  where ibt s = Just (iban s)
             == lookup s ibanTestList

ibanTestListGen :: Gen String
ibanTestListGen = elements (map fst ibanTestList)
-- Most of the examples from (2)
ibanTestList :: [(String, Bool)]
ibanTestList = [("AL47212110090000000235698741", True),
                ("AD1200012030200359100100", True),
                ("AT611904300234573201", True),
                ("AZ21NABZ00000000137010001944", True),
                ("BH67BMAG00001299123456", True),
                ("BE62510007547061", True),
                ("BA391290079401028494", True),
                ("BG80BNBG96611020345678", True),
                ("HR1210010051863000160", True),
                ("CY17002001280000001200527600", True),
                ("CZ6508000000192000145399", True),
                ("DK5000400440116243", True),
                ("EE382200221020145685", True),
                ("FO9754320388899944", True),
                ("FI2112345600000785", True),
                ("FR1420041010050500013M02606", True),
                ("GE29NB0000000101904917", True),
                ("DE89370400440532013000", True),
                ("GI75NWBK000000007099453", True),
                ("GR1601101250000000012300695", True),
                ("GL5604449876543210", True),
                ("HU42117730161111101800000000", True),
                ("IS140159260076545510730339", True),
                ("IE29AIBK93115212345678", True),
                ("IL620108000000099999999", True),
                ("IT40S0542811101000000123456", True),
                ("JO94CBJO0010000000000131000302", True),
                ("KW81CBKU0000000000001234560101", True),
                ("LV80BANK0000435195001", True),
                ("LB62099900000001001901229114", True),
                ("LI21088100002324013AA", True),
                ("LT121000011101001000", True),
                ("LU280019400644750000", True),
                ("MK07250120000058984", True),
                ("MT84MALT011000012345MTLCAST001S", True),
                ("MU17BOMM0101101030300200000MUR", True),
                ("MD24AG000225100013104168", True),
                ("MC9320052222100112233M44555", True),
                ("ME25505000012345678951", True),
                ("NL39RABO0300065264", True),
                ("NO9386011117947", True),
                ("PK36SCBL0000001123456702", True),
                ("PL60102010260000042270201111", True),
                ("PT50000201231234567890154", True),
                ("QA58DOHB00001234567890ABCDEFG", True),
                ("RO49AAAA1B31007593840000", True),
                ("SM86U0322509800000000270100", True),
                ("SA0380000000608010167519", True),
                ("RS35260005601001611379", True),
                ("SK3112000000198742637541", True),
                ("SI56191000000123438", True),
                ("ES8023100001180000012345", True),
                ("SE3550000000054910000003", True),
                ("CH9300762011623852957", True),
                ("TN5910006035183598478831", True),
                ("TR330006100519786457841326", True),
                ("AE070331234567890123456", True),
                ("GB29RBOS60161331926819", False),
                ("", False),
                ("NL0000", False),
                ("SE355000000005491000003", False),
                ("PL6010201026000004220201111", False),
                ("NL39RAB00300065264", False),
                ("ME25505100012345678951", False)]

-- Note that GB29RBOS60161331926819 is from the list of examples but it is not a
-- valid IBAN.

-- The test can be automated by running 'quickCheck ibanTest'

-- Time: 1 hour and 45 minutes

-- (1) International Bank Account Number. Retrieved September 16, 2017 from https://en.wikipedia.org/wiki/International_Bank_Account_Number
-- (2) IBAN examples. Retrieved September 16, 2017 from http://www.rbs.co.uk/corporate/international/g0/guide-to-international-business/regulatory-information/iban/iban-example.ashx
