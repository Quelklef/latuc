module Main where

import           Data.Foldable   (for_, toList)
import           Data.List.Extra (trim)
import qualified Data.Map        as Map

import qualified Latuc           as Latuc

main :: IO ()
main = do
  impl
  putStrLn ""

impl :: IO ()
impl = do

  let header s = putStrLn $ "||========:: " <> s <> " ::========||"

  let a `shouldBe` b =
        if a == b then
          -- putStrLn "[ ok ]"
          pure ()
        else do
          putStrLn $ "[FAIL]" <> " '" <> a <> "' == '" <> b <> "'"

  let a `shouldFail` () =
        case a of
          Left _ -> pure ()
          Right s ->
            putStrLn $ "[FAIL]" <> " '" <> s <> "' == Left _"


  header "Command names should be trimmed"
  for_ (toList . Map.keys $ Latuc.commands)
    $ \name -> trim name `shouldBe` name


  header "Empty string"
  Latuc.convert "" `shouldBe` ""


  header "Literal"
  Latuc.convert "This, is a test." `shouldBe` "This, is a test."


  header "Spaces"
  Latuc.convert
    "\nthis  \t \tis    \n\t\n \n \t\t\na  \n\t  test   "
   `shouldBe` " this is\n\na test "


  header "Brackets"
  Latuc.convert
    "{this {{i}{s}}{ a \n\n} test}"
   `shouldBe` "this is a\n\n test"
  Latuc.parse "{" `shouldFail` ()
  Latuc.parse "{ " `shouldFail` ()
  Latuc.parse "{ \n\n" `shouldFail` ()


  header "Escape"
  Latuc.convert
    "\\S\\{this is ~$\\alpha$~ test\\}"
   `shouldBe` "§{this is  α  test}"


  header "Dashes"
  Latuc.convert
    "5-1 is between 1--10---obviously. ----anonymous"
   `shouldBe` "5-1 is between 1–10—obviously. ----anonymous"


  header "ams arrows"
  Latuc.convert "\\to" `shouldBe` "→"
  Latuc.convert "\\gets" `shouldBe` "←"
  Latuc.convert "\\iff" `shouldBe` "⇔"


  header "Subscript"
  Latuc.convert "i_{}" `shouldBe` "i"
  Latuc.convert "i_123" `shouldBe` "i₁23"
  Latuc.convert "i_\n  {123}" `shouldBe` "i₁₂₃"
  Latuc.convert "i_\n  { 123 }" `shouldBe` "i₁₂₃"
  Latuc.convert "i_{i_{123 }}" `shouldBe` "i_(i₁₂₃)"
  Latuc.convert "i_{i_{1~2~3 }}" `shouldBe` "i_(i₁ ₂ ₃)"
  Latuc.convert "i\\textsubscript 123" `shouldBe` "i₁23"
  Latuc.convert "i\\textsubscript{123}" `shouldBe` "i₁₂₃"
  Latuc.convert "i\\textsubscript\n  { 123 }" `shouldBe` "i₁₂₃"
  Latuc.convert
    "i\\textsubscript{i\\textsubscript{123 }}"
   `shouldBe` "i_(i₁₂₃)"
  Latuc.convert "e_-" `shouldBe` "e₋"

  Latuc.parse "_" `shouldFail` ()
  Latuc.parse "_ " `shouldFail` ()
  Latuc.parse "_ \n\n" `shouldFail` ()
  Latuc.parse "_ \n\nx" `shouldFail` ()


  header "Superscript"
  Latuc.convert "i^{}" `shouldBe` "i"
  Latuc.convert "i^123" `shouldBe` "i¹23"
  Latuc.convert "i^{123}" `shouldBe` "i¹²³"
  Latuc.convert "i^\n  { 123 }" `shouldBe` "i¹²³"
  Latuc.convert "i^{i^{123 }}" `shouldBe` "i^(i¹²³)"
  Latuc.convert "i^{i^{1~2~3 }}" `shouldBe` "i^(i¹ ² ³)"
  Latuc.convert "i\\textsuperscript 123" `shouldBe` "i¹23"
  Latuc.convert "i\\textsuperscript{123}" `shouldBe` "i¹²³"
  Latuc.convert "i\\textsuperscript\n  { 123 }" `shouldBe` "i¹²³"
  Latuc.convert
    "i\\textsuperscript{i\\textsuperscript{123 }}"
   `shouldBe` "i^(i¹²³)"
  Latuc.convert "e^-" `shouldBe` "e⁻"

  Latuc.parse "^" `shouldFail` ()
  Latuc.parse "^ " `shouldFail` ()
  Latuc.parse "^ \n\n" `shouldFail` ()
  Latuc.parse "^ \n\nx" `shouldFail` ()


  header "\\not"
  Latuc.convert "\\not 1" `shouldBe` "1̸"
  Latuc.convert "\\not{123}" `shouldBe` "1̸23"
  Latuc.convert "\\not{ 123 }" `shouldBe` "1̸23"
  Latuc.convert "\\not=" `shouldBe` "≠"
  Latuc.convert "\\not \\in" `shouldBe` "∉"
  Latuc.convert "\\not{}" `shouldBe` " ̸"

  Latuc.parse "\\not" `shouldFail` ()
  Latuc.parse "\\not " `shouldFail` ()
  Latuc.parse "\\not \n\n" `shouldFail` ()
  Latuc.parse "\\not \n\nx" `shouldFail` ()


  header "Combining"
  Latuc.convert "\\bar ab" `shouldBe` "a\x0304\&b"
  Latuc.convert "\\bar12" `shouldBe` "1\x0304\&2"
  Latuc.convert "\\bar{}" `shouldBe` " \x0304"
  Latuc.convert "\\=ab" `shouldBe` "a\x0304\&b"
  Latuc.convert "\\=\nab" `shouldBe` "a\x0304\&b"
  Latuc.convert "\\={}" `shouldBe` " \x0304"
  Latuc.convert "\\bar{ab}" `shouldBe` "a\x0304\&b"
  Latuc.convert "\\={ab}" `shouldBe` "a\x0304\&b"
  Latuc.convert
    "\\=\\k\\underline\\overline{a\\=bc}"
   `shouldBe` "a\x0305\x0332\x0304\&b\x0304\x0305\x0332\&c\x0305\x0332\x0328"

  Latuc.parse "\\bar" `shouldFail` ()
  Latuc.parse "\\bar " `shouldFail` ()
  Latuc.parse "\\bar \n\n" `shouldFail` ()
  Latuc.parse "\\=" `shouldFail` ()
  Latuc.parse "\\= " `shouldFail` ()
  Latuc.parse "\\= \n\n" `shouldFail` ()
  Latuc.parse "\\= \n\nx" `shouldFail` ()


  header "Style"
  Latuc.convert "\\mathbf{}" `shouldBe` ""
  Latuc.convert "\\mathbf ABC \\mathit ABC" `shouldBe` "𝐀BC 𝐴BC"
  Latuc.convert
    "\\mathbf {ABC} \\mathit {ABC}"
   `shouldBe` "𝐀𝐁𝐂 𝐴𝐵𝐶"
  Latuc.convert "\\bf \\it " `shouldBe` ""
  Latuc.convert
    "ABC {\\bf ABC} {\\it ABC} ABC"
   `shouldBe` "ABC 𝐀𝐁𝐂 𝐴𝐵𝐶 ABC"
  Latuc.convert
    "ABC \\bf ABC \\it ABC ABC"
   `shouldBe` "ABC 𝐀𝐁𝐂 𝐴𝐵𝐶 𝐴𝐵𝐶"
  Latuc.convert "A\\bf\n\nB\n\nC" `shouldBe` "A\n\n𝐁\n\n𝐂"

  Latuc.parse "\\mathbf" `shouldFail` ()
  Latuc.parse "\\mathbf " `shouldFail` ()
  Latuc.parse "\\mathbf \n\n" `shouldFail` ()
  Latuc.parse "\\mathbf \n\nx" `shouldFail` ()


  header "\\sqrt"
  Latuc.convert "\\sqrt{}" `shouldBe` "√"
  Latuc.convert "\\sqrt x" `shouldBe` "√x̅"
  Latuc.convert "\\sqrt \\alpha" `shouldBe` "√α̅"
  Latuc.convert "\\sqrt\nx" `shouldBe` "√x̅"
  Latuc.convert "\\sqrt[]x" `shouldBe` "√x̅"
  Latuc.convert "\\sqrt[]\nx" `shouldBe` "√x̅"
  Latuc.convert "\\sqrt{x+1}" `shouldBe` "√x̅+̅1̅"
  Latuc.convert "\\sqrt2" `shouldBe` "√2̅"
  Latuc.convert "\\sqrt1+1" `shouldBe` "√1̅+1"
  Latuc.convert "\\sqrt[2]x" `shouldBe` "√x̅"
  Latuc.convert "\\sqrt[3]{x}" `shouldBe` "∛x̅"
  Latuc.convert "\\sqrt[3]{}" `shouldBe` "∛"
  Latuc.convert "\\sqrt[\\alpha+1]x" `shouldBe` "ᵅ⁺¹√x̅"
  Latuc.convert "\\sqrt[\\alpha+1]\nx" `shouldBe` "ᵅ⁺¹√x̅"
  Latuc.convert "\\sqrt[q]{x}" `shouldBe` "(q)√x̅"

  Latuc.parse "\\sqrt" `shouldFail` ()
  Latuc.parse "\\sqrt  " `shouldFail` ()
  Latuc.parse "\\sqrt  \n\n{}" `shouldFail` ()
  Latuc.parse "\\sqrt[]" `shouldFail` ()
  Latuc.parse "\\sqrt\n\n[]{}" `shouldFail` ()
  Latuc.parse "\\sqrt[]\n\n{}" `shouldFail` ()


  header "\\frac"
  Latuc.convert "\\frac{}{}" `shouldBe` ""
  Latuc.convert "\\frac34" `shouldBe` "¾"
  Latuc.convert "\\frac 34" `shouldBe` "¾"
  Latuc.convert "\\frac\n34" `shouldBe` "¾"
  Latuc.convert
    "\\frac{\\hat\\alpha_1^2}{test\\_test}"
   `shouldBe` "(α̂₁²/test_test)"
  Latuc.convert "\\frac{1}{}" `shouldBe` "(1/)"
  Latuc.convert "\\frac{}{1}" `shouldBe` "(/1)"
  Latuc.convert "\\frac{a+b}{c}" `shouldBe` "((a+b)/c)"
  Latuc.convert "\\frac{a}{b+c}" `shouldBe` "(a/(b+c))"
  Latuc.convert "\\frac{a+b}{c+d}" `shouldBe` "((a+b)/(c+d))"

  Latuc.parse "\\frac" `shouldFail` ()
  Latuc.parse "\\frac  " `shouldFail` ()
  Latuc.parse "\\frac  \n\n" `shouldFail` ()
  Latuc.parse "\\frac{}" `shouldFail` ()
  Latuc.parse "\\frac{}  \n\n{}" `shouldFail` ()
  Latuc.parse "\\frac\n\n{}{}" `shouldFail` ()


  header "Unknown commands"
  Latuc.convert
    "\\this \\is \\alpha test"
   `shouldBe` "\\this \\is α test"
  Latuc.convert "\\unknown command" `shouldBe` "\\unknown command"
  Latuc.convert
    "\\unknown{} empty params"
   `shouldBe` "\\unknown{} empty params"
  Latuc.convert "\\unknown{cmd}" `shouldBe` "\\unknown{cmd}"
  Latuc.convert "\\unknown{1}{2}" `shouldBe` "\\unknown{1}{2}"
  Latuc.convert "\\unknown{1}{2}{3}" `shouldBe` "\\unknown{1}{2}{3}"
