import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMain)
import Application (withKestrel)

main :: IO ()
main = defaultMain fromArgs withKestrel
