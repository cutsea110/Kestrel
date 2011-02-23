import Controller (withKestrel)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withKestrel $ run 3000
