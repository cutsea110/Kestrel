import Yesod (develServer)

main :: IO ()
main = develServer 3002 "Controller" "withKestrel"
