import Yesod (develServer)

main :: IO ()
main = develServer 3000 "Controller" "withKestrel"
