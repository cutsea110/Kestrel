import Yesod (develServer)

main :: IO ()
main = develServer 3000 "Application" "withKestrel"
