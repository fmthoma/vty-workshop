import Graphics.Vty
import Control.Exception
import Control.Monad

main :: IO ()
main = vtyTest $ vertCat
  [ string defAttr "It works!"
  , string defAttr "Press any key to continue" ]

vtyTest :: Image -> IO ()
vtyTest image = bracket startVty shutdown drawImage
  where
    startVty = standardIOConfig >>= mkVty
    drawImage vty = do
        update vty (picForImage image)
        void (nextEvent vty)

