import Window
import Keyboard
import Text


bound x = let max = x `div` 2
              min = -1 * max
          in clamp min max

board : {w: Int, h: Int, bind_w: Int -> Int, bind_h: Int -> Int}
board = {w=80, h=80, bind_w=bound 80, bind_h=bound 80}



type Positioned a = {a | x: Int, y: Int}

type Position = Positioned {}

type Drawable a = {a | symbol: Form}
type Hero = Drawable Position

hero : Hero
hero = {x=0, y=0, symbol=(toForm <| text <| toText "@")}
place : (Int, Int) -> (Int, Int) -> Form -> Form
place (w, h) (x, y) form = let x' = toFloat <| x * w
                               y' = toFloat <| y * h
                           in form |> move (x' , y')


render : (Int, Int) -> Hero -> Element
render (w, h) hero = let p = place (w `div` board.w, h `div` board.h)
                     in collage w h [
                             hero.symbol |> p (hero.x, hero.y)
                     ]


step : Position -> Hero -> Hero
step i hero = {hero | x <- board.bind_w <| hero.x + i.x
                    , y <- board.bind_h <| hero.y + i.y}


input : Signal Position
input = let delta = (fps 10)
        in sampleOn delta Keyboard.arrows

main = lift2 render Window.dimensions (foldp step hero input)
