module Test.MySolutions where

import Prelude
import Data.Int (rem)
import Math (pi, sqrt, pow)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * (pow r 2.0)

leftoverCents n = rem n 100
