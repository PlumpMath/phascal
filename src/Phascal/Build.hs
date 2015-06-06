module Phascal.Build where

import System.Process (callProcess)

assemble obj src = callProcess "arm-none-eabi-as" ["-c", "-o", obj, src]
link exe obj = callProcess "arm-none-eabi-ld" ["-o", exe, obj]
