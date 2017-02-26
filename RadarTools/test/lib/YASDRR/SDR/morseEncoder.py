#!/usr/bin/env python
#Copyright Robert C. Taylor, 2017. All Rights Reserved
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


import sys



def main():
    qbfjold = "- .... . / --.- ..- .. -.-. -.- / -... .-. --- .-- -. / ..-. --- -..- / .--- ..- -- .--. ... / --- ...- . .-. / - .... . / .-.. .- --.. -.-- / -.. --- --."
    for i, c in enumerate(qbfjold):
        if c == "-":
            sys.stdout.write("Morse.MorseDash, Morse.MorseSpace, ")
        elif c == " ":
            sys.stdout.write("Morse.MorseSpace, Morse.MorseSpace, ")
        elif c == ".":
            sys.stdout.write("Morse.MorseDot, Morse.MorseSpace, ")
        elif c == "/":
            sys.stdout.write("Morse.MorseSpace, Morse.MorseSpace,")
        else:
            raise "Invalid character detected."

main()
