#<b>Pockonsolve - Version 0.1.1</b>

<i>Expirimental & Professional Grade DMX Microcontroller</i>

##LIBRARIES USED:
-   TeensyDmx Library
-   U8G2 Library
-   Keypad Library
-   EasingLibrary

##FEATURES:
-   8 or 16-bit DMX Support
-   string input support
-   12 Key keypad parsing (1, 2, 3, 4, 5, 6, 7, 8, 9, 0, And, Minus, S, @, Enter, Thru)
-   momentary, latch or instantaneous buttons
-   Stable analog to DMX conversion for potentiometers and other sensors

##IN THE WORKS:
-   interpolation algorithms using the easing library
-   Pockonsole Mini (faders are replaced by knobs)
-   DMX timing variables and state machine

0.1.1 - Walked away and came back a year to refactor and rewrite
0.1.0 - Slowly implementing Easing first with display then with DMX (very early stages)
0.0.9 - Added clearbuffer and sendbuffer statements into previous U8G2 print functions
0.0.8 - Added a switch for Serial debug or OLED
0.0.7 - keypad control flow cleanup (still needs work)
0.0.6 - keypad control flow begun
0.0.5 - keypad command buidling
0.0.4 - initial work on keypad parsing
0.0.3 - initial repository start
0.0.2 - sending DMX values and reading faders with master scaler
0.0.1 - display values on OLED using U8G2 and reading faders with master scaler
0.0.0 - initial repository start

<b>These sketches require TeensyDmx, Keypad, U8G2, and Easing Libraries \*\*\*</b>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

contact: harryprayiv@gmail.com
