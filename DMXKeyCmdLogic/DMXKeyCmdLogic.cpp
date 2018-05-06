/*********************************************************************
 Harry Pockonsole V2
 **********************************************************************
 Board: VoV1366v0.8
 Designed by Volt-Vision

 Code:
 ______________________________
 DMX Connections (hardware UART @ Serial1) to TI SN75LBC184D:
 DMX1RX on pin 0
 DMX1TX on pin 1
 DMX1DE and RE (DMX_REDE) on pin 24
 _____________________________
 OLED connections (software SPI):
 OLED MOSI0 on pin 11
 OLED SCK0 on pin 13
 OLED_DC on pin 32
 OLED_RST on pin 33
 OLED_CS on pin 34
 ______________________________
 Wiper Connections (10k mini pots):
 Wiper 1: A1
 Wiper 2: A2
 Wiper 3: A3
 Wiper 4: A4
 Wiper 5: A5
 Wiper 6: A6
 Wiper 7: A7
 Wiper 8: A8
 Wiper 9: A9
 ______________________________
 4x4 Keypad:
 Rows: D9, D8, D7, D6
 Columns = D5, D4, D3, D2
 ______________________________
 Breadboard Area:
 Digital Pins: D10,D12,D25,D25,D27,D28,D29,D30 (D2-D9 used in keypad)
 Analog Pins: D31/A12,D35/A16,D36/A17,D37/A18,D38/A19,D39/A20
 3V3 Rail
 Ground Rail (future iterations: switch for decoupling cap)
 ***********************************************************************/

#include <Arduino.h> /* Arduino Includes*/
#include <TeensyDmx.h> /* Teensy DMX Library */
#include <U8g2lib.h> /* U8G2 Library */
#include <U8x8lib.h> /* U8x8 Library */
#include <Keypad.h> /* Keypad Library */
#include <EasingLibrary.h> /* Easing Library */
#include <Fsm.h> /* Finite State Machine Library */


#ifdef U8X8_HAVE_HW_SPI
#include <SPI.h>
#endif
#ifdef U8X8_HAVE_HW_I2C
#include <Wire.h>
#endif

//#ifndef Keypadlib_KEY_H_
//#define Keypadlib_KEY_H_


/*Teensy DMX Settings_________________________________________________*/
#define DMX_REDE 24
TeensyDmx Dmx(Serial1, DMX_REDE);

/*DMX Values__________________________________________________________*/
const long analogFaders = 9;            //there will always be the same number of pots (9)
const int analogFaderMap[analogFaders] = {1, 2, 3, 4, 5, 6, 7, 8, 9}; // this is to allow reconfigurable complex pin assignments
long dmxChannels = 512;           // intializing with a limiting to the number of values that can take up a DMX instruction

byte dmxVal[512];           // currently limiting to one universe, though that won't always be the case
byte dmxValBuffer[512];    //place for storing values to transition to
int displayVal[512] = {0};      /* space for display values*/

bool dmxSelection[512] = { false };           //enables non-destructive DMX kpd channel selection using a for loop
int channelMap[512] = {0};                    //The beginning of being able to map channels together to create large submasters

// floating point scaler
float scalerVal;

// __________________________________PROGRAM MODES___________________________
enum pgmMode {
    FADER_MODE,
    KPD_MODE,
    KPDFADER_MODE,
    ANIMATION_MODE
};
pgmMode controlMode;

bool modeChosen = false; //used to decide whether the mode has already been set


// __________________________________DISPLAY MODES___________________________
enum displayMode {
    POCKONSOLED,
    SERIALDISPLAY
};
displayMode display = POCKONSOLED;

// _________________________________SELECTION MODES__________________________
enum selectionMode {
    NONE,
    SINGLECHANNEL,
    AND,
    THROUGH
};
selectionMode selectionType = NONE;



// _________________________________ANIMATION MODES__________________________

enum transMode {                // for selecting the transition
    BACK_EASE,
    BOUNCE_EASE,
    CIRCULAR_EASE,
    CUBIC_EASE,
    ELASTIC_EASE,
    EXPONENTIAL_EASE,
    LINEAR_EASE,
    QUADRATIC_EASE,
    QUARTIC_EASE,
    QUINTIC_EASE,
    SINE_EASE
};
transMode transType = LINEAR_EASE;        // default curve is Linear

// creating instances of variables
BackEase back;
BounceEase bounce;
CircularEase circular;
CubicEase cubic;
ElasticEase elastic;
ExponentialEase exponential;
LinearEase linear;
QuadraticEase quadratic;
QuarticEase quartic;
QuinticEase quintic;
SineEase sine;

enum transubMode {          // for selecting which part of the transition has a curve applied to it
    IN,
    OUT,
    IN_OUT,
};
transubMode transPart = IN_OUT;  // intializing with both in and out with a curve on them.

// __________________________________ANIMATION STATES__________________________
enum animState {
    OFF,
    STOP,
    PLAY,
    PAUSE,
    REWIND,
    FASTFORWARD,
    RECORD,
    LOOP
};
animState playBackState = OFF;        // initialize to off to prevent empty animations from playing

float playBackDuration = 30.0         // 30 seconds
float playBackSpeed = 1.0             //multiplier for speed
byte keyFrames[512] = {0};             // storage for DMX keyframe values in the animation

// __________________________________KEYPAD PROGRESS__________________________
enum kpdProgress {
    MODE_SELECT,
    NO_CMD,
    DMXCH_ONE,
    DMXCH_TWO,
    DMX_INTENSITY
};
kpdProgress kpdState = MODE_SELECT;

//___________________________U8G2 CONSTRUCTOR (declares pinout for Teensy 3.6 with the U8g2lib.h OLED library)
U8G2_SSD1306_128X64_NONAME_F_4W_SW_SPI u8g2(U8G2_R0, /* clock=*/ 13, /* data=*/ 11, /* cs=*/ 34, /* dc=*/ 32, /* reset=*/ 33);


/* keypad constants                  ____________________________________________________*/
const byte ROWS = 4; //four rows
const byte COLS = 4; //three columns
char keys[ROWS][COLS] = {
    {'1', '2', '3', '&'},
    {'4', '5', '6', '-'},
    {'7', '8', '9', 'S'},
    {'@', '0', 'T', 'E'}
};

byte rowPins[ROWS] = {9, 8, 7, 6}; //connect to the row pinouts of the keypad
byte colPins[COLS] = {5, 4, 3, 2}; //connect to the column pinouts of the keypad

Keypad keypad = Keypad( makeKeymap(keys), rowPins, colPins, ROWS, COLS );

int pgmModeSelectionInt = 0; // used to decide which mode is selected with integers 1-3 for now (more as modes expand)

char chOneKpdChar[5];           // first channel in commmand
int channelOneInt;                // storage for the array of characters into an integer

int intCount = 0;      // initializing the integer count at 0

char chTwoKpdChar[5];           // second channel in commmand
int channelTwoInt;                // storage for the array of characters into an integer

char intensityString[9];           // first channel in commmand
float kpdIntensityFloat;      // first intensity channel


//___________________________

// void setup() {
//     Dmx.setMode(TeensyDmx::DMX_OUT);  // Teensy DMX Declaration of Output
//     analogReadRes(16);
//     analogReadAveraging(8);
//     introPage(display);
//     delay(600);
//     u8g2.clearBuffer();
// }
//
// void loop() {
//        char key = keypad.getKey();
//        if (modeChosen == false){
//
//             if (key != NO_KEY) {
//                 kpdToCommand(key);
//
//             }
//         }else {
//         switch (controlMode) {
//                case FADER_MODE:
//                    u8g2.clearBuffer();
//                    fadersToDmxWscaler(16,9);
//
//
//                    break;
//
//                case KPD_MODE:
//                    if (key != NO_KEY) {
//                       kpdToCommand(key);
//                     }
//                     break;
//
//                 case KPDFADER_MODE:
//                     if (key != NO_KEY) {
//                         kpdToCommand(key);
//                     }
//                     break;
//
//                case ANIMATION_MODE:
//                      if (key != NO_KEY) {
//                         kpdToCommand(key);
//                     }
//                     break;
//             }
//         }
// }



void kpdToCommand(char key) {
    switch (key) {
            //___________________________________________________________________________________________________
        case '@':                       //  fall through switch for the '@' key with function trigger
        case 'T':                       //  fall through switch for the 'T' (through) key with function trigger
        case '&':                       //  fall through switch for the '&' key with function trigger
        case '-':                       //  fall through switch for the '-' key with function trigger
        case 'E':                       //  fall through switch for the 'E' key with function trigger
        case 'S':                       //  mapping for 'S' key with function trigger
            keypadLogic(false, key);
            break;
        case '0':                       //  fall through switch for the '0' key with function trigger
        case '1':                       //  fall through switch for the '1' key with function trigger
        case '2':                       //  fall through switch for the '2' key with function trigger
        case '3':                       //  fall through switch for the '3' key with function trigger
        case '4':                       //  fall through switch for the '4' key with function trigger
        case '5':                       //  fall through switch for the '5' key with function trigger
        case '6':                       //  fall through switch for the '6' key with function trigger
        case '7':                       //  fall through switch for the '7' key with function trigger
        case '8':                       //  fall through switch for the '8' key with function trigger
        case '9':                       //  mapping for '9' key with function trigger
            keypadLogic(true, key);
            break;
    }
}





void keypadLogic(bool isAnInteger, char kpdInput) {
    switch (kpdState) {
            //MODE_SELECT______________________________________
        case MODE_SELECT:
            if ((isAnInteger == false) && (pgmModeSelectionInt > 0)) {
                if (kpdInput == 'E') {
                    if (pgmModeSelectionInt == 1) {
                        smpleDisplay("Fader Mode", true, true);
                        controlMode = FADER_MODE;
                        kpdState = NO_CMD;
                        modeChosen = true;
                        break;
                    } if (pgmModeSelectionInt == 2) {
                        smpleDisplay("Keypad Mode", true, true);
                        controlMode = KPD_MODE;
                        kpdState = NO_CMD;
                        modeChosen = true;
                        break;
                    } if (pgmModeSelectionInt == 3) {
                        smpleDisplay("Keypad Fader Mode", true, true);
                        controlMode = KPDFADER_MODE;
                        kpdState = NO_CMD;
                        modeChosen = true;
                        break;
                    }  if (pgmModeSelectionInt == 4) {
                        smpleDisplay("Animation Mode", true, true);
                        controlMode = ANIMATION_MODE;
                        kpdState = NO_CMD;
                        modeChosen = true;
                        break;
                    }
                    kpdState = MODE_SELECT; // any non-integer than "Enter" won't work here
                    break;
                }
                kpdState = MODE_SELECT;
                break;
            } else {
                if (kpdInput == '1') {      //if 1 is entered
                    pgmModeSelectionInt = 1;
                    smpleDisplay(pgmModeSelectionInt, true, true);
                    kpdState = MODE_SELECT;
                    break;
                } if (kpdInput == '2') {     //if 2 is entered
                    pgmModeSelectionInt = 2;
                    smpleDisplay(pgmModeSelectionInt, true, true);
                    kpdState = MODE_SELECT;
                    break;
                } if (kpdInput == '3') {     //if 3 is entered
                    pgmModeSelectionInt = 3;
                    smpleDisplay(pgmModeSelectionInt, true, true);
                    kpdState = MODE_SELECT;
                    break;
                } if (kpdInput == '4') {     //if 4 is entered
                    pgmModeSelectionInt = 4;
                    smpleDisplay(pgmModeSelectionInt, true, true);
                    kpdState = MODE_SELECT;
                    break;
                } else {
                    smpleDisplay("Number not allowed", true, true);
                    kpdState = MODE_SELECT;
                    break;
                }
            }
            //NO_CMD______________________________________
        case NO_CMD:
            if (isAnInteger == false) {
                kpdState = NO_CMD;
                break;
            } else {
                if (kpdInput == '0') {      //don't count a zero as the first integer in the array
                    kpdState = NO_CMD;
                    break;
                }
                chOneKpdChar[intCount] = kpdInput;
                smpleDisplay(chOneKpdChar, true, true);
                intCount++;
                kpdState = DMXCH_ONE;
                break;
            }
            //___DMXCH_ONE____________________________________
        case DMXCH_ONE:                                                     // First Channel Assignment command
            if (isAnInteger == false) {                                     // is this an integer?
                /*___________AT__________________________*/
                if ((kpdInput == '@') && (intCount > 0)) { // if it is '@' and there are more than 0 integers
                    smpleDisplay(" @ ", true, true);
                    channelOneInt = atoi (chOneKpdChar);  //parse array into an int
                    if (channelOneInt > 512) {                    //greater than 512?? (one universe)
                        channelOneInt = 512;                      // max out channel number to 512
                    }
                    selectionType = SINGLECHANNEL;          // classify the command as soon as it is known
                    kpdState = DMX_INTENSITY;                  // move to the stage where we assign intensity
                    intCount = 0;      //zero the int Count
                    break;
                }
                /*___________THROUGH__________________________*/
                if ((kpdInput == 'T') && (intCount > 0)) { // if it is 'T' and there are more than 0 integers
                    smpleDisplay(" thru ", true, true);
                    channelOneInt = atoi (chOneKpdChar); //parse array into an int
                    if (channelOneInt > 512) {            //greater than 512?? (one universe)
                        channelOneInt = 512;              // max out channel number to 512
                    }
                    selectionType = THROUGH;   // enum & select proper bool index
                    kpdState = DMXCH_TWO;                  // move to the stage where you assign intensity
                    intCount = 0;      //zero the int Count
                    break;
                }
                /*___________AND__________________________*/
                if ((kpdInput == '&') && (intCount > 0)) { // if it is '&' and there are more than 0 integers
                    smpleDisplay(" and ", true, true);
                    channelOneInt = atoi (chOneKpdChar);    //parse array into an int
                    if (channelOneInt > 512) {
                        channelOneInt = 512;
                    }
                    selectionType = AND;   // enum & select proper bool index
                    kpdState = DMXCH_TWO;                  // move to the stage where you assign intensity
                    intCount = 0;      //parse and zero the int Count
                    break;
                }
                break;
                /*___________3 INTEGERS__________________________*/
            } else if (intCount == 2) {                          //more than 2 integer places
                chOneKpdChar[intCount - 2] = chOneKpdChar[intCount - 1]; //shifting values to next array position
                chOneKpdChar[intCount - 1] = chOneKpdChar[intCount]; //shifting values to next array position
                chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
                smpleDisplay(chOneKpdChar, true, true);
                kpdState = DMXCH_ONE;                   // keep wrapping digits in this controlModeuntil modifier
                intCount = 2;
                break;
                /*___________< 3 INTEGERS________________________*/
            } else {                                                 // if we aren't overflowing, do this
                chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
                smpleDisplay(chOneKpdChar, true, true);
                kpdState = DMXCH_ONE;                   // stay in this controlModeuntil modifier is pressed
                intCount++;
                break;                                          // leave the switch
            }
            //___DMXCH_TWO____________________________________
        case DMXCH_TWO:                  // Second Channel Assignment command
            if (isAnInteger == false) {                                     // is this an integer?
                /*___________AT__________________________*/
                if ((kpdInput == '@') && (intCount > 0)) { // if input = '@' and > than 0 integers
                    /*___________AND__________________________*/
                    if (selectionType == AND) {            // if input = 'AND'
                        smpleDisplay(" at ", true, true);
                        channelTwoInt = atoi (chTwoKpdChar);  //parse array into an int
                        if (channelTwoInt > 512) {            // if input > 512
                            channelTwoInt = 512;              // make it 512
                        }
                        kpdState = DMX_INTENSITY;                  // move to the stage where you assign intensity
                        intCount = 0;      //parse and zero the int Count
                        break;
                        /*___________THROUGH__________________________*/
                    } if (selectionType == THROUGH) {           // if input = 'THROUGH'
                        smpleDisplay(" at ", true, true);
                        channelTwoInt = atoi (chTwoKpdChar);      //parse array to an int
                        if (channelTwoInt > 512) {          // prevent values from going over the max
                            channelTwoInt = 512;
                        }
                        kpdState = DMX_INTENSITY;                  // move to the stage where you assign intensity
                        intCount = 0;      //parse and zero the int Count
                        break;
                    }
                }
                break;
                /*___________< 3 INTEGERS________________________*/
            } else if (intCount == 2) {                          //more than 2 integer places
                chTwoKpdChar[intCount - 2] = chTwoKpdChar[intCount - 1]; chTwoKpdChar[intCount - 1] = chTwoKpdChar[intCount]; //shifting values to next array position
                chTwoKpdChar[intCount] = kpdInput;   // adding the char to the array
                smpleDisplay(chTwoKpdChar, true, true);
                kpdState = DMXCH_TWO;                   // keep wrapping digits in this controlModeuntil modifier
                intCount = 2;
                break;
                /*___________< 3 INTEGERS________________________*/
            } else {                                                 // if we aren't overflowing, do this
                chTwoKpdChar[intCount] = kpdInput;   // adding the char to the array
                smpleDisplay(chTwoKpdChar, true, true);
                kpdState = DMXCH_TWO;                   // stay in this controlModeuntil modifier is pressed
                intCount++;
                break;                                          // leave the switch
            }

            //___DMX_INTENSITY____________________________________
        case DMX_INTENSITY:                  // Intensity Assignment Part of the Function
            if (isAnInteger == false) {
                if ((kpdInput == 'E') && (intCount > 0)) {
                    if (controlMode == KPD_MODE) {      // if it is in KPD_MODE control mode
                        if (selectionType == SINGLECHANNEL) {
                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, SINGLECHANNEL, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, SINGLECHANNEL, 0, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            selectionType = NONE; channelOneInt = 0; channelTwoInt = 0; intensityString[0] = '0';
                            break;
                        } if (selectionType == AND) {
                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, AND, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, AND, channelTwoInt, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            break;
                        } if (selectionType == THROUGH) {
                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, THROUGH, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, THROUGH, channelTwoInt, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            break;
                        }
                    } if (controlMode == KPDFADER_MODE) {     // if it is in KPDFADER_MODE control mode
                        if (selectionType == SINGLECHANNEL) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;
                            intCount = 0;
                            break;
                        } if (selectionType == AND) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;                  // move to the stage where you assign intensity
                            intCount = 0;
                            break;
                        } if (selectionType == THROUGH) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;                  // move to the stage where you assign intensity
                            intCount = 0;
                            break;
                        }
                    }
                } break;
                /*___________ONLY ALLOW 1 through 8 keys Representing Faders__________________________*/
            } else if ((controlMode == KPDFADER_MODE) && (kpdInput != '0') && (kpdInput != '9')) {
                intCount = 0;
                intensityString[intCount] = kpdInput;
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount = 1;
                break;
            }
            /*___________9 INTEGERS__________________________*/
            else if ((controlMode == KPD_MODE) && (intCount > 8 )) {
                intWrap(intensityString, kpdInput, 9);
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount = 9;
                break;
                /*___________>9 INTEGERS__________________________*/
            } else if ((controlMode == KPD_MODE) && (intCount < 9 )) {
                intensityString[intCount] = kpdInput;
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount++;
                break;
            }
    }
}










keypadIntensity(bool isAnInteger, char kpdInput, selectionType SINGLECHANNEL){
  if (isAnInteger == false) {
                if ((kpdInput == 'E') && (intCount > 0)) {
                    if (controlMode == KPD_MODE) {      // if it is in KPD_MODE control mode


                        if (selectionType == SINGLECHANNEL) {
                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, SINGLECHANNEL, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, SINGLECHANNEL, 0, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            selectionType = NONE; channelOneInt = 0; channelTwoInt = 0; intensityString[0] = '0';
                            break;
                        } if (selectionType == AND) {



                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, AND, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, AND, channelTwoInt, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            break;
                        } if (selectionType == THROUGH) {



                            intCount = 0;
                            kpdIntensityFloat = atof (intensityString);
                            dmxDisplay(channelOneInt, THROUGH, channelTwoInt, intensityString, true, true);
                            kpdSubIntensity(channelOneInt, THROUGH, channelTwoInt, kpdIntensityFloat);
                            kpdState = NO_CMD;
                            break;
                        }
                    } if (controlMode == KPDFADER_MODE) {     // if it is in KPDFADER_MODE control mode
                        if (selectionType == SINGLECHANNEL) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;
                            intCount = 0;
                            break;
                        } if (selectionType == AND) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;                  // move to the stage where you assign intensity
                            intCount = 0;
                            break;
                        } if (selectionType == THROUGH) {
                            int i = atoi (intensityString);
                            dmxDisplay(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]), true, true);
                            kpdfaderSubIntensity(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]));
                            kpdState = NO_CMD;                  // move to the stage where you assign intensity
                            intCount = 0;
                            break;
                        }
                    }
                } break;
                /*___________ONLY ALLOW 1 through 8 keys Representing Faders__________________________*/
            } else if ((controlMode == KPDFADER_MODE) && (kpdInput != '0') && (kpdInput != '9')) {
                intCount = 0;
                intensityString[intCount] = kpdInput;
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount = 1;
                break;
            }
            /*___________9 INTEGERS__________________________*/
            else if ((controlMode == KPD_MODE) && (intCount > 8 )) {
                intWrap(intensityString, kpdInput, 9);
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount = 9;
                break;
                /*___________>9 INTEGERS__________________________*/
            } else if ((controlMode == KPD_MODE) && (intCount < 9 )) {
                intensityString[intCount] = kpdInput;
                kpdState = DMX_INTENSITY;
                smpleDisplay(intensityString, true, true);
                intCount++;
                break;
            }
    }
}




int main () {
   // Local variable declaration:
   int a = 10;

   // while loop execution
   while( a < 20 ) {
      cout << "value of a: " << a << endl;
      a++;
   }

   return 0;
}
