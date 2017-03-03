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

#include <Arduino.h> /*Arduino Includes*/
#include <TeensyDmx.h> /*Arduino Includes*/
#include <U8g2lib.h> /*U8G2 Includes*/
#include <U8x8lib.h> /*U8G2 Includes*/
#include <Keypad.h> /*Keypad Includes*/
#include <EasingLibrary.h>




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
const int analogFaderMap[analogFaders] = {1, 2, 3, 4, 5, 6, 7, 8, 9}; // this is to allow complex pin assignments
long dmxChannels = 512;           // intializing with a limiting to the number of values that can take up a DMX instruction

byte dmxVal[512];           //currently limiting to one universe, though that won't always be the case
bool dmxSelection[512] = { false };           //enables non-destructive DMX kpd channel selection using a for loop
int channelMap[512] = {0};                    //The beginning of being able to map channels together to create large submasters

float scalerVal;

// _________________________________ANIMATION MODES__________________________

enum transMode {
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

transMode transType = QUADRATIC_EASE;        // default curve is Quadratic

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

enum transubMode {          // controls which part of the transition has a curve applied to it
  IN,
  OUT,
  IN_OUT,
};

transubMode transPart = IN_OUT;  // intializing with both in and out with a curve on them.

// _________________________________SELECTION MODES__________________________
enum selectionMode {
  NONE,
  SINGLECHANNEL,
  AND,
  THROUGH
};
selectionMode selectionType = NONE;

// __________________________________PROGRAM MODES___________________________
enum pgmMode {
  FADER_MODE,
  KPD_MODE,
  KPDFADER_MODE
};
pgmMode controlMode;
bool modeChosen = false; //used to decide whether the mode has already been set


// __________________________________DISPLAY MODES___________________________
enum displayMode {
  POCKONSOLED,
  SERIALDISPLAY
};
displayMode display = POCKONSOLED;

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

void setup() {
  Dmx.setMode(TeensyDmx::DMX_OUT);  // Teensy DMX Declaration of Output
  analogReadRes(16);
  analogReadAveraging(8);
  introPage(display);
  delay(500);
  delay(100);
  u8g2.clearBuffer();
}

void loop() {
compareInterpolations(120);
  
//  drawInterpolation();

  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, LINEAR_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, CUBIC_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, EXPONENTIAL_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, QUADRATIC_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, QUARTIC_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, QUINTIC_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 0, 125, 120, SINE_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 40, 80, 120, BACK_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 40, 80, 120, BOUNCE_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 40, 80, 120, CIRCULAR_EASE, IN_OUT);
  //  interpolateDMXVals(1, THROUGH, 512, 40, 80, 120, ELASTIC_EASE, IN_OUT);

  //   char key = keypad.getKey();
  //   if (modeChosen == false){
  //
  //        if (key != NO_KEY) {
  //            kpdToCommand(key);
  //
  //        }
  //    }else {
  //    switch (controlMode) {
  //           case FADER_MODE:
  //               u8g2.clearBuffer();
  //               fadersToDmxWscaler(16,9);
  //
  //
  //               break;
  //
  //           case KPD_MODE:
  //               if (key != NO_KEY) {
  //                  kpdToCommand(key);
  //                }
  //                break;
  //
  //            case KPDFADER_MODE:
  //                if (key != NO_KEY) {
  //                    kpdToCommand(key);
  //                }
  //                break;
  //        }
  //    }
}


/*//a simple display for keys being entered
  void modeMenuDisplay(String charinput) {

  switch (display) {
  //POCKONSOLED______________________________________
  case POCKONSOLED:

  delay(1000);
  u8g2.clearBuffer();
  break;
  //SERIALDISPLAY______________________________________
  case SERIALDISPLAY:
  Serial.println(charinput);
  break;
  }

  }*/



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
          }
          kpdState = MODE_SELECT; // any non-integer than "Enter" won't work here
          break;
        }
        kpdState = MODE_SELECT;
        break;
      } else {
        if (kpdInput == '1') {      //don't count a zero as the first integer in the array
          pgmModeSelectionInt = 1;
          smpleDisplay(pgmModeSelectionInt, true, true);
          kpdState = MODE_SELECT;
          break;
        } if (kpdInput == '2') {     //2 equals
          pgmModeSelectionInt = 2;
          smpleDisplay(pgmModeSelectionInt, true, true);
          kpdState = MODE_SELECT;
          break;
        } if (kpdInput == '3') {     //don't count a zero as the first integer in the array
          pgmModeSelectionInt = 3;
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


// to call this function: faderSubIntensity(1, 27, THROUGH, 1) would get you a submaster of channels 1 through 27 on fader 1
// to call this function: faderSubIntensity(1, 27, AND, 1) would get you a submaster of channels 1 and 27 on fader 1
// to call this function: faderSubIntensity(1, 0, SINGLECHANNEL, 1) would get you a submaster of channel 1 on fader 1
void kpdfaderSubIntensity(int chOne, selectionMode selType, int chTwo, int sMfader) {
  switch (selType) {
    /*_______PARSING 'SINGLECHANNEL'__________*/
    case SINGLECHANNEL:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      dmxVal[chOne - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chOne - 1] < 2) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chOne - 1] = 0;
      }
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      break;
    /*_______PARSING 'AND'____________________*/
    case AND:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      dmxVal[chOne - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chOne - 1] < 2) {                // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chOne - 1] = 0;
      }
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      dmxVal[chTwo - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chTwo - 1] < 2) {                // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chTwo - 1] = 0;
      }
      Dmx.setDmxChannel(chTwo, dmxVal[chTwo - 1]);
      break;
    /*_______PARSING 'THROUGH'__________________*/
    case THROUGH:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      for (int i = (chOne - 1); i > (chTwo - chOne); ++i) {
        dmxVal[i] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
        if (dmxVal[i] < 2) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
          dmxVal[i] = 0;
        }
        Dmx.setDmxChannel((i + 1), dmxVal[i]);
      }
      break;
    case NONE:
      break;
  }
}

// to call this function: kpdSubIntensity(1, SINGLECHANNEL, 0, 100.00) would get you channel 1 @ 100% of 255
// to call this function: kpdSubIntensity(1, AND, 37, 66.666666) would get you channel 1 AND 37 @ 66% of 255
// to call this function: kpdSubIntensity(1, THROUGH, 37, 75.000) would get you channel 1 THROUGH 37 @ 75% of 255
void kpdSubIntensity(int chOne, selectionMode selType, int chTwo, float intensity) {
  switch (selType) {
    /*_______PARSING 'SINGLECHANNEL'__________*/
    case SINGLECHANNEL:
      dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      break;
    /*_______PARSING 'AND'____________________*/
    case AND:
      dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      dmxVal[chTwo - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chTwo, dmxVal[chTwo - 1]);
      break;
    /*_______PARSING 'THROUGH'__________________*/
    case THROUGH:
      if (chOne == chTwo) {
        dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
        Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      }
      if (chOne < chTwo) {
        for (int i = (chOne - 1); i > (chTwo - chOne); ++i) {       //loop through all channels and assign this intensity
          dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
          Dmx.setDmxChannel((i + 1), dmxVal[i]);
        }
        break;
      } if (chOne > chTwo) {
        for (int i = (chTwo - 1); i > (chOne - chTwo); ++i) {       //loop through all channels and assign this intensity
          dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
          Dmx.setDmxChannel((i + 1), dmxVal[i]);
        }
        break;
      }
    case NONE:
      break;
  }
}

//a simple display for keys being entered that allows me to prevent U8G2 from sending and clearing the buffer without affecting the serial functions.
void smpleDisplay(String charinput, bool clear, bool send) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();   // clear internal memory on the display
      }
      u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
      u8g2.setCursor(0, 10);
      u8g2.print(charinput);
      if (send == true) {
        u8g2.sendBuffer();   // transfer internal memory to the display
      }
      break;
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.println(charinput);
      break;
  }
}

//a simple display for keys being entered that allows me to prevent U8G2 from sending and clearing the buffer without affecting the serial functions.
void smpleDisplayWCursor(String charinput, int x, int y, bool clear, bool send) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();   // clear internal memory on the display
      }
      u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
      u8g2.setCursor(x, y);
      u8g2.print(charinput);
      if (send == true) {
        u8g2.sendBuffer();   // transfer internal memory to the display
      }
      break;
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.println(charinput);
      break;
  }
}

//a simple display for keys being entered that allows me to prevent U8G2 from sending and clearing the buffer without affecting the serial functions.
void smpleSquareAnimation(int x, int y, bool clear, bool send) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();   // clear internal memory on the display
      }
      u8g2.drawBox(3, 7, x, 15);
      if (send == true) {
        u8g2.sendBuffer();   // transfer internal memory to the display
      }
      break;
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.println("a fucking square, dude");
      break;
  }
}

//a simple display for faders to prevent U8G2 from sending and clearing the buffer without affecting the serial functions
void vertSquareAnimate(int x, int y, int xOffset, bool clear, bool send) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();   // clear internal memory on the display
      }
      u8g2.drawBox(x, y, xOffset, 65);
      if (send == true) {
        u8g2.sendBuffer();   // transfer internal memory to the display
      }
      break;
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.println("a fucking square, dude");
      break;
  }  
}


//a simple display for faders to prevent U8G2 from sending and clearing the buffer without affecting the serial functions.
void drawpixel(int x, int y, bool clear = false, bool send = false) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();   // clear internal memory on the display
      }
      u8g2.setDrawColor(1);
      u8g2.drawPixel(x, y);
      u8g2.drawPixel(x, y + 1);
      if (send == true) {
        u8g2.sendBuffer();   // transfer internal memory to the display
      }
      break;
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.println("a fucking square, dude");
      break;
  }
}

// a more complex display to allow for entire commands to be previewed
void dmxDisplay(int chOne, selectionMode selType, int chTwo, String intensity, bool clear, bool send) {
  switch (display) {
    //POCKONSOLED______________________________________
    case POCKONSOLED:
      if (clear == true) {
        u8g2.clearBuffer();
      }
      u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
      u8g2.setCursor(5, 10);
      u8g2.print(chOne);
      u8g2.print(" ");
      switch (selType) {
        case NONE:
          u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
          u8g2.setCursor(20, 10);
          u8g2.print("none");
          break;
        case SINGLECHANNEL:
          u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
          u8g2.setCursor(20, 10);
          u8g2.print("at");
          break;
        case AND:
          u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
          u8g2.setCursor(20, 10);
          u8g2.print("and");
          break;
        case THROUGH:
          u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
          u8g2.setCursor(20, 10);
          u8g2.print("thru");
          break;
      }
      if (selType == AND) {
        u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
        u8g2.setCursor(55, 10);
        u8g2.print(chTwo);
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
        u8g2.print("@");
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_5x8_mn);
        u8g2.setCursor(86, 10);
        u8g2.print(intensity);
        u8g2.sendBuffer();   // transfer internal memory to the display
        break;
      }
      if (selType == THROUGH) {
        u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
        u8g2.setCursor(55, 10);
        u8g2.print(chTwo);
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
        u8g2.print("@");
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_5x8_mn);
        u8g2.setCursor(86, 10);
        u8g2.print(intensity);
        u8g2.sendBuffer();   // transfer internal memory to the display
        break;
      } else {
        u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
        u8g2.setCursor(44, 10);
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
        u8g2.print("@");
        u8g2.print(" ");
        u8g2.setFont(u8g2_font_5x8_mn);
        u8g2.print(intensity);
        if (send == true) {
          u8g2.sendBuffer();   // transfer internal memory to the display;
        }
        break;
      }
    //SERIALDISPLAY______________________________________
    case SERIALDISPLAY:
      Serial.print(chOne);
      Serial.print(" ");
      switch (selType) {
        case NONE:
          Serial.print("How did you get here? Impossible, I thought.");
          break;
        case SINGLECHANNEL:
          Serial.print("@");
          break;
        case AND:
          Serial.print("And");
          break;
        case THROUGH:
          Serial.print("Through");
          break;
      }
      Serial.print(" ");
      if (selType != SINGLECHANNEL) {
        Serial.print(chTwo);
        Serial.print(" ");
        Serial.print(" @ ");
      }
      Serial.print(intensity);
      Serial.println(" ");
      break;
  }
}


void interpolateDMXVals(int chOne, selectionMode selType, int chTwo, float transStartIntensity, float transEndIntensity, int duration, transMode transType, transubMode transPart) {
  double currentTime = 0;
  double change = (transEndIntensity - transStartIntensity);
  double subdivisions = round((change / duration));
  double easedPosition;

  switch (transType) {
    //BACK_EASE______________________________________
    case BACK_EASE:
      back.setDuration(duration);
      back.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = back.easeIn(currentTime);
        smpleDisplayWCursor("Back", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Back", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;
    //BOUNCE_EASE______________________________________
    case BOUNCE_EASE:
      bounce.setDuration(duration);
      bounce.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = bounce.easeIn(currentTime);
        smpleDisplayWCursor("Bounce", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Bounce", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;         //CIRCULAR_EASE______________________________________
    case CIRCULAR_EASE:
      circular.setDuration(duration);
      circular.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = circular.easeInOut(currentTime);
        smpleDisplayWCursor("Circular", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Circular", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;         //CUBIC_EASE______________________________________
    case CUBIC_EASE:
      cubic.setDuration(duration);
      cubic.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = cubic.easeInOut(currentTime);
        smpleDisplayWCursor("Cubic", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Cubic", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;            //ELASTIC_EASE______________________________________
    case ELASTIC_EASE:
      elastic.setDuration(duration);
      elastic.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = elastic.easeInOut(currentTime);
        smpleDisplayWCursor("Elastic", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Elastic", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;            //EXPONENTIAL_EASE______________________________________
    case EXPONENTIAL_EASE:
      exponential.setDuration(duration);
      exponential.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = exponential.easeInOut(currentTime);
        smpleDisplayWCursor("Exponential", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Exponential", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;            //LINEAR_EASE______________________________________
    case LINEAR_EASE:
      linear.setDuration(duration);
      linear.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = linear.easeInOut(currentTime);
        smpleDisplayWCursor("Linear", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Linear", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;            //QUADRATIC_EASE______________________________________
    case QUADRATIC_EASE:
      quadratic.setDuration(duration);
      quadratic.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = quadratic.easeInOut(currentTime);
        smpleDisplayWCursor("Quadratic", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Quadratic", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;
    //QUARTIC_EASE______________________________________
    case QUARTIC_EASE:
      quartic.setDuration(duration);
      quartic.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = quartic.easeInOut(currentTime);
        smpleDisplayWCursor("Quartic", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Quartic", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;
    //QUINTIC_EASE______________________________________
    case QUINTIC_EASE:
      quintic.setDuration(duration);
      quintic.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = quintic.easeInOut(currentTime);
        smpleDisplayWCursor("Quintic", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Quintic", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;
    //SINE_EASE______________________________________
    case SINE_EASE:
      sine.setDuration(duration);
      sine.setTotalChangeInPosition(change);
      for (int i = 0; i <= duration; i++) {
        easedPosition = sine.easeInOut(currentTime);
        smpleDisplayWCursor("Sine", 10, 55, true, false);
        smpleSquareAnimation(easedPosition, 25, false, false);
        smpleDisplayWCursor(easedPosition, 10, 18, false, true);
        currentTime += subdivisions;
        delay(5);
      }
      smpleDisplayWCursor("Sine", 10, 55, true, false);
      smpleSquareAnimation(transEndIntensity, 25, false, true);
      delay(1000);
      break;
  }
}

// CURRENTLY BROKEN***************************************************
void drawInterpolation() {
  double duration;
  double currentTime;
  double easedPosition = 62;
  smpleDisplayWCursor("Linear", 1, 20, true, false);
  linear.setDuration(duration);
  linear.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, false, false);
  for (int i = 0; i <= duration; i++) {
    double currentTime = 0;
    smpleDisplayWCursor("Linear", 1, 20, false, true);
    easedPosition = linear.easeIn(currentTime);
    drawpixel(currentTime, (62 - easedPosition), false, false);
    currentTime ++;
  }
  smpleDisplayWCursor("Linear", 1, 20, false, true);
  delay(6);
  duration = 120;
  currentTime = 0;
  easedPosition = 62;
  smpleDisplayWCursor("Quadratic", 1, 20, true, false);
  quadratic.setDuration(duration);
  quadratic.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Quadratic", 1, 20, false, true);
    easedPosition = quadratic.easeIn(currentTime);
    drawpixel(currentTime, (120 - easedPosition), false, true);
    currentTime ++;
  }
  smpleDisplayWCursor("Quadratic", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Cubic", 1, 20, true, false);
  cubic.setDuration(duration);
  cubic.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Cubic", 1, 20, false, true);
    easedPosition = cubic.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, false);
    currentTime ++;
  }
  smpleDisplayWCursor("Cubic", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Quartic", 1, 20, true, false);
  quartic.setDuration(duration);
  quartic.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Quartic", 1, 20, false, true);
    easedPosition = quartic.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Quartic", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Sine", 1, 20, true, false);
  sine.setDuration(duration);
  sine.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Sine", 1, 20, false, true);
    easedPosition = sine.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Sine", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Quintic", 1, 20, true, false);
  quintic.setDuration(duration);
  quintic.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Quintic", 1, 20, false, true);
    easedPosition = quintic.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Quintic", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Exponential", 1, 20, true, false);
  exponential.setDuration(duration);
  exponential.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Exponential", 1, 20, false, true);
    easedPosition = exponential.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Exponential", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 52;
  smpleDisplayWCursor("Back", 1, 20, true, false);
  back.setDuration(duration);
  back.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 52, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Back", 1, 20, false, true);
    easedPosition = back.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Back", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 62;
  smpleDisplayWCursor("Bounce", 1, 20, true, false);
  bounce.setDuration(duration);
  bounce.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 62, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Bounce", 1, 20, false, true);
    easedPosition = bounce.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Bounce", 1, 20, false, true);
  delay(60);

  duration = 120;
  currentTime = 62;
  easedPosition = 52;
  elastic.setDuration(duration);
  elastic.setTotalChangeInPosition(duration);
  smpleDisplayWCursor("Elastic", 1, 20, true, false);
  drawpixel(currentTime, 42, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Elastic", 1, 20, false, true);
    easedPosition = elastic.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Elastic", 1, 20, false, true);
  delay(60);

  duration = 124;
  currentTime = 62;
  easedPosition = 52;
  smpleDisplayWCursor("Circular", 1, 20, true, false);
  circular.setDuration(duration);
  circular.setTotalChangeInPosition(duration);
  drawpixel(currentTime, 52, true, false);
  for (int i = 0; i <= duration; i++) {
    smpleDisplayWCursor("Circular", 1, 20, false, true);
    easedPosition = back.easeIn(currentTime);
    drawpixel(currentTime, (128 - easedPosition), false, true);
    currentTime ++;
    delay(2);
  }
  smpleDisplayWCursor("Circular", 1, 20, false, true);
  delay(60);
}

// CURRENTLY BRO
void compareInterpolations(double duration) {
  u8g2.clearBuffer();
  double currentTime = 0;
  double change = 62;
//  double subdivisions = round((change / duration));
  double easedPosition[9] = {0,0,0,0,0,0,0,0,0};

  linear.setDuration(duration);       /*Linear*/
  linear.setTotalChangeInPosition(change);
  quadratic.setDuration(duration);        /*Quadratic*/
  quadratic.setTotalChangeInPosition(change);
  quartic.setDuration(duration);        /*Quartic*/
  quartic.setTotalChangeInPosition(change);
  quintic.setDuration(duration);        /*Quintic*/
  quintic.setTotalChangeInPosition(change);
  exponential.setDuration(duration);        /*Exponential*/
  exponential.setTotalChangeInPosition(change);
  cubic.setDuration(duration);        /*Cubic*/
  cubic.setTotalChangeInPosition(change);
  sine.setDuration(duration);       /*Sine*/
  sine.setTotalChangeInPosition(change);
  bounce.setDuration(duration);       /*Bounce*/
  bounce.setTotalChangeInPosition(change);
  circular.setDuration(duration);       /*Circular*/
  circular.setTotalChangeInPosition(change);
  elastic.setDuration(duration);        /*Elastic*/
  elastic.setTotalChangeInPosition(change);
  for (int i = 0; i <= duration; i++) { // an attempt to get all of the interpolation algorithms firing at the same time
    easedPosition[0] = linear.easeIn(currentTime);
    easedPosition[1] = quadratic.easeIn(currentTime);
    easedPosition[2] = quartic.easeIn(currentTime);
    easedPosition[3] = quintic.easeIn(currentTime);
    easedPosition[4] = exponential.easeIn(currentTime);
    easedPosition[5] = cubic.easeIn(currentTime);
    easedPosition[6] = sine.easeIn(currentTime);
    easedPosition[7] = bounce.easeIn(currentTime);
    easedPosition[8] = circular.easeIn(currentTime);
    easedPosition[9] = elastic.easeIn(currentTime);
    for (int j = 0; 1 <= 9; j++){
      int k = ((j + 1) * j);
      int h = (k * 7);
      vertSquareAnimate(j, (60-(easedPosition[j])), h, false, false);
      
    }
    currentTime ++;
  }
  delay(6);
}

/* generic function for mapping the faders to 16 or 8 bit values for the LED box I am using */
void fadersToDmxWscaler(int bitRate, int masterFader) {
  u8g2.clearBuffer();
  if (bitRate == 16) {
    scalerVal = (floatmap(analogRead(analogFaderMap[(masterFader - 1)]), 1, 65536, 65025, 1) / 65025); // MasterFader__________
    if (scalerVal <= .01) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
      scalerVal = 0;
    };
    int i = 0;
    int pin = 0;
    while (i < ((analogFaders - 1) * 2)) { // cycle through analog faders with an algorithm to catch other cases
      int k = (i + 1);
      dmxVal[k] = round(floatmap(analogRead(analogFaderMap[pin]), 1.0, 65536.0, 255.0, 1.0) * scalerVal);
      dmxVal[i] = round((floatmap(analogRead(analogFaderMap[pin]), 1.0, 65536.0, 65025.0, 1.0) * scalerVal) / dmxVal[k]);
      if (dmxVal[i] < 3) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[i] = 0;
      };
      if (dmxVal[k] < 3) {            // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[k] = 0;
      };
      Dmx.setDmxChannel((i + 1), dmxVal[i]);
      Dmx.setDmxChannel((k + 1), dmxVal[k]);
      i = (i + 2);
      pin = (pin + 1);
    }
    u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
    u8g2.drawStr(2, 24, "A");
    u8g2.drawStr(26, 24, "B");
    u8g2.drawStr(50, 24, "C");
    u8g2.drawStr(72, 24, "D");
    u8g2.drawStr(2, 60, "E");
    u8g2.drawStr(26, 60, "F");
    u8g2.drawStr(50, 60, "G");
    u8g2.drawStr(72, 60, "H");
    u8g2.drawStr(94, 41, "All");

    u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
    u8g2.setCursor(0, 10);
    u8g2.print(dmxVal[8]);

    u8g2.setCursor(22, 10);
    u8g2.print(dmxVal[9]);

    u8g2.setCursor(44, 10);
    u8g2.print(dmxVal[10]);

    u8g2.setCursor(66, 10);
    u8g2.print(dmxVal[11]);

    u8g2.setCursor(0, 47);
    u8g2.print(dmxVal[12]);

    u8g2.setCursor(22, 47);
    u8g2.print(dmxVal[13]);

    u8g2.setCursor(44, 47);
    u8g2.print(dmxVal[14]);

    u8g2.setCursor(66, 47);
    u8g2.print(dmxVal[15]);

    u8g2.setCursor(93, 30);
    u8g2.print(scalerVal * 100);

    u8g2.drawLine(17, 0, 17, 64);
    u8g2.drawLine(39, 0, 39, 64);
    u8g2.drawLine(61, 0, 61, 64);
    u8g2.drawLine(83, 0, 83, 64);
    u8g2.drawLine(0, 30, 83, 30);
    u8g2.drawFrame(87, 21, 94, 24);

  } else {
    scalerVal = (floatmap(analogRead(masterFader), 1, 65536, 65025, 1) / 65025);   // Master Fader________________________________
    if (scalerVal <= .01) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
      scalerVal = 0;
    };

    for (int i = 0; i < (analogFaders - 1); ++i) {
      dmxVal[i] = round(floatmap(analogRead(i + 1), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[i] < 2) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[i] = 0;
      };
      Dmx.setDmxChannel((i + 1), dmxVal[i]);
    }
    u8g2.setFont(u8g2_font_5x8_mn);  // choose a suitable font
    u8g2.setCursor(0, 10);
    u8g2.print(dmxVal[0]);

    u8g2.setCursor(22, 10);
    u8g2.print(dmxVal[1]);

    u8g2.setCursor(44, 10);
    u8g2.print(dmxVal[2]);

    u8g2.setCursor(66, 10);
    u8g2.print(dmxVal[3]);

    u8g2.setCursor(0, 47);
    u8g2.print(dmxVal[4]);

    u8g2.setCursor(22, 47);
    u8g2.print(dmxVal[5]);

    u8g2.setCursor(44, 47);
    u8g2.print(dmxVal[6]);

    u8g2.setCursor(66, 47);
    u8g2.print(dmxVal[7]);

    u8g2.setCursor(93, 30);
    u8g2.print(scalerVal * 100);

    u8g2.setFont(u8g2_font_profont15_tf);  // choose a suitable font
    u8g2.drawStr(2, 24, "1");
    u8g2.drawStr(26, 24, "2");
    u8g2.drawStr(50, 24, "3");
    u8g2.drawStr(72, 24, "4");
    u8g2.drawStr(2, 60, "5");
    u8g2.drawStr(26, 60, "6");
    u8g2.drawStr(50, 60, "7");
    u8g2.drawStr(72, 60, "8");
    u8g2.drawStr(94, 41, "All");

    u8g2.drawLine(17, 0, 17, 64);
    u8g2.drawLine(39, 0, 39, 64);
    u8g2.drawLine(61, 0, 61, 64);
    u8g2.drawLine(83, 0, 83, 64);
    u8g2.drawLine(0, 30, 83, 30);
    u8g2.drawFrame(87, 21, 94, 24);
  }
}

/* Pockonsole Intro Animation */
void introPage(displayMode dispmode) {
  switch (dispmode) {
    case POCKONSOLED:
      u8g2.begin();
      delay(100);
      u8g2.setFontMode(1);  /* activate transparent font mode */
      u8g2.setDrawColor(1); /* color 1 for the box */
      u8g2.drawBox(0, 2, 128, 50);
      u8g2.setFont(u8g2_font_7x13_tf);
      u8g2.setDrawColor(0);
      u8g2.drawStr(5, 18, "Pockonsole");
      u8g2.setDrawColor(1);
      u8g2.drawStr(5, 33, "by Harry");
      u8g2.setDrawColor(2);
      u8g2.drawStr(5, 48, "Pray IV");
      //            u8g2.setFont(u8g2_font_helvB14_tf); // choose a font
      //            u8g2.drawStr(5, 21, "Pockonsole"); // write something to the internal memory
      //            u8g2.setFont(u8g2_font_baby_tf); // choose a font
      //            u8g2.drawStr(103, 9, "V 1.9"); // write something to the internal memory
      //            u8g2.setFont(u8g2_font_7x13_tf); // choose a font
      //            u8g2.drawStr(8, 36, "by Harry Pray IV"); // write something to the internal memory
      u8g2.sendBuffer();   // transfer internal memory to the display
      break;
    case SERIALDISPLAY:
      Serial.begin(9600);
      Serial.println("Pockonsole by Harry Pray IV");
      Serial.print("Version 2.0 Beta");
      break;
  }
}

/* custom mapping for floats, which come up often in intensity values and division */
float floatmap(float x, float in_min, float in_max, float out_min, float out_max)  {
  return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}

void intWrap(String numToWrap, char inputNum, int spaces) {
  for (int i = 0; (i = spaces); i++) {
    numToWrap[i] = numToWrap[(i + 1)];            //shifting values to next array position until we get to the alotted spaces
  }
  numToWrap[spaces] = inputNum;   // adding the char to the array
}
