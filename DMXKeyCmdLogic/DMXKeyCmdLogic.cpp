/*********************************************************************
 Harry Keypad V3
 ********************************************************************/
//#include <Arduino.h> /* Arduino Includes */
//#include <TeensyDmx.h> /* Teensy DMX Library */
//#include <Keypad.h> /* Keypad Library */

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

bool modeChosen = false; //mode set state

// _________________________________SELECTION MODES__________________________
enum selectionMode {
    NONE,
    SINGLECHANNEL,
    AND,
    THROUGH
};
selectionMode selectionType = NONE;

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

// __________________________________KEYPAD PROGRESS__________________________
enum kpdProgress {
    MODE_SELECT,
    NO_CMD,
    DMXCH_ONE,
    DMXCH_TWO,
    DMX_INTENSITY
};
kpdProgress kpdState = MODE_SELECT;

KpdParser::KpdParser(HardwareSerial& uart, RdmInit* rdm) :
    m_newFrame(false),
    m_rdmNeedsProcessing(false),
    m_rdmBuffer(),
    m_rdmChecksum(0),
    m_deviceLabel{0}
{

void setup() {
    // Dmx.setMode(TeensyDmx::DMX_OUT);  // Teensy DMX Declaration of Output
    // analogReadRes(16);
    // analogReadAveraging(8);
    // introPage(display);
    // delay(600);
    // u8g2.clearBuffer();
}

void loop() {
   // char key = keypad.getKey();
   // if (modeChosen == false){
   //
   //      if (key != NO_KEY) {
   //          kpdToCommand(key);
   //
   //      }
   //  }else {
   //  switch (controlMode) {
   //         case FADER_MODE:
   //             u8g2.clearBuffer();
   //             fadersToDmxWscaler(16,9);
   //
   //
   //             break;
   //
   //         case KPD_MODE:
   //             if (key != NO_KEY) {
   //                kpdToCommand(key);
   //              }
   //              break;
   //
   //          case KPDFADER_MODE:
   //              if (key != NO_KEY) {
   //                  kpdToCommand(key);
   //              }
   //              break;
   //
   //         case ANIMATION_MODE:
   //               if (key != NO_KEY) {
   //                  kpdToCommand(key);
   //              }
   //              break;
   //      }
   //  }
}

void kpdToCommand(char key) {
    switch (key) {
        //______________________________________________________________________________
        case '@':  //  fall through switch for the '@' key with function trigger
        case 'T':  //  fall through switch for the 'T' (through) key with function trigger
        case '&':  //  fall through switch for the '&' key with function trigger
        case '-':  //  fall through switch for the '-' key with function trigger
        case 'E':  //  fall through switch for the 'E' key with function trigger
        case 'S': //  mapping for 'S' key with function trigger
            keypadLogic(false, key);
            break;
        case '0': //  fall through switch for the '0' key with function trigger
        case '1': //  fall through switch for the '1' key with function trigger
        case '2': //  fall through switch for the '2' key with function trigger
        case '3': //  fall through switch for the '3' key with function trigger
        case '4': //  fall through switch for the '4' key with function trigger
        case '5': //  fall through switch for the '5' key with function trigger
        case '6': //  fall through switch for the '6' key with function trigger
        case '7': //  fall through switch for the '7' key with function trigger
        case '8': //  fall through switch for the '8' key with function trigger
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

// Mode Select
void kpdModeSelect(bool isAnInteger, char kpdInput){
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
}

//  KPD NoCMD
void kpdNoCMD(bool isAnInteger, char kpdInput){
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
}

// Keypad Input to Number
void kpdNum((bool isAnInteger, char kpdInput, enum ){
/*___________3 INTEGERS__________________________*/
    } if (intCount == 2) {       //more than 2 integer places
        chOneKpdChar[intCount - 2] = chOneKpdChar[intCount - 1]; //shifting values to next array position
        chOneKpdChar[intCount - 1] = chOneKpdChar[intCount]; //shifting values to next array position
        chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chOneKpdChar, true, true);
        kpdState = DMXCH_ONE;                   // keep wrapping digits in this controlModeuntil modifier
        intCount = 2;
        break;
        /*___________< 3 INTEGERS________________________*/
    } else {      // if we aren't overflowing, do this
        chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chOneKpdChar, true, true);
        kpdState = DMXCH_ONE;  // stay in this controlModeuntil modifier is pressed
        intCount++;
        break;    // leave the switch
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
