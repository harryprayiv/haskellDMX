/*********************************************************************
 Pockonsole Keypad DMX V1.1
 ********************************************************************/
//#include <Arduino.h> /* Arduino Includes */
#include <TeensyDmx.h> /* Teensy DMX Library */
#include <Keypad.h> /* Keypad Library */



/*Teensy DMX Settings_____________________________*/
#define DMX_REDE 24
TeensyDmx Dmx(Serial1, DMX_REDE);
/* keypad constants_______________________________*/
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

/*DMX Values____________________________________________*/
const long analogFaders = 9;            //there will always be the same number of pots (9)
const int analogFaderMap[analogFaders] = {1, 2, 3, 4, 5, 6, 7, 8, 9}; // this is to allow reconfigurable complex pin assignments
long dmxChannels = 512;  // intializing the number of values in a DMX instruction
float scalerVal;  // floating point scaler
byte dmxVal[512];           // limit to one universe
byte dmxValBuffer[512];    //place for storing values to transition to
int displayVal[512] = {0};  /* space for display values*/

int lengthCount = 0;  // initializing the integer count at 0
char chanAchars[5];   // first channel in commmand
char chanBchars[5];   // second channel in commmand
char intensityChars[9]; // intensity assignment variable
int chanAint;   // storage for the array of characters into an integer
int chanBint;  // storage for the array of characters into an integer
bool dmxSelection[512] = { false };    //enables non-destructive DMX kpd channel selection
int channelMap[512] = {0};  // map channels to create large submasters
float kpdIntensityFloat;   // first intensity channel

int pgmModeSelectionInt = 0; // mode selection with integers 1-3 for now (more as modes expand)
bool modeChosen = false; //mode set state
// ____________________KEYPAD PROGRESS_______________
enum kpdProgress {
    CMD_EMPTY,
    DMXCH_A,
    DMXCH_B,
    DMX_INTENSITY
};
kpdProgress kpdState = CMD_EMPTY;

// ________________________PROGRAM MODES_______________
enum pgmMode {
    FADER_MODE,
    KPD_MODE,
    KPDFADER_MODE,
    ANIMATION_MODE
};
pgmMode controlMode;

// _____________________SELECTION MODES__________________
enum selectionMode {
    NONE,
    SINGLECHANNEL,
    AND,
    THROUGH
};
selectionMode selectionType = NONE;

void setup() {
    Dmx.setMode(TeensyDmx::DMX_OUT);  // Teensy DMX Declaration of Output
    analogReadRes(16);
    analogReadAveraging(8);
}

void loop() {
char key = keypad.getKey();
kpdToCommand(key);
}

void kpdToCommand(char key) {
    switch (key) {
        //______________________________________________________
        case 'T':  //  fall through switch for the 'T' (through) key with function trigger
        case '&':  //  fall through switch for the '&' key with function trigger
        case '-':  //  fall through switch for the '-' key with function trigger
        case 'S': //  mapping for 'S' key with function trigger
            kpdMOD(key);
            break;
        case '@':  //  switch for the '@' key with function trigger
            kpdASSIGN();
            break;
        case 'E':  //  fall through switch for the 'E' key with function trigger
            kpdENTER();
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
        case '9': //  mapping for '9' key with function trigger
            kpdNUM(key);
            break;
    }
}

void kpdNUM(char kpdInput) {
    switch (kpdState) {
        case CMD_EMPTY:
            if (kpdInput == '0') {      //don't count a zero as the first integer in the array
                    break;
                }
                chanAchars[lengthCount] = kpdInput;
                lengthCount++;
                break;
            }
        case DMXCH_A:
            if (lengthCount == 2) {/*___________3 INTEGERS_______________*/
                chanAchars[lengthCount - 2] = chanAchars[lengthCount - 1]; //shifting to next array position
                chanAchars[lengthCount - 1] = chanAchars[lengthCount];
                chanAchars[lengthCount] = kpdInput;   // adding the char to the array
                kpdState = DMXCH_A;    // keep wrapping digits in this controlModeuntil modifier
                lengthCount = 2;
                break;
            } else { /*___________< 3 INTEGERS________________*/
                chanAchars[lengthCount] = kpdInput;   // adding the char to the array
                kpdState = DMXCH_A; // stay in this controlModeuntil modifier is pressed
                lengthCount++;
                break;    // leave the switch
            }
        case DMXCH_B:
            if (lengthCount == 2) { /*___________3 INTEGERS_______________*/
                chanBchars[lengthCount - 2] = chanBchars[lengthCount - 1]; chanBchars[lengthCount - 1] = chanBchars[lengthCount]; //shifting values to next array position
                chanBchars[lengthCount] = kpdInput;   // adding the char to the array
                lengthCount = 2;
                break;
            } else {  /*___________< 3 INTEGERS________________*/
                chanBchars[lengthCount] = kpdInput;   // adding the char to the array
                lengthCount++;
                break;                    // leave the switch
            }
        case DMX_INTENSITY:
            if (lengthCount > 8 ) {
                intWrap(intensityChars, kpdInput, 9);
                lengthCount = 9;
                break;

            } else if (lengthCount < 9 ) { /*___________>9 INTEGERS______________*/
                intensityChars[lengthCount] = kpdInput;
                lengthCount++;
                break;
            }
}

// deal with  '@' button logic
void kpdASSIGN() {
    if ((controlMode == KPDFADER_MODE) && (kpdInput != '0') && (kpdInput != '9')) {
        lengthCount = 0;
        intensityChars[lengthCount] = kpdInput;
        lengthCount = 1;
        break;
    }
    /*___________9 INTEGERS__________________________*/
    else if ((controlMode == KPD_MODE) && (lengthCount > 8 )) {
        intWrap(intensityChars, kpdInput, 9);
        lengthCount = 9;
        break;
        /*___________>9 INTEGERS__________________________*/
    } else if ((controlMode == KPD_MODE) && (lengthCount < 9 )) {
        intensityChars[lengthCount] = kpdInput;
        lengthCount++;
        break;
    }
}

// deal with  modifier ('T', '&', '-', 'S') logic
void kpdMOD(key){

}

// deal with a Enter button press
void kpdENTER() {
// take the boolen array and assemble and parse the entire string
}

void kpdSubIntensity(int chOne, selectionMode selType, int chTwo, float intensity) {
    switch (selType) {
        case SINGLECHANNEL: /*_______PARSING 'SINGLECHANNEL'__________*/
            dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255));
            Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
            break;
        case AND: /*_______PARSING 'AND'____________________*/
            dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255));
            Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
            dmxVal[chTwo - 1] = round(floatmap(intensity, 0, .99999999, 0, 255));
            Dmx.setDmxChannel(chTwo, dmxVal[chTwo - 1]);
            break;
        case THROUGH: /*_______PARSING 'THROUGH'__________________*/
            if (chOne == chTwo) {
                dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255));
                Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
            }
            if (chOne < chTwo) {
                for (int i = (chOne - 1); i > (chTwo - chOne); ++i) {
                    dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
                    Dmx.setDmxChannel((i + 1), dmxVal[i]);
                }
                break;
            } if (chOne > chTwo) {
                for (int i = (chTwo - 1); i > (chOne - chTwo); ++i) {
                    dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
                    Dmx.setDmxChannel((i + 1), dmxVal[i]);
                }
                break;
            }
        case NONE:
            break;
    }
}

void kpdBoolDMX(bool selection, float intensity) {
    if (selection == true) {
            dmxVal = round(floatmap(intensity, 0, .99999999, 0, 255));
            Dmx.setDmxChannel(chOne, dmxVal);
            break;
        }

}

/* floats mapping */
float floatmap(float x, float in_min, float in_max, float out_min, float out_max)  {
    return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}


/* wrapping integers with number of desired digits */
void intWrap(String numToWrap, char inputNum, int spaces) {
    for (int i = 0; (i = spaces); i++) {
        numToWrap[i] = numToWrap[(i + 1)];            //shifting values to next array position until we get to the alotted spaces
    }
    numToWrap[spaces] = inputNum;   // adding the char to the array
}
