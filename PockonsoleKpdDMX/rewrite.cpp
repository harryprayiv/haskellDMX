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

int currentNumCount = 0;      // initializing the integer count at 0
bool dmxSelection[512] = { false };    //enables non-destructive DMX kpd channel selection
int channelMap[512] = {0};  // map channels to create large submasters
char chOneKpdChar[5];     // first channel in commmand
int channelOneInt;   // storage for the array of characters into an integer

char chTwoKpdChar[5];  // second channel in commmand
int channelTwoInt;  // storage for the array of characters into an integer

char intensityString[9];        // first channel in commmand
float kpdIntensityFloat;   // first intensity channel

int pgmModeSelectionInt = 0; // used to decide which mode is selected with integers 1-3 for now (more as modes expand)


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

bool modeChosen = false; //mode set state

// _____________________SELECTION MODES__________________
enum selectionMode {
    NONE,
    SINGLECHANNEL,
    AND,
    THROUGH
};
selectionMode selectionType = NONE;

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
        //______________________________________________________
        case 'T':  //  fall through switch for the 'T' (through) key with function trigger
        case '&':  //  fall through switch for the '&' key with function trigger
        case '-':  //  fall through switch for the '-' key with function trigger
        case 'S': //  mapping for 'S' key with function trigger
            keypadLogic(key);
            break;
        case '@':  //  switch for the '@' key with function trigger
            kpdINTSYinput(key);
            break;
        case 'E':  //  fall through switch for the 'E' key with function trigger
            kpdENTER(key);
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
            kpdCHinput(key);
            break;
    }
}

void kpdCHinput(char kpdInput) {
    switch (kpdState) {
        case CMD_EMPTY:
            if (kpdInput == '0') {      //don't count a zero as the first integer in the array
                    break;
                }
                chOneKpdChar[currentNumCount] = kpdInput;
                currentNumCount++;
                break;
            }
        case DMXCH_A:
            if (currentNumCount == 2) {/*___________3 INTEGERS_______________*/
                chOneKpdChar[currentNumCount - 2] = chOneKpdChar[currentNumCount - 1]; //shifting values to next array position
                chOneKpdChar[currentNumCount - 1] = chOneKpdChar[currentNumCount];
                chOneKpdChar[currentNumCount] = kpdInput;   // adding the char to the array
                kpdState = DMXCH_A;    // keep wrapping digits in this controlModeuntil modifier
                currentNumCount = 2;
                break;
            } else { /*____________________< 3 INTEGERS________________*/
                chOneKpdChar[currentNumCount] = kpdInput;   // adding the char to the array
                kpdState = DMXCH_A; // stay in this controlModeuntil modifier is pressed
                currentNumCount++;
                break;    // leave the switch
            }
        case DMXCH_B:
            if (currentNumCount == 2) { /*___________3 INTEGERS_______________*/
                chTwoKpdChar[currentNumCount - 2] = chTwoKpdChar[currentNumCount - 1]; chTwoKpdChar[currentNumCount - 1] = chTwoKpdChar[currentNumCount]; //shifting values to next array position
                chTwoKpdChar[currentNumCount] = kpdInput;   // adding the char to the array
                currentNumCount = 2;
                break;
            } else {  /*____________________< 3 INTEGERS________________*/
                chTwoKpdChar[currentNumCount] = kpdInput;   // adding the char to the array
                currentNumCount++;
                break;                    // leave the switch
            }
        case DMX_INTENSITY:
            if (currentNumCount > 8 ) {
                intWrap(intensityString, kpdInput, 9);
                currentNumCount = 9;
                break;
                /*___________>9 INTEGERS__________________________*/
            } else if (currentNumCount < 9 ) {
                intensityString[currentNumCount] = kpdInput;
                currentNumCount++;
                break;
            }
    }


    void kpdINTSYinput(char kpdInput) {
        if ((controlMode == KPDFADER_MODE) && (kpdInput != '0') && (kpdInput != '9')) {
            currentNumCount = 0;
            intensityString[currentNumCount] = kpdInput;
            currentNumCount = 1;
            break;
        }
        /*___________9 INTEGERS__________________________*/
        else if ((controlMode == KPD_MODE) && (currentNumCount > 8 )) {
            intWrap(intensityString, kpdInput, 9);
            currentNumCount = 9;
            break;
            /*___________>9 INTEGERS__________________________*/
        } else if ((controlMode == KPD_MODE) && (currentNumCount < 9 )) {
            intensityString[currentNumCount] = kpdInput;
            currentNumCount++;
            break;
        }
    }
}

void kpdINTSYinput(char kpdInput) {

}

// number button presses (1,2,3,4,5,6,7,8,9,0)
void kpdNum(char cmdString[], int currentLength, char kpdInput){
        switch (kpdState) {
            //MODE_SELECT______________________________________
            case MODE_SELECT:
                if (cmdString == empty){
                // do nothing and print a warning
                break;
                }else(){
                // add 'input' to the string
                char c = kpdInput;
                strncat(cmdString, &c, 1);
                break;
                }

        }
}

// modifier button press (&,-,T,S, etc)
void kpdMOD(char cmdString[], int currentLength, char kpdInput){
  if (currentLength == 0){
    // do nothing and print a warning
    break;
  }else(){
    // add 'input' to the string
    char c = kpdInput;
    strncat(cmdString, &c, 1);
    break;
  }
}

// deal with a Enter button press
void kpdENT(char cmdString[], char kpdInput){
// take the boolen array and apply the
}

void kpdCmd((bool isAnInteger, char kpdInput, enum ){
//deal with
}



byte dmxVal[512];  // dmx levels for use in TeensyDMX
byte dmxValBuffer[512];    //place for storing values to transition to
int channelMap[512] = {0}; //The ability to map channels together to create large submasters
char intensityString[9];         // intensity string
bool dmxSelection[512] = { false };//non-destructive DMX kpd channel selection
int dmxSubMaster[512] = { 0 };//non-destructive DMX kpd channel submaster selection
char chOneKpdChar[5];           // first string
char chTwoKpdChar[5];           // second string
int pgmModeSelectionInt = 0; // used to decide which mode is selected with integers 1-3
int channelOneInt;                // storage for the array of characters into an integer
int currentNumCount = 0;      // initializing the integer count at 0
int channelTwoInt;                // storage for the array of characters into an integer
float kpdIntensityFloat;      // first intensity channel
